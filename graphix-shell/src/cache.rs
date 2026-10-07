//! The image cache. The executable that reads an image is the
//! executable that wrote it, on its first run: entries live under the
//! cache directory by the executable's build id, which also covers the
//! packages compiled into it, keyed by the root that declares them and
//! the image format, so a rebuilt executable misses and recompiles; an
//! executable with no build id has no cache. A program's entry adds the
//! program (its file's path, or an embedded program's text) to the key
//! and carries the sources its compile read, re-verified on load.
//! Entries are written to a temporary file and renamed into place; a
//! load touches its entry, and the entries unused longest are collected
//! when one is written.

use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{
    expr::{Origin, Source},
    image,
};
use graphix_rt::ProgramImage;
use log::{info, warn};
use netidx_core::utils::make_sha3_token;
use std::{
    fmt::Write,
    fs,
    path::{Path as FsPath, PathBuf},
    sync::atomic::{AtomicU64, Ordering},
    time::{Duration, SystemTime},
};

/// The bytes the entries of every build id may hold together.
const KEEP_BYTES: u64 = 512 << 20;
/// An entry unused this long is collected whatever the total.
const KEEP_UNUSED: Duration = Duration::from_secs(30 * 24 * 3600);
/// A temporary file this old is a writer that died.
const ABANDONED: Duration = Duration::from_secs(3600);
/// A program entry opens with its sources: this magic, their length
/// (u64 LE), the text, and zeros to a multiple of `IMAGE_ALIGN`.
const DEPS_MAGIC: &[u8; 8] = b"GXDEPS\0\0";
const IMAGE_ALIGN: usize = 64;

pub(crate) struct RegistrationCache {
    root: PathBuf,
    build: String,
    registration: String,
    program: Option<String>,
}

/// Which entry: the packages' registration alone, or the session with
/// the program compiled in.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Entry {
    Registration,
    Program,
}

fn hex(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        let _ = write!(s, "{b:02x}");
    }
    s
}

fn digest(parts: &[&[u8]]) -> String {
    hex(&make_sha3_token(parts.iter().copied())[..16])
}

/// The running executable's per-build id: the GNU build id note an ELF
/// linker stamps, a Mach-O `LC_UUID`, or a PE link stamp and size.
fn build_id() -> Option<String> {
    let file = fs::File::open(std::env::current_exe().ok()?).ok()?;
    // Mapped, so only the pages the walk touches are read: the headers
    // and the note, not the executable.
    let file = unsafe { memmap2::Mmap::map(&file) }.ok()?;
    elf_build_id(&file).or_else(|| macho_build_id(&file)).or_else(|| pe_build_id(&file))
}

fn u16_le(b: &[u8], i: usize) -> Option<u16> {
    Some(u16::from_le_bytes(b.get(i..i + 2)?.try_into().ok()?))
}

fn u32_le(b: &[u8], i: usize) -> Option<u32> {
    Some(u32::from_le_bytes(b.get(i..i + 4)?.try_into().ok()?))
}

fn u64_le(b: &[u8], i: usize) -> Option<u64> {
    Some(u64::from_le_bytes(b.get(i..i + 8)?.try_into().ok()?))
}

fn u32_be(b: &[u8], i: usize) -> Option<u32> {
    Some(u32::from_be_bytes(b.get(i..i + 4)?.try_into().ok()?))
}

fn u64_be(b: &[u8], i: usize) -> Option<u64> {
    Some(u64::from_be_bytes(b.get(i..i + 8)?.try_into().ok()?))
}

/// A PE executable's COFF `TimeDateStamp` and its length: the stamp is
/// the link time, or a hash of the contents under a reproducible link,
/// and the length separates two builds that share one. Any malformed
/// field is `None`, never a panic.
fn pe_build_id(file: &[u8]) -> Option<String> {
    if file.get(..2)? != b"MZ" {
        return None;
    }
    let pe = u32_le(file, 0x3c)? as usize;
    if file.get(pe..pe + 4)? != b"PE\0\0" {
        return None;
    }
    let stamp = u32_le(file, pe + 8)?;
    Some(format!("pe-{stamp:08x}-{:x}", file.len()))
}

/// A 64-bit Mach-O's `LC_UUID`, or for a universal binary its first
/// slice's. Any malformed field is `None`, never a panic.
fn macho_build_id(file: &[u8]) -> Option<String> {
    const LC_UUID: u32 = 0x1b;
    let thin = match file.get(..4)? {
        [0xca, 0xfe, 0xba, 0xbe] => {
            let (off, len) = (u32_be(file, 16)? as usize, u32_be(file, 20)? as usize);
            file.get(off..off.checked_add(len)?)?
        }
        [0xca, 0xfe, 0xba, 0xbf] => {
            let (off, len) = (u64_be(file, 16)? as usize, u64_be(file, 24)? as usize);
            file.get(off..off.checked_add(len)?)?
        }
        _ => file,
    };
    if thin.get(..4)? != [0xcf, 0xfa, 0xed, 0xfe] {
        return None;
    }
    let mut at = 32;
    for _ in 0..u32_le(thin, 16)? {
        let (cmd, size) = (u32_le(thin, at)?, u32_le(thin, at + 4)? as usize);
        if cmd == LC_UUID {
            return Some(format!("macho-{}", hex(thin.get(at + 8..at + 24)?)));
        }
        if size == 0 {
            return None;
        }
        at = at.checked_add(size)?;
    }
    None
}

/// Walk the ELF64 program headers for a `PT_NOTE` holding
/// `NT_GNU_BUILD_ID`. Any malformed field is `None`, never a panic.
fn elf_build_id(file: &[u8]) -> Option<String> {
    if file.get(..4)? != b"\x7fELF" || file.get(4)? != &2 || file.get(5)? != &1 {
        return None;
    }
    let phoff = u64_le(file, 32)? as usize;
    let phentsize = u16_le(file, 54)? as usize;
    let phnum = u16_le(file, 56)? as usize;
    for i in 0..phnum {
        let ph = file.get(phoff + i * phentsize..)?;
        if u32_le(ph, 0)? != 4 {
            continue;
        }
        let offset = u64_le(ph, 8)? as usize;
        let size = u64_le(ph, 32)? as usize;
        let mut notes = file.get(offset..offset + size)?;
        while notes.len() >= 12 {
            let namesz = u32_le(notes, 0)? as usize;
            let descsz = u32_le(notes, 4)? as usize;
            let ntype = u32_le(notes, 8)?;
            let name_end = 12 + namesz;
            let desc_start = (name_end + 3) & !3;
            let desc_end = desc_start + descsz;
            if ntype == 3 && notes.get(12..name_end)? == b"GNU\0" {
                return Some(hex(notes.get(desc_start..desc_end)?));
            }
            notes = notes.get((desc_end + 3) & !3..)?;
        }
    }
    None
}

/// The program entry's record of the sources its compile read: a line
/// `+<digest> <path>` per file read, and `-<path>` per file whose
/// appearance would change what the compile read (an interface beside
/// a module, a `m.gx` beside the `m/mod.gx` it would win over). `None`
/// when it read a source no file vouches for on the next start.
fn sources_record(sources: &[triomphe::Arc<Origin>]) -> Option<String> {
    let mut read: Vec<(&FsPath, &Origin)> = vec![];
    for o in sources {
        match &o.source {
            Source::File(p) => read.push((p, o)),
            // the packages are the build's, an embedded program is the key
            Source::Internal(_) | Source::Unspecified => (),
            Source::Netidx(p) => {
                info!("no program entry: the program reads {p} from netidx");
                return None;
            }
        }
    }
    let mut record = String::new();
    let absent = |p: PathBuf, record: &mut String| {
        if !read.iter().any(|(r, _)| *r == p) {
            let _ = writeln!(record, "-{}", p.display());
        }
    };
    for (p, o) in &read {
        let _ = writeln!(record, "+{} {}", digest(&[o.text.as_bytes()]), p.display());
    }
    for (p, o) in &read {
        if p.extension().is_some_and(|e| e == "gxi") {
            continue;
        }
        absent(p.with_extension("gxi"), &mut record);
        if o.parent.is_some()
            && p.file_name().is_some_and(|n| n == "mod.gx")
            && let Some(dir) = p.parent()
        {
            absent(dir.with_extension("gx"), &mut record);
        }
    }
    Some(record)
}

/// Whether every source `record` lists is as the compile read it.
fn sources_unchanged(record: &str) -> bool {
    record.lines().all(|line| match line.split_at_checked(1) {
        Some(("-", path)) => {
            matches!(fs::symlink_metadata(path), Err(e) if e.kind() == std::io::ErrorKind::NotFound)
        }
        Some(("+", rest)) => match rest.split_once(' ') {
            Some((want, path)) => {
                fs::read(path).is_ok_and(|text| digest(&[&text]) == want)
            }
            None => false,
        },
        _ => false,
    })
}

static TEMP: AtomicU64 = AtomicU64::new(0);

impl RegistrationCache {
    /// The cache entries for this root and, when it is one a later start
    /// can find again, the program; an error when there is no cache
    /// directory or no build id to file the entries under.
    pub(crate) fn new(root: &str, program: Option<&Source>, flags: u64) -> Result<Self> {
        let root_dir = dirs::cache_dir()
            .ok_or_else(|| anyhow!("no cache directory"))?
            .join("graphix")
            .join("registration");
        let build =
            build_id().ok_or_else(|| anyhow!("the executable has no build id"))?;
        let format = [image::REGISTRATION_FORMAT];
        let flags = flags.to_le_bytes();
        let registration = digest(&[&format, root.as_bytes()]);
        // CR claude for claude: [bug] The program entry is keyed by the root file's bytes
        // alone. The modules the compile read (`mod m;` files beside the script,
        // GRAPHIX_MODPATH, netidx) and the script's path are not in the key, and
        // nothing re-checks them on load. A warm start therefore runs stale module code
        // after an edit, and it hides a type error, parse error or deleted module that
        // --no-cache and --check refuse. A byte-identical main.gx in another directory
        // runs the first project's modules and reports the first project's path in its
        // error origins. design/program_image.md specifies a depfile re-verified on the
        // next run: record (path, hash) of every source the compile read plus the
        // canonical script path, treat any mismatch as a miss, and hash the bytes
        // RootFile::load parsed, not the separate read at lib.rs:255. probe:
        // design/review-2026-10-05/repro/x-image-01.sh (x-image-01)
        // 2026-10-07 claude: the program entry is keyed by the script's path (or an
        // embedded program's text) and carries sources_record: each file the compile
        // parsed with its digest, from the origins the resolved program holds, plus
        // the interface and `m.gx` that would change a module's resolution if they
        // appeared; load re-verifies it, and a netidx module means no entry. Pin:
        // cache::tests::a_program_entry_misses_when_a_source_changes. What remains: a
        // file added in a search directory ahead of the one a module came from (the
        // script's own, ahead of GRAPHIX_MODPATH) shadows it unseen, since an origin
        // does not record the search that found it.
        let program = program.and_then(|p| {
            let id = match p {
                Source::File(path) => path.to_string_lossy().into_owned().into_bytes(),
                Source::Internal(text) => text.as_bytes().to_vec(),
                Source::Netidx(_) | Source::Unspecified => return None,
            };
            // the search path decides which file a module name reaches
            let modpath = std::env::var_os("GRAPHIX_MODPATH").unwrap_or_default();
            let modpath = modpath.as_encoded_bytes();
            Some(digest(&[
                &format,
                root.as_bytes(),
                &flags,
                &[p.is_file() as u8],
                &id,
                modpath,
            ]))
        });
        Ok(RegistrationCache { root: root_dir, build, registration, program })
    }

    pub(crate) fn has_program(&self) -> bool {
        self.program.is_some()
    }

    fn dir(&self) -> PathBuf {
        self.root.join(&self.build)
    }

    fn key(&self, entry: Entry) -> Option<&str> {
        match entry {
            Entry::Registration => Some(&self.registration),
            Entry::Program => self.program.as_deref(),
        }
    }

    fn path(&self, entry: Entry) -> Option<PathBuf> {
        Some(self.dir().join(format!("{}.img", self.key(entry)?)))
    }

    /// The entry mapped into memory, with its path: only the pages a
    /// restore touches are read, and an instance decoded later reads its
    /// own. An entry is never rewritten in place (written to a temporary
    /// file and renamed), so the mapping stays valid. A program entry
    /// whose sources changed is a miss.
    pub(crate) fn load(&self, entry: Entry) -> Option<(ArcStr, Bytes)> {
        let path = self.path(entry)?;
        let file = match fs::File::open(&path) {
            Ok(f) => f,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return None,
            Err(e) => {
                warn!("opening the {entry:?} image {}: {e}", path.display());
                return None;
            }
        };
        let bytes = match unsafe { memmap2::Mmap::map(&file) } {
            Ok(map) => Bytes::from_owner(map),
            Err(e) => {
                warn!("mapping the {entry:?} image {}: {e}", path.display());
                return None;
            }
        };
        let image = match entry {
            Entry::Registration => bytes,
            Entry::Program => {
                let image = (|| {
                    if bytes.get(..8)? != DEPS_MAGIC {
                        return None;
                    }
                    let len = u64_le(&bytes, 8)? as usize;
                    let record =
                        std::str::from_utf8(bytes.get(16..16usize.checked_add(len)?)?)
                            .ok()?;
                    let start = (16 + len).next_multiple_of(IMAGE_ALIGN);
                    (sources_unchanged(record) && start <= bytes.len())
                        .then(|| bytes.slice(start..))
                })();
                match image {
                    Some(image) => image,
                    None => {
                        info!("{entry:?} image {} is stale", path.display());
                        return None;
                    }
                }
            }
        };
        let _ = file.set_modified(SystemTime::now());
        info!("{entry:?} image {}", path.display());
        Some((ArcStr::from(path.display().to_string()), image))
    }

    pub(crate) fn store_registration(&self, image: &[u8]) -> Result<()> {
        self.store(Entry::Registration, &[image])
    }

    /// Write the program entry, unless its compile read a source the
    /// next start cannot verify.
    pub(crate) fn store_program(&self, program: &ProgramImage) -> Result<()> {
        let Some(record) = sources_record(&program.sources) else { return Ok(()) };
        let mut head =
            Vec::with_capacity((16 + record.len()).next_multiple_of(IMAGE_ALIGN));
        head.extend_from_slice(DEPS_MAGIC);
        head.extend_from_slice(&(record.len() as u64).to_le_bytes());
        head.extend_from_slice(record.as_bytes());
        head.resize(head.len().next_multiple_of(IMAGE_ALIGN), 0);
        self.store(Entry::Program, &[&head, &program.image])
    }

    /// Write the entry, then collect what has gone unused longest.
    fn store(&self, entry: Entry, parts: &[&[u8]]) -> Result<()> {
        let path = self.path(entry).ok_or_else(|| anyhow!("no {entry:?} entry"))?;
        let dir = self.dir();
        fs::create_dir_all(&dir)
            .with_context(|| format!("creating {}", dir.display()))?;
        let tmp = dir.join(format!(
            "{}.img.{}.{}",
            self.key(entry).unwrap_or_default(),
            std::process::id(),
            TEMP.fetch_add(1, Ordering::Relaxed)
        ));
        let written = (|| {
            use std::io::Write;
            let mut f = fs::File::create(&tmp)?;
            for part in parts {
                f.write_all(part)?;
            }
            f.sync_all()?;
            fs::rename(&tmp, &path)
        })();
        if let Err(e) = written {
            let _ = fs::remove_file(&tmp);
            bail!("writing {}: {e}", path.display());
        }
        info!("{entry:?} image written to {}", path.display());
        self.collect();
        Ok(())
    }

    /// Remove abandoned temporary files, entries unused past
    /// `KEEP_UNUSED`, and the least recently used past `KEEP_BYTES`,
    /// across every build id; then the build ids left empty.
    fn collect(&self) {
        let now = SystemTime::now();
        let age = |m: &fs::Metadata| {
            m.modified().ok().and_then(|t| now.duration_since(t).ok()).unwrap_or_default()
        };
        let mut entries = vec![];
        for build in fs::read_dir(&self.root).into_iter().flatten().flatten() {
            for f in fs::read_dir(build.path()).into_iter().flatten().flatten() {
                let Ok(m) = f.metadata() else { continue };
                if f.path().extension().is_some_and(|e| e == "img") {
                    entries.push((age(&m), m.len(), f.path()));
                } else if age(&m) > ABANDONED {
                    let _ = fs::remove_file(f.path());
                }
            }
        }
        entries.sort_unstable_by_key(|(age, ..)| *age);
        let mut kept = 0;
        for (age, len, path) in entries {
            kept += len;
            if age > KEEP_UNUSED || kept > KEEP_BYTES {
                let _ = fs::remove_file(path);
            }
        }
        for build in fs::read_dir(&self.root).into_iter().flatten().flatten() {
            // only an empty directory is removed
            let _ = fs::remove_dir(build.path());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The test binary is linked with a build id on Linux; the walker
    /// finds it and it is stable across calls.
    #[test]
    fn build_id_is_read_from_the_executable() {
        let id = build_id();
        assert_eq!(id, build_id());
        if cfg!(target_os = "linux") {
            let id = id.unwrap();
            assert_eq!(id.len(), 40, "{id}");
        }
    }

    fn thin_macho(uuid: [u8; 16]) -> Vec<u8> {
        let mut b = vec![0xcf, 0xfa, 0xed, 0xfe];
        b.resize(32, 0);
        b[16..20].copy_from_slice(&2u32.to_le_bytes());
        // LC_SEGMENT_64 stand-in, then LC_UUID
        b.extend_from_slice(&0x19u32.to_le_bytes());
        b.extend_from_slice(&16u32.to_le_bytes());
        b.extend_from_slice(&[0; 8]);
        b.extend_from_slice(&0x1bu32.to_le_bytes());
        b.extend_from_slice(&24u32.to_le_bytes());
        b.extend_from_slice(&uuid);
        b
    }

    #[test]
    fn macho_uuid_is_the_build_id() {
        let uuid = *b"0123456789abcdef";
        let want = format!("macho-{}", hex(&uuid));
        let thin = thin_macho(uuid);
        assert_eq!(macho_build_id(&thin).as_deref(), Some(want.as_str()));
        let mut fat = vec![0xca, 0xfe, 0xba, 0xbe, 0, 0, 0, 1];
        fat.extend_from_slice(&[0; 8]);
        fat.extend_from_slice(&64u32.to_be_bytes());
        fat.extend_from_slice(&(thin.len() as u32).to_be_bytes());
        fat.resize(64, 0);
        fat.extend_from_slice(&thin);
        assert_eq!(macho_build_id(&fat).as_deref(), Some(want.as_str()));
        assert_eq!(macho_build_id(&thin[..40]), None);
    }

    #[test]
    fn a_program_entry_misses_when_a_source_changes() {
        let dir = tempfile::tempdir().unwrap();
        let main = dir.path().join("main.gx");
        let m = dir.path().join("m.gx");
        fs::write(&main, "mod m;\nm::x").unwrap();
        fs::write(&m, "let x = 1").unwrap();
        let file = |p: &FsPath, parent: Option<triomphe::Arc<Origin>>| {
            triomphe::Arc::new(Origin {
                parent,
                source: Source::File(p.to_path_buf()),
                text: ArcStr::from(fs::read_to_string(p).unwrap()),
            })
        };
        let root = file(&main, None);
        let record = sources_record(&[root.clone(), file(&m, Some(root))]).unwrap();
        assert!(sources_unchanged(&record), "{record}");
        fs::write(&m, "let x = 2").unwrap();
        assert!(!sources_unchanged(&record), "an edited module");
        fs::write(&m, "let x = 1").unwrap();
        fs::write(dir.path().join("m.gxi"), "val x: i64").unwrap();
        assert!(!sources_unchanged(&record), "an interface added beside a module");
        fs::remove_file(dir.path().join("m.gxi")).unwrap();
        assert!(sources_unchanged(&record), "{record}");
        fs::remove_file(&m).unwrap();
        assert!(!sources_unchanged(&record), "a deleted module");
    }
}
