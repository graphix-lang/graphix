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
use poolshark::local::LPooled;
use std::{
    ffi::OsStr,
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

/// The parts' hash, each framed by its length, so no two lists of parts
/// read alike.
fn digest(parts: &[&[u8]]) -> String {
    let lens: LPooled<Vec<[u8; 8]>> =
        parts.iter().map(|p| (p.len() as u64).to_le_bytes()).collect();
    let framed = lens.iter().zip(parts).flat_map(|(len, p)| [&len[..], *p]);
    hex(&make_sha3_token(framed)[..16])
}

/// The running executable's per-build id: the GNU build id note an ELF
/// linker stamps, a Mach-O `LC_UUID`, or a PE link stamp and size.
fn build_id() -> Option<String> {
    // CR claude for claude: [risk/low] `current_exe()` returns a path, so if the binary
    // is replaced between that call and the open (cargo relinking it, a package
    // upgrade), this process reads the new binary's build id. It then files its images
    // under that id, and the new binary later restores images written by different
    // code. The checksum and ISA checks do not catch that. On Linux, open
    // "/proc/self/exe" directly: it is the running inode even after the path is
    // replaced. Keep `current_exe` elsewhere. (shell-05)
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
/// a module, a `m.gx` beside the `m/mod.gx` it would win over, a root
/// module's files in each directory `search` lists ahead of its own).
/// `None` when it read a source no file vouches for on the next start.
// CR claude for claude: [structure/medium] `sources_record` and `root_module_site`
// rebuild, from the finished program's origins, which files module resolution read and
// which it checked for. That covers the interface beside a module, mod.gx versus m.gx,
// and the root directory then the search path. `resolve_from_files` and the resolver
// chain already make those decisions. The copy has drifted: x-image-03 (interfaces) and
// x-image-04 (layout conflicts). Any new resolver rule will silently produce stale or
// never-hitting entries, and the fuzzer's session axis bypasses this file. Fix: have
// the resolver (and `RootFile::load`) push each path it read, with its text, and each
// path it found absent into a sink returned with the program image. This function then
// writes that list verbatim, and `root_module_site` and the `absent` heuristics go
// away. Pin it at shell level by running a temp-dir program twice across the edit,
// add-interface, layout-conflict and script-interface cases. (x-image-05)
fn sources_record(
    sources: &[triomphe::Arc<Origin>],
    search: &[PathBuf],
) -> Option<String> {
    let mut read: Vec<(&FsPath, &Origin)> = vec![];
    for o in sources {
        match &o.source {
            Source::File(p) => read.push((p, o)),
            // the packages are the build's, an embedded program is the key
            Source::Vfs(_) | Source::Program(_) | Source::Unspecified => (),
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
        // CR claude for claude: [bug/medium] The record misses files whose appearance
        // makes the cold compile refuse the program. resolve_from_files reads both
        // MODULE_LAYOUTS and breaks the module when both implementations exist or when
        // the unused layout has an interface. Here, a read m.gx records only m.gxi as
        // absent, not m/mod.gx or m/mod.gxi, and a read m/mod.gx does not record m.gxi.
        // So after one of those files appears, `--no-cache` fails ("both define module
        // m" / "is no interface"), but a cached start still verifies the entry and runs
        // the old program. Fix the root cause: resolve_from_files should report every
        // path it probed, read or found absent, and ProgramImage.sources should carry
        // that set. sources_record then writes `+`/`-` lines from it and stops
        // re-deriving the resolver's layout and search rules (root_module_site goes
        // away). Probe: design/review-2026-10-12/repro/shell-01.sh. Also reported as
        // (x-image-04). (shell-01)
        absent(p.with_extension("gxi"), &mut record);
        if o.parent.is_some()
            && p.file_name().is_some_and(|n| n == "mod.gx")
            && let Some(dir) = p.parent()
        {
            absent(dir.with_extension("gx"), &mut record);
        }
        // CR claude for claude: [bug/medium] x-image-01 variant: a warm start masks the
        // m.gx / m/mod.gx conflict a cold compile refuses. sources_record records a
        // module file's absent alternatives only through root_module_site. For a module
        // beside the root that resolved to `m.gx`, it does not record `m/mod.gx` as
        // absent (cache.rs; the root_module_site loop breaks at once). Creating
        // `m/mod.gx` later leaves the program entry valid. A warm start then runs the
        // old module, while --no-cache refuses the program because both files define
        // module m. Shadowing through GRAPHIX_MODPATH, which I also probed, is handled
        // correctly. Failure: The cold run prints "file". After `m/mod.gx` is created,
        // the warm run still prints "file", but --no-cache fails with '.../m.gx and
        // .../m/mod.gx both define module m'. Fix: For every module resolved from a
        // directory, record each candidate path the resolver would also have probed
        // (`m.gx`, `m/mod.gx`, `m.gxi`, `m/mod.gxi`) as present-or-absent in the
        // entry's sources, not only those reached through root_module_site. Unverified:
        // the reviewer's report, not checked by a verifier. (x-rerun-8-07)
        if let Some((base, name)) = root_module_site(o, p) {
            let root_dir = o.parent.as_ref().and_then(|r| match &r.source {
                Source::File(r) => r.parent(),
                _ => None,
            });
            for ahead in root_dir.into_iter().chain(search.iter().map(|d| d.as_path())) {
                if ahead == base {
                    break;
                }
                absent(ahead.join(name).with_extension("gx"), &mut record);
                absent(ahead.join(name).join("mod.gx"), &mut record);
            }
        }
    }
    Some(record)
}

/// The directory a root's module `p` was searched in and its name: a
/// module the root's own `mod` reached, which the root's directory and
/// then the search path are asked for in turn.
fn root_module_site<'a>(o: &Origin, p: &'a FsPath) -> Option<(&'a FsPath, &'a OsStr)> {
    let parent = o.parent.as_ref()?;
    if parent.parent.is_some() {
        return None;
    }
    match p.file_name()? == "mod.gx" {
        true => {
            let dir = p.parent()?;
            Some((dir.parent()?, dir.file_name()?))
        }
        false => Some((p.parent()?, p.file_stem()?)),
    }
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
        let program = program.and_then(|p| {
            let id = match p {
                Source::File(path) => path.to_string_lossy().into_owned().into_bytes(),
                Source::Program(text) => text.as_bytes().to_vec(),
                Source::Netidx(_) | Source::Vfs(_) | Source::Unspecified => return None,
            };
            // the search path decides which file a module name reaches
            // CR claude for claude: [bug/medium] The program key hashes only
            // GRAPHIX_MODPATH, but root modules are also searched in
            // dirs::data_dir()/graphix (graphix_rt::search_path). sources_record
            // records absences only for directories searched before the one a module
            // came from. So after XDG_DATA_HOME (or HOME) changes, the old module file
            // still exists and still matches its digest. The entry hits, and the warm
            // run executes the module from the previous data dir, while a cold run
            // resolves the new one. Fix: hash the whole search path into the key,
            // meaning the GRAPHIX_MODPATH entries plus each directory
            // graphix_rt::search_dirs returns, so a different data dir is a different
            // entry. probe: design/review-2026-10-12/repro/x-image-02-datadir-stale.sh
            // (prints 1 1 1 2; the third run, under the new data dir, is the stale 1).
            // (x-image-02)
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

    // CR claude for claude: [doc-drift/low] The comment says only the pages a restore
    // touches are read. But `decode_registration` checks `image::sealed`, which runs a
    // CRC over the whole image, so every page is read at load. Rewrite it to say what
    // is true: the mapping saves a copy, the whole image is checksummed at restore, and
    // the mapping stays valid because an entry is never rewritten in place. (shell-09)
    /// The entry mapped into memory, with its path: only the pages a
    /// restore touches are read, and an instance decoded later reads its
    /// own. An entry is never rewritten in place (written to a temporary
    /// file and renamed), so the mapping stays valid. A program entry
    /// whose sources changed is a miss.
    // CR claude for claude: [doc-drift/low] The doc above says only the pages a restore
    // touches are read and that an instance decoded later reads its own. That stopped
    // being true when decode_registration began checking image::sealed: it hashes every
    // byte after the header, the instance heap included. So every warm start reads the
    // whole entry first, and the I/O scales with the image size, not with what is
    // decoded. design/program_image.md makes the same claim. Correct both to say the
    // entry is read once in full for the checksum. If lazy reads matter for large
    // images, a crc per instance-heap record, checked at its lazy decode, with the
    // eager part sealed at load, would bring them back. (x-fix-fusion-06)
    // CR claude for claude: [doc-drift/low] The doc above says only the pages a restore
    // touches are read, and that an instance decoded later reads its own.
    // design/program_image.md says the same. But decode_registration calls
    // image::sealed before decoding, and sealed runs crc32 over everything after the
    // header. Every page of the entry is therefore read on every warm start, and lazy
    // instance decode saves decode work but no I/O. Fix both texts to say the checksum
    // reads the whole entry. If the I/O matters, the alternative is a checksum per
    // region that is verified when the region is first decoded. (t-image-06)
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
        // CR claude for claude: [bug/low] When the compile read a module from netidx,
        // there is no record and this returns `Ok(())`, so a `--warm` run succeeds
        // without writing the program entry. `CacheMode::Warm` promises that an entry
        // it cannot write fails the run. Return an error here (e.g. "the program reads
        // a netidx module and cannot be cached"). `Shell::stored` already logs that as
        // a warning under `CacheMode::On` and fails a warm run. (shell-07)
        let Some(record) = sources_record(&program.sources, &graphix_rt::search_dirs())
        else {
            return Ok(());
        };
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
        // CR claude for claude: [risk/low] The cache directories are created at the
        // umask default (755), and the entries are created by `fs::File::create` (644).
        // A program entry holds the compiled program, string literals included, and the
        // paths of the sources it read. A script kept at mode 600 because it embeds a
        // credential therefore leaves a copy readable by every local user wherever the
        // home directory is not 0700. On Unix, create the cache root with mode 0700
        // (`DirBuilderExt::mode`) and the temporary entry with mode 0600
        // (`OpenOptionsExt::mode`). probe:
        // design/review-2026-10-12/repro/x-security-10.gx (x-security-10)
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
        // CR claude for claude: [risk/low] This removes every empty build directory,
        // including this build's own `self.dir()`. Another process of the same build
        // may have just run `create_dir_all` for it in `store` and not yet created its
        // temporary file. Its `File::create` then fails with NotFound and the entry is
        // lost; under `--warm` the run fails. Skip `self.dir()` here: it is about to
        // hold an entry anyway. (shell-08)
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
        let record = sources_record(&[root.clone(), file(&m, Some(root))], &[]).unwrap();
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

    #[test]
    fn a_program_entry_misses_when_a_module_is_shadowed() {
        let dir = tempfile::tempdir().unwrap();
        let (proj, early, late) =
            (dir.path().join("proj"), dir.path().join("early"), dir.path().join("late"));
        for d in [&proj, &early, &late] {
            fs::create_dir(d).unwrap();
        }
        let main = proj.join("main.gx");
        let m = late.join("m.gx");
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
        let search = [early.clone(), late.clone()];
        let record =
            sources_record(&[root.clone(), file(&m, Some(root))], &search).unwrap();
        assert!(sources_unchanged(&record), "{record}");
        for shadow in [proj.join("m.gx"), early.join("m.gx")] {
            fs::write(&shadow, "let x = 2").unwrap();
            assert!(!sources_unchanged(&record), "{} shadows m", shadow.display());
            fs::remove_file(&shadow).unwrap();
        }
        fs::create_dir(early.join("m")).unwrap();
        fs::write(early.join("m").join("mod.gx"), "let x = 2").unwrap();
        assert!(!sources_unchanged(&record), "early/m/mod.gx shadows m");
    }
}
