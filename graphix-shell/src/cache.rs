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

use ahash::AHashSet;
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use compact_str::format_compact;
use graphix_compiler::{
    expr::{Load, Probe, names_nothing},
    image,
};
use graphix_rt::ProgramImage;
use log::{info, warn};
use netidx_core::utils::make_sha3_token;
use poolshark::local::LPooled;
use std::{
    fmt::Write,
    fs,
    io::ErrorKind,
    path::{Path as FsPath, PathBuf},
    sync::atomic::{AtomicBool, AtomicU64, Ordering},
    time::{Duration, SystemTime},
};

#[cfg(unix)]
use std::os::unix::fs::{DirBuilderExt, MetadataExt, OpenOptionsExt};

/// A cache path the current user cannot trust: the cache is not used.
#[derive(Debug)]
struct Untrusted(&'static str);

impl std::fmt::Display for Untrusted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl std::error::Error for Untrusted {}

static UNTRUSTED_REPORTED: AtomicBool = AtomicBool::new(false);

/// Log `e`, and when it is a trust refusal also say so on stderr, once a
/// process: without a log nothing else tells why every start is cold.
pub(crate) fn report(e: &anyhow::Error) {
    warn!("{e:#}");
    if e.chain().any(|c| c.is::<Untrusted>())
        && !UNTRUSTED_REPORTED.swap(true, Ordering::Relaxed)
    {
        eprintln!("graphix: not using the image cache: {e:#}");
    }
}

fn check_owner(metadata: &fs::Metadata) -> Result<()> {
    #[cfg(unix)]
    if metadata.uid() != unsafe { libc::geteuid() } {
        return Err(Untrusted("cache path must be owned by the current user").into());
    }
    #[cfg(not(unix))]
    let _ = metadata;
    Ok(())
}

fn check_private(metadata: &fs::Metadata) -> Result<()> {
    check_owner(metadata)?;
    #[cfg(unix)]
    if metadata.mode() & 0o022 != 0 {
        return Err(
            Untrusted("cache path must not be writable by group or others").into()
        );
    }
    Ok(())
}

fn cache_directory(path: &FsPath, create: bool, links_left: u8) -> Result<()> {
    let metadata = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(e) if create && e.kind() == ErrorKind::NotFound => {
            let mut builder = fs::DirBuilder::new();
            builder.recursive(true);
            #[cfg(unix)]
            builder.mode(0o700);
            builder.create(path)?;
            fs::symlink_metadata(path)?
        }
        Err(e) => {
            return Err(anyhow::Error::from(e))
                .with_context(|| format_compact!("cache directory {}", path.display()));
        }
    };
    if metadata.is_symlink() && links_left > 0 {
        check_owner(&metadata)
            .with_context(|| format_compact!("cache link {}", path.display()))?;
        let target = fs::read_link(path)?;
        let target = match target.is_absolute() {
            true => target,
            false => path.parent().unwrap_or(FsPath::new(".")).join(target),
        };
        return cache_directory(&target, false, links_left - 1);
    }
    if !metadata.is_dir() {
        return Err(anyhow::Error::from(Untrusted("cache path is not a directory")))
            .with_context(|| format_compact!("{}", path.display()));
    }
    check_private(&metadata)
        .with_context(|| format_compact!("cache directory {}", path.display()))
}

fn cache_file(metadata: &fs::Metadata) -> Result<()> {
    if !metadata.is_file() {
        return Err(Untrusted("cache entry is not a regular file").into());
    }
    check_private(metadata)
}

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
    // the running inode, even when the path's file was replaced since
    #[cfg(target_os = "linux")]
    let file = fs::File::open("/proc/self/exe").ok()?;
    #[cfg(not(target_os = "linux"))]
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

/// A program entry's record of its sources: `+<digest> <path>` for each
/// file module resolution read, `-<path>` for each it looked for and did
/// not find, once each. `None` when it read a module from netidx, which
/// no file vouches for on the next start.
fn sources_record(probes: &[Probe]) -> Option<String> {
    let mut record = String::new();
    let mut seen: LPooled<AHashSet<&FsPath>> = LPooled::take();
    for probe in probes {
        match probe {
            Probe::Netidx(p) => {
                info!("no program entry: the program reads {p} from netidx");
                return None;
            }
            Probe::File(p, _) if !seen.insert(p) => (),
            Probe::File(p, Some(text)) => {
                let _ =
                    writeln!(record, "+{} {}", digest(&[text.as_bytes()]), p.display());
            }
            Probe::File(p, None) => {
                let _ = writeln!(record, "-{}", p.display());
            }
        }
    }
    Some(record)
}

/// Whether every source `record` lists is as the compile read it.
fn sources_unchanged(record: &str) -> bool {
    record.lines().all(|line| match line.split_at_checked(1) {
        Some(("-", path)) => fs::File::open(path).is_err_and(|e| names_nothing(&e)),
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
    pub(crate) fn new(root: &str, program: Option<&Load>, flags: u64) -> Result<Self> {
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
                Load::File(path) => path.to_string_lossy().into_owned().into_bytes(),
                Load::Text(text) => text.as_bytes().to_vec(),
                Load::Netidx(_) => return None,
            };
            // the search path decides which file a module name reaches
            // the whole module search path, the data dir's included
            let mut search = Vec::new();
            for dir in graphix_rt::search_dirs() {
                let dir = dir.into_os_string();
                search.extend_from_slice(&(dir.len() as u64).to_le_bytes());
                search.extend_from_slice(dir.as_encoded_bytes());
            }
            Some(digest(&[
                &format,
                root.as_bytes(),
                &flags,
                &[matches!(p, Load::File(_)) as u8],
                &id,
                &search,
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

    fn directories(&self, create: bool) -> Result<()> {
        let graphix = self.root.parent().ok_or_else(|| anyhow!("cache has no parent"))?;
        let base = graphix.parent().ok_or_else(|| anyhow!("cache has no base"))?;
        cache_directory(base, create, 40)?;
        for dir in [graphix, &self.root, &self.dir()] {
            cache_directory(dir, create, 0)?;
        }
        Ok(())
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

    /// The entry mapped into memory, with its path. The mapping saves a
    /// copy; the restore's checksum reads the whole entry once. An entry is
    /// never rewritten in place (written to a temporary file and renamed),
    /// so the mapping stays valid. A program entry whose sources changed
    /// is a miss.
    // XCR claude for claude: [risk/medium] An entry's integrity rests on the cache
    // directory's permissions alone, and nothing checks them. load restores any entry
    // at the keyed path whatever its owner or mode, store applies 0700 only to
    // directories it creates itself, and the crc32 seal can be recomputed by anyone.
    // The entry carries JIT machine code. With a cache root another user can write
    // first (HOME or XDG_CACHE_HOME under /tmp, a shared CI cache), a planted or
    // patched entry runs as the next user's program; patching a literal and resealing
    // ran the modified program. Proposal: on Unix, refuse an entry or directory not
    // owned by the euid or writable by group/other. probe:
    // design/review-2026-10-26/repro/x-security-cache-trust.sh (x-security-05)
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q70, Trusting
    // the image cache directory): yes, and set the permissions correctly when
    // creating it (the recommendation: refuse.)
    // 2026-10-28 codex: Reworked the unmerged cache trust checks. The cache base
    // follows up to 40 symlinks, checking each link's effective-user ownership and
    // the final directory's ownership and permissions before creating managed
    // subdirectories. Relative targets work. Managed directories and entry files
    // still refuse symlinks; files open O_NOFOLLOW/O_NONBLOCK and are checked before
    // mapping; collection skips untrusted paths. Created directories/files use
    // 0700/0600. owned_cache_base_links_are_trusted failed against the earlier
    // no-symlink implementation and passes now. Restoring main's unchecked
    // load/store/collect bodies made five refusal pins fail (shared entries, shared
    // directories, entry/build links, collector links, unsafe symlink targets).
    // Removing only the link-owner check failed
    // foreign_cache_base_link_is_refused_even_with_a_private_target, using
    // /proc/self's foreign-owned link and current-user-owned target; it also covers a
    // link chain. Invalid/dangling/cyclic links are refused. cargo test -p
    // graphix-shell --offline --lib passes all 19 tests after restoring the fix;
    // focused mutation runs used --lib cache::tests::privacy and --lib
    // foreign_cache_base_link. The named poisoning probe printed
    // PWNED-by-a-cache-file on main and hello-from-the-author with this fix.
    // /tmp/x-security-05-symlink-cli.py verified --warm and a warm run through an
    // owned relative cache-base symlink, then --warm refusal after making its target
    // 0777. cargo build -p graphix-shell --offline, cargo fmt --all and mdbook build
    // -d /tmp/graphix-cr-rework-book passed. Rules, book and design updated;
    // AGENTS.md regenerated. Tested on Linux; other platforms were not exercised. No
    // render/update/dispatch/fork path changed.
    // 2026-10-28 claude: Review (2026-10-28) fixes on merge: a write renames over a
    // bad entry instead of refusing (the cache healed only by hand), collection
    // removes an untrusted entry in a trusted directory, load warnings keep the
    // reason ({e:#}), and a dangling cache-base link names its path.
    // design/program_image.md states the path-based check's limit (an ancestor writer
    // can swap the tree between check and use). Open for Eric: a trust refusal is
    // silent without --log-dir; should it print one stderr line?
    // 2026-10-28 claude: Eric ruled (2026-10-28): yes, a trust refusal prints one
    // stderr line. Built: the trust checks raise cache::Untrusted (foreign owner,
    // group/other writable, not a directory or regular file, a symlinked entry via
    // O_NOFOLLOW's ELOOP), and cache::report logs every cache failure and prints
    // 'graphix: not using the image cache: <reason>' for the first refusal in a
    // process; a store's failure goes through it too. Pins:
    // graphix-shell/tests/cache_trust_report.rs (an_untrusted_cache_is_reported_once,
    // a_trusted_cache_is_quiet); without the stderr line the first fails with 0
    // lines, without the once guard with 4. Book (shell.md), CLAUDE.md and
    // design/program_image.md say so.
    pub(crate) fn load(&self, entry: Entry) -> Option<(ArcStr, Bytes)> {
        let path = self.path(entry)?;
        let file = (|| -> Result<fs::File> {
            self.directories(false)?;
            let mut open = fs::OpenOptions::new();
            open.read(true);
            #[cfg(unix)]
            open.custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK);
            let file = match open.open(&path) {
                #[cfg(unix)]
                Err(e) if e.raw_os_error() == Some(libc::ELOOP) => {
                    return Err(Untrusted("cache entry is a symbolic link").into());
                }
                r => r?,
            };
            cache_file(&file.metadata()?)?;
            Ok(file)
        })();
        let file = match file {
            Ok(f) => f,
            Err(e)
                if e.downcast_ref::<std::io::Error>()
                    .is_some_and(|e| e.kind() == ErrorKind::NotFound) =>
            {
                return None;
            }
            Err(e) => {
                report(&e.context(format_compact!(
                    "opening the {entry:?} image {}",
                    path.display()
                )));
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
        Some((format_compact!("{}", path.display()).as_str().into(), image))
    }

    pub(crate) fn store_registration(&self, image: &[u8]) -> Result<()> {
        self.store(Entry::Registration, &[image])
    }

    /// Write the program entry; a compile that read a source the next
    /// start cannot verify has none, which the caller reports.
    pub(crate) fn store_program(&self, program: &ProgramImage) -> Result<()> {
        let Some(record) = sources_record(&program.sources) else {
            bail!(
                "the program reads a source the next start cannot verify (a netidx module)"
            )
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
        self.directories(true)?;
        let tmp = dir.join(
            format_compact!(
                "{}.img.{}.{}",
                self.key(entry).unwrap_or_default(),
                std::process::id(),
                TEMP.fetch_add(1, Ordering::Relaxed)
            )
            .as_str(),
        );
        let written = (|| {
            use std::io::Write;
            let mut open = fs::OpenOptions::new();
            open.write(true).create_new(true);
            #[cfg(unix)]
            open.mode(0o600);
            let mut f = open.open(&tmp)?;
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
        let mut entries: LPooled<Vec<(Duration, u64, PathBuf)>> = LPooled::take();
        for build in fs::read_dir(&self.root).into_iter().flatten().flatten() {
            let build = build.path();
            if cache_directory(&build, false, 0).is_err() {
                continue;
            }
            for f in fs::read_dir(&build).into_iter().flatten().flatten() {
                let Ok(m) = fs::symlink_metadata(f.path()) else { continue };
                if cache_file(&m).is_err() {
                    let _ = fs::remove_file(f.path());
                    continue;
                }
                if f.path().extension().is_some_and(|e| e == "img") {
                    entries.push((age(&m), m.len(), f.path()));
                } else if age(&m) > ABANDONED {
                    let _ = fs::remove_file(f.path());
                }
            }
        }
        entries.sort_unstable_by_key(|(age, ..)| *age);
        let mut kept = 0;
        for (age, len, path) in entries.drain(..) {
            kept += len;
            if age > KEEP_UNUSED || kept > KEEP_BYTES {
                let _ = fs::remove_file(path);
            }
        }
        // only an empty directory is removed, never this build's: another
        // process may be about to write its entry there
        let own = self.dir();
        for build in fs::read_dir(&self.root).into_iter().flatten().flatten() {
            let path = build.path();
            if path != own && cache_directory(&path, false, 0).is_ok() {
                let _ = fs::remove_dir(path);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(unix)]
    mod privacy {
        use super::*;
        use std::os::unix::fs::{PermissionsExt, symlink};

        fn cache(base: &FsPath) -> RegistrationCache {
            RegistrationCache {
                root: base.join("graphix/registration"),
                build: "build".into(),
                registration: "registration".into(),
                program: None,
            }
        }

        fn mode(path: &FsPath, mode: u32) {
            fs::set_permissions(path, fs::Permissions::from_mode(mode)).unwrap();
        }

        #[test]
        fn owned_cache_base_links_are_trusted() {
            for relative in [false, true] {
                let tmp = tempfile::tempdir().unwrap();
                let base = tmp.path().join("cache");
                let target = tmp.path().join("real-cache");
                fs::create_dir(&target).unwrap();
                mode(&target, 0o700);
                symlink(
                    if relative { FsPath::new("real-cache") } else { &target },
                    &base,
                )
                .unwrap();
                let cache = cache(&base);
                cache.store_registration(b"entry").unwrap();
                let (_, bytes) = cache.load(Entry::Registration).unwrap();
                assert_eq!(bytes.as_ref(), b"entry");
                assert!(
                    target.join("graphix/registration/build/registration.img").is_file()
                );
                assert!(fs::symlink_metadata(base).unwrap().is_symlink());
            }
        }

        #[test]
        fn unsafe_cache_base_link_targets_are_refused_before_creation() {
            let tmp = tempfile::tempdir().unwrap();
            let base = tmp.path().join("cache");
            let target = tmp.path().join("real-cache");
            fs::create_dir(&target).unwrap();
            symlink(&target, &base).unwrap();
            let cache = cache(&base);
            for bits in [0o720, 0o702, 0o777] {
                mode(&target, bits);
                assert!(cache.load(Entry::Registration).is_none());
                assert!(cache.store_registration(b"entry").is_err());
                assert!(!target.join("graphix").exists());
            }
            mode(&target, 0o700);
            cache.store_registration(b"entry").unwrap();
            for bits in [0o720, 0o702, 0o777] {
                mode(&target, bits);
                assert!(cache.load(Entry::Registration).is_none());
                assert!(cache.store_registration(b"replacement").is_err());
            }
            fs::remove_file(&base).unwrap();
            symlink("/", &base).unwrap();
            if fs::metadata("/").unwrap().uid() != unsafe { libc::geteuid() } {
                assert!(cache.load(Entry::Registration).is_none());
                assert!(cache.store_registration(b"entry").is_err());
            }
        }

        #[cfg(target_os = "linux")]
        #[test]
        fn foreign_cache_base_link_is_refused_even_with_a_private_target() {
            let link = FsPath::new("/proc/self");
            let metadata = fs::symlink_metadata(link).unwrap();
            assert!(metadata.is_symlink());
            check_private(&fs::metadata(link).unwrap()).unwrap();
            if metadata.uid() != unsafe { libc::geteuid() } {
                let tmp = tempfile::tempdir().unwrap();
                let chain = tmp.path().join("cache");
                symlink(link, &chain).unwrap();
                for base in [link, &chain] {
                    let error = cache(base).directories(false).unwrap_err();
                    assert!(
                        format_compact!("{error:#}")
                            .contains("owned by the current user"),
                        "{error:#}"
                    );
                }
            }
        }

        #[test]
        fn invalid_cache_base_links_are_refused() {
            let tmp = tempfile::tempdir().unwrap();
            let base = tmp.path().join("cache");
            let target = tmp.path().join("target");
            let cache = cache(&base);
            symlink(&target, &base).unwrap();
            assert!(cache.store_registration(b"entry").is_err());
            assert!(!target.exists());
            fs::write(&target, b"file").unwrap();
            assert!(cache.store_registration(b"entry").is_err());
            fs::remove_file(&target).unwrap();
            symlink(&base, &target).unwrap();
            assert!(cache.store_registration(b"entry").is_err());
            assert!(cache.load(Entry::Registration).is_none());
        }

        #[test]
        fn shared_entries_are_never_loaded() {
            let tmp = tempfile::tempdir().unwrap();
            let cache = cache(tmp.path());
            cache.store_registration(b"entry").unwrap();
            assert!(cache.load(Entry::Registration).is_some());
            let path = cache.path(Entry::Registration).unwrap();
            for bits in [0o660, 0o602, 0o666] {
                mode(&path, bits);
                assert!(cache.load(Entry::Registration).is_none(), "mode {bits:o}");
                cache.store_registration(b"replacement").unwrap();
                assert!(cache.load(Entry::Registration).is_some(), "mode {bits:o}");
            }
        }

        #[test]
        fn shared_directories_are_never_used() {
            let tmp = tempfile::tempdir().unwrap();
            let base = tmp.path().join("cache");
            let cache = cache(&base);
            cache.store_registration(b"entry").unwrap();
            for dir in [&base, &base.join("graphix"), &cache.root, &cache.dir()] {
                for bits in [0o720, 0o702, 0o777] {
                    mode(dir, bits);
                    assert!(
                        cache.load(Entry::Registration).is_none(),
                        "{} mode {bits:o}",
                        dir.display()
                    );
                    assert!(cache.store_registration(b"replacement").is_err());
                    mode(dir, 0o700);
                }
            }
            assert!(cache.load(Entry::Registration).is_some());
        }

        #[test]
        fn cache_links_are_not_followed() {
            let tmp = tempfile::tempdir().unwrap();
            let cache = cache(&tmp.path().join("cache"));
            cache.store_registration(b"entry").unwrap();
            let entry = cache.path(Entry::Registration).unwrap();
            let real = tmp.path().join("real-entry");
            fs::rename(&entry, &real).unwrap();
            symlink(&real, &entry).unwrap();
            assert!(cache.load(Entry::Registration).is_none());
            cache.store_registration(b"replacement").unwrap();
            assert!(!fs::symlink_metadata(&entry).unwrap().is_symlink());
            assert_eq!(fs::read(&real).unwrap(), b"entry");
            fs::remove_file(&entry).unwrap();
            fs::rename(real, &entry).unwrap();
            let dir = cache.dir();
            let real = tmp.path().join("real-build");
            fs::rename(&dir, &real).unwrap();
            symlink(&real, &dir).unwrap();
            assert!(cache.load(Entry::Registration).is_none());
            assert!(cache.store_registration(b"replacement").is_err());
        }

        #[test]
        fn foreign_ownership_is_refused() {
            let metadata = fs::metadata("/").unwrap();
            if metadata.uid() != unsafe { libc::geteuid() } {
                assert!(check_private(&metadata).is_err());
            }
        }

        #[test]
        fn collection_does_not_follow_a_build_link() {
            let tmp = tempfile::tempdir().unwrap();
            let cache = cache(&tmp.path().join("cache"));
            cache.store_registration(b"entry").unwrap();
            let outside = tmp.path().join("outside");
            fs::create_dir(&outside).unwrap();
            let file = outside.join("keep.img");
            fs::write(&file, b"keep").unwrap();
            fs::File::open(&file).unwrap().set_modified(SystemTime::UNIX_EPOCH).unwrap();
            symlink(&outside, cache.root.join("another-build")).unwrap();
            cache.collect();
            assert_eq!(fs::read(file).unwrap(), b"keep");
        }

        #[test]
        fn newly_created_cache_is_private() {
            let tmp = tempfile::tempdir().unwrap();
            let base = tmp.path().join("cache");
            let cache = cache(&base);
            cache.store_registration(b"entry").unwrap();
            for dir in [&base, &base.join("graphix"), &cache.root, &cache.dir()] {
                assert_eq!(
                    fs::metadata(dir).unwrap().permissions().mode() & 0o777,
                    0o700
                );
            }
            let entry = cache.path(Entry::Registration).unwrap();
            assert_eq!(fs::metadata(entry).unwrap().permissions().mode() & 0o777, 0o600);
        }
    }

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

    /// The record of `main`'s program as module resolution makes it,
    /// with `main`'s directory and then `search` on the module path, as
    /// the shell puts them.
    fn record_of(main: &FsPath, search: &[PathBuf]) -> String {
        use graphix_compiler::expr::{FilesResolver, Probes, ResolverRef, RootFile};
        let rt =
            tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap();
        rt.block_on(async {
            let probes = Probes::default();
            let root =
                RootFile::load(&main.to_path_buf(), None, Some(&probes)).await.unwrap();
            let dirs = std::iter::once(main.parent().unwrap().to_path_buf())
                .chain(search.iter().cloned());
            let resolvers: Vec<ResolverRef> =
                dirs.map(|d| FilesResolver::new(d, None)).collect();
            let resolvers = std::sync::Arc::from(resolvers);
            for e in root.exprs.iter() {
                e.resolve_modules(&resolvers, Some(&probes)).await.unwrap();
            }
            sources_record(&probes.take()).unwrap()
        })
    }

    #[test]
    fn a_program_entry_misses_when_a_source_changes() {
        let dir = tempfile::tempdir().unwrap();
        let main = dir.path().join("main.gx");
        let m = dir.path().join("m.gx");
        fs::write(&main, "mod m;\nm::x").unwrap();
        fs::write(&m, "let x = 1").unwrap();
        let record = record_of(&main, &[]);
        assert!(sources_unchanged(&record), "{record}");
        fs::write(&m, "let x = 2").unwrap();
        assert!(!sources_unchanged(&record), "an edited module");
        fs::write(&m, "let x = 1").unwrap();
        for added in [dir.path().join("m.gxi"), dir.path().join("main.gxi")] {
            fs::write(&added, "val x: i64").unwrap();
            assert!(!sources_unchanged(&record), "{} added", added.display());
            fs::remove_file(&added).unwrap();
        }
        // the layout m.gx would conflict with
        fs::create_dir(dir.path().join("m")).unwrap();
        for added in [dir.path().join("m/mod.gx"), dir.path().join("m/mod.gxi")] {
            fs::write(&added, "let x = 2").unwrap();
            assert!(!sources_unchanged(&record), "{} added", added.display());
            fs::remove_file(&added).unwrap();
        }
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
        fs::write(&main, "mod m;\nm::x").unwrap();
        fs::write(late.join("m.gx"), "let x = 1").unwrap();
        let record = record_of(&main, &[early.clone(), late.clone()]);
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

    /// A path probed through a regular file, or at a dangling symlink, is
    /// absent to the compile and so to the check of its record.
    #[cfg(unix)]
    #[test]
    fn a_program_entry_restores_over_paths_that_name_nothing() {
        let dir = tempfile::tempdir().unwrap();
        let main = dir.path().join("main.gx");
        fs::write(&main, "mod m;\nm::x").unwrap();
        fs::write(dir.path().join("m.gx"), "let x = 1").unwrap();
        fs::write(dir.path().join("m"), "not a module").unwrap();
        std::os::unix::fs::symlink("/nonexistent/target", dir.path().join("m.gxi"))
            .unwrap();
        let record = record_of(&main, &[]);
        assert!(record.contains("m/mod.gx"), "{record}");
        assert!(record.contains("m.gxi"), "{record}");
        assert!(sources_unchanged(&record), "{record}");
    }
}
