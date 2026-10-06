//! The image cache. The executable that reads an image is the
//! executable that wrote it, on its first run: entries live under the
//! cache directory by the executable's build id, which also covers the
//! packages compiled into it, keyed by the root that declares them and
//! the image format, so a rebuilt executable misses and recompiles. A
//! program's entry adds the program's source to the key. Entries are
//! written to a temporary file and renamed into place, and other build
//! ids' directories are removed when one is written.

use anyhow::{Context, Result, anyhow};
use bytes::Bytes;
use graphix_compiler::image;
use log::{info, warn};
use netidx_core::utils::make_sha3_token;
use std::{
    fmt::Write,
    fs,
    path::{Path as FsPath, PathBuf},
};

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

/// The GNU build id note of the running executable, which the linker
/// stamps per link, or for a PE executable its link stamp and size; a
/// build with neither falls back to the compiler version, which is not
/// unique per build but still separates releases.
fn build_id() -> String {
    match std::env::current_exe()
        .ok()
        .and_then(|p| elf_build_id(&p).or_else(|| pe_build_id(&p)))
    {
        Some(id) => id,
        // CR claude for claude: [bug] Every Mach-O executable lands here (its magic is
        // neither "\x7fELF" nor "MZ"), and aarch64-apple-darwin is a release target, so
        // on macOS every build of one graphix-shell version shares one cache directory.
        // The registration key is only the image format plus the package names, and the
        // header checks only magic, format and ISA. A source rebuild after a stdlib
        // edit, or the package manager's same-version rebuild after an external package
        // update, therefore restores the previous build's definitions without warning,
        // along with program entries whose kernel bytes link by helper name against the
        // new binary. Read the Mach-O LC_UUID the way the ELF note is read, and when no
        // per-build id exists, disable the cache rather than key it by version. probe:
        // design/review-2026-10-05/repro/x-image-09.sh (Linux, binaries without a build
        // id: the stdlib-edited build prints the old pi = 3.141592653589793 from the
        // cache, pi = 3 with --no-cache). (x-image-09)
        None => format!("v{}", env!("CARGO_PKG_VERSION")),
    }
}

/// A PE executable's COFF `TimeDateStamp` and its length: the stamp is
/// the link time, or a hash of the contents under a reproducible link,
/// and the length separates two builds that share one. Any malformed
/// field is `None`, never a panic.
fn pe_build_id(exe: &FsPath) -> Option<String> {
    fn u32_at(b: &[u8], i: usize) -> Option<u32> {
        Some(u32::from_le_bytes(b.get(i..i + 4)?.try_into().ok()?))
    }
    let file = fs::File::open(exe).ok()?;
    let file = unsafe { memmap2::Mmap::map(&file) }.ok()?;
    if file.get(..2)? != b"MZ" {
        return None;
    }
    let pe = u32_at(&file, 0x3c)? as usize;
    if file.get(pe..pe + 4)? != b"PE\0\0" {
        return None;
    }
    let stamp = u32_at(&file, pe + 8)?;
    Some(format!("pe-{stamp:08x}-{:x}", file.len()))
}

/// Walk the ELF64 program headers for a `PT_NOTE` holding
/// `NT_GNU_BUILD_ID`. Any malformed field is `None`, never a panic.
fn elf_build_id(exe: &FsPath) -> Option<String> {
    fn u16_at(b: &[u8], i: usize) -> Option<u16> {
        Some(u16::from_le_bytes(b.get(i..i + 2)?.try_into().ok()?))
    }
    fn u32_at(b: &[u8], i: usize) -> Option<u32> {
        Some(u32::from_le_bytes(b.get(i..i + 4)?.try_into().ok()?))
    }
    fn u64_at(b: &[u8], i: usize) -> Option<u64> {
        Some(u64::from_le_bytes(b.get(i..i + 8)?.try_into().ok()?))
    }
    // Mapped, so only the pages the walk touches are read: the headers
    // and the note, not the executable.
    let file = fs::File::open(exe).ok()?;
    let file = unsafe { memmap2::Mmap::map(&file) }.ok()?;
    if file.get(..4)? != b"\x7fELF" || file.get(4)? != &2 || file.get(5)? != &1 {
        return None;
    }
    let phoff = u64_at(&file, 32)? as usize;
    let phentsize = u16_at(&file, 54)? as usize;
    let phnum = u16_at(&file, 56)? as usize;
    for i in 0..phnum {
        let ph = file.get(phoff + i * phentsize..)?;
        if u32_at(ph, 0)? != 4 {
            continue;
        }
        let offset = u64_at(ph, 8)? as usize;
        let size = u64_at(ph, 32)? as usize;
        let mut notes = file.get(offset..offset + size)?;
        while notes.len() >= 12 {
            let namesz = u32_at(notes, 0)? as usize;
            let descsz = u32_at(notes, 4)? as usize;
            let ntype = u32_at(notes, 8)?;
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

impl RegistrationCache {
    /// The cache entries for this root and, when its source is known,
    /// the program; an error when there is no cache directory.
    pub(crate) fn new(root: &str, program: Option<&[u8]>, flags: u64) -> Result<Self> {
        let root_dir = dirs::cache_dir()
            .ok_or_else(|| anyhow!("no cache directory"))?
            .join("graphix")
            .join("registration");
        let format = [image::REGISTRATION_FORMAT];
        let flags = flags.to_le_bytes();
        let registration = hex(&make_sha3_token([&format[..], root.as_bytes()])[..16]);
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
        let program = program.map(|p| {
            hex(&make_sha3_token([&format[..], root.as_bytes(), &flags[..], p])[..16])
        });
        Ok(RegistrationCache { root: root_dir, build: build_id(), registration, program })
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

    /// The entry mapped into memory: only the pages a restore touches
    /// are read, and an instance decoded later reads its own. An entry
    /// is never rewritten in place (written to a temporary file and
    /// renamed), so the mapping stays valid.
    pub(crate) fn load(&self, entry: Entry) -> Option<Bytes> {
        let path = self.path(entry)?;
        let file = match fs::File::open(&path) {
            Ok(f) => f,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return None,
            Err(e) => {
                warn!("opening the {entry:?} image {}: {e}", path.display());
                return None;
            }
        };
        match unsafe { memmap2::Mmap::map(&file) } {
            Ok(map) => {
                info!("{entry:?} image {}", path.display());
                Some(Bytes::from_owner(map))
            }
            Err(e) => {
                warn!("mapping the {entry:?} image {}: {e}", path.display());
                None
            }
        }
    }

    /// Write the entry and remove other build ids' directories.
    pub(crate) fn store(&self, entry: Entry, image: &[u8]) -> Result<()> {
        let path = self.path(entry).ok_or_else(|| anyhow!("no {entry:?} entry"))?;
        let dir = self.dir();
        fs::create_dir_all(&dir)
            .with_context(|| format!("creating {}", dir.display()))?;
        // CR claude for claude: [risk] The temp name is `{key}.img.{pid}`. Two Shells in
        // one process that store one entry at once open, truncate and write the same
        // file, and the first rename installs whatever interleaving landed; the
        // parallel #[test]s of check_whole_script.rs and check_numeric_singleton.rs do
        // this on a fresh build id. Images differ from run to run (three --warm runs:
        // 1519550, 1522275 and 1519802 bytes), so the installed entry can be a mix. A
        // mixed entry restores as InvalidFormat ('compiling cold'), and a failed load
        // is never rewritten, so every later start of that binary compiles the root
        // cold. Give each writer its own temp name (pid plus a process-wide counter, or
        // tempfile::NamedTempFile::new_in(dir) then persist). (shell-13)
        let tmp = dir.join(format!(
            "{}.img.{}",
            self.key(entry).unwrap_or_default(),
            std::process::id()
        ));
        fs::write(&tmp, image).with_context(|| format!("writing {}", tmp.display()))?;
        fs::rename(&tmp, &path).with_context(|| format!("renaming {}", tmp.display()))?;
        // CR claude for claude: [perf] Every cold write deletes every other build id's
        // directory, so executables that share the cache evict each other and
        // alternating runs always start cold. That covers a dev and a quick graphix,
        // two standalone package builds, and every `cargo test`: its ShellBuilder tests
        // (check_runs_analyze.rs, examples_compile.rs) write under their own build ids
        // and delete the user's graphix entries. Within one build id nothing is
        // collected: each edit of a script adds a program entry holding the whole
        // session (1.5 MB for a one-line script, debug build).
        // design/program_image.md:60 says only build ids older than the current few are
        // collected. Collect by recency instead (touch an entry on load, remove what
        // has gone unused longest past a bound), and give the tests a cache directory
        // of their own. probe: a registration/<other-id>/ directory is gone after one
        // cold run of graphix or of the check_runs_analyze test executable; four
        // one-line edits of a script left four 1.5 MB program entries. (x-image-08)
        if let Ok(entries) = fs::read_dir(&self.root) {
            for entry in entries.flatten() {
                if entry.file_name() != self.build.as_str() {
                    let _ = fs::remove_dir_all(entry.path());
                }
            }
        }
        info!("{entry:?} image written to {}", path.display());
        Ok(())
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
        assert!(!id.is_empty());
        assert_eq!(id, build_id());
        if cfg!(target_os = "linux") {
            assert!(!id.starts_with('v'), "{id}");
            assert_eq!(id.len(), 40, "{id}");
        }
    }
}
