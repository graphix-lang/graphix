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
/// stamps per link; a build without one falls back to the compiler
/// version, which is not unique per build but still separates releases.
fn build_id() -> String {
    match std::env::current_exe().ok().and_then(|p| elf_build_id(&p)) {
        Some(id) => id,
        None => format!("v{}", env!("CARGO_PKG_VERSION")),
    }
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
    let file = fs::read(exe).ok()?;
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
    pub(crate) fn new(root: &str, program: Option<&[u8]>) -> Result<Self> {
        let root_dir = dirs::cache_dir()
            .ok_or_else(|| anyhow!("no cache directory"))?
            .join("graphix")
            .join("registration");
        let format = [image::REGISTRATION_FORMAT];
        let registration = hex(&make_sha3_token([&format[..], root.as_bytes()])[..16]);
        let program = program
            .map(|p| hex(&make_sha3_token([&format[..], root.as_bytes(), p])[..16]));
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
        let tmp = dir.join(format!(
            "{}.img.{}",
            self.key(entry).unwrap_or_default(),
            std::process::id()
        ));
        fs::write(&tmp, image).with_context(|| format!("writing {}", tmp.display()))?;
        fs::rename(&tmp, &path).with_context(|| format!("renaming {}", tmp.display()))?;
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
