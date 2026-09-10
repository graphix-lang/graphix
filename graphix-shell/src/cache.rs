//! The registration image cache. The executable that reads an image is
//! the executable that wrote it, on its first run: entries live under
//! the cache directory by the executable's build id, which also covers
//! the packages compiled into it, keyed by the root that declares them
//! and the image format, so a rebuilt executable misses and recompiles.
//! Entries are written to a temporary file and renamed into place, and
//! other build ids' directories are removed when one is written.

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
    key: String,
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
    /// The cache entry for this root, or an error when there is no
    /// cache directory.
    pub(crate) fn new(root: &str) -> Result<Self> {
        let root_dir = dirs::cache_dir()
            .ok_or_else(|| anyhow!("no cache directory"))?
            .join("graphix")
            .join("registration");
        let parts: [&[u8]; 2] = [&[image::REGISTRATION_FORMAT], root.as_bytes()];
        let key = hex(&make_sha3_token(parts)[..16]);
        Ok(RegistrationCache { root: root_dir, build: build_id(), key })
    }

    fn dir(&self) -> PathBuf {
        self.root.join(&self.build)
    }

    fn path(&self) -> PathBuf {
        self.dir().join(format!("{}.img", self.key))
    }

    pub(crate) fn load(&self) -> Option<Bytes> {
        let path = self.path();
        match fs::read(&path) {
            Ok(bytes) => {
                info!("registration image {}", path.display());
                Some(Bytes::from(bytes))
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
            Err(e) => {
                warn!("reading the registration image {}: {e}", path.display());
                None
            }
        }
    }

    /// Write the entry and remove other build ids' directories.
    pub(crate) fn store(&self, image: &[u8]) -> Result<()> {
        let dir = self.dir();
        fs::create_dir_all(&dir)
            .with_context(|| format!("creating {}", dir.display()))?;
        let tmp = dir.join(format!("{}.img.{}", self.key, std::process::id()));
        fs::write(&tmp, image).with_context(|| format!("writing {}", tmp.display()))?;
        fs::rename(&tmp, self.path())
            .with_context(|| format!("renaming {}", tmp.display()))?;
        if let Ok(entries) = fs::read_dir(&self.root) {
            for entry in entries.flatten() {
                if entry.file_name() != self.build.as_str() {
                    let _ = fs::remove_dir_all(entry.path());
                }
            }
        }
        info!("registration image written to {}", self.path().display());
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
