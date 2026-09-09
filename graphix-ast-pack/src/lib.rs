//! Build-time helper for graphix packages. A package's `build.rs` calls
//! [`emit`], which parses every module under `src/graphix/*` and writes
//! `OUT_DIR/graphix_ast.pack`, a blob of `(vfs_path, source, packed_ast)`
//! entries that `defpackage!` embeds and decodes at `register` time.
//! The blob is produced by the same compiler that consumes it, so it
//! carries no version field.

use anyhow::{Context, Result};
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::expr::{Origin, Source, parser, serialize};
use std::{
    env, fs,
    path::{Component, Path, PathBuf},
};
use walkdir::WalkDir;

/// The package name as `defpackage!` computes it: the crate name with
/// hyphens as underscores and the `graphix_package_` prefix stripped.
/// Must match `graphix-derive`'s `PACKAGE_NAME`.
fn package_name() -> Result<String> {
    let crate_name =
        env::var("CARGO_PKG_NAME").context("CARGO_PKG_NAME not set")?.replace('-', "_");
    Ok(match crate_name.strip_prefix("graphix_package_") {
        Some(name) => name.to_string(),
        None => crate_name,
    })
}

/// The VFS key for a source file: `/{package}/{rel-path-with-extension}`,
/// matching `graphix-derive`'s `graphix_files` key scheme.
fn vfs_path(package: &str, rel: &Path) -> String {
    let mut s = format!("/{package}");
    for c in rel.components() {
        if let Component::Normal(p) = c {
            s.push('/');
            s.push_str(&p.to_string_lossy());
        }
    }
    s
}

/// Parse and pack every `.gx`/`.gxi` under `src/graphix` (except `main.gx`,
/// the standalone user program) into `OUT_DIR/graphix_ast.pack`.
/// Call from a package `build.rs`.
pub fn emit() -> Result<()> {
    let manifest =
        env::var("CARGO_MANIFEST_DIR").context("CARGO_MANIFEST_DIR not set")?;
    let src_dir = PathBuf::from(&manifest).join("src").join("graphix");
    let out_dir = env::var("OUT_DIR").context("OUT_DIR not set")?;
    let blob_path = PathBuf::from(&out_dir).join("graphix_ast.pack");
    let package = package_name()?;
    println!("cargo:rerun-if-changed={}", src_dir.display());

    // `register` always `include_bytes!`s a blob, so an empty one is written.
    if !src_dir.exists() {
        fs::write(&blob_path, &serialize::pack_index(&[])?)
            .with_context(|| format!("writing {blob_path:?}"))?;
        return Ok(());
    }

    let mut entries: Vec<(ArcStr, ArcStr, Bytes)> = Vec::new();
    for entry in WalkDir::new(&src_dir).sort_by_file_name() {
        let entry = entry?;
        let path = entry.path();
        if !entry.file_type().is_file() {
            continue;
        }
        let is_gx = path.extension().is_some_and(|e| e == "gx");
        let is_gxi = path.extension().is_some_and(|e| e == "gxi");
        if !is_gx && !is_gxi {
            continue;
        }
        let rel = path.strip_prefix(&src_dir)?;
        if rel == Path::new("main.gx") {
            continue;
        }
        println!("cargo:rerun-if-changed={}", path.display());
        let source = ArcStr::from(
            fs::read_to_string(path).with_context(|| format!("reading {path:?}"))?,
        );
        let key = vfs_path(&package, rel);
        let ori = Origin {
            parent: None,
            source: Source::Internal(ArcStr::from(rel.to_string_lossy().as_ref())),
            text: source.clone(),
        };
        let packed = if is_gx {
            let exprs =
                parser::parse(ori).with_context(|| format!("parsing {path:?}"))?;
            serialize::pack_module(&exprs)?
        } else {
            let sig =
                parser::parse_sig(ori).with_context(|| format!("parsing {path:?}"))?;
            serialize::pack_sig(&sig)?
        };
        entries.push((ArcStr::from(key), source, packed));
    }

    let blob = serialize::pack_index(&entries)?;
    fs::write(&blob_path, &blob).with_context(|| format!("writing {blob_path:?}"))?;
    Ok(())
}
