use std::{
    path::Path,
    sync::{Mutex, Once},
    time::Duration,
};

// vendor.py must only run once — concurrent runs would clobber each other.
static VENDOR_ONCE: Once = Once::new();

// Serialize tests that spawn cargo builds. They're expensive in CPU,
// memory, and disk, and concurrent cargo invocations sharing a target
// dir will fight over the lock file.
static BUILD_LOCK: Mutex<()> = Mutex::new(());

// Acquire BUILD_LOCK, recovering it if a previous holder panicked. The lock
// guards nothing but mutual exclusion, so a poisoned lock is fine to reuse —
// without this, a panic in one build test cascades into an opaque PoisonError
// in the next, masking the original failure.
fn build_lock() -> std::sync::MutexGuard<'static, ()> {
    BUILD_LOCK.lock().unwrap_or_else(|e| e.into_inner())
}

/// Extract the version string from a TOML dependency item.
/// Handles both `dep = "version"` and `dep = { version = "...", ... }`.
fn item_version(name: &str, item: &toml_edit::Item) -> String {
    if let Some(s) = item.as_str() {
        return s.to_string();
    }
    item.get("version")
        .and_then(|v| v.as_str())
        .unwrap_or_else(|| panic!("{name} has no version"))
        .to_string()
}

/// Read `[package].version` from a crate's Cargo.toml.
fn crate_version(path: &Path) -> String {
    let content = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
    let doc: toml_edit::DocumentMut = content.parse().unwrap();
    doc["package"]["version"]
        .as_str()
        .unwrap_or_else(|| panic!("no [package].version in {}", path.display()))
        .to_string()
}

/// Resolve the expected version for a skeleton dependency from the
/// workspace. For graphix-* crates this is their own package version;
/// for third-party crates it's the workspace dependency version.
fn expected_version(ws: &Path, name: &str, ws_doc: &toml_edit::DocumentMut) -> String {
    if name.starts_with("graphix-") {
        let dir = if name == "graphix-package" {
            ws.join(name)
        } else if name.starts_with("graphix-package-") {
            ws.join("stdlib").join(name)
        } else {
            ws.join(name)
        };
        crate_version(&dir.join("Cargo.toml"))
    } else {
        let dep = &ws_doc["workspace"]["dependencies"][name];
        item_version(name, dep)
    }
}

#[test]
fn skel_cargo_toml_versions_match_workspace() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let ws = manifest_dir.parent().expect("workspace root");
    let ws_content = std::fs::read_to_string(ws.join("Cargo.toml")).unwrap();
    let ws_doc: toml_edit::DocumentMut = ws_content.parse().unwrap();
    let skel_doc: toml_edit::DocumentMut = super::SKEL.cargo_toml.parse().unwrap();
    let mut mismatches = vec![];
    // Check both [dependencies] and [build-dependencies]: graphix-ast-pack
    // is a build-dependency whose version must track the workspace, else a
    // created package's build.rs would request a version that isn't vendored
    // (nor published), breaking the build with no other warning.
    for section in ["dependencies", "build-dependencies"] {
        let Some(deps) = skel_doc.get(section).and_then(|t| t.as_table()) else {
            continue;
        };
        for (name, item) in deps {
            let actual = item_version(name, item);
            let expected = expected_version(ws, name, &ws_doc);
            if actual != expected {
                mismatches.push(format!(
                    "  [{section}] {name}: skel has {actual:?}, workspace has {expected:?}"
                ));
            }
        }
    }
    assert!(
        mismatches.is_empty(),
        "skel/Cargo.toml version mismatches:\n{}",
        mismatches.join("\n")
    );
}

#[test]
fn stdlib_package_versions_match_graphix_package() {
    let ws = Path::new(env!("CARGO_MANIFEST_DIR")).parent().expect("workspace root");
    let mut mismatches = vec![];
    for &(name, _) in super::DEFAULT_PACKAGES {
        let crate_name = format!("graphix-package-{name}");
        let version =
            crate_version(&ws.join("stdlib").join(&crate_name).join("Cargo.toml"));
        if version != super::SKEL.version {
            mismatches.push(format!(
                "  {crate_name}: {version:?}, graphix-package: {:?}",
                super::SKEL.version
            ));
        }
    }
    assert!(
        mismatches.is_empty(),
        "stdlib package versions don't match graphix-package ({}):\n{}",
        super::SKEL.version,
        mismatches.join("\n")
    );
}

#[tokio::test]
async fn download_source_extracts_package_at_expected_root() {
    let tmp = tempfile::tempdir().unwrap();
    let cratesio = crates_io_api::AsyncClient::new(
        "Graphix Package Tests <eestokes@pm.me>",
        Duration::from_secs(1),
    )
    .unwrap();
    let source_dir =
        super::download_source(&cratesio, tmp.path(), "0.5.0").await.unwrap();
    let nested = source_dir.join("graphix-shell-0.5.0");
    assert_eq!(source_dir, tmp.path().join("build").join("graphix-shell-0.5.0"));
    assert!(
        source_dir.join("Cargo.toml").is_file(),
        "missing Cargo.toml at extracted root: {}",
        source_dir.display()
    );
    assert!(
        source_dir.join("src").join("deps.rs").is_file(),
        "missing src/deps.rs at extracted root: {}",
        source_dir.display()
    );
    assert!(
        !nested.join("Cargo.toml").exists(),
        "crate archive was unpacked one level too deep: {}",
        nested.display()
    );
}

fn vendor(ws: &Path) {
    VENDOR_ONCE.call_once(|| {
        let status = std::process::Command::new("python3")
            .arg(ws.join("vendor.py"))
            .current_dir(ws)
            .status()
            .expect("vendor.py");
        assert!(status.success(), "vendor.py failed");
        // vendor.py writes .cargo/config.toml into the workspace root,
        // but tests write their own per-package configs. Remove it so
        // we don't leave the workspace pointing at vendored sources.
        let _ = std::fs::remove_file(ws.join(".cargo/config.toml"));
    });
}

fn write_vendor_config(dir: &Path, ws: &Path) {
    std::fs::create_dir_all(dir.join(".cargo")).unwrap();
    std::fs::write(
        dir.join(".cargo").join("config.toml"),
        format!(
            "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n\
             [source.vendored-sources]\ndirectory = \"{}\"\n",
            ws.join("vendor").display().to_string().replace('\\', "/")
        ),
    )
    .unwrap();
}

#[tokio::test]
async fn created_package_compiles() {
    let ws = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    vendor(ws);
    let _lock = build_lock();
    let tmp = tempfile::tempdir().unwrap();
    super::create_package(tmp.path(), "graphix-package-testpkg").await.unwrap();
    let pkg_dir = tmp.path().join("graphix-package-testpkg");
    write_vendor_config(&pkg_dir, ws);
    let status = tokio::process::Command::new("cargo")
        .arg("check")
        .current_dir(&pkg_dir)
        .status()
        .await
        .expect("cargo check");
    assert!(status.success(), "cargo check failed on generated package");
}

#[tokio::test]
async fn build_standalone_produces_working_binary() {
    use tokio::io::AsyncBufReadExt;
    let ws = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    vendor(ws);
    let _lock = build_lock();
    // Create package with main.gx
    let tmp = tempfile::tempdir().unwrap();
    super::create_package(tmp.path(), "graphix-package-testpkg").await.unwrap();
    let pkg_dir = tmp.path().join("graphix-package-testpkg");
    let gx_dir = pkg_dir.join("src").join("graphix");
    // main.gx imports the package's OWN graphix module (src/graphix/mod.gx,
    // which the package build.rs parses and packs into the pre-parsed AST
    // blob). Reaching `testpkg::var` (a pure .gx value = 42) and
    // `testpkg::example` (the rust builtin bound in mod.gx) proves the
    // created package's packed AST was decoded and the module resolved at
    // runtime — the end-to-end build.rs → blob → unpack_index → unpack_module
    // path, exercised through a freshly created package rather than the
    // in-tree stdlib.
    let main_gx = "\
let v = testpkg::var;
let ex = testpkg::example(v);
println(\"GRAPHIX_STANDALONE_OK var=[v] ex=[ex]\")
";
    tokio::fs::write(gx_dir.join("main.gx"), main_gx).await.unwrap();
    write_vendor_config(&pkg_dir, ws);
    // Copy vendored graphix-shell source (already has resolved deps)
    let vendored = std::fs::read_dir(ws.join("vendor"))
        .unwrap()
        .filter_map(|e| e.ok())
        .find(|e| {
            e.file_name()
                .to_str()
                .map(|n| n.starts_with("graphix-shell-"))
                .unwrap_or(false)
        })
        .expect("vendored graphix-shell not found")
        .path();
    let source_dir = tmp.path().join("graphix-shell");
    cp_r::CopyOptions::new()
        .copy_tree(&vendored, &source_dir)
        .expect("copy vendored graphix-shell");
    write_vendor_config(&source_dir, ws);
    // Build standalone. Surface a build_standalone error directly rather than
    // letting it manifest later as a confusing "binary not found".
    let pm = super::GraphixPM::new().await.unwrap();
    pm.build_standalone(&pkg_dir, Some(&source_dir)).await.expect("build_standalone");
    // Run the binary
    let bin_name = format!("testpkg{}", std::env::consts::EXE_SUFFIX);
    let bin_path = pkg_dir.join(&bin_name);
    assert!(bin_path.exists(), "standalone binary not found");
    let mut child = tokio::process::Command::new(&bin_path)
        .arg("--no-netidx")
        .arg("-i")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("spawn standalone binary");
    let _stdin = child.stdin.take();
    let stdout = child.stdout.take().unwrap();
    let stderr = child.stderr.take().unwrap();
    let mut out_lines = tokio::io::BufReader::new(stdout).lines();
    let mut err_lines = tokio::io::BufReader::new(stderr).lines();
    let sentinel = "GRAPHIX_STANDALONE_OK";
    let mut captured_stdout = Vec::new();
    let mut captured_stderr = Vec::new();
    let line = tokio::time::timeout(Duration::from_secs(30), async {
        loop {
            tokio::select! {
                line = out_lines.next_line() => match line.unwrap() {
                    Some(l) => {
                        if l.contains(sentinel) { return Some(l); }
                        captured_stdout.push(l);
                    }
                    None => return None,
                },
                line = err_lines.next_line() => match line.unwrap() {
                    Some(l) => captured_stderr.push(l),
                    None => {},
                },
            }
        }
    })
    .await
    .unwrap_or(None);
    child.kill().await.ok();
    let line = line.unwrap_or_else(|| {
        panic!(
            "sentinel not found.\nstdout: {:?}\nstderr: {:?}",
            captured_stdout, captured_stderr
        )
    });
    // The sentinel line embeds values read from the created package's packed
    // graphix module: var=42 (a pure .gx value) and ex=false (the rust builtin
    // `example` applied to 42, a non-error). Their presence proves the packed
    // AST decoded correctly and the module resolved end-to-end.
    assert!(
        line.contains("var=42") && line.contains("ex=false"),
        "created package's packed module decoded incorrectly.\n\
         line: {line:?}\nstderr: {:?}",
        captured_stderr
    );
}
