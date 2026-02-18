use std::path::Path;
use std::time::Duration;

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
    let skel_deps = skel_doc["dependencies"].as_table().unwrap();
    let mut mismatches = vec![];
    for (name, item) in skel_deps {
        let actual = item_version(name, item);
        let expected = expected_version(ws, name, &ws_doc);
        if actual != expected {
            mismatches.push(format!("  {name}: skel has {actual:?}, workspace has {expected:?}"));
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
        let version = crate_version(&ws.join("stdlib").join(&crate_name).join("Cargo.toml"));
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

fn vendor(ws: &Path) {
    let status = std::process::Command::new("python3")
        .arg(ws.join("vendor.py"))
        .current_dir(ws)
        .status()
        .expect("vendor.py");
    assert!(status.success(), "vendor.py failed");
}

fn write_vendor_config(dir: &Path, ws: &Path) {
    std::fs::create_dir_all(dir.join(".cargo")).unwrap();
    std::fs::write(
        dir.join(".cargo").join("config.toml"),
        format!(
            "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n\
             [source.vendored-sources]\ndirectory = \"{}\"\n",
            ws.join("vendor").display()
        ),
    )
    .unwrap();
}

#[tokio::test]
async fn created_package_compiles() {
    let ws = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    vendor(ws);
    let tmp = tempfile::tempdir().unwrap();
    super::create_package(tmp.path(), "graphix-package-testpkg")
        .await
        .unwrap();
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
    // Create package with main.gx
    let tmp = tempfile::tempdir().unwrap();
    super::create_package(tmp.path(), "graphix-package-testpkg")
        .await
        .unwrap();
    let pkg_dir = tmp.path().join("graphix-package-testpkg");
    let gx_dir = pkg_dir.join("src").join("graphix");
    tokio::fs::write(gx_dir.join("main.gx"), "println(\"GRAPHIX_STANDALONE_OK\")\n")
        .await
        .unwrap();
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
    // Build standalone
    let pm = super::GraphixPM::new().await.unwrap();
    pm.build_standalone(&pkg_dir, Some(&source_dir)).await.unwrap();
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
    let found = tokio::time::timeout(Duration::from_secs(30), async {
        loop {
            tokio::select! {
                line = out_lines.next_line() => match line.unwrap() {
                    Some(l) => {
                        if l.contains(sentinel) { return true; }
                        captured_stdout.push(l);
                    }
                    None => return false,
                },
                line = err_lines.next_line() => match line.unwrap() {
                    Some(l) => captured_stderr.push(l),
                    None => {},
                },
            }
        }
    })
    .await
    .unwrap_or(false);
    child.kill().await.ok();
    assert!(
        found,
        "sentinel not found.\nstdout: {:?}\nstderr: {:?}",
        captured_stdout, captured_stderr
    );
}
