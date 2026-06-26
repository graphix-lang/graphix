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
    let pm_version = env!("CARGO_PKG_VERSION");
    let mut mismatches = vec![];
    for &name in super::DEFAULT_PACKAGES {
        let crate_name = format!("graphix-package-{name}");
        let version =
            crate_version(&ws.join("stdlib").join(&crate_name).join("Cargo.toml"));
        if version != pm_version {
            mismatches.push(format!(
                "  {crate_name}: {version:?}, graphix-package: {pm_version:?}"
            ));
        }
    }
    assert!(
        mismatches.is_empty(),
        "stdlib package versions don't match graphix-package ({pm_version}):\n{}",
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

// C2: the package manager only adds external deps to Cargo.toml — the permanent
// stdlib optional deps and the entire [features] table must survive untouched
// (regressing this would break feature-selected builds).
#[tokio::test]
async fn update_cargo_toml_preserves_stdlib_and_features() {
    let ws = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let content =
        std::fs::read_to_string(ws.join("graphix-shell").join("Cargo.toml")).unwrap();
    let pm = super::GraphixPM::new().await.unwrap();
    let mut external = std::collections::BTreeMap::new();
    external
        .insert("widgets".to_string(), super::PackageEntry::Version("1.2.3".to_string()));
    let updated = pm.update_cargo_toml(&content, &external).unwrap();
    let orig: toml_edit::DocumentMut = content.parse().unwrap();
    let new: toml_edit::DocumentMut = updated.parse().unwrap();
    // [features] is byte-for-byte untouched
    assert_eq!(orig["features"].to_string(), new["features"].to_string());
    // every stdlib graphix-package-* dep survives unchanged
    let od = orig["dependencies"].as_table().unwrap();
    let nd = new["dependencies"].as_table().unwrap();
    for (k, v) in od.iter() {
        if k.starts_with("graphix-package-") {
            assert!(nd.contains_key(k), "stdlib dep {k} was dropped");
            assert_eq!(v.to_string(), nd[k].to_string(), "stdlib dep {k} changed");
        }
    }
    // the external was added
    assert_eq!(nd["graphix-package-widgets"].as_str(), Some("1.2.3"));
}

// C3: the marquee "removal really works" test — build the workspace shell with a
// reduced feature set and confirm dropped packages are genuinely gone from the
// binary (not just absent from a recorded list). Real build, no stubs.
#[test]
fn reduced_feature_build_drops_packages() {
    let ws = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let _lock = build_lock();
    let tmp = tempfile::tempdir().unwrap();
    let target = tmp.path().join("target");
    // Headless feature set (no gui/tui): keep str + re, drop http/sqlite/...
    let status = std::process::Command::new(env!("CARGO"))
        .current_dir(ws)
        .args([
            "build",
            "-p",
            "graphix-shell",
            "--no-default-features",
            "--features",
            "str re",
        ])
        .arg("--target-dir")
        .arg(&target)
        .status()
        .expect("spawn cargo build");
    assert!(status.success(), "reduced-feature shell build failed");
    let bin = target.join("debug").join("graphix");
    assert!(bin.is_file(), "binary not found at {}", bin.display());
    let check = |src: &str| -> bool {
        let f = tmp.path().join("prog.gx");
        std::fs::write(&f, src).unwrap();
        std::process::Command::new(&bin)
            .arg("--check")
            .arg(&f)
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .expect("run graphix --check")
            .success()
    };
    // present packages check; dropped packages fail to resolve their module
    assert!(check("use str;\nstr::len(\"hi\")\n"), "use str should check");
    assert!(check("use re;\nre::is_match(#pat:\"x\", \"x\")\n"), "use re should check");
    assert!(!check("use http\n"), "use http should fail (feature dropped)");
    assert!(!check("use sqlite\n"), "use sqlite should fail (feature dropped)");
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
    // C6: the standalone binary is minimal — built with only the embedded
    // package's dependency closure (testpkg depends on core only), so a package
    // it never depended on (gui) is genuinely absent.
    let gui_prog = tmp.path().join("use_gui.gx");
    tokio::fs::write(&gui_prog, "use gui\n").await.unwrap();
    let gui_status = tokio::process::Command::new(&bin_path)
        .arg("--no-netidx")
        .arg("--check")
        .arg(&gui_prog)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .await
        .expect("run standalone --check");
    assert!(!gui_status.success(), "standalone binary should not resolve `use gui`");
}

// ---- Pure-function unit tests (no stdin / network / filesystem) ----
mod pure {
    use super::super::{
        apply_selection, compute_update_plan, feature_depends_on, feature_edges,
        installed_dependents, normalize_selection, parse_packages, parse_toggles,
        plan_items, selection_from_indices, stdlib_packages_in_cargo_toml,
        to_toml_string, version_gt, PackageEntry, Packages, Selection, UpdatePlan,
        DEFAULT_PACKAGES,
    };
    use std::{
        collections::{BTreeMap, BTreeSet},
        path::{Path, PathBuf},
    };

    fn sset(names: &[&str]) -> BTreeSet<String> {
        names.iter().map(|s| s.to_string()).collect()
    }

    fn ext(pairs: &[(&str, PackageEntry)]) -> BTreeMap<String, PackageEntry> {
        pairs.iter().map(|(k, v)| (k.to_string(), v.clone())).collect()
    }

    fn ver(v: &str) -> PackageEntry {
        PackageEntry::Version(v.to_string())
    }

    fn parse(s: &str) -> Packages {
        parse_packages(s).unwrap().0
    }

    fn migrated(s: &str) -> bool {
        parse_packages(s).unwrap().1
    }

    #[test]
    fn migrate_all_default_stdlib() {
        let mut toml = String::from("[packages]\n");
        for name in DEFAULT_PACKAGES {
            toml.push_str(&format!("{name} = \"0.4.0\"\n"));
        }
        let p = parse(&toml);
        assert_eq!(p.stdlib_installed, sset(DEFAULT_PACKAGES));
        assert!(p.stdlib_removed.is_empty());
        assert!(p.external.is_empty());
    }

    #[test]
    fn migrate_missing_stdlib_goes_to_removed() {
        let toml = "[packages]\ncore = \"0.4.0\"\narray = \"0.4.0\"\nfoo = \"1.0.0\"\n";
        let p = parse(toml);
        assert!(p.stdlib_installed.contains("core"));
        assert!(p.stdlib_installed.contains("array"));
        assert!(!p.stdlib_installed.contains("gui"));
        // stdlib names absent from the old file land in removed
        assert!(p.stdlib_removed.contains("gui"));
        assert!(p.stdlib_removed.contains("xls"));
        // unknown names are preserved as external
        assert_eq!(p.external.get("foo"), Some(&ver("1.0.0")));
    }

    #[test]
    fn migrate_remaps_legacy_packages_to_sys() {
        // a pre-`sys` file: fs/net/time were the old way to get that
        // functionality, now merged into sys.
        let toml = "[packages]\n\
                    core = \"0.4.0\"\narray = \"0.4.0\"\n\
                    fs = \"0.4.0\"\nnet = \"0.4.0\"\ntime = \"0.4.0\"\n\
                    mywidgets = \"1.0.0\"\n";
        let p = parse(toml);
        // dead names dropped, sys installed in their place
        assert!(p.stdlib_installed.contains("sys"));
        assert!(!p.external.contains_key("fs"));
        assert!(!p.external.contains_key("net"));
        assert!(!p.external.contains_key("time"));
        assert!(!p.stdlib_removed.contains("sys"));
        // a real third-party package is still preserved
        assert_eq!(p.external.get("mywidgets"), Some(&ver("1.0.0")));
    }

    #[test]
    fn migrate_stdlib_path_drops_override() {
        let toml = "[packages]\ncore = \"0.4.0\"\ngui = { path = \"/local/gui\" }\n";
        let p = parse(toml);
        // stdlib path override is dropped; treated as a plain installed stdlib
        assert!(p.stdlib_installed.contains("gui"));
        assert!(!p.external.contains_key("gui"));
    }

    #[test]
    fn core_cannot_be_removed() {
        let mut p = Packages {
            stdlib_installed: BTreeSet::new(),
            stdlib_removed: sset(&["core"]),
            external: BTreeMap::new(),
        };
        p.enforce_invariants();
        assert!(p.stdlib_installed.contains("core"));
        assert!(!p.stdlib_removed.contains("core"));
    }

    #[test]
    fn roundtrip_v2() {
        let p = Packages {
            stdlib_installed: sset(&["array", "core"]),
            stdlib_removed: sset(&["gui"]),
            external: ext(&[
                ("widgets", ver("1.2.3")),
                ("local", PackageEntry::Path(PathBuf::from("/tmp/local"))),
            ]),
        };
        let s = to_toml_string(&p).unwrap();
        assert!(s.contains("[stdlib]"));
        // already v2 -> parsed without migration
        assert_eq!(parse(&s), p);
    }

    #[test]
    fn roundtrip_empty_removed() {
        let p = Packages {
            stdlib_installed: sset(&["core"]),
            stdlib_removed: BTreeSet::new(),
            external: BTreeMap::new(),
        };
        let s = to_toml_string(&p).unwrap();
        assert!(s.contains("removed"));
        assert_eq!(parse(&s), p);
    }

    #[test]
    fn format_detection() {
        // a [stdlib] file parses as v2 (not migrated)
        assert!(!migrated("[stdlib]\ninstalled = [\"core\"]\nremoved = []\n"));
        // a flat [packages] file is migrated
        assert!(migrated("[packages]\ncore = \"0.4.0\"\n"));
    }

    #[test]
    fn enumerate_stdlib_from_cargo_toml() {
        let content = "\
[dependencies]\n\
graphix-package-foo = \"0.9.0\"\n\
graphix-package-gui = { version = \"0.9.0\", optional = true }\n\
graphix-package = { version = \"0.9.0\", path = \"../graphix-package\" }\n\
anyhow = \"1\"\n";
        let set = stdlib_packages_in_cargo_toml(content).unwrap();
        // includes optional gui, excludes graphix-package (no dash) and anyhow
        assert_eq!(set, sset(&["foo", "gui"]));
    }

    #[test]
    fn version_gt_is_semver() {
        assert!(version_gt("1.10.0", "1.9.0"));
        assert!(!version_gt("1.9.0", "1.10.0"));
        assert!(!version_gt("0.9.0", "0.9.0"));
        assert!(version_gt("0.10.0", "0.9.0"));
    }

    #[test]
    fn plan_empty_when_nothing_changed() {
        let pkgs = Packages {
            stdlib_installed: sset(&["core", "array"]),
            stdlib_removed: BTreeSet::new(),
            external: BTreeMap::new(),
        };
        let plan = compute_update_plan(
            "0.9.0",
            "0.9.0",
            &sset(&["core", "array"]),
            &pkgs,
            &BTreeMap::new(),
        );
        assert!(plan.is_empty());
    }

    #[test]
    fn plan_detects_shell_new_and_external() {
        let pkgs = Packages {
            stdlib_installed: sset(&["core"]),
            stdlib_removed: sset(&["xls"]),
            external: ext(&[
                ("widgets", ver("1.2.0")),
                ("local", PackageEntry::Path(PathBuf::from("/x"))),
            ]),
        };
        // math is new; bench is internal; xls is already removed
        let src = sset(&["core", "math", "bench", "xls"]);
        let mut latest = BTreeMap::new();
        latest.insert("widgets".to_string(), "1.5.0".to_string());
        let plan = compute_update_plan("0.9.0", "0.10.0", &src, &pkgs, &latest);
        assert_eq!(plan.shell, Some(("0.9.0".to_string(), "0.10.0".to_string())));
        assert_eq!(plan.new_stdlib, vec!["math".to_string()]);
        assert_eq!(
            plan.external_updates,
            vec![("widgets".to_string(), "1.2.0".to_string(), "1.5.0".to_string())]
        );
    }

    #[test]
    fn apply_accept_all_with_bump() {
        let mut pkgs = Packages {
            stdlib_installed: sset(&["core"]),
            stdlib_removed: BTreeSet::new(),
            external: ext(&[("widgets", ver("1.2.0"))]),
        };
        let plan = UpdatePlan {
            shell: Some(("0.9.0".to_string(), "0.10.0".to_string())),
            new_stdlib: vec!["math".to_string(), "time".to_string()],
            external_updates: vec![(
                "widgets".to_string(),
                "1.2.0".to_string(),
                "1.5.0".to_string(),
            )],
        };
        // accept math, decline time
        let sel = Selection {
            shell: true,
            new_stdlib: sset(&["math"]),
            external: sset(&["widgets"]),
        };
        let bv = apply_selection("0.9.0", "0.10.0", &plan, &sel, &mut pkgs);
        assert_eq!(bv, "0.10.0");
        assert!(pkgs.stdlib_installed.contains("math"));
        assert!(pkgs.stdlib_removed.contains("time"));
        assert_eq!(pkgs.external.get("widgets"), Some(&ver("1.5.0")));
    }

    #[test]
    fn apply_shell_declined_keeps_new_pending() {
        let mut pkgs = Packages {
            stdlib_installed: sset(&["core"]),
            stdlib_removed: BTreeSet::new(),
            external: ext(&[("widgets", ver("1.2.0"))]),
        };
        let plan = UpdatePlan {
            shell: Some(("0.9.0".to_string(), "0.10.0".to_string())),
            new_stdlib: vec!["math".to_string()],
            external_updates: vec![(
                "widgets".to_string(),
                "1.2.0".to_string(),
                "1.5.0".to_string(),
            )],
        };
        let sel = Selection {
            shell: false,
            new_stdlib: BTreeSet::new(),
            external: sset(&["widgets"]),
        };
        let bv = apply_selection("0.9.0", "0.10.0", &plan, &sel, &mut pkgs);
        assert_eq!(bv, "0.9.0");
        // new stdlib stays pending (neither installed nor removed)
        assert!(!pkgs.stdlib_installed.contains("math"));
        assert!(!pkgs.stdlib_removed.contains("math"));
        // external update still applied
        assert_eq!(pkgs.external.get("widgets"), Some(&ver("1.5.0")));
    }

    #[test]
    fn apply_no_shell_item_allows_new_optin() {
        let mut pkgs = Packages {
            stdlib_installed: sset(&["core"]),
            stdlib_removed: BTreeSet::new(),
            external: BTreeMap::new(),
        };
        let plan = UpdatePlan {
            shell: None,
            new_stdlib: vec!["math".to_string()],
            external_updates: vec![],
        };
        let sel = Selection {
            shell: false,
            new_stdlib: sset(&["math"]),
            external: BTreeSet::new(),
        };
        // build version is current == latest, so the opt-in still applies
        let bv = apply_selection("0.9.0", "0.9.0", &plan, &sel, &mut pkgs);
        assert_eq!(bv, "0.9.0");
        assert!(pkgs.stdlib_installed.contains("math"));
    }

    #[test]
    fn parse_toggles_ignores_invalid() {
        assert_eq!(parse_toggles("1 3, 5", 5), vec![0, 2, 4]);
        // 0 and 6 are out of range, x is non-numeric
        assert_eq!(parse_toggles("0 6 x 2", 5), vec![1]);
        assert!(parse_toggles("", 5).is_empty());
    }

    #[test]
    fn normalize_deselects_new_stdlib_when_shell_off() {
        // items: [0]=shell, [1]=new stdlib "math", [2]=external "widgets"
        let plan = UpdatePlan {
            shell: Some(("0.9.0".to_string(), "0.10.0".to_string())),
            new_stdlib: vec!["math".to_string()],
            external_updates: vec![(
                "widgets".to_string(),
                "1.2.0".to_string(),
                "1.5.0".to_string(),
            )],
        };
        let items = plan_items(&plan);
        // shell (0) deselected, math (1) + widgets (2) still selected
        let mut selected: BTreeSet<usize> = [1usize, 2].into_iter().collect();
        let off = normalize_selection(&items, &mut selected);
        assert_eq!(off, vec!["math".to_string()]);
        // math forced off; the external is untouched (independent of the shell)
        assert!(!selected.contains(&1));
        assert!(selected.contains(&2));
        // with the shell selected, nothing is forced off
        let mut keep: BTreeSet<usize> = [0usize, 1, 2].into_iter().collect();
        assert!(normalize_selection(&items, &mut keep).is_empty());
        assert!(keep.contains(&1));
    }

    #[test]
    fn selection_from_indices_maps_items() {
        let plan = UpdatePlan {
            shell: Some(("0.9.0".to_string(), "0.10.0".to_string())),
            new_stdlib: vec!["math".to_string()],
            external_updates: vec![(
                "widgets".to_string(),
                "1.2.0".to_string(),
                "1.5.0".to_string(),
            )],
        };
        let items = plan_items(&plan);
        // select only the shell (0) and the external (2)
        let selected: BTreeSet<usize> = [0usize, 2].into_iter().collect();
        let sel = selection_from_indices(&items, &selected);
        assert!(sel.shell);
        assert!(sel.new_stdlib.is_empty());
        assert_eq!(sel.external, sset(&["widgets"]));
    }

    // ----- C1: BuildPlan + the feature dependency graph -----

    #[test]
    fn build_plan_features_exclude_core_sorted() {
        let pkgs = Packages {
            stdlib_installed: sset(&["core", "str", "json", "sys"]),
            external: ext(&[("widgets", ver("1.0.0"))]),
            ..Default::default()
        };
        let plan = pkgs.build_plan();
        // sorted (from a BTreeSet), core excluded (always compiled, not a feature)
        assert_eq!(plan.features, ["json", "str", "sys"].map(String::from).to_vec());
        assert_eq!(plan.external, ext(&[("widgets", ver("1.0.0"))]));
    }

    const FEATURES_TOML: &str = "\
[features]
default = [\"all\"]
all = [\"str\", \"sys\", \"json\", \"hbs\", \"tui\", \"array\"]
str = [\"dep:graphix-package-str\"]
sys = [\"dep:graphix-package-sys\"]
array = [\"dep:graphix-package-array\"]
json = [\"dep:graphix-package-json\", \"sys\"]
hbs = [\"dep:graphix-package-hbs\", \"json\"]
tui = [\"dep:graphix-package-tui\", \"array\", \"sys\"]
krb5_iov = [\"graphix-package-sys?/krb5_iov\", \"graphix-package-http?/krb5_iov\"]
";

    #[test]
    fn feature_graph_transitive_dependents() {
        let edges = feature_edges(FEATURES_TOML).unwrap();
        assert!(feature_depends_on("json", "sys", &edges)); // direct
        assert!(feature_depends_on("hbs", "sys", &edges)); // transitive via json
        assert!(feature_depends_on("tui", "array", &edges));
        assert!(!feature_depends_on("str", "sys", &edges));
        assert!(!feature_depends_on("sys", "json", &edges)); // not the reverse
                                                             // `?/` weak entries are not edges, so krb5_iov depends on nothing
        assert!(!feature_depends_on("krb5_iov", "sys", &edges));
        let installed = sset(&["str", "sys", "json", "hbs", "tui", "array"]);
        let mut deps = installed_dependents("sys", &installed, &edges);
        deps.sort();
        assert_eq!(deps, ["hbs", "json", "tui"].map(String::from).to_vec());
    }

    // ----- C5/C7: the committed shell's feature wiring + files -----

    fn shell_cargo_toml() -> String {
        let ws = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
        std::fs::read_to_string(ws.join("graphix-shell").join("Cargo.toml")).unwrap()
    }

    #[test]
    fn every_stdlib_package_has_a_feature() {
        let content = shell_cargo_toml();
        let doc: toml_edit::DocumentMut = content.parse().unwrap();
        let feats = doc["features"].as_table().unwrap();
        let all: BTreeSet<String> = feats["all"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|v| v.as_str())
            .map(String::from)
            .collect();
        for &name in DEFAULT_PACKAGES {
            if name == "core" {
                continue;
            }
            assert!(feats.contains_key(name), "no [features] entry for {name}");
            assert!(all.contains(name), "`all` is missing stdlib package {name}");
        }
        // bench is internal but must still be registered by a default build (M2)
        assert!(all.contains("bench"), "`all` must include bench");
        // core is non-optional — never a feature
        assert!(!feats.contains_key("core"), "core must not be a feature");
    }

    #[test]
    fn krb5_iov_uses_weak_dep_syntax() {
        let content = shell_cargo_toml();
        // krb5_iov must reference sys/http weakly (`?/`) so enabling it never
        // force-enables those optional packages, and so it creates no edge.
        assert!(content.contains("graphix-package-sys?/krb5_iov"));
        assert!(content.contains("graphix-package-http?/krb5_iov"));
        let edges = feature_edges(&content).unwrap();
        assert!(!feature_depends_on("krb5_iov", "sys", &edges));
        assert!(!feature_depends_on("krb5_iov", "http", &edges));
    }

    #[test]
    fn shell_ships_packages_rs_and_deps_rs() {
        let ws = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
        let src = ws.join("graphix-shell").join("src");
        assert!(src.join("packages.rs").is_file(), "committed src/packages.rs missing");
        assert!(src.join("deps.rs").is_file(), "src/deps.rs missing");
    }
}
