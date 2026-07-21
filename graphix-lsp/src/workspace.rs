//! Workspace scanner that turns a set of root directories into a
//! project graph.
//!
//! For each `.gx` and `.gxi` file under the workspace, we parse just
//! enough to extract every `mod foo;` declaration, then resolve each
//! declaration to the concrete file the module resolver would have
//! picked. Files with no incoming edges are project roots; everything
//! reachable from a root forms that project.

use ahash::{AHashMap, AHashSet};
use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::expr::{ExprKind, ModuleKind, Origin, SigKind, Source, parser};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileKind {
    Gx,
    Gxi,
}

/// A single file in the workspace plus the names of every external
/// module it declares (`mod foo;` lines, including those inside
/// signatures of `.gxi` files).
#[derive(Debug, Clone)]
pub struct WorkspaceFile {
    pub path: PathBuf,
    pub kind: FileKind,
    pub mod_decls: Vec<ArcStr>,
}

/// One project rooted at a single file. `files` is every file
/// reachable from `root` via `mod` declarations. The same file can
/// appear in multiple projects (e.g. a util shared by several tools).
#[derive(Debug, Clone)]
pub struct Project {
    pub root: PathBuf,
    pub files: AHashSet<PathBuf>,
    /// If `root` lives at `<crate>/src/graphix/mod.gx` of a Cargo crate
    /// named `graphix-package-<x>`, this is `Some("<x>")` — the
    /// graphix-side namespace under which the crate's modules
    /// register. Tells the runtime to typecheck the project as if the
    /// source were the body of `mod <x> { ... }`.
    pub package_scope: Option<ArcStr>,
}

/// The scanner output: every file, every project, and a reverse
/// index from file → projects containing it.
#[derive(Debug, Clone, Default)]
pub struct WorkspaceModel {
    pub files: AHashMap<PathBuf, WorkspaceFile>,
    pub projects: Vec<Project>,
    pub file_to_projects: AHashMap<PathBuf, Vec<usize>>,
}

const SKIP_DIRS: &[&str] = &["target", ".git", "node_modules", ".cache", "vendor"];

/// Walk each root and collect the project graph. Errors from
/// individual files (parse failures, I/O) are recorded inline as
/// empty `mod_decls` so partially-broken workspaces still produce a
/// useful graph.
pub fn scan(roots: &[PathBuf]) -> WorkspaceModel {
    let mut files: AHashMap<PathBuf, WorkspaceFile> = AHashMap::default();
    for root in roots {
        for entry in WalkDir::new(root).into_iter().filter_entry(|e| {
            !e.file_name().to_str().map(|s| SKIP_DIRS.contains(&s)).unwrap_or(false)
        }) {
            let Ok(entry) = entry else { continue };
            if !entry.file_type().is_file() {
                continue;
            }
            let kind = match entry.path().extension().and_then(|s| s.to_str()) {
                Some("gx") => FileKind::Gx,
                Some("gxi") => FileKind::Gxi,
                _ => continue,
            };
            let path = entry.path().to_path_buf();
            let mod_decls = match read_and_extract_mods(&path, kind) {
                Ok(v) => v,
                Err(_) => Vec::new(),
            };
            files.insert(path.clone(), WorkspaceFile { path, kind, mod_decls });
        }
    }
    build_projects(files)
}

fn read_and_extract_mods(path: &Path, kind: FileKind) -> Result<Vec<ArcStr>> {
    let text = std::fs::read_to_string(path)?;
    extract_mod_decls(&text, kind, path.to_path_buf())
}

/// Parse `text` and return every `mod foo` declaration's name.
pub fn extract_mod_decls(
    text: &str,
    kind: FileKind,
    path: PathBuf,
) -> Result<Vec<ArcStr>> {
    let ori =
        Origin { parent: None, source: Source::File(path), text: ArcStr::from(text) };
    let mut out = Vec::new();
    match kind {
        FileKind::Gx => {
            let exprs = parser::parse(ori)?;
            for e in exprs.iter() {
                walk_expr_for_mods(&e.kind, &mut out);
            }
        }
        FileKind::Gxi => {
            let sig = parser::parse_sig(ori)?;
            for item in sig.items.iter() {
                if let SigKind::Module(name) = &item.kind {
                    out.push(name.clone());
                }
            }
        }
    }
    Ok(out)
}

/// Recursively walk an `ExprKind` tree collecting external module
/// declarations. We descend into nested `Resolved` modules so that
/// inner `mod` decls aren't missed (though right now graphix's
/// parser doesn't produce nested-on-parse mods — every `mod foo;`
/// from source is `Unresolved`).
fn walk_expr_for_mods(kind: &ExprKind, out: &mut Vec<ArcStr>) {
    if let ExprKind::Module { name, value } = kind {
        match value {
            ModuleKind::Unresolved { .. } | ModuleKind::Dynamic { .. } => {
                out.push(name.clone());
            }
            ModuleKind::Resolved { exprs, .. } => {
                for e in exprs.iter() {
                    walk_expr_for_mods(&e.kind, out);
                }
            }
        }
    }
    // Otherwise: parse output is shallow at the top level (mods only
    // appear at top-level positions), so we don't need to walk other
    // kinds. If that ever changes we'll add visitor cases here.
}

/// Compute the reachable file set for every potential project root
/// (every `.gx` file), mark files imported by any project, and
/// declare project roots as the unimported `.gx` files.
fn build_projects(files: AHashMap<PathBuf, WorkspaceFile>) -> WorkspaceModel {
    // Pre-compute reachable sets for every .gx file as if it were a
    // project root.
    let mut reachable: AHashMap<PathBuf, AHashSet<PathBuf>> = AHashMap::default();
    for (path, wf) in &files {
        if wf.kind == FileKind::Gxi {
            continue;
        }
        reachable.insert(path.clone(), bfs_from_root(path, &files));
    }
    // A file is "imported" if some other root's BFS reaches it.
    let mut imported: AHashSet<PathBuf> = AHashSet::default();
    for (root, set) in &reachable {
        for f in set {
            if f != root {
                imported.insert(f.clone());
            }
        }
    }
    // Roots: .gx files not imported by any other root.
    let mut roots: Vec<PathBuf> = files
        .iter()
        .filter(|(_, wf)| wf.kind == FileKind::Gx)
        .map(|(p, _)| p.clone())
        .filter(|p| !imported.contains(p))
        .collect();
    roots.sort();
    let mut projects: Vec<Project> = Vec::new();
    for root in roots {
        let project_files = reachable.get(&root).cloned().unwrap_or_else(|| {
            let mut s = AHashSet::default();
            s.insert(root.clone());
            s
        });
        let package_scope = detect_package_scope(&root);
        projects.push(Project { root, files: project_files, package_scope });
    }
    let mut file_to_projects: AHashMap<PathBuf, Vec<usize>> = AHashMap::default();
    for (idx, project) in projects.iter().enumerate() {
        for f in &project.files {
            file_to_projects.entry(f.clone()).or_default().push(idx);
        }
    }
    WorkspaceModel { files, projects, file_to_projects }
}

/// BFS from a hypothetical project root, mirroring the runtime's
/// filesystem resolver: `mod foo` declared at scope `<rel>` from
/// the root resolves to `<base>/<rel>/foo.gx` (or `.gxi`, or
/// `<base>/<rel>/foo/mod.gx`). The scope path accumulates as we
/// descend.
fn bfs_from_root(
    root: &Path,
    files: &AHashMap<PathBuf, WorkspaceFile>,
) -> AHashSet<PathBuf> {
    let base = root.parent().map(|p| p.to_path_buf()).unwrap_or_default();
    let mut out: AHashSet<PathBuf> = AHashSet::default();
    let mut stack: Vec<(PathBuf, PathBuf)> = vec![(root.to_path_buf(), PathBuf::new())];
    while let Some((file, rel)) = stack.pop() {
        if !out.insert(file.clone()) {
            continue;
        }
        // Pull in the file's `.gxi`/`.gx` sibling.
        if let Some(wf) = files.get(&file) {
            let sibling = match wf.kind {
                FileKind::Gx => file.with_extension("gxi"),
                FileKind::Gxi => file.with_extension("gx"),
            };
            if files.contains_key(&sibling) {
                out.insert(sibling);
            }
        }
        // Resolve every external `mod foo` declared in either the
        // .gx or its .gxi sibling and queue the targets.
        let to_walk = [&file, &file.with_extension("gxi"), &file.with_extension("gx")];
        for f in to_walk {
            let Some(wf) = files.get(f) else { continue };
            for name in &wf.mod_decls {
                if let Some(target) = resolve_mod(&base, &rel, name, files) {
                    let new_rel = rel.join(name.as_str());
                    stack.push((target, new_rel));
                }
            }
        }
    }
    out
}

/// If `root` is `<crate>/src/graphix/mod.gx` (or `mod.gxi`) and the
/// crate's `Cargo.toml` declares a `graphix-package-<x>` package, return
/// `Some("<x>")`. Used to typecheck the crate's source under that
/// scope so its modules don't collide with the runtime's pre-loaded
/// copy of the same package.
pub fn detect_package_scope(root: &Path) -> Option<ArcStr> {
    let stem = root.file_stem().and_then(|s| s.to_str())?;
    if stem != "mod" {
        return None;
    }
    let graphix_dir = root.parent()?; // .../src/graphix
    if graphix_dir.file_name().and_then(|s| s.to_str()) != Some("graphix") {
        return None;
    }
    let src_dir = graphix_dir.parent()?; // .../src
    if src_dir.file_name().and_then(|s| s.to_str()) != Some("src") {
        return None;
    }
    let crate_dir = src_dir.parent()?; // crate root
    let manifest = crate_dir.join("Cargo.toml");
    let text = std::fs::read_to_string(&manifest).ok()?;
    // Tiny scanner: find `name = "..."` inside the `[package]` section
    // (or before any other `[...]` table). Avoids pulling in a TOML
    // parser dependency just for this one field. The contents of
    // `name` are a TOML basic string — no escapes used in any cargo
    // package name in practice, so naive split-on-`"` is safe.
    let mut in_package = false;
    let mut started_section = false;
    let mut pkg_name: Option<&str> = None;
    for line in text.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix('[') {
            let header = rest.split(']').next().unwrap_or("").trim();
            in_package = header == "package";
            started_section = true;
            continue;
        }
        if !started_section {
            // Pre-table area only happens for non-Cargo TOMLs; cargo
            // manifests always put `[package]` first.
            continue;
        }
        if in_package {
            if let Some(rest) = line.strip_prefix("name") {
                let rest = rest.trim_start();
                if let Some(rest) = rest.strip_prefix('=') {
                    let rest = rest.trim();
                    if let Some(rest) = rest.strip_prefix('"') {
                        if let Some(end) = rest.find('"') {
                            pkg_name = Some(&rest[..end]);
                            break;
                        }
                    }
                }
            }
        }
    }
    let name = pkg_name?;
    let suffix = name.strip_prefix("graphix-package-")?;
    if suffix.is_empty() {
        return None;
    }
    Some(ArcStr::from(suffix))
}

/// Resolve `mod name` declared at scope `<rel>` from project base
/// `<base>`. Tries `<base>/<rel>/<name>.gx`, `.gxi`, then
/// `<base>/<rel>/<name>/mod.gx`, `mod.gxi`.
fn resolve_mod(
    base: &Path,
    rel: &Path,
    name: &str,
    files: &AHashMap<PathBuf, WorkspaceFile>,
) -> Option<PathBuf> {
    let dir = base.join(rel);
    let candidates = [
        dir.join(format!("{name}.gx")),
        dir.join(format!("{name}.gxi")),
        dir.join(name).join("mod.gx"),
        dir.join(name).join("mod.gxi"),
    ];
    candidates.into_iter().find(|c| files.contains_key(c))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write(p: &Path, s: &str) {
        if let Some(parent) = p.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(p, s).unwrap();
    }

    fn make_dir() -> tempfile::TempDir {
        tempfile::tempdir().unwrap()
    }

    #[test]
    fn flat_project_with_shared_util() {
        let dir = make_dir();
        let root = dir.path();
        write(&root.join("util.gx"), "let helper = |x: i64| -> i64 x;\n");
        write(&root.join("tool_a.gx"), "mod util;\nuse util;\nutil::helper(1)\n");
        write(&root.join("tool_b.gx"), "mod util;\nuse util;\nutil::helper(2)\n");
        let m = scan(&[root.to_path_buf()]);
        assert_eq!(m.files.len(), 3);
        // util is shared, tool_a and tool_b are distinct project roots
        let project_names: Vec<String> = m
            .projects
            .iter()
            .filter_map(|p| p.root.file_name()?.to_str().map(str::to_string))
            .collect();
        assert!(project_names.contains(&"tool_a.gx".into()));
        assert!(project_names.contains(&"tool_b.gx".into()));
        assert!(!project_names.contains(&"util.gx".into()));
        // util.gx appears in both projects' file sets
        let util = root.join("util.gx");
        let containing = m.file_to_projects.get(&util).cloned().unwrap_or_default();
        assert_eq!(containing.len(), 2);
    }

    #[test]
    fn nested_module_lookup() {
        let dir = make_dir();
        let root = dir.path();
        write(&root.join("main.gx"), "mod sub;\n");
        write(&root.join("sub.gx"), "mod inner;\nlet x = 1;\n");
        write(&root.join("sub").join("inner.gx"), "let y = 2;\n");
        let m = scan(&[root.to_path_buf()]);
        let main_project = m
            .projects
            .iter()
            .find(|p| p.root.ends_with("main.gx"))
            .expect("main is a root");
        assert!(main_project.files.contains(&root.join("sub.gx")));
        assert!(main_project.files.contains(&root.join("sub").join("inner.gx")));
    }

    #[test]
    fn detects_graphix_package_scope() {
        let dir = make_dir();
        let crate_dir = dir.path().join("graphix-package-tui");
        write(
            &crate_dir.join("Cargo.toml"),
            "[package]\nname = \"graphix-package-tui\"\nversion = \"0.1.0\"\n",
        );
        let mod_path = crate_dir.join("src").join("graphix").join("mod.gx");
        write(&mod_path, "let x = 1\n");
        let m = scan(&[crate_dir.clone()]);
        let project = m
            .projects
            .iter()
            .find(|p| p.root == mod_path)
            .expect("mod.gx is a project root");
        assert_eq!(project.package_scope.as_deref(), Some("tui"));
    }

    #[test]
    fn no_package_scope_outside_graphix_layout() {
        let dir = make_dir();
        // mod.gx not under src/graphix → not a package root
        let mod_path = dir.path().join("graphix-package-tui").join("mod.gx");
        write(&mod_path, "let x = 1\n");
        write(
            &dir.path().join("graphix-package-tui").join("Cargo.toml"),
            "[package]\nname = \"graphix-package-tui\"\n",
        );
        let m = scan(&[dir.path().to_path_buf()]);
        let project = m
            .projects
            .iter()
            .find(|p| p.root == mod_path)
            .expect("mod.gx is a project root");
        assert!(project.package_scope.is_none());
    }

    #[test]
    fn no_package_scope_for_non_graphix_crate() {
        let dir = make_dir();
        let crate_dir = dir.path().join("some-other-crate");
        write(&crate_dir.join("Cargo.toml"), "[package]\nname = \"some-other-crate\"\n");
        let mod_path = crate_dir.join("src").join("graphix").join("mod.gx");
        write(&mod_path, "let x = 1\n");
        let m = scan(&[crate_dir.clone()]);
        let project = m
            .projects
            .iter()
            .find(|p| p.root == mod_path)
            .expect("mod.gx is a project root");
        assert!(project.package_scope.is_none());
    }

    #[test]
    fn gxi_pulled_in_with_gx() {
        let dir = make_dir();
        let root = dir.path();
        write(&root.join("main.gx"), "mod api\n");
        write(&root.join("api.gxi"), "val foo: i64;\n");
        write(&root.join("api.gx"), "let foo = 1;\n");
        let m = scan(&[root.to_path_buf()]);
        let main = m
            .projects
            .iter()
            .find(|p| p.root.ends_with("main.gx"))
            .expect("main is a root");
        assert!(main.files.contains(&root.join("api.gx")));
        assert!(main.files.contains(&root.join("api.gxi")));
    }
}
