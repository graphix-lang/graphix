//! Two files that name each other as modules form no cycle: a file's
//! submodules are beside it, so `b`'s `mod a` is `b/a.gx`, reported
//! missing, not a stack overflow.

use graphix_compiler::expr::{FilesResolver, Source};
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use std::{fs, sync::Arc};

#[tokio::test(flavor = "multi_thread")]
async fn two_files_importing_each_other() {
    let dir =
        std::env::temp_dir().join(format!("gx-import-cycle-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    fs::write(dir.join("a.gx"), "mod b;\nlet x = b::y").expect("write a.gx");
    fs::write(dir.join("b.gx"), "mod a;\nlet y = 1").expect("write b.gx");
    let checked = ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::File(dir.join("a.gx"))))
        .module_resolvers(vec![Arc::new(FilesResolver {
            base: dir.clone(),
            overrides: None,
        })])
        .cache(CacheMode::Off)
        .build()
        .expect("building shell")
        .check()
        .await;
    let _ = fs::remove_dir_all(&dir);
    let err = format!("{:#}", checked.expect_err("b's `mod a` is not a.gx"));
    assert!(err.contains("module b::a could not be found"), "{err}");
}

/// A directory linking into itself is a cycle: `m`'s `mod m` is `m/m.gx`,
/// which is `m.gx` again, reported as an import cycle, not a load that
/// recurses until the kernel refuses the path.
#[cfg(unix)]
#[tokio::test(flavor = "multi_thread")]
async fn a_directory_linked_into_itself() {
    let dir =
        std::env::temp_dir().join(format!("gx-import-symlink-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    std::os::unix::fs::symlink(".", dir.join("m")).expect("symlink");
    fs::write(dir.join("main.gx"), "mod m;\n1").expect("write main.gx");
    fs::write(dir.join("m.gx"), "mod m;\nlet a = 1").expect("write m.gx");
    let checked = ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::File(dir.join("main.gx"))))
        .module_resolvers(vec![Arc::new(FilesResolver {
            base: dir.clone(),
            overrides: None,
        })])
        .cache(CacheMode::Off)
        .build()
        .expect("building shell")
        .check()
        .await;
    let _ = fs::remove_dir_all(&dir);
    let err = format!("{:#}", checked.expect_err("the link is a cycle"));
    assert!(err.contains("import cycle: m -> m"), "{err}");
}

/// A file in a directory whose name is not UTF-8 resolves its own
/// submodules beside it.
#[cfg(unix)]
#[tokio::test(flavor = "multi_thread")]
async fn submodules_beside_a_non_utf8_path() {
    use std::{ffi::OsStr, os::unix::ffi::OsStrExt};
    let dir = std::env::temp_dir()
        .join(format!("gx-non-utf8-{}", std::process::id()))
        .join(OsStr::from_bytes(b"d\xff"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(dir.join("a")).expect("tempdir");
    fs::write(dir.join("main.gx"), "mod a;\na::x").expect("write main.gx");
    fs::write(dir.join("a.gx"), "mod b;\nlet x = b::y").expect("write a.gx");
    fs::write(dir.join("a").join("b.gx"), "let y = 1").expect("write b.gx");
    let checked = ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::File(dir.join("main.gx"))))
        .module_resolvers(vec![Arc::new(FilesResolver {
            base: dir.clone(),
            overrides: None,
        })])
        .cache(CacheMode::Off)
        .build()
        .expect("building shell")
        .check()
        .await;
    let _ = fs::remove_dir_all(dir.parent().unwrap());
    checked.expect("a's `mod b` is a/b.gx");
}
