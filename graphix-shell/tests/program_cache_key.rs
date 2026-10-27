//! A program entry restores only where a cold compile would build the
//! same program: what decides that (the flags, the module search path,
//! the resolvers) keys the entry or keeps it from being written.

mod common;
use common::graphix;
use graphix_compiler::expr::{FilesResolver, Load};
use graphix_package::MainThreadHandle;
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use std::fs;

/// A warned program run cold, then with warnings as errors, fails.
#[test]
fn warnings_as_errors_refuse_a_cached_warned_program() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "let x = cast<i64>(\"1\")?;\n\
                   sys::exit(sys::time::after_idle(duration:10.ms, x - 1))\n";
    let cold = graphix(dir.path(), program, &[], &[]);
    assert!(cold.status.success(), "{}", cold.stderr);
    let strict = graphix(dir.path(), program, &["-W", "error"], &[]);
    assert!(!strict.status.success(), "the warned program ran under -W error");
    assert!(strict.stderr.contains("will not be caught"), "{}", strict.stderr);
}

/// A module in a directory put before the search path's others is the
/// one a later run takes, though the cached run never looked there.
#[test]
fn a_prepended_search_directory_is_seen() {
    let dir = tempfile::tempdir().expect("tempdir");
    let (a, b) = (dir.path().join("a"), dir.path().join("b"));
    fs::create_dir(&a).expect("mkdir a");
    fs::create_dir(&b).expect("mkdir b");
    fs::write(a.join("m.gx"), "let x = 2").expect("write a/m.gx");
    fs::write(b.join("m.gx"), "let x = 1").expect("write b/m.gx");
    let program = "mod m;\nsys::exit(sys::time::after_idle(duration:10.ms, m::x))\n";
    let run = |path: String| {
        graphix(dir.path(), program, &[], &[("GRAPHIX_MODPATH", &path)]).status.code()
    };
    assert_eq!(run(b.display().to_string()), Some(1));
    assert_eq!(run(format!("{},{}", a.display(), b.display())), Some(2));
}

/// An embedder's own module resolvers are not the cache's to vouch for:
/// the same program over another directory runs that directory's module.
#[test]
fn an_embedders_resolvers_restore_no_program() {
    let dir = tempfile::tempdir().expect("tempdir");
    let (d1, d2) = (dir.path().join("d1"), dir.path().join("d2"));
    fs::create_dir(&d1).expect("mkdir d1");
    fs::create_dir(&d2).expect("mkdir d2");
    fs::write(d1.join("m.gx"), "let x = 11").expect("write d1/m.gx");
    fs::write(d2.join("m.gx"), "let x = 22").expect("write d2/m.gx");
    // SAFETY: the only test in this binary that reads the environment in
    // process; the others pass theirs to a child
    unsafe { std::env::set_var("XDG_CACHE_HOME", dir.path().join("cache")) };
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("runtime");
    let src = arcstr::literal!(
        "mod m;\nsys::exit(sys::time::after_idle(duration:10.ms, m::x))\n"
    );
    let run = |d: &std::path::Path| {
        let shell = ShellBuilder::<NoExt>::default()
            .cache(CacheMode::On)
            .module_resolvers(vec![FilesResolver::new(d.to_path_buf(), None)])
            .mode(Mode::Script(Load::Text(src.clone())))
            .build()
            .expect("building shell");
        rt.block_on(shell.run(MainThreadHandle::new().0)).expect("run")
    };
    assert_eq!(run(&d1), Some(11));
    assert_eq!(run(&d2), Some(22));
}
