// Title: an embedder's module resolvers are not in the program entry key
// Command: copy to graphix-shell/tests/review_shell_1.rs; flock ~/tmp/cargo.lock cargo test -p graphix-shell --test review_shell_1 -- --nocapture
// Expected: d1 -> Some(11), d2 -> Some(22)
// Observed: d1 -> Some(11), d2 -> Some(11), d2 again -> Some(11)
//! An embedder's module resolvers are not in the program entry's key: a
//! second run over another module directory restores the first run's
//! module.

use anyhow::Result;
use graphix_compiler::expr::{FilesResolver, Load};
use graphix_package::MainThreadHandle;
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use std::fs;

#[test]
fn embedder_resolvers_key_the_program_entry() -> Result<()> {
    let dir = tempfile::tempdir()?;
    let (d1, d2) = (dir.path().join("d1"), dir.path().join("d2"));
    fs::create_dir(&d1)?;
    fs::create_dir(&d2)?;
    fs::write(d1.join("m.gx"), "let x = 11")?;
    fs::write(d2.join("m.gx"), "let x = 22")?;
    unsafe { std::env::set_var("XDG_CACHE_HOME", dir.path().join("cache")) };
    let rt = tokio::runtime::Builder::new_multi_thread().enable_all().build()?;
    let src = arcstr::literal!(
        "mod m;\nsys::exit(sys::time::after_idle(duration:10.ms, m::x))\n"
    );
    let run = |d: &std::path::Path| {
        let shell = ShellBuilder::<NoExt>::default()
            .cache(CacheMode::On)
            .module_resolvers(vec![FilesResolver::new(d.to_path_buf(), None)])
            .mode(Mode::Script(Load::Text(src.clone())))
            .build()
            .unwrap();
        rt.block_on(shell.run(MainThreadHandle::new().0))
    };
    let a = run(&d1)?;
    let b = run(&d2)?;
    let c = run(&d2)?;
    eprintln!("d1 -> {a:?}, d2 -> {b:?}, d2 again -> {c:?}");
    assert_eq!(a, Some(11));
    assert_eq!(b, Some(22), "the second run restored d1's module");
    Ok(())
}
