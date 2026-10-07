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
