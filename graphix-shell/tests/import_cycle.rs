//! Two files that import each other through a file resolver are an
//! import cycle, reported, not a stack overflow.

use graphix_compiler::expr::{FilesResolver, Source};
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
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
        .no_cache(true)
        .build()
        .expect("building shell")
        .check()
        .await;
    let _ = fs::remove_dir_all(&dir);
    let err = format!("{:#}", checked.expect_err("a cycle does not check"));
    assert!(err.contains("import cycle: b -> a -> b"), "{err}");
}
