//! A module the server reaches through GRAPHIX_MODPATH is read from its
//! open buffer, as a module beside the file is. Its own binary: the
//! search path is read from the environment when the server starts.

#[allow(dead_code)]
#[path = "lsp/harness.rs"]
mod harness;

use harness::Client;

#[test]
fn a_modpath_module_is_read_from_its_buffer() {
    let lib = tempfile::tempdir().unwrap();
    let m = lib.path().join("m.gx");
    std::fs::write(&m, "let a = 1;\n").unwrap();
    // SAFETY: the only test in this binary, set before any thread reads it.
    unsafe { std::env::set_var("GRAPHIX_MODPATH", lib.path()) };
    let mut c = Client::start(&[("main.gx", "mod m;\nm::b\n")]);
    let m = m.to_str().unwrap();
    c.open_unsaved(m, "let a = 1;\nlet b = 2;\n".into());
    c.open("main.gx");
    assert_eq!(c.diagnostics("main.gx"), vec![]);
}
