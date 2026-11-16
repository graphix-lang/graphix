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
    // CR claude for claude: [test-gap/low] This assertion also holds when main.gx is
    // never checked. The server publishes a clean file only to clear an earlier publish
    // (`ServerState::publish_files`), so the passing run publishes nothing for main.gx.
    // A check that panics (`guarded` publishes nothing) or never runs gives the same
    // empty result. Add a positive pin that the server can only give after checking
    // main.gx against the buffer: `c.definition("main.gx", "m::|b")` should land on
    // line 1 of the modpath file (it does today). Alternatively, first show the disk
    // text's `m::b not defined`, then its clearing. Every `files_with_diagnostics() ==
    // []` in tests/lsp/main.rs without a positive query beside it has the same
    // blindness. probe:
    // design/review-2026-11-16/repro/tests-shell-compiler-a-lsp-modpath-vacuous.py
    // (tests-shell-compiler-a-03)
    assert_eq!(c.diagnostics("main.gx"), vec![]);
}
