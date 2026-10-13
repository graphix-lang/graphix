//! `graphix lsp` over real pipes: a burst of edits queued before the
//! server reads them costs one check, not one per edit.

use lsp_server::{Message, Notification, Request, RequestId};
use serde_json::json;
use std::{
    io::{BufReader, Write},
    process::{Command, Stdio},
};

const EDITS: i32 = 40;

#[test]
fn a_burst_of_edits_is_checked_once() {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("main.gx");
    std::fs::write(&file, "let x = 1;\nx\n").unwrap();
    let uri = format!("file://{}", file.display());
    let mut child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("lsp")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("run graphix lsp");
    let mut out = Vec::new();
    let mut send = |m: Message| m.write(&mut out).unwrap();
    let req = |id: i32, method: &str, params| {
        Message::Request(Request::new(RequestId::from(id), method.into(), params))
    };
    let not = |method: &str, params| {
        Message::Notification(Notification::new(method.into(), params))
    };
    send(req(1, "initialize", json!({"processId": null, "capabilities": {}})));
    send(not("initialized", json!({})));
    send(not(
        "textDocument/didOpen",
        json!({"textDocument": {"uri": uri, "languageId": "graphix", "version": 0,
                                "text": "let x = 1;\nx\n"}}),
    ));
    for v in 1..=EDITS {
        send(not(
            "textDocument/didChange",
            json!({"textDocument": {"uri": uri, "version": v},
                   "contentChanges": [{"text": format!("let x = {v};\nx + \"a\"\n")}]}),
        ));
    }
    // a request is answered over the text as it stands: checked once
    send(req(
        2,
        "textDocument/hover",
        json!({"textDocument": {"uri": uri}, "position": {"line": 1, "character": 0}}),
    ));
    // the whole burst is in the pipe before the server reads past the open
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(&out).unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());
    let mut publishes = 0;
    while let Some(msg) = Message::read(&mut stdout).unwrap() {
        match msg {
            Message::Notification(n) if n.method == "textDocument/publishDiagnostics" => {
                publishes += 1
            }
            Message::Response(r) if r.id == RequestId::from(2) => break,
            _ => (),
        }
    }
    let mut out = Vec::new();
    req(3, "shutdown", json!(null)).write(&mut out).unwrap();
    not("exit", json!(null)).write(&mut out).unwrap();
    stdin.write_all(&out).unwrap();
    while Message::read(&mut stdout).unwrap().is_some() {}
    assert!(child.wait().unwrap().success());
    assert_eq!(publishes, 1, "{EDITS} queued edits");
}
