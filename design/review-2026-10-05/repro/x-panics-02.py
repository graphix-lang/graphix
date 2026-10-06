#!/usr/bin/env python3
# x-panics-02: the language server dies on any parser/formatter panic: the
# workspace scan, document/workspace symbols and formatting parse on its main
# thread with no guard.
#
# A check that panics becomes a "task N panicked" diagnostic and the server
# lives on. The workspace scan (workspace.rs scan -> extract_mod_decls, from
# ServerState::new at startup and ServerState::saved on didSave),
# textDocument/documentSymbol and workspace/symbol (symbols.rs declared) and
# textDocument/formatting (handlers/formatting.rs -> format_source) run on the
# server's main thread, so a panic there ends the process with exit code 101.
# The panics used: x-panics-01 (a negative duration literal panics the value
# parser) and x-panics-13 (an unvalidated graphixfmt.json indent).
#
# command:  python3 design/review-2026-10-05/repro/x-panics-02.py <graphix binary>
#           (e.g. ~/tmp/target/debug/graphix; each server runs under a 150 s
#           kill; about a minute in all)
#
# expected: every scenario ends "server alive": the scan records a file it
#           cannot parse as having no `mod` declarations (its doc comment says
#           so), a request that fails is an error response, never the end of
#           the server (server.rs respond).
# observed at c722befe (debug build):
#   control-scan:
#     server alive
#   scan:
#     server died: exit 101: thread 'main' panicked: cannot convert float seconds to Duration: value is negative
#   save:
#     server died: exit 101: thread 'main' panicked: cannot convert float seconds to Duration: value is negative
#   check:
#     diagnostics on open: ['task 203 panicked with message "cannot convert float seconds to Duration: value is negative"']
#     server alive
#   control-fmt:
#     server alive
#   format:
#     diagnostics on open: ['task 203 panicked with message "cannot convert float seconds to Duration: value is negative"']
#     server died: exit 101: thread 'tokio-rt-worker' panicked: cannot convert float seconds to Duration: value is negative; thread 'main' panicked: cannot convert float seconds to Duration: value is negative
#   symbols:
#     diagnostics on open: ['task 203 panicked with message "cannot convert float seconds to Duration: value is negative"']
#     server died: exit 101: thread 'tokio-rt-worker' panicked: cannot convert float seconds to Duration: value is negative; thread 'main' panicked: cannot convert float seconds to Duration: value is negative
#   fmtcfg:
#     server died: exit 101: thread 'main' panicked: capacity overflow
# (the 'tokio-rt-worker' panic is the check's, which the server survives.)
# Main-thread backtraces (RUST_BACKTRACE=1): scan: parser::parse <-
# workspace::extract_mod_decls <- workspace::scan <- ServerState::new <-
# server::serve; save: .. <- workspace::scan <- ServerState::saved <-
# server::handle_notification; format: parser::parse <- format::Parsed::new <-
# format::layout <- format_source <- handlers::formatting::handle; symbols:
# parser::parse <- symbols::declared <- ServerState::document_symbols.
#
# scenarios: control-scan / scan: a workspace file (never opened) holding
# duration:1.s / duration:-1.s; save: doc.gx open, duration:-1.s written to
# disk, didSave; check: didOpen with duration:-1.s; control-fmt: format a
# valid doc; format / symbols: didOpen with duration:-1.s, then formatting /
# documentSymbol; fmtcfg: graphixfmt.json {"indent": 18446744073709551615},
# format a valid doc.
import json, os, queue, shutil, subprocess, sys, tempfile, threading, time

BAD = "let x = duration:-1.s;\nx\n"
GOOD = "let x = duration:1.s;\nx\n"
NESTED = "let f = |x| select x {\n    0 => { let a = 1; let b = 2; a + b },\n    n => n\n};\nf(3)\n"


class Server:
    def __init__(self, graphix, ws, tmp):
        env = dict(os.environ, RUST_BACKTRACE="0",
                   XDG_CACHE_HOME=os.path.join(tmp, "cache"),
                   XDG_CONFIG_HOME=os.path.join(tmp, "config"))
        self.p = subprocess.Popen(["timeout", "-s", "KILL", "150", graphix, "lsp"],
                                  stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE, env=env)
        self.q, self.err, self.id, self.eof = queue.Queue(), [], 0, False
        threading.Thread(target=self._read, daemon=True).start()
        self.stderr = threading.Thread(target=lambda: self.err.extend(
            l.decode(errors="replace") for l in self.p.stderr), daemon=True)
        self.stderr.start()
        self.root = "file://" + ws
        self.request("initialize", {"processId": os.getpid(), "rootUri": self.root,
                                    "capabilities": {},
                                    "workspaceFolders": [{"uri": self.root, "name": "ws"}]})
        self.notify("initialized", {})

    def _read(self):
        try:
            while True:
                n = None
                while (line := self.p.stdout.readline().strip()) != b"":
                    if line.lower().startswith(b"content-length:"):
                        n = int(line.split(b":")[1])
                if n is None:
                    break
                self.q.put(json.loads(self.p.stdout.read(n)))
        finally:
            self.q.put(None)

    def _send(self, msg):
        try:
            body = json.dumps(dict(msg, jsonrpc="2.0")).encode()
            self.p.stdin.write(b"Content-Length: %d\r\n\r\n%s" % (len(body), body))
            self.p.stdin.flush()
        except OSError:
            pass

    def notify(self, method, params):
        self._send({"method": method, "params": params})

    def wait(self, pred, timeout):
        end = time.time() + timeout
        while not self.eof and (left := end - time.time()) > 0:
            try:
                m = self.q.get(timeout=left)
            except queue.Empty:
                break
            if m is None:
                self.eof = True
            elif pred(m):
                return m
        return "EOF" if self.eof else None

    def request(self, method, params, timeout=120):
        self.id += 1
        rid = self.id
        self._send({"id": rid, "method": method, "params": params})
        return self.wait(lambda m: m.get("id") == rid and "method" not in m, timeout)

    def diagnostics(self, uri, timeout=30):
        m = self.wait(lambda m: m.get("method") == "textDocument/publishDiagnostics"
                      and m["params"]["uri"] == uri, timeout)
        return m if m in (None, "EOF") else [d["message"] for d in m["params"]["diagnostics"]]

    def verdict(self):
        """Ask the server to shut down; report whether it answered."""
        r = self.request("shutdown", None, timeout=60)
        if r != "EOF" and r is not None:
            self.notify("exit", None)
            self.p.wait(timeout=20)
            return "server alive"
        if r is None:
            self.p.kill()
            return "server hung"
        rc = self.p.wait(timeout=20)
        self.stderr.join(timeout=5)
        panic = [f"{l.split(' (')[0]} panicked: {self.err[i + 1].strip()}"
                 for i, l in enumerate(self.err[:-1]) if "panicked at" in l]
        try:
            self.p.stdin.close()
        except OSError:
            pass
        return f"server died: exit {rc}: {'; '.join(panic)}"


def run(graphix, tmp, name, files, steps):
    ws = os.path.join(tmp, name)
    os.makedirs(ws)
    for f, text in files.items():
        open(os.path.join(ws, f), "w").write(text)
    print(f"{name}:")
    s = Server(graphix, ws, tmp)
    for step in steps:
        step(s, ws)
    print(f"  {s.verdict()}")


def open_doc(text):
    def step(s, ws):
        s.notify("textDocument/didOpen", {"textDocument": {
            "uri": s.root + "/doc.gx", "languageId": "graphix", "version": 1, "text": text}})
        d = s.diagnostics(s.root + "/doc.gx", timeout=10)
        if d:
            print(f"  diagnostics on open: {d}")
    return step


def save_to_disk(text):
    def step(s, ws):
        path = os.path.join(ws, "doc.gx")
        open(path, "w").write(text)
        os.utime(path, (time.time() + 5, time.time() + 5))
        s.notify("textDocument/didSave", {"textDocument": {"uri": s.root + "/doc.gx"}})
    return step


def format_doc(s, ws):
    s.request("textDocument/formatting", {"textDocument": {"uri": s.root + "/doc.gx"},
                                          "options": {"tabSize": 4, "insertSpaces": True}})


def doc_symbols(s, ws):
    s.request("textDocument/documentSymbol", {"textDocument": {"uri": s.root + "/doc.gx"}})


def main():
    graphix = sys.argv[1]
    tmp = tempfile.mkdtemp(prefix="x-panics-02-")
    run(graphix, tmp, "control-scan", {"other.gx": GOOD}, [])
    run(graphix, tmp, "scan", {"other.gx": BAD}, [])
    run(graphix, tmp, "save", {"doc.gx": GOOD}, [open_doc(GOOD), save_to_disk(BAD)])
    run(graphix, tmp, "check", {}, [open_doc(BAD)])
    run(graphix, tmp, "control-fmt", {}, [open_doc(NESTED), format_doc])
    run(graphix, tmp, "format", {}, [open_doc(BAD), format_doc])
    run(graphix, tmp, "symbols", {}, [open_doc(BAD), doc_symbols])
    run(graphix, tmp, "fmtcfg",
        {"graphixfmt.json": '{"indent": 18446744073709551615, "width": 10}'},
        [open_doc(NESTED), format_doc])
    shutil.rmtree(tmp)


main()
