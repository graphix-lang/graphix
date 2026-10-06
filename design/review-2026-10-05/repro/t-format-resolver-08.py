#!/usr/bin/env python3
# t-format-resolver-08: a module's file is inferred from its first
# expression, which can be an item spliced in from its .gxi.
#
# add_interface_modules puts the interface-only declarations that come
# before the .gxi's first val ahead of every implementation statement,
# with the interface's origin. resolve_modules_int
# (graphix-types/src/expr/resolver.rs:711) takes the body's source from
# the first expression that has one (the load chain, and the submodule
# base through for_source), and compile_module_inner
# (graphix-compiler/src/node/compiler.rs:249) takes exprs.first()'s
# origin as the module's def_ori, which ide.rs documents as "the file the
# body was loaded from".
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/t-format-resolver-08.py <graphix>
#
# expected: go-to-definition on `mod |a` lands in a.gx whatever the
# order of a.gxi's items, and an import cycle through `a` names a.gx,
# printed as a path.
# observed (HEAD c722befe, debug build):
#   gxi "use str::len; val x: i64" -> definition a.gxi
#   gxi "val x: i64; use str::len" -> definition a.gx
#   cyc1 (a.gxi opens with a use): import cycle: a -> b -> a (File("<tmp>/cyc1/a.gxi"))
#   cyc2 (no a.gxi):               import cycle: a -> b -> a (File("<tmp>/cyc2/a.gx"))
# (b's `mod a` reaches <tmp>/cycN/a.gx through the leaf-name fallback of
# t-format-resolver-05; the point here is which file the message names.)
import atexit, json, os, queue, shutil, subprocess, sys, tempfile, threading, urllib.parse

graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
tmp = os.path.realpath(tempfile.mkdtemp(prefix="t-format-resolver-08."))
atexit.register(shutil.rmtree, tmp, True)
files = {
    "use_first/main.gx": "mod a;\na::x\n",
    "use_first/a.gx": "let x = 1\n",
    "use_first/a.gxi": "use str::len;\nval x: i64\n",
    "val_first/main.gx": "mod a;\na::x\n",
    "val_first/a.gx": "let x = 1\n",
    "val_first/a.gxi": "val x: i64;\nuse str::len\n",
    "cyc1/main.gx": "mod a;\na::x\n",
    "cyc1/a.gx": "mod b;\nlet x = 1\n",
    "cyc1/a.gxi": "use str::len;\nval x: i64\n",
    "cyc1/b.gx": "mod a;\nlet y = 2\n",
    "cyc2/main.gx": "mod a;\na::x\n",
    "cyc2/a.gx": "mod b;\nlet x = 1\n",
    "cyc2/b.gx": "mod a;\nlet y = 2\n",
}
for rel, text in files.items():
    os.makedirs(os.path.dirname(os.path.join(tmp, rel)), exist_ok=True)
    open(os.path.join(tmp, rel), "w").write(text)
env = dict(os.environ, XDG_CACHE_HOME=os.path.join(tmp, "cache"))


def uri(p):
    return "file://" + urllib.parse.quote(p)


def definition(root, file, line, col):
    proc = subprocess.Popen([graphix, "lsp"], env=env, stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    msgs = queue.Queue()

    def reader():
        while True:
            headers = {}
            while True:
                l = proc.stdout.readline()
                if not l:
                    return msgs.put(None)
                l = l.decode().strip()
                if not l:
                    break
                k, v = l.split(":", 1)
                headers[k.strip().lower()] = v.strip()
            msgs.put(json.loads(proc.stdout.read(int(headers["content-length"]))))

    threading.Thread(target=reader, daemon=True).start()

    def send(m):
        b = json.dumps(dict(m, jsonrpc="2.0")).encode()
        proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
        proc.stdin.flush()

    def wait_for(id):
        while True:
            m = msgs.get(timeout=90)
            if m is None:
                raise SystemExit("server exited early")
            if m.get("id") == id:
                return m

    send({"id": 1, "method": "initialize", "params": {
        "processId": os.getpid(), "rootUri": uri(root),
        "workspaceFolders": [{"uri": uri(root), "name": "p"}], "capabilities": {}}})
    wait_for(1)
    send({"method": "initialized", "params": {}})
    send({"method": "textDocument/didOpen", "params": {"textDocument": {
        "uri": uri(file), "languageId": "graphix", "version": 1,
        "text": open(file).read()}}})
    send({"id": 2, "method": "textDocument/definition", "params": {
        "textDocument": {"uri": uri(file)},
        "position": {"line": line, "character": col}}})
    res = wait_for(2).get("result")
    send({"id": 3, "method": "shutdown", "params": None})
    wait_for(3)
    send({"method": "exit", "params": None})
    proc.stdin.close()
    proc.wait(timeout=30)
    if isinstance(res, list):
        res = res[0] if res else None
    return os.path.basename(urllib.parse.unquote(res["uri"])) if res else None


for proj in ["use_first", "val_first"]:
    root = os.path.join(tmp, proj)
    gxi = open(os.path.join(root, "a.gxi")).read().replace("\n", " ").strip()
    print(f'gxi "{gxi}" -> definition {definition(root, os.path.join(root, "main.gx"), 0, 4)}')
for proj in ["cyc1", "cyc2"]:
    r = subprocess.run([graphix, "--no-cache", "--check", os.path.join(tmp, proj, "main.gx")],
                       env=env, capture_output=True, text=True)
    print(f"{proj}: " + (r.stderr.strip() or r.stdout.strip()).replace(tmp, "<tmp>"))
