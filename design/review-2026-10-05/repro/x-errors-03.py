#!/usr/bin/env python3
# x-errors-03: a parse error (or any other failure) under `mod init` is
# silently ignored by the REPL; the init module just goes missing.
#
# resolve_modules_int (graphix-types/src/expr/resolver.rs:702) wraps every
# failure of `resolve` in CouldNotResolve: the file failing to parse
# (parse_module), a Resolution::Broken read error, and "could not be
# found". A nested `mod` fails through its own CouldNotResolve. The REPL
# (graphix-shell/src/lib.rs:399) reads `e.is::<CouldNotResolve>()` (anyhow
# matches any context in the chain) as "there is no init module" and goes
# on without a word. book/src/shell.md promises silence only "if not found".
#
# Each case writes $XDG_DATA_HOME/graphix/init.gx in a temp dir, starts the
# REPL (`graphix --no-cache -n`) on a pty, answers the line editor's
# cursor-position queries, types one line, then ctrl-d.
#   parse         init.gx: let greeting = "hi"; let x = (1 + ;
#   nested-parse  init.gx: mod bad; let greeting = "hi"   init/bad.gx: let y = (2 + ;
#   nested-miss   init.gx: mod missing; let greeting = "hi"
#   book          init.gx: the example of book/src/shell.md:406 (it does not parse)
#   type          init.gx: let greeting = "hi"; let x = 1 + "a"     (control)
#   ok            init.gx: let greeting = "hi"                      (control)
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/x-errors-03.py <graphix>
#
# expected: every case but `ok` prints "error in init module: ..." at
# startup, as `type` does.
# observed (HEAD c722befe, debug build):
#   parse         startup: (nothing)   init::greeting -> init::greeting not defined
#   nested-parse  startup: (nothing)   init::greeting -> init::greeting not defined
#   nested-miss   startup: (nothing)   init::greeting -> init::greeting not defined
#   book          startup: (nothing)   init::debug(1) -> init::debug not defined
#   type          startup: error in init module: in expr mod init
#                                      init::greeting -> init::greeting not defined
#   ok            startup: (nothing)   init::greeting -> "hi"
import os, pty, re, select, signal, sys, tempfile, time

BIN = sys.argv[1] if len(sys.argv) > 1 else "graphix"

CASES = [
    ("parse", {"init.gx": 'let greeting = "hi";\nlet x = (1 + ;\n'}, "init::greeting"),
    (
        "nested-parse",
        {"init.gx": 'mod bad;\nlet greeting = "hi";\n', "init/bad.gx": "let y = (2 + ;\n"},
        "init::greeting",
    ),
    ("nested-miss", {"init.gx": 'mod missing;\nlet greeting = "hi";\n'}, "init::greeting"),
    (
        "book",
        {
            "init.gx": 'let debug = |x| { print("DEBUG: [x]"); x };\n'
            + r'let clear = || print("\x1b[2J\x1b[H");'
            + "\n"
        },
        "init::debug(1)",
    ),
    ("type", {"init.gx": 'let greeting = "hi";\nlet x = 1 + "a";\n'}, "init::greeting"),
    ("ok", {"init.gx": 'let greeting = "hi";\n'}, "init::greeting"),
]

ANSI = re.compile(r"\x1b\[[0-9;?]*[A-Za-z]|\x1b[78]")


def repl(data_home, cache_home, line):
    env = dict(os.environ, XDG_DATA_HOME=data_home, XDG_CACHE_HOME=cache_home)
    env.pop("GRAPHIX_MODPATH", None)
    pid, fd = pty.fork()
    if pid == 0:
        os.execvpe(BIN, [BIN, "--no-cache", "-n"], env)
    inputs = [line.encode() + b"\r", b"\x04"]
    out, answered, sent, last = b"", 0, 0, 0.0
    deadline = time.time() + 25
    while time.time() < deadline:
        r, _, _ = select.select([fd], [], [], 0.2)
        if fd in r:
            try:
                data = os.read(fd, 4096)
            except OSError:
                break
            if not data:
                break
            out += data
            while out.count(b"\x1b[6n") > answered:
                os.write(fd, b"\x1b[1;1R")
                answered += 1
        if b"Welcome" in out and sent < len(inputs) and time.time() - last > 3:
            os.write(fd, inputs[sent])
            sent += 1
            last = time.time()
    try:
        os.killpg(pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    os.waitpid(pid, 0)
    return ANSI.sub("", out.decode(errors="replace")).replace("\r", "")


with tempfile.TemporaryDirectory() as tmp:
    for name, files, line in CASES:
        data = os.path.join(tmp, name)
        for rel, text in files.items():
            path = os.path.join(data, "graphix", rel)
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w") as f:
                f.write(text)
        text = repl(data, os.path.join(tmp, "cache"), line)
        startup = text.split("Welcome to the graphix shell")[0].strip()
        startup = startup.splitlines()[0] if startup else "(nothing)"
        after = text.split("Welcome to the graphix shell")[-1]
        lines = [l.strip() for l in after.splitlines() if l.strip().strip("〉")]
        answer = re.sub(r"^\d+: ", "", lines[-1]) if lines else "(no answer)"
        print(f"{name:13} startup: {startup}")
        print(f"{'':13} {line} -> {answer}")
