#!/usr/bin/env python3
# ide-tooling.r2-04: highlights.scm colors every plain reference and every
# plain callee as @namespace and a namespaced callee as @variable; no
# identifier is ever @function; trait/impl/for, the dynamic-module words
# and Array/Map/Error/Abstract have no capture.
#
# ide/tree-sitter-graphix/queries/highlights.scm:130 (131 for use paths)
# `(module_path . (identifier) @namespace)` matches the first named
# identifier of EVERY path, and a plain `x` is (reference (module_path
# (identifier))). The @function rule (95-98) tags every segment of a callee
# path and the @variable rule (124) re-tags all of them. With the
# precedence the file states for itself ("later wins", lines 91 and 155),
# which is what tree-sitter 0.24's highlighter and Helix 25.07 do, the `f`
# of `f(x)` carries function (98), variable (124) and namespace (130) and
# shows namespace; the `f` of `m::f(x)` shows variable. The CLI also gives
# a name that resolves to a local definition the definition's color, so
# there a let-bound name stays a variable; Helix takes no color from
# locals.scm's bare @local.definition (its bundled queries write
# @local.definition.<scope>), so there every reference is a namespace and
# every let-bound, positional parameter and typedef name is uncolored.
# locals.scm defines no labeled param: `[greeting]` below is a namespace in
# both.
#
# The script compiles ide/tree-sitter-graphix/src into a temp dir, gives
# every capture name its own color, highlights the program below with the
# tree-sitter CLI and, when helix and tmux are on PATH, renders it in Helix
# under a throwaway XDG config holding this checkout's queries (no LSP, a
# private tmux server), then maps the colors back to capture names.
#
# command (needs python3, cc and `npm install` in ide/tree-sitter-graphix;
# TS=/path/to/tree-sitter overrides the CLI):
#   python3 design/review-2026-10-05/repro/ide-tooling.r2-04.py
#
# expected (highlights.scm:88-91, 126-129): a callee @function, a path
#   prefix @namespace, a reference @variable, every keyword colored; exit 0
# observed (HEAD c722befe, tree-sitter 0.24.7, helix 25.07.1), exit 1:
#   line           token     expected   cli        helix
#   println(f(2))  println   function   namespace  namespace  WRONG
#   array::map     map       function   variable   variable   WRONG
#   str::len       len       function   variable   variable   WRONG
#   sys::time      time      namespace  variable   variable   WRONG
#   array::map(xs  xs        variable   variable   namespace  WRONG
#   [greeting]     greeting  variable   namespace  namespace  WRONG
#   [name]         name      variable   variable   namespace  WRONG
#   trait Show     trait     keyword    -          -          WRONG
#   impl Show for  impl      keyword    -          -          WRONG
#   impl Show for  for       keyword    -          -          WRONG
#   Abstract<i64>  Abstract  type       -          -          WRONG
#   Array<i64>     Array     type       -          -          WRONG
#   @function captured anywhere: cli=False helix=False
# With lines 130/131 as `(module_path (identifier) @namespace . "::")`
# (use_path likewise) and the callee rule as `(apply (reference
# (module_path (identifier) @function .)))` placed after line 124, both
# highlighters give println, map, len and after_idle @function and array,
# str, sys and time @namespace.
import json, os, re, shutil, subprocess, sys, tempfile, time

PROGRAM = '''type T = Abstract<i64>;
trait Show {
    val show: fn(self) -> string
};
impl Show for T {
    let show = |t| "T"
};
let xs: Array<i64> = [1, 2, 3];
let f = |x| (x, x);
let greet = |#greeting = "hello", name| "[greeting], [name]!";
let ys = array::map(xs, f);
println(f(2));
println(greet("world"));
println(Show::show(T(str::len("abc"))));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
'''

# (line containing, token, nth occurrence of token in that line, expected)
CHECKS = [
    ("println(f(2))", "println", 0, "function"),
    ("array::map", "map", 0, "function"),
    ("str::len", "len", 0, "function"),
    ("sys::time", "time", 0, "namespace"),
    ("array::map(xs", "xs", 0, "variable"),
    ("[greeting]", "greeting", 1, "variable"),
    ("[name]", "name", 1, "variable"),
    ("trait Show", "trait", 0, "keyword"),
    ("impl Show for", "impl", 0, "keyword"),
    ("impl Show for", "for", 0, "keyword"),
    ("Abstract<i64>", "Abstract", 0, "type"),
    ("Array<i64>", "Array", 0, "type"),
]

here = os.path.dirname(os.path.abspath(__file__))
root = subprocess.run(["git", "-C", here, "rev-parse", "--show-toplevel"],
                      capture_output=True, text=True, check=True).stdout.strip()
grammar = os.path.join(root, "ide", "tree-sitter-graphix")
ts = os.environ.get("TS") or os.path.join(grammar, "node_modules", "tree-sitter-cli", "tree-sitter")
if not os.access(ts, os.X_OK):
    ts = shutil.which("tree-sitter")
    if not ts:
        sys.exit("no tree-sitter CLI: run npm install in ide/tree-sitter-graphix or set TS")

tmp = tempfile.mkdtemp(prefix="r2-04-")
sock = "r2-04-%d" % os.getpid()
SGR = re.compile(r"\x1b\[([0-9;:]*)m")


def decode(text, color_of):
    """rows of per-column capture names from SGR-colored text"""
    rows = []
    for line in text.split("\n"):
        cur, pos, cols = None, 0, []
        for m in SGR.finditer(line):
            cols += [cur] * (m.start() - pos)
            codes = m.group(1)
            if codes in ("", "0"):
                cur = None
            else:
                c = color_of(codes)
                if c is not False:
                    cur = c
            pos = m.end()
        cols += [cur] * (len(line) - pos)
        rows.append((SGR.sub("", line), cols))
    return rows


def lookup(rows, contains, token, nth):
    for text, cols in rows:
        if contains in text:
            idx = -1
            for _ in range(nth + 1):
                idx = text.index(token, idx + 1)
            return cols[idx] or "-"
    return "?"


def show(rows):
    for text, cols in rows:
        out, cur = "", None
        for ch, c in zip(text, cols):
            if c != cur:
                out += ">" if cur else ""
                out += "<%s:" % c if c else ""
                cur = c
            out += ch
        out += ">" if cur else ""
        if out.strip():
            print("  " + out)


try:
    pdir = os.path.join(tmp, "parsers", "tree-sitter-graphix")
    os.makedirs(pdir)
    for f in ["src", "queries"]:
        shutil.copytree(os.path.join(grammar, f), os.path.join(pdir, f))
    for f in ["tree-sitter.json", "grammar.js", "package.json"]:
        shutil.copy(os.path.join(grammar, f), pdir)
    hl = open(os.path.join(grammar, "queries", "highlights.scm")).read()
    hl = "\n".join(l for l in hl.split("\n") if not l.lstrip().startswith(";"))
    names = sorted(set(re.findall(r"@([a-z][a-z.]*[a-z])", hl)))
    prog = os.path.join(tmp, "probe.gx")
    open(prog, "w").write(PROGRAM)

    # -- tree-sitter CLI ------------------------------------------------
    theme = {n: 16 + i for i, n in enumerate(names)}
    by_index = {16 + i: n for i, n in enumerate(names)}
    cfg = os.path.join(tmp, "config.json")
    json.dump({"parser-directories": [os.path.join(tmp, "parsers")], "theme": theme}, open(cfg, "w"))
    env = dict(os.environ, TREE_SITTER_LIBDIR=os.path.join(tmp, "lib"),
               XDG_CACHE_HOME=os.path.join(tmp, "cache"))
    os.makedirs(env["TREE_SITTER_LIBDIR"])
    ver = subprocess.run([ts, "--version"], capture_output=True, text=True).stdout.strip()
    r = subprocess.run([ts, "highlight", "--config-path", cfg, prog], env=env,
                       capture_output=True, text=True, timeout=300)
    if r.returncode != 0:
        sys.exit("tree-sitter highlight failed:\n" + r.stderr)

    def cli_color(codes):
        m = re.search(r"38;5;(\d+)", codes)
        return by_index.get(int(m.group(1))) if m else False

    cli = decode(r.stdout, cli_color)
    print("== %s highlight" % ver)
    show(cli)

    # -- helix ----------------------------------------------------------
    hx = None
    helix = shutil.which("helix") or shutil.which("hx")
    if helix and shutil.which("tmux"):
        xdg = {k: os.path.join(tmp, "hx", k) for k in ["config", "cache", "data", "state", "home"]}
        hc = os.path.join(xdg["config"], "helix")
        for d in ["runtime/grammars", "runtime/queries/graphix", "themes"]:
            os.makedirs(os.path.join(hc, d))
        for d in ["cache", "data", "state", "home"]:
            os.makedirs(xdg[d])
        shutil.copy(os.path.join(env["TREE_SITTER_LIBDIR"], "graphix.so"),
                    os.path.join(hc, "runtime/grammars/graphix.so"))
        for q in ["highlights.scm", "locals.scm", "indents.scm"]:
            shutil.copy(os.path.join(grammar, "queries", q), os.path.join(hc, "runtime/queries/graphix", q))
        rgb = {}
        lines = ['"ui.background" = { bg = "#000000" }', '"ui.text" = "#fefefe"',
                 '"ui.cursor" = { bg = "#010101" }', '"ui.cursor.primary" = { bg = "#010101" }',
                 '"ui.selection" = { bg = "#000000" }']
        for i, n in enumerate(names):
            c = (0x40 + 3 * i, 0x81, 0x42)
            rgb["%d;%d;%d" % c] = n
            lines.append('"%s" = "#%02x%02x%02x"' % ((n,) + c))
        open(os.path.join(hc, "themes", "probe.toml"), "w").write("\n".join(lines) + "\n")
        open(os.path.join(hc, "config.toml"), "w").write(
            'theme = "probe"\n[editor]\ntrue-color = true\ngutters = []\n[editor.lsp]\nenable = false\n')
        open(os.path.join(hc, "languages.toml"), "w").write(
            '[[language]]\nname = "graphix"\nscope = "source.graphix"\nfile-types = ["gx", "gxi"]\n'
            'comment-token = "//"\nlanguage-servers = []\nroots = []\n')
        cmd = ("env HOME=%s XDG_CONFIG_HOME=%s XDG_CACHE_HOME=%s XDG_DATA_HOME=%s XDG_STATE_HOME=%s "
               "COLORTERM=truecolor %s %s" % (xdg["home"], xdg["config"], xdg["cache"], xdg["data"],
                                             xdg["state"], helix, prog))
        hver = subprocess.run([helix, "--version"], capture_output=True, text=True).stdout.strip()
        subprocess.run(["tmux", "-L", sock, "-f", "/dev/null", "new-session", "-d", "-s", "p",
                        "-x", "160", "-y", "40", cmd], check=True)
        screen = ""
        for _ in range(40):
            time.sleep(0.25)
            screen = subprocess.run(["tmux", "-L", sock, "capture-pane", "-p", "-t", "p"],
                                    capture_output=True, text=True).stdout
            if "sys::exit" in screen:
                break
        time.sleep(1.0)
        screen = subprocess.run(["tmux", "-L", sock, "capture-pane", "-e", "-p", "-t", "p"],
                                capture_output=True, text=True).stdout

        def hx_color(codes):
            m = re.search(r"(?:^|;)38;2;(\d+);(\d+);(\d+)", codes)
            if m:
                return rgb.get(";".join(m.groups()))
            return None if re.search(r"(?:^|;)39(?:;|$)", codes) else False

        hx = decode(screen, hx_color)
        print("== %s (isolated config, this checkout's queries)" % hver)
        show([row for row in hx if row[0].strip() and "NOR" not in row[0] and "Loaded" not in row[0]])
    else:
        print("== helix or tmux not on PATH: helix column skipped")

    print("== verdict")
    print("  %-14s %-9s %-10s %-10s %s" % ("line", "token", "expected", "cli", "helix"))
    bad = False
    for contains, token, nth, want in CHECKS:
        got_cli = lookup(cli, contains, token, nth)
        got_hx = lookup(hx, contains, token, nth) if hx else ""
        ok = got_cli.startswith(want) and (not hx or got_hx.startswith(want))
        bad |= not ok
        print("  %-14s %-9s %-10s %-10s %-10s %s" % (contains[:14], token, want, got_cli, got_hx,
                                                    "" if ok else "WRONG"))
    anyfn = lambda rows: any(c == "function" for _, cols in rows for c in cols)
    print("  @function captured anywhere: cli=%s helix=%s"
          % (anyfn(cli), anyfn(hx) if hx else "skipped"))
    sys.exit(1 if bad else 0)
finally:
    if shutil.which("tmux"):
        subprocess.run(["tmux", "-L", sock, "kill-server"], capture_output=True)
        sockdir = os.path.join(os.environ.get("TMUX_TMPDIR") or "/tmp", "tmux-%d" % os.getuid())
        try:
            os.remove(os.path.join(sockdir, sock))
        except OSError:
            pass
    shutil.rmtree(tmp, ignore_errors=True)
