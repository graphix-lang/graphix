# Title: names the seq lowering redirects to its `#seq..` cells lose their IDE
#   sites, and completion inside a seq offers the `#seq..` names themselves.
# Command: timeout -s KILL 175 python3 design/review-2026-10-26/repro/x-lsp-probe-seq-lowered-names.py
# Expected: hover answers each name below with its type and references
#   list every written site of the binding (CLAUDE.md, Language server: a
#   query resolves the cursor from the check's sinks; every declared name is
#   a site); completion offers no name starting with `#` (Expr::is_lowered).
# Observed:
#   - `n * n` in a step: the first `n` answers, the second is null and is
#     missing from n's references (only the entry snapshot's cloned read is
#     a reference);
#   - `clk` read in the body of `seq clk { .. }`: null, missing from clk's
#     references;
#   - `seq let t = clk { .. t .. }`: `t` null at its declaration and its use;
#   - `seq let (u, v) = ..`: u's references are its declaration and the `seq`
#     keyword (the lowering's read at the seq's position); its use is null;
#   - `try { .. } with(err) { .. "[err]" .. }`: `err` null at both sites;
#   - completion in the with body lists #seqv.._N, #seqgo.., #seqj..,
#     #seqe.., #seqr.., #seqidle.., #seqpc.. and not `err`.
import importlib.util, os, sys, tempfile
_s = importlib.util.spec_from_file_location("lspc", os.path.join(os.path.dirname(os.path.abspath(__file__)), "x-lsp-probe-client.py"))
lspc = importlib.util.module_from_spec(_s); _s.loader.exec_module(lspc)
Client, uri = lspc.Client, lspc.uri
D = tempfile.mkdtemp(prefix="x-lsp-probe-")
def write(files):
    for name, text in files.items():
        p = os.path.join(D, name)
        os.makedirs(os.path.dirname(p), exist_ok=True)
        open(p, "w").write(text)
    return {n: os.path.join(D, n) for n in files}
src = '''let clk = sys::time::timer(duration:10.ms, true);
let n = 0;
n <- clk ~ n + 1;
let f = |x: i64| -> Result<i64, `E> select x { 0 => error(`E), x => x };
let a = seq clk {
    let p = n * n;
    let k = clk;
    try { let c = f(p)?; c } with(err) { let m = "[err]"; 0 }
};
let b = seq let t = clk { let w = t; w };
let d = seq let (u, v) = (clk ~ n, clk ~ 2) { let s = u + v; s };
(a, b, d)
'''
P = write({"main.gx": src})
f = P["main.gx"]
c = Client(root=D)
c.open(f, src)
c.sync()
L = src.split("\n")
print("diagnostics:", [d["message"] for v in c.diags.values() for d in v["diagnostics"]])
def q(label, ln, col):
    h = c.hover(f, ln, col).get("result")
    r = c.references(f, ln, col).get("result") or []
    print(label, "hover:", h and h["contents"]["value"].split("\n")[1],
          "references:", [(x["range"]["start"]["line"], x["range"]["start"]["character"]) for x in r])
q("first n of `n * n`:", 5, L[5].find("n *"))
q("second n of `n * n`:", 5, L[5].find("* n") + 2)
q("clk in `let k = clk`:", 6, L[6].find("clk"))
q("err at with(err):", 7, L[7].find("(err)") + 1)
q("err in \"[err]\":", 7, L[7].find("[err]") + 1)
q("t at `let t`:", 9, L[9].find("t ="))
q("t in `let w = t`:", 9, L[9].find("= t;") + 2)
q("u at `(u, v)`:", 10, L[10].find("(u") + 1)
q("u in `u + v`:", 10, L[10].find("u +"))
items = c.completion(f, 7, L[7].find("0 }")).get("result") or []
print("completion in the with body:", [i["label"] for i in items if i["label"].startswith("#") or i["label"] in ("err", "m")])
c.close()
