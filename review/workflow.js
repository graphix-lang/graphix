export const meta = {
  name: 'graphix-review',
  description: 'Full-repo review of Graphix: one deep reviewer per file unit or cross-cutting dimension, each giving CR placements directly (no verify phase)',
  phases: [
    { title: 'Review', detail: 'one deep reviewer per unit or cross-cutting dimension' },
  ],
}

// args is STATE/args.json, written by review/dims.py:
// {date, head, state, work, repro, prev: {date, commit}, context, fable: [key], order: [key]}
// A reviewer named in `fable` runs on Fable, the rest on Opus.
const S = args.state
const W = args.work
const B = W + '/bin'
const HEAD = args.head
const REPRO = args.repro
const REPRO_ABS = '/home/eric/proj/graphix/' + REPRO
const TAGS = ['bug', 'risk', 'structure', 'style', 'readability', 'perf', 'dead', 'doc-drift', 'test-gap']
const SEVS = ['critical', 'high', 'medium', 'low']

const FINDINGS = {
  type: 'object',
  properties: {
    summary: { type: 'string', description: '2-6 sentences: what this code does and your overall assessment of its health (for a re-run dimension: the per-file tally; for a design reviewer: the design assessment its assignment asks for)' },
    findings: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          title: { type: 'string', description: 'one line, <= 90 chars' },
          tag: { type: 'string', enum: TAGS },
          severity: { type: 'string', enum: SEVS },
          evidence: { type: 'string', enum: ['ran', 'read'], description: 'ran: you executed a program/test and observed the defect; read: established by reading the code only' },
          description: { type: 'string', description: 'precise statement of the defect, citing the code (file:line is fine here)' },
          failure_scenario: { type: 'string', description: 'concrete input/program/state -> wrong outcome (bug/risk); for others, the concrete cost' },
          suggested_fix: { type: 'string' },
          probe: { type: 'string', description: 'the command you ran and what it printed (abridged), or "not run"' },
          repro_path: { type: 'string', description: `repo-relative path of the saved repro under ${REPRO}/, or empty` },
          cr_file: { type: 'string', description: 'repo-relative file the CR goes in' },
          cr_line: { type: 'integer', description: '1-based line at HEAD the CR annotates; it is inserted above this line' },
          cr_anchor: { type: 'string', description: 'exact text of that line, trimmed' },
          cr_text: { type: 'string', description: 'the CR body, without the "// CR claude for <who>: [tag/sev]" prefix and without a trailing id' },
          addressee: { type: 'string', enum: ['eric', 'claude'] },
        },
        required: ['title', 'tag', 'severity', 'evidence', 'description', 'failure_scenario', 'suggested_fix', 'probe', 'repro_path', 'cr_file', 'cr_line', 'cr_anchor', 'cr_text', 'addressee'],
      },
    },
  },
  required: ['summary', 'findings'],
}

const PRE = (key) => `You are one reviewer in a full-repository code review of Graphix (/home/eric/proj/graphix, branch main, HEAD ${HEAD}), commissioned by its author Eric. CLAUDE.md (already in your context) holds the project's rules and the intended language semantics: treat it as the spec. Design docs live in design/ (index: design/README.md) and hold the rules as built.

CONTEXT: ${args.context} Review the code as it stands; a fix that looks narrow (one site of a rule that applies at several, a symptom treated instead of the root cause) is worth a finding.

THERE IS NO VERIFICATION PHASE after you: your report goes into the code as CRs as you write it. So be accurate. Confirm by running wherever you can, and drop a suspicion you could not substantiate rather than report it at low confidence. Precision matters more than count.

PRIORITIES, in order:
1. Correctness: wrong results; crashes, panics, aborts or hangs; memory unsafety / UB; type-checker unsoundness (accepts a program that then misbehaves) or incompleteness (refuses a valid one); the check and the build disagreeing; divergence between the node-walk interpreter and the JIT, between serial and parallel evaluation, between pruned and unpruned walks, or between a cold run and a warm image start; resource leaks; nondeterminism; security problems in code that handles external input.
2. Performance: needless work or allocation on hot paths (per-cycle runtime paths, the typechecker), superlinear algorithms where input size is program- or data-controlled.
3. Structure: duplicated logic (DRY), needless layers of abstraction, invalid states representable, wrong layering, functions doing too much.
4. Readability: misleading names; comments that contradict the code or narrate history (Eric's rule: a comment is allowed only for an invariant the types cannot say); dead code; confusing control flow.

GROUND RULES
- You are READ-ONLY with respect to the repository, except that you may create repro files under ${REPRO_ABS}/. Never commit, never run a \`cr\` command that writes, never run cargo fmt. Scratch files go under ${W}/work/${key}/ (create it).
- Prebuilt DEBUG binaries at HEAD (debug assertions and overflow checks on, opt-level s):
  * ${B}/graphix — \`timeout -s KILL 30 ${B}/graphix --no-cache prog.gx\` runs a program and prints the value of its last expression each time it updates. It never exits on its own: let the timeout kill it (exit 137), or end the program with a gated exit, e.g. \`sys::exit(sys::time::after_idle(duration:100.ms, 0))\` as a statement before the final expression. \`--check\` parses + typechecks only (the check alone); \`--expand\` builds too and prints every seq's lowered machine. \`--no-fusion\` runs the node-walk only. \`${B}/graphix fmt --stdout f.gx\` formats. \`${B}/graphix lsp\` is the language server on stdio. Without --no-cache a session image is written/read under $XDG_CACHE_HOME/graphix/ — point XDG_CACHE_HOME into your scratch dir whenever you run without --no-cache.
  * ${B}/graphix-fuzz — \`timeout -s KILL 180 ${B}/graphix-fuzz check prog.gx\` runs a program under the node-walk vs the JIT, each with no cache vs cold image vs warm image, forked-parallel vs serial and pruned vs unpruned, and the check against the build, and prints AGREE, or DIVERGENCE (exit 1) with both outcomes. A program for the fuzzer must NOT call sys::exit (the fuzzer ends the run itself; with sys::exit, timers, random values or the wall clock it prints EXCLUDED: nothing was compared, so the probe proves nothing). \`graphix-fuzz run prog.gx\` prints every mode's outcome plus fusion stats — use it to confirm an expression actually FUSED (top-level code or code with effects does not fuse; a JIT probe that never fuses proves nothing). Only check, run, minimize and typemorph <file> are available.
  * Env knobs (design/debugging.md lists all): GRAPHIX_PAR=off|auto|force, GRAPHIX_PAR_AUDIT=1, GRAPHIX_PRUNE=off|on|audit, GRAPHIX_ELAB_AUDIT=1, GRAPHIX_FUSE_SERIAL=1, GRAPHIX_TASK_AUDIT=1, GRAPHIX_DUMP_CLIF=1, GRAPHIX_DBG_BIND=1, GRAPHIX_PROFILE=1.
- Before reading or writing ANY Graphix code (.gx/.gxi, or Graphix inside Rust test strings), Read /home/eric/.claude/skills/graphix-lang/SKILL.md, and the topic file beside it that its table names for what you are writing — Graphix is not in your training data.
- git: ALWAYS pass --no-ext-diff to git diff / git show / git log -p (the configured external diff tool dies mid-diff).
- ../netidx (sibling checkout) holds graphix-package-netidx-admin, an external consumer of these crates: grep it too before calling anything dead or unused.
- Open CRs already in the tree are listed in ${S}/open_crs.txt (\`cr show <id>\` prints one). Do not re-report what an open CR already says; a new aspect of it may be a finding that cites its id.
- GUI: never open a window on the desktop: run GUI programs only as \`env -u WAYLAND_DISPLAY -u DISPLAY ...\` or check them with --check. Package manager: reproduce only in a throwaway copy under your scratch dir, never against ~/.config or the repo's own manifests. Network: localhost only.
- RESOURCES (an earlier review exhausted this machine's memory and the session was killed; follow these strictly): the two binaries are memory-capped wrappers (6 GB / 10 GB per run, no swap, all probes share a 40 GB pool). A run killed by the cap prints "[probe] OOM-KILLED: ..." and exits 251 — runaway memory, NOT a hang (a timeout kill is a silent 137). A REASONABLE program hitting the cap is itself a finding — record it, don't retry it bigger. Run any other program you write (python drivers, etc.) under \`timeout -s KILL N\` too. Never leave processes running after your command returns: no nohup, no trailing &, no setsid/disown, no run_in_background loops. Every probe runs under \`timeout -s KILL N\` with N <= 180. At most 2 probe processes at a time: no \`xargs -P\`, no parallel fuzz loops; a self-written fuzz/mutation loop runs at most ~150 iterations. No perf. Generated inputs under ~50 MB. About 16 review agents share this machine: timings are noisy, so re-run a slow or timed-out probe alone before calling it a hang.
- Cargo: avoid it — the build directory is shared by many agents. Only when NO program/CLI path can reach a defect may you write a temporary integration test (in the owning crate's tests/ dir, named review_<key_with_underscores>_<n>.rs) and run exactly \`flock /home/eric/tmp/cargo.lock timeout -s KILL 2400 cargo test -p <crate> --test <that name> -- --nocapture\` (builds serialize on a lock: expect waits). Afterwards MOVE that file to ${REPRO_ABS}/ if it demonstrates a defect, else delete it — never leave it under tests/. Never run any other cargo command.
- Comments and docs in this tree go stale fast; trust the code. Where code and CLAUDE.md or a design doc disagree, that is a finding — say which side you believe is wrong. CLAUDE.md drift: put cr_file "CLAUDE.md" with the line; it goes in the summary doc, not a CR.

WHAT TO REPORT
- Tags: bug (wrong behaviour reachable today), risk (latent hazard: fragile invariant, unsound API, UB not yet shown reachable), perf, structure, style (repo conventions: grouped \`use\`, anyhow, poolshark pooling, CompactString / ArcStr / LPooled string hierarchy, triomphe::Arc, parking_lot, history comments), readability, dead, doc-drift, test-gap (an important rule with no pin, or a test that cannot fail: name the code change it would miss).
- Severity: critical (UB / memory unsafety; wrong answers or crashes on ordinary programs; unsound typing that crashes the runtime), high (wrong answer / crash / engine divergence in a reachable less-common case; image corruption; deadlock or hang; a big perf cliff on ordinary programs), medium (edge-case misbehaviour, misleading diagnostics, leaks, fragile invariants likely to break), low (minor).
- A bug needs evidence "ran" wherever a program can reach the code: save the smallest program that shows it as ${REPRO_ABS}/${key}-<short-slug>.gx (or .sh / .py / .rs) with a header comment giving the title, the exact command, the expected and the observed output, and put that path in repro_path. A bug you could not trigger is a risk unless the code trace is certain. Agreement between engines on a WRONG answer is still a bug: decide the intended value from CLAUDE.md first. Before reporting that the check wrongly refuses or accepts a program, make sure the program is what you think it is: --check the smallest variant that differs only in the point at issue.
- Every structure/readability/perf finding needs a specific, actionable fix that adds no layer of abstraction.
- Depth over breadth: at most ~20 findings, most important first. No trivia, no rustfmt-level nits, nothing a compiler warning already says. If the code is healthy, say so and return few findings — do not pad.

THE CR (each finding becomes one comment in the code, inserted verbatim)
- cr_line is the line the comment annotates (the CR is inserted ABOVE it, at that line's indentation); cr_anchor is that line's exact trimmed text. It must be code, never inside a string literal (for a Graphix fixture inside a Rust string, annotate the Rust line that opens the fixture) and never inside a multi-line comment or doc block (annotate the item below it). In .gx/.gxi files it must start an expression, a select arm, an impl/trait method, a struct-literal field or an interface item. In .md files it becomes an HTML comment above the line (never inside a code fence: annotate the line that opens the fence).
- cr_text: plain words: what is wrong, what it causes, what would fix it, and the evidence: "probe: ${REPRO}/<file>" when you ran it, or "Found by reading; not run." when you did not. Cite other code by symbol (never by line number: CR text is frozen). No history, no hedging filler. Usually 2-6 sentences.
- addressee: "eric" when it needs Eric's decision (a language or semantics rule, a design trade-off, a security or platform policy); "claude" for a bug with a clear fix, a doc fix, a test gap, a refactor or a local optimization.

Your key is "${key}". Your specific assignment is in ${S}/prompts/${key}.md — Read it first, then do the work.`

phase('Review')
const FABLE = new Set(args.fable)
const results = await parallel(args.order.map((key) => () => {
  const model = FABLE.has(key) ? 'fable' : 'opus'
  return agent(PRE(key), { label: key, phase: 'Review', schema: FINDINGS, model })
    .then((r) => r ? { key, model, n: r.findings.length } : { key, model, missing: true })
}))

const ok = results.filter((r) => r && !r.missing)
return {
  units: ok.length,
  missing: results.filter((r) => !r || r.missing).map((r) => r ? r.key : '?'),
  findings: ok.reduce((a, r) => a + r.n, 0),
  byModel: ok.reduce((a, r) => { a[r.model] = (a[r.model] || 0) + r.n; return a }, {}),
}
