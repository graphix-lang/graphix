#!/usr/bin/env python3
"""Compare compile/check latency using two Graphix binaries of the same profile."""

import argparse
import pathlib
import statistics
import subprocess
import tempfile
import time


def measure(binary, source):
    start = time.perf_counter_ns()
    subprocess.run(
        # CR claude for claude: [test-gap] `--check` runs the check alone (no elaboration,
        # no fusion, `#[native]` never verified), so this harness times parse and
        # typecheck only and the generated `#[native] f(0)` asserts nothing: a fusion or
        # elaboration startup regression shows no change. design/jit_startup.md:126 and
        # bench/README.md:60-62 still say it requires the calls to fuse. Time `--expand`
        # instead, which builds, fuses and checks `#[native]`, and correct those two
        # descriptions. Probe: `let f = |x: i64| { let a0 = x + 1; println("[a0]"); a0
        # }; #[native] f(0)` exits 0 under `--check` and fails under `--expand` with
        # 'did not fully fuse'. (ide-tooling.r2-06)
        [binary, "--no-netidx", "--no-init", "--check", str(source)],
        check=True,
        capture_output=True,
        text=True,
    )
    return (time.perf_counter_ns() - start) / 1_000_000


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline", type=pathlib.Path)
    parser.add_argument("candidate", type=pathlib.Path)
    parser.add_argument("--runs", type=int, default=7)
    args = parser.parse_args()
    if args.runs < 1:
        parser.error("--runs must be positive")
    binaries = [str(p.resolve(strict=True)) for p in (args.baseline, args.candidate)]
    print("bindings  baseline_ms  candidate_ms  change", flush=True)
    with tempfile.TemporaryDirectory(prefix="graphix-startup-") as directory:
        for size in (64, 128, 256, 512):
            source = pathlib.Path(directory) / f"block-{size}.gx"
            statements = ["let f = |x: i64| {", "let a0 = x + 1;"]
            statements.extend(f"let a{i} = a{i - 1} + 1;" for i in range(1, size))
            statements.extend([f"a{size - 1}", "};", "#[native] f(0)"])
            source.write_text("\n".join(statements) + "\n")
            for binary in binaries:
                measure(binary, source)
            samples = [[], []]
            for run in range(args.runs):
                for i in ((0, 1) if run % 2 == 0 else (1, 0)):
                    samples[i].append(measure(binaries[i], source))
            before, after = map(statistics.median, samples)
            print(
                f"{size:8}  {before:11.2f}  {after:12.2f}  {(after / before - 1):+.1%}",
                flush=True,
            )


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as error:
        raise SystemExit(error.stderr or error.stdout or str(error)) from error
