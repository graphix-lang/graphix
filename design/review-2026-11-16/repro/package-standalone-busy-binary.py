#!/usr/bin/env python3
# Title: build-standalone fails at its very end, after the whole release build,
#        when the package's previous standalone binary is running: the copy
#        over it hits ETXTBSY.
# Command: python3 -I design/review-2026-11-16/repro/package-standalone-busy-binary.py [GRAPHIX]
#   Sandbox: a fake `cargo` on PATH (logs, never builds), a fake cargo home
#   holding the shell source, private HOME/XDG dirs, network blocked. The
#   "previous standalone binary" is a copy of /usr/bin/sleep, running.
# Expected: "Done! Binary written to .../foo" (replace_exe already swaps the
#   shell's own binary by staging beside it and renaming).
# Observed: "Error: copying .../fakebuilt to .../graphix-package-foo/foo
#   Caused by: Text file busy (os error 26)", rc=1.
import os, shutil, subprocess, sys, tempfile

real = sys.argv[1] if len(sys.argv) > 1 else "/home/eric/tmp/review-2026-11-16/bin/real/graphix"
here = os.path.dirname(os.path.abspath(__file__))
shell_toml = os.path.join(here, "../../../graphix-shell/Cargo.toml")
w = tempfile.mkdtemp()
try:
    src = os.path.join(w, "cargo_home/registry/src/index.crates.io-0000000000000000/graphix-shell-0.9.0")
    os.makedirs(os.path.join(src, "src"))
    shutil.copy(shell_toml, os.path.join(src, "Cargo.toml"))
    open(os.path.join(src, "src/main.rs"), "w").write("fn main(){}\n")
    for d in ("fakebin", "home", "data", "pkgs", "bin"):
        os.makedirs(os.path.join(w, d))
    open(os.path.join(w, "fakebuilt"), "w").write("built\n")
    cargo = os.path.join(w, "fakebin/cargo")
    open(cargo, "w").write(
        '#!/bin/bash\ncase "$1" in\n'
        "  metadata) echo '{\"packages\":[],\"resolve\":{\"nodes\":[],\"root\":null}}' ;;\n"
        '  build) echo \'{"reason":"compiler-artifact","target":{"name":"graphix","kind":["bin"]},"executable":"%s/fakebuilt"}\' ;;\n'
        "esac\n" % w)
    os.chmod(cargo, 0o755)
    exe = os.path.join(w, "bin/graphix")
    shutil.copy(real, exe)
    env = dict(HOME=w + "/home", XDG_DATA_HOME=w + "/data", CARGO_HOME=w + "/cargo_home",
               PATH=w + "/fakebin:/usr/bin:/bin", https_proxy="http://127.0.0.1:9",
               HTTPS_PROXY="http://127.0.0.1:9", ALL_PROXY="http://127.0.0.1:9")
    run = lambda args, cwd=w: subprocess.run(["timeout", "-s", "KILL", "60", exe] + args, cwd=cwd,
                                             env=env, stdin=subprocess.DEVNULL)
    run(["package", "create", "foo", "--dir", w + "/pkgs"])
    pkg = w + "/pkgs/graphix-package-foo"
    open(pkg + "/src/graphix/main.gx", "w").write('println("hi")\n')
    shutil.copy("/usr/bin/sleep", pkg + "/foo")
    os.chmod(pkg + "/foo", 0o755)
    running = subprocess.Popen([pkg + "/foo", "30"])
    try:
        r = run(["package", "build-standalone"], cwd=pkg)
        print("rc=", r.returncode)
    finally:
        running.kill()
        running.wait()
finally:
    shutil.rmtree(w)
