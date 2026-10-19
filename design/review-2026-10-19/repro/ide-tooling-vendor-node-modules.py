#!/usr/bin/env python3
# ide-tooling: vendor.py's copy_crate copies a member's untracked, git-ignored files,
# so the vendored tree-sitter-graphix carries its 28 MB node_modules (the
# tree-sitter CLI binary included, symlinks followed).
#
# command: timeout -s KILL 120 python3 design/review-2026-10-19/repro/ide-tooling-vendor-node-modules.py
#   (calls vendor.copy_crate on ide/tree-sitter-graphix into a temp dir; runs no cargo)
# expected: the copy holds what `cargo package` would ship (Cargo.toml, lib.rs,
#   build.rs, src/, queries/, ...), a few MB
# observed (HEAD 9445a1d21): node_modules present, 35M copied
import subprocess
import sys
import tempfile
from pathlib import Path

repo = Path(__file__).resolve().parents[3]
sys.dont_write_bytecode = True
sys.path.insert(0, str(repo))
import vendor  # noqa: E402

with tempfile.TemporaryDirectory() as d:
    dest = Path(d) / "tree-sitter-graphix-0.9.0"
    vendor.copy_crate(repo / "ide/tree-sitter-graphix", dest)
    print("node_modules copied:", (dest / "node_modules").exists())
    print(subprocess.run(["du", "-sh", str(dest)], capture_output=True, text=True).stdout.strip())
