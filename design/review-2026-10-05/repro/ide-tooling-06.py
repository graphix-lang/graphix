#!/usr/bin/env python3
# ide-tooling-06: vendor.py drops a member's features/optional on
# `workspace = true` deps.
#
# resolve_deps (vendor.py:71) replaces a member's `{ workspace = true, .. }`
# with the workspace table and throws away the member's own keys. Cargo adds
# the member's `features` to the workspace's and takes `optional` from the
# member. The vendored netidx-tpm loses its `windows` feature
# Win32_System_TpmBaseServices (../netidx/netidx-tpm/Cargo.toml:32), which
# netidx-tpm/src/lib.rs:598 imports under cfg(windows). The windows crate
# gates that module behind the feature (vendor/windows/src/Windows/Win32/
# System/mod.rs:143), no other crate in the vendored graph enables it, and
# netidx depends on netidx-tpm unconditionally (netidx/Cargo.toml:51), so a
# Windows build from vendor/ (the graphix-package slow tests
# created_package_compiles, build_standalone_produces_working_binary) fails.
#
# Part 1 runs the exact call vendor_external_path_deps makes for netidx-tpm
# (vendor.py:264) into a temp dir; part 2 shows the same function's latent
# defects on synthetic input.
#
# command: timeout -s KILL 60 python3 design/review-2026-10-05/repro/ide-tooling-06.py
#
# expected: the vendored windows dep keeps Win32_System_TpmBaseServices; an
#   inherited optional dep stays optional with its features unioned; an
#   inherited path resolves against the workspace root; written strings
#   are escaped TOML.
# observed (HEAD c722befe):
#   member windows dep:   {'workspace': True, 'features': ['Win32_System_TpmBaseServices']}
#   vendored windows dep has Win32_System_TpmBaseServices: False
#   optional: {'foo': {'version': '1', 'features': ['a']}}
#   inherited path: FileNotFoundError .../crates/bar/crates/foo/Cargo.toml
#   description 'say "hi"': TOMLDecodeError Expected newline or end of document ...
#   description 'C:\\dir': TOMLDecodeError Unescaped '\' in a string ...

import importlib.util
import pathlib
import tempfile
import tomllib

REPO = pathlib.Path(__file__).resolve().parents[3]
spec = importlib.util.spec_from_file_location("vendor", REPO / "vendor.py")
v = importlib.util.module_from_spec(spec)
spec.loader.exec_module(v)

with tempfile.TemporaryDirectory() as d:
    tmp = pathlib.Path(d)

    crate = (REPO / "../netidx/netidx-tpm").resolve()
    with open(v.find_workspace_root(crate) / "Cargo.toml", "rb") as f:
        ext_ws_deps = tomllib.load(f)["workspace"]["dependencies"]
    with open(crate / "Cargo.toml", "rb") as f:
        parsed = tomllib.load(f)
    v.write_cargo_toml(parsed, ext_ws_deps, crate, tmp / "Cargo.toml")
    with open(tmp / "Cargo.toml", "rb") as f:
        vendored = tomllib.load(f)
    member = parsed["target"]["cfg(windows)"]["dependencies"]["windows"]
    written = vendored["target"]["cfg(windows)"]["dependencies"]["windows"]
    print("member windows dep:  ", member)
    print(
        "vendored windows dep has Win32_System_TpmBaseServices:",
        "Win32_System_TpmBaseServices" in written["features"],
    )

    r = v.resolve_deps(
        {"foo": {"workspace": True, "optional": True, "features": ["x"]}},
        {"foo": {"version": "1", "features": ["a"]}},
        tmp,
    )
    print("optional:", r)

    (tmp / "crates/foo").mkdir(parents=True)
    (tmp / "crates/foo/Cargo.toml").write_text(
        '[package]\nname = "foo"\nversion = "0.1.0"\n'
    )
    (tmp / "crates/bar").mkdir(parents=True)
    try:
        r = v.resolve_deps(
            {"foo": {"workspace": True}}, {"foo": {"path": "crates/foo"}}, tmp / "crates/bar"
        )
        print("inherited path:", r)
    except FileNotFoundError as e:
        print("inherited path: FileNotFoundError", e.filename)

    for desc in ['say "hi"', "C:\\dir"]:
        dest = tmp / "desc.toml"
        v.write_cargo_toml(
            {"package": {"name": "x", "version": "0.1.0", "description": desc}}, {}, tmp, dest
        )
        try:
            tomllib.loads(dest.read_text())
            print(f"description {desc!r}: parses")
        except tomllib.TOMLDecodeError as e:
            print(f"description {desc!r}: TOMLDecodeError {e}")
