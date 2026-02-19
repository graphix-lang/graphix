#!/usr/bin/env python3
"""
Vendor all dependencies for graphix, including workspace members.

1. Runs `cargo vendor vendor/` to vendor crates.io deps
2. Copies each workspace member into vendor/ with resolved Cargo.toml
   (workspace = true replaced, path deps stripped)
3. Writes .cargo/config.toml with source replacement
"""

import json
import shutil
import subprocess
import sys
from pathlib import Path

import tomllib

ROOT = Path(__file__).resolve().parent


def read_workspace():
    with open(ROOT / "Cargo.toml", "rb") as f:
        root = tomllib.load(f)
    ws = root.get("workspace", {})
    return ws.get("dependencies", {}), ws.get("members", [])


def format_toml_value(v):
    if isinstance(v, str):
        return f'"{v}"'
    if isinstance(v, bool):
        return "true" if v else "false"
    if isinstance(v, int):
        return str(v)
    if isinstance(v, list):
        items = ", ".join(format_toml_value(i) for i in v)
        return f"[{items}]"
    if isinstance(v, dict):
        parts = []
        for k, val in v.items():
            parts.append(f"{k} = {format_toml_value(val)}")
        return "{ " + ", ".join(parts) + " }"
    return str(v)


def resolve_deps(deps, ws_deps):
    """Resolve workspace refs and strip path keys from a deps table."""
    resolved = {}
    for name, dep in deps.items():
        if isinstance(dep, dict) and dep.get("workspace"):
            ws_val = ws_deps.get(name)
            if ws_val is None:
                print(f"  warning: {name} not in workspace deps", file=sys.stderr)
                continue
            dep = dict(ws_val) if isinstance(ws_val, dict) else ws_val
        if isinstance(dep, dict):
            dep = {k: v for k, v in dep.items() if k != "path"}
        resolved[name] = dep
    return resolved


def write_cargo_toml(parsed, ws_deps, dest):
    """Write a resolved Cargo.toml to dest."""
    lines = []

    # [package]
    lines.append("[package]")
    for k, v in parsed["package"].items():
        lines.append(f"{k} = {format_toml_value(v)}")

    # [[bin]] if present
    for b in parsed.get("bin", []):
        lines.append("")
        lines.append("[[bin]]")
        for k, v in b.items():
            lines.append(f"{k} = {format_toml_value(v)}")

    # [lib] if present
    if "lib" in parsed:
        lines.append("")
        lines.append("[lib]")
        for k, v in parsed["lib"].items():
            lines.append(f"{k} = {format_toml_value(v)}")

    # [features] if present
    if "features" in parsed:
        lines.append("")
        lines.append("[features]")
        for k, v in parsed["features"].items():
            lines.append(f"{k} = {format_toml_value(v)}")

    # [dependencies]
    if "dependencies" in parsed:
        lines.append("")
        lines.append("[dependencies]")
        for name, dep in resolve_deps(parsed["dependencies"], ws_deps).items():
            lines.append(f"{name} = {format_toml_value(dep)}")

    # [dev-dependencies]
    if "dev-dependencies" in parsed:
        lines.append("")
        lines.append("[dev-dependencies]")
        for name, dep in resolve_deps(parsed["dev-dependencies"], ws_deps).items():
            lines.append(f"{name} = {format_toml_value(dep)}")

    # [build-dependencies]
    if "build-dependencies" in parsed:
        lines.append("")
        lines.append("[build-dependencies]")
        for name, dep in resolve_deps(parsed["build-dependencies"], ws_deps).items():
            lines.append(f"{name} = {format_toml_value(dep)}")

    with open(dest, "w") as f:
        f.write("\n".join(lines) + "\n")


def vendor_workspace_member(member_path, ws_deps, vendor_dir):
    """Copy a workspace member into vendor/ with resolved Cargo.toml."""
    cargo_toml_path = ROOT / member_path / "Cargo.toml"
    with open(cargo_toml_path, "rb") as f:
        parsed = tomllib.load(f)

    name = parsed["package"]["name"]
    version = parsed["package"]["version"]
    dest = vendor_dir / f"{name}-{version}"

    if dest.exists():
        shutil.rmtree(dest)

    # Copy the entire crate source
    shutil.copytree(ROOT / member_path, dest, ignore=shutil.ignore_patterns("target"))

    # Overwrite Cargo.toml with resolved version
    write_cargo_toml(parsed, ws_deps, dest / "Cargo.toml")

    # Write dummy .cargo-checksum.json (required by cargo vendor source)
    with open(dest / ".cargo-checksum.json", "w") as f:
        json.dump({"files": {}}, f)

    print(f"  {name}-{version}")


def main():
    ws_deps, members = read_workspace()
    vendor_dir = ROOT / "vendor"

    # Step 1: cargo vendor for crates.io deps
    print("Running cargo vendor...")
    result = subprocess.run(
        ["cargo", "vendor", "vendor/"],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"cargo vendor failed:\n{result.stderr}", file=sys.stderr)
        sys.exit(1)

    # Step 2: vendor each workspace member
    print("Vendoring workspace members...")
    for member in members:
        vendor_workspace_member(member, ws_deps, vendor_dir)

    # Step 3: write .cargo/config.toml
    cargo_dir = ROOT / ".cargo"
    cargo_dir.mkdir(exist_ok=True)
    print("""\

# add to .config/cargo.toml to enable
[source.crates-io]
replace-with = "vendored-sources"

[source.vendored-sources]
directory = "vendor"
""")
    print("Done. vendor is ready")


if __name__ == "__main__":
    main()
