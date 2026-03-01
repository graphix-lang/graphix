#!/usr/bin/env python3
"""
Vendor all dependencies for graphix, including workspace members.

1. Runs `cargo vendor vendor/` to vendor crates.io deps
2. Copies each workspace member into vendor/ with resolved Cargo.toml
   (workspace = true replaced, path deps stripped)
3. Vendors external path deps (e.g. netidx) and their transitive path deps
4. Writes .cargo/config.toml with source replacement
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

    # [target.'cfg(...)'.dependencies] sections
    for key, val in parsed.items():
        if key == "target" and isinstance(val, dict):
            for target, sections in val.items():
                if isinstance(sections, dict):
                    for section_name in ["dependencies", "dev-dependencies", "build-dependencies"]:
                        if section_name in sections:
                            lines.append("")
                            lines.append(f"[target.'{target}'.{section_name}]")
                            for name, dep in resolve_deps(sections[section_name], ws_deps).items():
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


def find_workspace_root(crate_path):
    """Find the workspace root for a crate by walking up to a [workspace] Cargo.toml."""
    path = crate_path.resolve().parent  # start from parent in case crate_path is a member
    while True:
        cargo_toml = path / "Cargo.toml"
        if cargo_toml.exists():
            with open(cargo_toml, "rb") as f:
                parsed = tomllib.load(f)
            if "workspace" in parsed and "members" in parsed.get("workspace", {}):
                return path
        parent = path.parent
        if parent == path:
            break
        path = parent
    return None


def vendor_external_path_deps(ws_deps, vendor_dir):
    """Vendor workspace deps that reference paths outside the graphix workspace."""
    root_resolved = ROOT.resolve()

    # Collect directly-referenced external path deps
    queue = []
    for name, dep in ws_deps.items():
        if isinstance(dep, dict) and "path" in dep:
            path = (ROOT / dep["path"]).resolve()
            if not str(path).startswith(str(root_resolved)):
                queue.append(path)

    if not queue:
        return

    # Cache external workspace deps by workspace root
    ext_ws_cache = {}
    vendored = set()

    print("Vendoring external path dependencies...")
    while queue:
        crate_path = queue.pop().resolve()
        if str(crate_path) in vendored:
            continue
        vendored.add(str(crate_path))

        # Find the external workspace root and its deps
        ws_root = find_workspace_root(crate_path)
        if ws_root:
            ws_key = str(ws_root)
            if ws_key not in ext_ws_cache:
                with open(ws_root / "Cargo.toml", "rb") as f:
                    ext_ws_cache[ws_key] = tomllib.load(f).get(
                        "workspace", {}
                    ).get("dependencies", {})
            ext_ws_deps = ext_ws_cache[ws_key]
        else:
            ext_ws_deps = {}

        # Read and vendor the crate
        with open(crate_path / "Cargo.toml", "rb") as f:
            parsed = tomllib.load(f)

        name = parsed["package"]["name"]
        version = parsed["package"]["version"]
        dest = vendor_dir / f"{name}-{version}"

        if dest.exists():
            shutil.rmtree(dest)

        shutil.copytree(crate_path, dest, ignore=shutil.ignore_patterns("target", ".#*"))
        write_cargo_toml(parsed, ext_ws_deps, dest / "Cargo.toml")
        with open(dest / ".cargo-checksum.json", "w") as f:
            json.dump({"files": {}}, f)
        print(f"  {name}-{version}")

        # Enqueue transitive path deps (skip dev-deps — not needed for building)
        for section in ["dependencies", "build-dependencies"]:
            for dep_val in parsed.get(section, {}).values():
                if isinstance(dep_val, dict) and "path" in dep_val:
                    queue.append((crate_path / dep_val["path"]).resolve())


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

    # Step 3: vendor external path deps (e.g. netidx)
    vendor_external_path_deps(ws_deps, vendor_dir)

    # Step 4: write .cargo/config.toml
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
