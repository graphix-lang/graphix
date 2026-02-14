#!/usr/bin/env python3
"""
Export graphix-shell source with workspace dependencies resolved.

Copies graphix-shell/src/ and a fully-resolved Cargo.toml into the local
cargo registry cache so that `graphix package rebuild` can find it without
the crate being published to crates.io.
"""

import shutil
import sys
from pathlib import Path

import tomllib

SHELL_DIR = Path(__file__).resolve().parent
WORKSPACE_DIR = SHELL_DIR.parent


def read_workspace_deps():
    with open(WORKSPACE_DIR / "Cargo.toml", "rb") as f:
        root = tomllib.load(f)
    return root.get("workspace", {}).get("dependencies", {})


def resolve_dep(dep, ws_deps):
    """Resolve a single dependency, handling workspace = true."""
    if isinstance(dep, dict) and dep.get("workspace"):
        name_in_ws = dep.get("package", None)  # alias support
        return ws_deps.get(name_in_ws, dep) if name_in_ws else None  # sentinel
        # The caller handles the sentinel by using the ws_deps key lookup
    return dep


def strip_path(dep):
    """Remove path key from a dependency dict, keeping everything else."""
    if isinstance(dep, dict):
        return {k: v for k, v in dep.items() if k != "path"}
    return dep


def format_dep(dep):
    """Format a dependency value as a TOML string."""
    if isinstance(dep, str):
        return f'"{dep}"'
    if isinstance(dep, dict):
        parts = []
        for k, v in dep.items():
            if isinstance(v, str):
                parts.append(f'{k} = "{v}"')
            elif isinstance(v, bool):
                parts.append(f"{k} = {'true' if v else 'false'}")
            elif isinstance(v, list):
                items = ", ".join(f'"{i}"' for i in v)
                parts.append(f"{k} = [{items}]")
            else:
                parts.append(f"{k} = {v}")
        return "{ " + ", ".join(parts) + " }"
    return str(dep)


def resolve_cargo_toml(ws_deps):
    with open(SHELL_DIR / "Cargo.toml", "rb") as f:
        parsed = tomllib.load(f)

    version = parsed["package"]["version"]

    # Resolve workspace deps and strip paths
    deps = {}
    for name, dep in parsed.get("dependencies", {}).items():
        if isinstance(dep, dict) and dep.get("workspace"):
            resolved = (
                dict(ws_deps.get(name, {}))
                if isinstance(ws_deps.get(name), dict)
                else ws_deps.get(name)
            )
            if resolved is None:
                print(f"warning: {name} not found in workspace deps", file=sys.stderr)
                continue
        else:
            resolved = dep
        deps[name] = strip_path(resolved) if isinstance(resolved, dict) else resolved

    # Build the output
    lines = []
    lines.append("[package]")
    for k, v in parsed["package"].items():
        if isinstance(v, str):
            lines.append(f'{k} = "{v}"')
        elif isinstance(v, list):
            items = ", ".join(f'"{i}"' for i in v)
            lines.append(f"{k} = [{items}]")

    lines.append("")
    lines.append("[[bin]]")
    for b in parsed.get("bin", []):
        for k, v in b.items():
            lines.append(f'{k} = "{v}"')

    if "features" in parsed:
        lines.append("")
        lines.append("[features]")
        for k, v in parsed["features"].items():
            items = ", ".join(f'"{i}"' for i in v)
            lines.append(f"{k} = [{items}]")

    lines.append("")
    lines.append("[dependencies]")
    for name, dep in deps.items():
        lines.append(f"{name} = {format_dep(dep)}")

    return "\n".join(lines) + "\n", version


def find_cargo_registry_src():
    cargo_home = Path.home() / ".cargo"
    src_dir = cargo_home / "registry" / "src"
    if not src_dir.is_dir():
        print(f"error: {src_dir} does not exist", file=sys.stderr)
        sys.exit(1)
    for d in src_dir.iterdir():
        if d.is_dir():
            return d
    print(f"error: no index directories in {src_dir}", file=sys.stderr)
    sys.exit(1)


def main():
    ws_deps = read_workspace_deps()
    cargo_toml, version = resolve_cargo_toml(ws_deps)
    dest_parent = find_cargo_registry_src()
    dest = dest_parent / f"graphix-shell-{version}"

    # Clear any cached build dir so rebuild picks up the new source
    build_dir = Path.home() / ".local" / "share" / "graphix" / "build"
    cached = build_dir / f"graphix-shell-{version}"
    if cached.exists():
        print(f"Removing cached build dir: {cached}")
        shutil.rmtree(cached)

    if dest.exists():
        print(f"Removing existing: {dest}")
        shutil.rmtree(dest)

    print(f"Exporting graphix-shell {version} to {dest}")
    dest.mkdir(parents=True)
    shutil.copytree(SHELL_DIR / "src", dest / "src")
    with open(dest / "Cargo.toml", "w") as f:
        f.write(cargo_toml)

    print("Done.")


if __name__ == "__main__":
    main()
