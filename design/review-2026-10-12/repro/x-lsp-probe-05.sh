#!/bin/bash
# x-lsp-probe-05: URI <-> path conversion cannot handle Windows drive paths
# (graphix-lsp/src/uri.rs, the #[cfg(not(unix))] branches).
#
# No Windows host here, so this compiles uri.rs's non-unix branches on Linux
# (cfg flipped by sed, code otherwise verbatim) against the workspace's own
# lsp-types / percent-encoding rlibs and feeds them Windows inputs. Both
# observations are platform-independent string/URI-parse results.
#
# Command: bash design/review-2026-10-12/repro/x-lsp-probe-05.sh
# Expected: uri_to_path(file:///c%3A/Users/x.gx) = Some("c:/Users/x.gx") (or C:\Users\x.gx),
#           path_to_uri(C:\x\y.gx) = Some(file:///C:/x/y.gx)
# Observed: uri_to_path(file:///c%3A/Users/x.gx) = Some("/c:/Users/x.gx")
#             (on Windows a root-relative path on the current drive, first component "c:")
#           path_to_uri builds "file://C:%5Cx%5Cy.gx" -> Uri::from_str = Err(ParseError { index: 19, kind: UnexpectedChar })
#             so path_to_uri returns None for every Windows path
set -e
REPO=$(cd "$(dirname "$0")/../../.." && pwd)
W=$(mktemp -d); trap 'rm -rf "$W"' EXIT
D=~/tmp/target/debug/deps
LSP=$(ls $D/liblsp_types-*.rlib | head -1)
PE=$(ls $D/libpercent_encoding-*.rlib | head -1)
sed -n '/^#\[cfg(test)\]/q;p' "$REPO/graphix-lsp/src/uri.rs" \
  | sed -e 's/#\[cfg(not(unix))\]/#[cfg(all())]/' -e 's/#\[cfg(unix)\]/#[cfg(any())]/' > "$W/winuri.rs"
cat >> "$W/winuri.rs" <<'RS'

fn main() {
    let u = Uri::from_str("file:///c%3A/Users/x.gx").unwrap();
    println!("uri_to_path(file:///c%3A/Users/x.gx) = {:?}", uri_to_path(&u));
    // path_to_uri's body past its has_root() gate (true on Windows for C:\x\y.gx)
    let encoded = utf8_percent_encode("C:\\x\\y.gx", PATH_ENCODE).to_string();
    let s = format!("file://{encoded}");
    println!("path_to_uri builds {s:?} -> Uri::from_str = {:?}",
        Uri::from_str(&s).map(|u| u.as_str().to_string()).map_err(|e| format!("{e:?}")));
    let _ = path_to_uri; let _ = Path::new("");
}
RS
timeout -s KILL 170 rustc --edition 2024 "$W/winuri.rs" -o "$W/winuri" -L dependency=$D \
  --extern lsp_types=$LSP --extern percent_encoding=$PE
timeout -s KILL 10 "$W/winuri"
