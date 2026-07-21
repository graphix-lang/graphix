//! Multi-file wrappers — module sections riding the SAME single text
//! artifact as the schedule header, so a modular program flows
//! unchanged through every protocol (check / minimize / regress /
//! corpus / the isolated-child stdin).
//!
//! ```text
//! // schedule-v1: …                (optional, before the body)
//! { mod m0; m0::f(i64:2) }
//! // file-v1: m0.gxi
//! val f: fn(x: i64) -> i64;
//! // file-v1: m0.gx
//! let f = |x: i64| -> i64 x + i64:1
//! ```
//!
//! The MAIN body comes first (a findings file reads program-first, and
//! a wrapper with no marker is exactly the old single-file format —
//! zero corpus migration); each `// file-v1: <name>` marker line opens
//! a section running to the next marker or EOF. Sections mount in the
//! oracle's VFS at the ROOT (`/<name>`), and the driver declares
//! `mod <stem>;` for each `.gx` section at the COMPILE-TEXT top level —
//! beside the schedule decls, for the same reason (the `{ mod test; …}`
//! wrap scopes under an anonymous `do<ExprId>`, root-level items are
//! the reliable place) — so the body reaches them by absolute path
//! (`m0::f(…)`). Interfaces are pure VFS data: `m0.gxi` is picked up by
//! the resolver next to `m0.gx` automatically.
//!
//! Section names are flat `stem.gx` / `stem.gxi` (no directories in
//! v1); a malformed marker is an error in every protocol — a generator
//! or minimizer bug, never silently a comment (the schedule rule).

pub const FILE_PREFIX: &str = "// file-v1: ";

/// Split a (schedule-stripped) body into the MAIN body and its file
/// sections. No marker → the whole text and no sections.
pub fn split(body: &str) -> Result<(&str, Vec<(String, String)>), String> {
    let Some(first) = find_marker(body) else {
        return Ok((body, Vec::new()));
    };
    let main = &body[..first];
    let mut files = Vec::new();
    let mut rest = &body[first..];
    while !rest.is_empty() {
        debug_assert!(rest.starts_with(FILE_PREFIX));
        let (line, tail) = match rest.split_once('\n') {
            Some(x) => x,
            None => (rest, ""),
        };
        let name = line[FILE_PREFIX.len()..].trim();
        validate_name(name)?;
        let end = find_marker(tail).unwrap_or(tail.len());
        if files.iter().any(|(n, _)| n == name) {
            return Err(format!("duplicate file section {name}"));
        }
        // Trailing whitespace is normalized away (render re-joins with
        // single newlines) so split∘render is the identity on sections.
        files.push((name.to_string(), tail[..end].trim_end().to_string()));
        rest = &tail[end..];
    }
    Ok((main, files))
}

/// Assemble a multi-file body: main body + one section per file. The
/// inverse of [`split`] up to trailing whitespace.
pub fn render(body: &str, files: &[(String, String)]) -> String {
    let mut s = body.trim_end().to_string();
    for (name, text) in files {
        s.push('\n');
        s.push_str(FILE_PREFIX);
        s.push_str(name);
        s.push('\n');
        s.push_str(text.trim_end());
    }
    s
}

/// The compile-text `mod <stem>;` declarations, one per `.gx` section
/// (a `.gxi` never stands alone — the resolver finds it beside the
/// implementation).
pub fn mod_decls(files: &[(String, String)]) -> String {
    let mut s = String::new();
    for (name, _) in files {
        if let Some(stem) = name.strip_suffix(".gx") {
            s.push_str(&format!("mod {stem};\n"));
        }
    }
    s
}

/// Byte offset of the next section marker at a line start, if any.
fn find_marker(text: &str) -> Option<usize> {
    if text.starts_with(FILE_PREFIX) {
        return Some(0);
    }
    text.match_indices(FILE_PREFIX)
        .find(|(i, _)| text.as_bytes()[i - 1] == b'\n')
        .map(|(i, _)| i)
}

fn validate_name(name: &str) -> Result<(), String> {
    let stem = name
        .strip_suffix(".gx")
        .or_else(|| name.strip_suffix(".gxi"))
        .ok_or_else(|| format!("file section {name}: expected .gx or .gxi"))?;
    let ok = !stem.is_empty()
        && stem.chars().next().is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && stem.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');
    if !ok {
        return Err(format!("file section {name}: invalid module stem"));
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn no_marker_roundtrip() {
        let (main, files) = split("{ 1 + 1 }").unwrap();
        assert_eq!(main, "{ 1 + 1 }");
        assert!(files.is_empty());
    }

    #[test]
    fn split_render_roundtrip() {
        let body = "{ mod m0; m0::f(i64:2) }";
        let files = vec![
            ("m0.gxi".to_string(), "val f: fn(x: i64) -> i64;".to_string()),
            ("m0.gx".to_string(), "let f = |x: i64| -> i64 x + i64:1".to_string()),
        ];
        let text = render(body, &files);
        let (main, parsed) = split(&text).unwrap();
        assert_eq!(main.trim_end(), body);
        assert_eq!(parsed, files);
        assert_eq!(mod_decls(&parsed), "mod m0;\n");
    }

    #[test]
    fn malformed_names_reject() {
        for bad in ["// file-v1: nope.txt\nx", "// file-v1: 0m.gx\nx", "// file-v1: \nx"]
        {
            assert!(split(bad).is_err(), "{bad}");
        }
    }
}
