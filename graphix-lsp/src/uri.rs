//! `file://` URI ↔ filesystem path conversion.
//!
//! LSP transmits paths as URIs; on disk we have raw paths. RFC 3986 says
//! the path component is percent-encoded — so a file at `/tmp/a b.gx`
//! travels the wire as `file:///tmp/a%20b.gx`. Stripping `file://` and
//! treating the remainder as a path (which the previous helpers did)
//! gets `/tmp/a%20b.gx` and then fails to find the file.
//!
//! These helpers handle percent-encoding/decoding so paths containing
//! spaces, `#`, `%`, `?`, etc. round-trip correctly.

use lsp_types::Uri;
use percent_encoding::{percent_decode_str, utf8_percent_encode, AsciiSet, CONTROLS};
use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

/// Characters that must be percent-encoded inside a URI path segment.
/// We keep `/` unencoded so directory separators stay readable. The set
/// matches the WHATWG URL "path percent-encode set" plus `%` (which the
/// `percent-encoding` crate doesn't include automatically).
const PATH_ENCODE: &AsciiSet = &CONTROLS
    .add(b' ')
    .add(b'"')
    .add(b'#')
    .add(b'<')
    .add(b'>')
    .add(b'?')
    .add(b'`')
    .add(b'{')
    .add(b'}')
    .add(b'%');

/// Convert a `file://` URI to a filesystem path. Returns `None` for
/// non-`file` schemes, remote hosts (anything other than empty or
/// `localhost`), or URIs that don't decode to valid UTF-8.
pub fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    let s = uri.as_str();
    let rest = s.strip_prefix("file://")?;
    // After `file://` we expect either an absolute path beginning with
    // `/`, or `localhost/<path>`. Anything else is a remote host we
    // can't access.
    let raw = if let Some(p) = rest.strip_prefix("localhost/") {
        format!("/{p}")
    } else if rest.starts_with('/') {
        rest.to_string()
    } else {
        return None;
    };
    let decoded = percent_decode_str(&raw).decode_utf8().ok()?;
    Some(PathBuf::from(decoded.as_ref()))
}

/// Convert an absolute filesystem path to a `file://` URI. Returns
/// `None` for non-UTF-8 paths, non-absolute paths, or if URI parsing
/// rejects the result.
pub fn path_to_uri(path: &Path) -> Option<Uri> {
    let s = path.to_str()?;
    if !s.starts_with('/') {
        return None;
    }
    let encoded = utf8_percent_encode(s, PATH_ENCODE).to_string();
    let uri_str = format!("file://{encoded}");
    Uri::from_str(&uri_str).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn roundtrip(p: &str) {
        let path = PathBuf::from(p);
        let uri = path_to_uri(&path).expect("encode");
        let back = uri_to_path(&uri).expect("decode");
        assert_eq!(back, path, "roundtrip failed via uri {}", uri.as_str());
    }

    #[test]
    fn plain_path_roundtrip() {
        roundtrip("/tmp/foo.gx");
        roundtrip("/home/user/project/src/main.gx");
    }

    #[test]
    fn path_with_spaces_roundtrip() {
        roundtrip("/tmp/has space/file.gx");
        let uri = path_to_uri(Path::new("/tmp/has space/file.gx")).unwrap();
        assert_eq!(uri.as_str(), "file:///tmp/has%20space/file.gx");
    }

    #[test]
    fn path_with_special_chars_roundtrip() {
        roundtrip("/tmp/a#b.gx");
        roundtrip("/tmp/a%b.gx");
        roundtrip("/tmp/a?b.gx");
        roundtrip("/tmp/a b#c%d?e.gx");
    }

    #[test]
    fn path_with_unicode_roundtrip() {
        roundtrip("/tmp/café.gx");
        roundtrip("/tmp/日本語.gx");
    }

    #[test]
    fn localhost_host_accepted() {
        let uri = Uri::from_str("file://localhost/tmp/foo.gx").unwrap();
        assert_eq!(uri_to_path(&uri).unwrap(), PathBuf::from("/tmp/foo.gx"));
    }

    #[test]
    fn non_file_scheme_rejected() {
        let uri = Uri::from_str("https://example.com/foo").unwrap();
        assert!(uri_to_path(&uri).is_none());
    }

    #[test]
    fn remote_host_rejected() {
        let uri = Uri::from_str("file://otherhost/tmp/foo").unwrap();
        assert!(uri_to_path(&uri).is_none());
    }

    #[test]
    fn relative_path_rejected() {
        assert!(path_to_uri(Path::new("relative/path")).is_none());
    }
}
