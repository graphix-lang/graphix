//! `file://` URI ↔ filesystem path conversion, with percent-encoding
//! so paths containing spaces, `#`, `%`, `?` round-trip.

use lsp_types::Uri;
use percent_encoding::{AsciiSet, CONTROLS, percent_decode_str, utf8_percent_encode};
use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

/// Characters percent-encoded inside a URI path segment: the WHATWG
/// path percent-encode set plus `%`, with `/` left readable.
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
    // After `file://`: an absolute path beginning with `/`, or
    // `localhost/<path>`; anything else is a remote host.
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
