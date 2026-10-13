//! `file://` URI ↔ filesystem path conversion, with percent-encoding
//! so any absolute path round-trips, spelled as VS Code spells it.

use lsp_types::Uri;
use percent_encoding::{AsciiSet, CONTROLS};
#[cfg(unix)]
use percent_encoding::{percent_decode_str, percent_encode};
use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

/// Characters percent-encoded inside a URI path segment: the WHATWG
/// path percent-encode set plus `%`, what the URI type refuses in a path
/// (`[ ] ^ | \`), and what VS Code encodes besides (`( ) ! $ & ' + , ; = @ : *`),
/// with `/` left readable.
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
    .add(b'%')
    .add(b'[')
    .add(b']')
    .add(b'^')
    .add(b'|')
    .add(b'\\')
    .add(b'(')
    .add(b')')
    .add(b'!')
    .add(b'$')
    .add(b'&')
    .add(b'\'')
    .add(b'+')
    .add(b',')
    .add(b';')
    .add(b'=')
    .add(b'@')
    .add(b':')
    .add(b'*');

/// Convert a `file://` URI to a filesystem path. Returns `None` for
/// non-`file` schemes and, on Unix, remote hosts (anything other than
/// empty or `localhost`).
pub fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    let rest = uri.as_str().strip_prefix("file://")?;
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        // an absolute path beginning with `/`, or `localhost/<path>`
        let path = match rest.strip_prefix("localhost") {
            Some(p) if p.starts_with('/') => p,
            _ if rest.starts_with('/') => rest,
            _ => return None,
        };
        let bytes: Vec<u8> = percent_decode_str(path).collect();
        Some(PathBuf::from(std::ffi::OsString::from_vec(bytes)))
    }
    #[cfg(not(unix))]
    windows::path_of(rest).map(PathBuf::from)
}

/// Convert an absolute filesystem path to a `file://` URI; `None` for a
/// relative one. On Unix a path's bytes are encoded, so one that is not
/// UTF-8 has a URI too.
pub fn path_to_uri(path: &Path) -> Option<Uri> {
    if !path.has_root() {
        return None;
    }
    #[cfg(unix)]
    let uri_str = {
        use std::os::unix::ffi::OsStrExt;
        format!("file://{}", percent_encode(path.as_os_str().as_bytes(), PATH_ENCODE))
    };
    #[cfg(not(unix))]
    let uri_str = windows::uri_of(path.to_str()?)?;
    Uri::from_str(&uri_str).ok()
}

/// The Windows spelling: a drive path is `file:///C%3A/x/a%20b.gx` for
/// `C:\x\a b.gx`, and a host names a UNC share, `file://server/share/a.gx`
/// for `\\server\share\a.gx`.
#[cfg(any(not(unix), test))]
mod windows {
    use super::PATH_ENCODE;
    use percent_encoding::{percent_decode_str, utf8_percent_encode};

    fn is_drive(p: &str) -> bool {
        matches!(p.as_bytes(), [l, b':', ..] if l.is_ascii_alphabetic())
    }

    /// The path of the URI whose text after `file://` is `rest`.
    pub(super) fn path_of(rest: &str) -> Option<String> {
        let (host, path) = rest.split_at(rest.find('/')?);
        let path = percent_decode_str(path).decode_utf8().ok()?;
        let path = match host {
            "" | "localhost" => match path.strip_prefix('/') {
                Some(p) if is_drive(p) => p.to_string(),
                _ => return None,
            },
            host => format!("//{host}{path}"),
        };
        Some(path.replace('/', "\\"))
    }

    /// The `file://` URI of the absolute path `path`.
    pub(super) fn uri_of(path: &str) -> Option<String> {
        let path = path.replace('\\', "/");
        let (host, path) = match path.strip_prefix("//") {
            Some(unc) => unc.split_at(unc.find('/')?),
            None if is_drive(&path) => ("", &*format!("/{path}")),
            None => return None,
        };
        Some(format!("file://{host}{}", utf8_percent_encode(path, PATH_ENCODE)))
    }
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
    fn path_with_brackets_and_vscode_specials_roundtrip() {
        roundtrip("/tmp/[x]/a^b|c\\d.gx");
        roundtrip("/tmp/proj (copy)/a!$&'+,;=@.gx");
        let uri = path_to_uri(Path::new("/tmp/proj (copy)/main.gx")).unwrap();
        assert_eq!(uri.as_str(), "file:///tmp/proj%20%28copy%29/main.gx");
    }

    #[cfg(unix)]
    #[test]
    fn non_utf8_path_roundtrip() {
        use std::os::unix::ffi::OsStrExt;
        let path = Path::new(std::ffi::OsStr::from_bytes(b"/tmp/a \xff.gx"));
        let uri = path_to_uri(path).expect("encode");
        assert_eq!(uri_to_path(&uri).expect("decode"), path);
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
    fn windows_paths_roundtrip() {
        for (path, uri) in [
            (r"C:\x\a b.gx", "file:///C%3A/x/a%20b.gx"),
            (r"\\server\share\a.gx", "file://server/share/a.gx"),
        ] {
            assert_eq!(windows::uri_of(path).as_deref(), Some(uri));
            assert!(Uri::from_str(uri).is_ok(), "{uri}");
            let rest = uri.strip_prefix("file://").unwrap();
            assert_eq!(windows::path_of(rest).as_deref(), Some(path));
        }
        // as VS Code spells a drive
        assert_eq!(
            windows::path_of("/c%3A/proj/main.gx").as_deref(),
            Some(r"c:\proj\main.gx")
        );
        assert_eq!(windows::uri_of(r"\x\rootless.gx"), None);
    }

    #[test]
    fn relative_path_rejected() {
        assert!(path_to_uri(Path::new("relative/path")).is_none());
    }
}
