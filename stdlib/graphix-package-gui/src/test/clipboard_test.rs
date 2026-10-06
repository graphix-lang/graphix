use arcstr::literal;
use netidx::publisher::Value;
use std::path::PathBuf;

use crate::clipboard::{
    file_list_from_value, files_to_value, html_args_from_value, image_args_from_value,
    image_to_value,
};

#[test]
fn image_value_roundtrip() {
    let img = arboard::ImageData {
        width: 2,
        height: 1,
        bytes: std::borrow::Cow::Owned(vec![
            255, 0, 0, 255, // red pixel
            0, 255, 0, 255, // green pixel
        ]),
    };
    let v = image_to_value(img);

    let args = image_args_from_value(&v).expect("should parse image value");
    assert_eq!(args.width, 2);
    assert_eq!(args.height, 1);
    assert_eq!(args.pixels.as_ref(), &[255, 0, 0, 255, 0, 255, 0, 255]);
}

#[test]
fn html_value_parse() {
    let v: Value = [
        (literal!("alt_text"), Value::from("hello")),
        (literal!("html"), Value::from("<b>hello</b>")),
    ]
    .into();

    let args = html_args_from_value(&v).expect("should parse html value");
    assert_eq!(&*args.html, "<b>hello</b>");
    assert_eq!(&*args.alt_text, "hello");
}

#[test]
fn file_list_roundtrip() {
    let paths = vec![PathBuf::from("/tmp/test.txt"), PathBuf::from("/home/user/doc.pdf")];
    let v = files_to_value(paths.clone());

    let parsed = file_list_from_value(&v).expect("should parse file list");
    assert_eq!(parsed, vec!["/tmp/test.txt", "/home/user/doc.pdf"]);
}

#[test]
fn file_list_from_non_array_returns_none() {
    assert!(file_list_from_value(&Value::Null).is_none());
    assert!(file_list_from_value(&Value::from(42)).is_none());
}

#[test]
fn image_args_from_bad_value_returns_none() {
    assert!(image_args_from_value(&Value::Null).is_none());
    assert!(image_args_from_value(&Value::from("not a struct")).is_none());
}

#[test]
fn html_args_from_bad_value_returns_none() {
    assert!(html_args_from_value(&Value::Null).is_none());
    assert!(html_args_from_value(&Value::from(42)).is_none());
}

// These share the real system clipboard; run with --test-threads=1.

// CR claude for eric: [test-gap] These four ignored tests call arboard directly, so
// they test the third-party crate; graphix's clipboard builtins (ClipboardBuiltin,
// with_clipboard, the nine ops and their ClipboardError mapping) have no test. They
// also share the system clipboard and need --test-threads=1, which CLAUDE.md says never
// to rely on: under --include-ignored, clipboard_clear can clear the clipboard between
// clipboard_write_read_text's set and get. Replace them with one ignored test, with a
// reason, that writes and reads back through the graphix builtins in a single program.
// (tests-ui.r2-15)
// CR claude for eric: [dead] These four #[ignore] tests call arboard directly and
// exercise no graphix code. A plain ignore runs in no gate, and the comment asks for
// --test-threads=1, which the project never relies on: delete them. What goes unpinned
// is the graphix side. image_args_from_value (clipboard.rs:300) accepts pixels whose
// length is not width*height*4, and on Linux arboard's PNG encode hits image's
// assert_eq in PngEncoder::write_image. So write_image panics inside spawn_blocking,
// and the program gets ClipboardError("spawn_blocking: task .. panicked ..") with a
// panic on stderr. Check the length there and pin it beside
// image_args_from_bad_value_returns_none. (tests-ui-15)
#[test]
#[ignore]
fn clipboard_write_read_text() {
    let mut cb = arboard::Clipboard::new().unwrap();
    cb.set_text("graphix_clipboard_test").unwrap();
    let text = cb.get_text().unwrap();
    assert_eq!(text, "graphix_clipboard_test");
}

#[test]
#[ignore]
fn clipboard_clear() {
    let mut cb = arboard::Clipboard::new().unwrap();
    cb.set_text("graphix_clear_test").unwrap();
    assert!(cb.get_text().is_ok());
    cb.clear().unwrap();
    assert!(cb.get_text().is_err());
}

#[test]
#[ignore]
fn clipboard_image_roundtrip() {
    let pixels = vec![255, 0, 0, 255, 0, 255, 0, 255];
    let img = arboard::ImageData {
        width: 2,
        height: 1,
        bytes: std::borrow::Cow::Owned(pixels.clone()),
    };
    let mut cb = arboard::Clipboard::new().unwrap();
    cb.set_image(img).unwrap();
    let read_back = cb.get_image().unwrap();
    assert_eq!(read_back.width, 2);
    assert_eq!(read_back.height, 1);
    assert_eq!(read_back.bytes.as_ref(), pixels.as_slice());
}

#[test]
#[ignore]
fn clipboard_html_roundtrip() {
    let mut cb = arboard::Clipboard::new().unwrap();
    cb.set().html("<b>hello</b>", Some("hello")).unwrap();
    let html = cb.get().html().unwrap();
    assert!(html.contains("<b>hello</b>"));
}
