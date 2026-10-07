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
fn image_args_from_bad_value_is_an_error() {
    assert!(image_args_from_value(&Value::Null).is_err());
    assert!(image_args_from_value(&Value::from("not a struct")).is_err());
}

#[test]
fn html_args_from_bad_value_returns_none() {
    assert!(html_args_from_value(&Value::Null).is_none());
    assert!(html_args_from_value(&Value::from(42)).is_none());
}

/// An image whose pixels are not width x height x 4 bytes is refused
/// where it is decoded, before arboard's encoder asserts on it.
#[test]
fn image_args_refuse_a_wrong_length() {
    let v = image_to_value(arboard::ImageData {
        width: 3,
        height: 1,
        bytes: std::borrow::Cow::Owned(vec![0; 8]),
    });
    assert!(image_args_from_value(&v).is_err());
}

/// The builtins write and read back the system clipboard.
#[tokio::test(flavor = "current_thread")]
#[ignore = "uses the system clipboard and a display"]
async fn clipboard_text_round_trips() -> anyhow::Result<()> {
    let code = "\
        use gui::text::text;\n\
        let got = \"\";\n\
        got <- gui::clipboard::read_text(gui::clipboard::write_text(\"graphix_clip\")$)$;\n\
        let result = text(&got)";
    let mut h = super::GuiTestHarness::new(code).await?;
    h.watch("test::got").await?;
    let back = |h: &mut super::GuiTestHarness| {
        h.get_watched("test::got") == Some(&Value::from("graphix_clip"))
    };
    h.wait_until(back, std::time::Duration::from_secs(5), "read back").await
}
