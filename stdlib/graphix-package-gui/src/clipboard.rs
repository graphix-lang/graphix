use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{errf, typ::FnType};
use graphix_package_core::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx::publisher::Value;
use std::{fmt::Debug, marker::PhantomData, path::PathBuf, sync::LazyLock};

/// Trait for individual clipboard operations, parameterizing the generic
/// [`ClipboardBuiltin`] wrapper.
pub(crate) trait ClipboardOp: Debug + Default + Send + Sync + 'static {
    const NAME: &str;
    const TYP: LazyLock<FnType>;
    type Args: Debug + Send + Sync + 'static;

    fn prepare(cached: &CachedVals) -> Option<Self::Args>;
    fn exec(args: Self::Args) -> Value;
}

/// Generic [`EvalCachedAsync`] impl wrapping any [`ClipboardOp`].
///
/// All clipboard operations create a fresh `arboard::Clipboard` inside
/// `spawn_blocking` (it's `!Send`, so we can't hold one across awaits).
#[derive(Debug, Default)]
pub(crate) struct ClipboardBuiltin<Op: ClipboardOp>(PhantomData<Op>);

impl<Op: ClipboardOp> EvalCachedAsync for ClipboardBuiltin<Op> {
    const NAME: &str = Op::NAME;
    const TYP: LazyLock<FnType> = Op::TYP;
    type Args = Op::Args;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Op::Args> {
        Op::prepare(cached)
    }

    fn eval(args: Op::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || Op::exec(args)).await {
                Ok(v) => v,
                Err(e) => errf!("ClipboardError", "spawn_blocking: {e}"),
            }
        }
    }
}

fn with_clipboard(
    f: impl FnOnce(&mut arboard::Clipboard) -> Result<Value, arboard::Error>,
) -> Value {
    match arboard::Clipboard::new() {
        Ok(mut cb) => match f(&mut cb) {
            Ok(v) => v,
            Err(e) => errf!("ClipboardError", "{e}"),
        },
        Err(e) => errf!("ClipboardError", "{e}"),
    }
}

// ── ReadText ────────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct ReadTextOp;

impl ClipboardOp for ReadTextOp {
    const NAME: &str = "gui_clipboard_read_text";
    deftype!(r#"fn(Any) -> Result<string, `ClipboardError(string)>"#);
    type Args = ();

    fn prepare(cached: &CachedVals) -> Option<()> {
        cached.0[0].as_ref()?;
        Some(())
    }

    fn exec((): ()) -> Value {
        with_clipboard(|cb| Ok(Value::from(cb.get_text()?)))
    }
}

pub(crate) type ReadText = CachedArgsAsync<ClipboardBuiltin<ReadTextOp>>;

// ── WriteText ───────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct WriteTextOp;

impl ClipboardOp for WriteTextOp {
    const NAME: &str = "gui_clipboard_write_text";
    deftype!(r#"fn(string) -> Result<null, `ClipboardError(string)>"#);
    type Args = ArcStr;

    fn prepare(cached: &CachedVals) -> Option<ArcStr> {
        cached.get::<ArcStr>(0)
    }

    fn exec(text: ArcStr) -> Value {
        with_clipboard(|cb| {
            cb.set_text(text.to_string())?;
            Ok(Value::Null)
        })
    }
}

pub(crate) type WriteText = CachedArgsAsync<ClipboardBuiltin<WriteTextOp>>;

// ── ReadImage ───────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct ReadImageOp;

impl ClipboardOp for ReadImageOp {
    const NAME: &str = "gui_clipboard_read_image";
    deftype!(
        r#"fn(Any) -> Result<{height: u32, pixels: bytes, width: u32}, `ClipboardError(string)>"#
    );
    type Args = ();

    fn prepare(cached: &CachedVals) -> Option<()> {
        cached.0[0].as_ref()?;
        Some(())
    }

    fn exec((): ()) -> Value {
        with_clipboard(|cb| {
            let img = cb.get_image()?;
            Ok(image_to_value(img))
        })
    }
}

pub(crate) type ReadImage = CachedArgsAsync<ClipboardBuiltin<ReadImageOp>>;

// ── WriteImage ──────────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct ImageArgs {
    pub(crate) width: usize,
    pub(crate) height: usize,
    pub(crate) pixels: Bytes,
}

#[derive(Debug, Default)]
pub(crate) struct WriteImageOp;

impl ClipboardOp for WriteImageOp {
    const NAME: &str = "gui_clipboard_write_image";
    deftype!(
        r#"fn({height: u32, pixels: bytes, width: u32}) -> Result<null, `ClipboardError(string)>"#
    );
    type Args = ImageArgs;

    fn prepare(cached: &CachedVals) -> Option<ImageArgs> {
        image_args_from_value(cached.0[0].as_ref()?)
    }

    fn exec(args: ImageArgs) -> Value {
        with_clipboard(|cb| {
            let img = arboard::ImageData {
                width: args.width,
                height: args.height,
                bytes: std::borrow::Cow::Owned(args.pixels.to_vec()),
            };
            cb.set_image(img)?;
            Ok(Value::Null)
        })
    }
}

pub(crate) type WriteImage = CachedArgsAsync<ClipboardBuiltin<WriteImageOp>>;

// ── ReadHtml ────────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct ReadHtmlOp;

impl ClipboardOp for ReadHtmlOp {
    const NAME: &str = "gui_clipboard_read_html";
    deftype!(r#"fn(Any) -> Result<string, `ClipboardError(string)>"#);
    type Args = ();

    fn prepare(cached: &CachedVals) -> Option<()> {
        cached.0[0].as_ref()?;
        Some(())
    }

    fn exec((): ()) -> Value {
        with_clipboard(|cb| Ok(Value::from(cb.get().html()?)))
    }
}

pub(crate) type ReadHtml = CachedArgsAsync<ClipboardBuiltin<ReadHtmlOp>>;

// ── WriteHtml ───────────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct HtmlArgs {
    pub(crate) html: ArcStr,
    pub(crate) alt_text: ArcStr,
}

#[derive(Debug, Default)]
pub(crate) struct WriteHtmlOp;

impl ClipboardOp for WriteHtmlOp {
    const NAME: &str = "gui_clipboard_write_html";
    deftype!(
        r#"fn({alt_text: string, html: string}) -> Result<null, `ClipboardError(string)>"#
    );
    type Args = HtmlArgs;

    fn prepare(cached: &CachedVals) -> Option<HtmlArgs> {
        html_args_from_value(cached.0[0].as_ref()?)
    }

    fn exec(args: HtmlArgs) -> Value {
        with_clipboard(|cb| {
            let html = args.html.to_string();
            let alt = args.alt_text.to_string();
            cb.set().html(html, Some(alt))?;
            Ok(Value::Null)
        })
    }
}

pub(crate) type WriteHtml = CachedArgsAsync<ClipboardBuiltin<WriteHtmlOp>>;

// ── ReadFiles ───────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct ReadFilesOp;

impl ClipboardOp for ReadFilesOp {
    const NAME: &str = "gui_clipboard_read_files";
    deftype!(r#"fn(Any) -> Result<Array<string>, `ClipboardError(string)>"#);
    type Args = ();

    fn prepare(cached: &CachedVals) -> Option<()> {
        cached.0[0].as_ref()?;
        Some(())
    }

    fn exec((): ()) -> Value {
        with_clipboard(|cb| Ok(files_to_value(cb.get().file_list()?)))
    }
}

pub(crate) type ReadFiles = CachedArgsAsync<ClipboardBuiltin<ReadFilesOp>>;

// ── WriteFiles ──────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct WriteFilesOp;

impl ClipboardOp for WriteFilesOp {
    const NAME: &str = "gui_clipboard_write_files";
    deftype!(r#"fn(Array<string>) -> Result<null, `ClipboardError(string)>"#);
    type Args = Vec<String>;

    fn prepare(cached: &CachedVals) -> Option<Vec<String>> {
        file_list_from_value(cached.0[0].as_ref()?)
    }

    fn exec(paths: Vec<String>) -> Value {
        with_clipboard(|cb| {
            let paths: Vec<PathBuf> = paths.iter().map(PathBuf::from).collect();
            cb.set().file_list(&paths)?;
            Ok(Value::Null)
        })
    }
}

pub(crate) type WriteFiles = CachedArgsAsync<ClipboardBuiltin<WriteFilesOp>>;

// ── Clear ───────────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct ClearOp;

impl ClipboardOp for ClearOp {
    const NAME: &str = "gui_clipboard_clear";
    deftype!(r#"fn(Any) -> Result<null, `ClipboardError(string)>"#);
    type Args = ();

    fn prepare(cached: &CachedVals) -> Option<()> {
        cached.0[0].as_ref()?;
        Some(())
    }

    fn exec((): ()) -> Value {
        with_clipboard(|cb| {
            cb.clear()?;
            Ok(Value::Null)
        })
    }
}

pub(crate) type Clear = CachedArgsAsync<ClipboardBuiltin<ClearOp>>;

// ── Value conversion helpers ────────────────────────────────────────

pub(crate) fn image_to_value(img: arboard::ImageData<'_>) -> Value {
    use arcstr::literal;
    [
        (literal!("height"), Value::U32(img.height as u32)),
        (literal!("pixels"), Value::from(Bytes::from(img.bytes.into_owned()))),
        (literal!("width"), Value::U32(img.width as u32)),
    ]
    .into()
}

pub(crate) fn image_args_from_value(v: &Value) -> Option<ImageArgs> {
    let [(_, height), (_, pixels), (_, width)] =
        v.clone().cast_to::<[(ArcStr, Value); 3]>().ok()?;
    let width = width.cast_to::<u32>().ok()? as usize;
    let height = height.cast_to::<u32>().ok()? as usize;
    let pixels = match pixels {
        Value::Bytes(b) => Bytes::copy_from_slice(&b),
        _ => return None,
    };
    Some(ImageArgs { width, height, pixels })
}

pub(crate) fn html_args_from_value(v: &Value) -> Option<HtmlArgs> {
    let [(_, alt_text), (_, html)] = v.clone().cast_to::<[(ArcStr, Value); 2]>().ok()?;
    Some(HtmlArgs {
        html: html.cast_to::<ArcStr>().ok()?,
        alt_text: alt_text.cast_to::<ArcStr>().ok()?,
    })
}

pub(crate) fn files_to_value(files: Vec<PathBuf>) -> Value {
    use netidx::protocol::valarray::ValArray;
    Value::Array(ValArray::from_iter(
        files.iter().map(|p| Value::from(p.display().to_string())),
    ))
}

pub(crate) fn file_list_from_value(v: &Value) -> Option<Vec<String>> {
    match v {
        Value::Array(a) => {
            let mut paths = Vec::with_capacity(a.len());
            for item in a.iter() {
                match item {
                    Value::String(s) => paths.push(s.to_string()),
                    _ => return None,
                }
            }
            Some(paths)
        }
        _ => None,
    }
}
