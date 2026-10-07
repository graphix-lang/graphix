use ::bytes::{BufMut, Bytes, BytesMut};
use arcstr::ArcStr;
use graphix_compiler::{BindId, ExecCtx, FastCall, Rt, UserEvent, effects::Effect, errf};
use netidx_value::{PBytes, ValArray, Value};

use crate::{ByRefChain, CachedArgs, CachedVals, EvalCached, fast_eval, fast_get};

fn fc_bytes_to_string(args: &[Value]) -> Option<Value> {
    let b = fast_get::<Bytes>(args, 0)?;
    match std::str::from_utf8(&b) {
        Ok(s) => Some(Value::String(ArcStr::from(s))),
        Err(e) => Some(errf!("EncodingError", "invalid UTF-8: {e}")),
    }
}

#[derive(Debug, Default)]
pub(crate) struct BytesToStringEv;
crate::unit_image_state!(BytesToStringEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesToStringEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bytes_to_string)));
    const NAME: &str = "core_bytes_to_string";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_bytes_to_string, from)
    }
}

pub(crate) type BytesToString = CachedArgs<BytesToStringEv>;

fn fc_bytes_to_string_lossy(args: &[Value]) -> Option<Value> {
    let b = fast_get::<Bytes>(args, 0)?;
    Some(Value::String(ArcStr::from(&*String::from_utf8_lossy(&b))))
}

#[derive(Debug, Default)]
pub(crate) struct BytesToStringLossyEv;
crate::unit_image_state!(BytesToStringLossyEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesToStringLossyEv {
    const EFFECT: Effect =
        Effect::Stateless(Some(FastCall::Plain(fc_bytes_to_string_lossy)));
    const NAME: &str = "core_bytes_to_string_lossy";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_bytes_to_string_lossy, from)
    }
}

pub(crate) type BytesToStringLossy = CachedArgs<BytesToStringLossyEv>;

fn fc_bytes_from_string(args: &[Value]) -> Option<Value> {
    let s = fast_get::<ArcStr>(args, 0)?;
    Some(Value::Bytes(PBytes::new(Bytes::copy_from_slice(s.as_bytes()))))
}

#[derive(Debug, Default)]
pub(crate) struct BytesFromStringEv;
crate::unit_image_state!(BytesFromStringEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesFromStringEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bytes_from_string)));
    const NAME: &str = "core_bytes_from_string";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_bytes_from_string, from)
    }
}

pub(crate) type BytesFromString = CachedArgs<BytesFromStringEv>;

fn fc_bytes_concat(args: &[Value]) -> Option<Value> {
    let mut buf = BytesMut::new();
    for v in args {
        match v {
            Value::Bytes(b) => buf.extend_from_slice(b),
            Value::Array(a) => {
                for elem in a.iter() {
                    match elem {
                        Value::Bytes(b) => buf.extend_from_slice(b),
                        _ => return None,
                    }
                }
            }
            _ => return None,
        }
    }
    Some(Value::Bytes(PBytes::new(buf.freeze())))
}

#[derive(Debug, Default)]
pub(crate) struct BytesConcatEv;
crate::unit_image_state!(BytesConcatEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesConcatEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bytes_concat)));
    const NAME: &str = "core_bytes_concat";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_bytes_concat, from)
    }
}

pub(crate) type BytesConcat = CachedArgs<BytesConcatEv>;

fn fc_bytes_to_array(args: &[Value]) -> Option<Value> {
    let b = fast_get::<Bytes>(args, 0)?;
    Some(Value::Array(ValArray::from_iter_exact(b.iter().map(|byte| Value::U8(*byte)))))
}

#[derive(Debug, Default)]
pub(crate) struct BytesToArrayEv;
crate::unit_image_state!(BytesToArrayEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesToArrayEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bytes_to_array)));
    const NAME: &str = "core_bytes_to_array";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_bytes_to_array, from)
    }
}

pub(crate) type BytesToArray = CachedArgs<BytesToArrayEv>;

fn fc_bytes_from_array(args: &[Value]) -> Option<Value> {
    let arr = match &args[0] {
        Value::Array(a) => a,
        _ => return None,
    };
    let mut buf = BytesMut::with_capacity(arr.len());
    for v in arr.iter() {
        match v {
            Value::U8(b) => buf.put_u8(*b),
            _ => return None,
        }
    }
    Some(Value::Bytes(PBytes::new(buf.freeze())))
}

#[derive(Debug, Default)]
pub(crate) struct BytesFromArrayEv;
crate::unit_image_state!(BytesFromArrayEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesFromArrayEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bytes_from_array)));
    const NAME: &str = "core_bytes_from_array";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_bytes_from_array, from)
    }
}

pub(crate) type BytesFromArray = CachedArgs<BytesFromArrayEv>;

fn fc_bytes_len(args: &[Value]) -> Option<Value> {
    let b = fast_get::<Bytes>(args, 0)?;
    Some(Value::U64(b.len() as u64))
}

#[derive(Debug, Default)]
pub(crate) struct BytesLenEv;
crate::unit_image_state!(BytesLenEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesLenEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bytes_len)));
    const NAME: &str = "core_bytes_len";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_bytes_len, from)
    }
}

pub(crate) type BytesLen = CachedArgs<BytesLenEv>;

fn variant_tag(v: &Value) -> Option<(&ArcStr, &[Value])> {
    match v {
        Value::Array(a) if !a.is_empty() => match &a[0] {
            Value::String(tag) => Some((tag, &a[1..])),
            _ => None,
        },
        _ => None,
    }
}

/// Write one spec element; a payload of another shape than its tag
/// declares (a checker hole) is no value, never undefined behaviour.
fn encode_spec(buf: &mut BytesMut, v: &Value) -> Option<()> {
    let (tag, args) = variant_tag(v)?;
    match (&**tag, args.first()?) {
        ("I8", Value::I8(x)) => buf.put_i8(*x),
        ("U8", Value::U8(x)) => buf.put_u8(*x),
        ("I16", Value::I16(x)) => buf.put_i16(*x),
        ("I16LE", Value::I16(x)) => buf.put_i16_le(*x),
        ("U16", Value::U16(x)) => buf.put_u16(*x),
        ("U16LE", Value::U16(x)) => buf.put_u16_le(*x),
        ("I32", Value::I32(x)) => buf.put_i32(*x),
        ("I32LE", Value::I32(x)) => buf.put_i32_le(*x),
        ("U32", Value::U32(x)) => buf.put_u32(*x),
        ("U32LE", Value::U32(x)) => buf.put_u32_le(*x),
        ("I64", Value::I64(x)) => buf.put_i64(*x),
        ("I64LE", Value::I64(x)) => buf.put_i64_le(*x),
        ("U64", Value::U64(x)) => buf.put_u64(*x),
        ("U64LE", Value::U64(x)) => buf.put_u64_le(*x),
        ("F32", Value::F32(x)) => buf.put_f32(*x),
        ("F32LE", Value::F32(x)) => buf.put_f32_le(*x),
        ("F64", Value::F64(x)) => buf.put_f64(*x),
        ("F64LE", Value::F64(x)) => buf.put_f64_le(*x),
        ("Bytes", Value::Bytes(b)) => buf.put_slice(b),
        ("Pad", Value::U64(n)) => {
            // `put_bytes` panics on capacity overflow, so an absurd pad
            // logs and bottoms instead.
            const MAX_PAD: u64 = 64 * 1024 * 1024;
            if *n > MAX_PAD {
                log::error!(
                    "buffer::encode: Pad({n}) exceeds the {MAX_PAD} byte limit — \
                     producing no value"
                );
                return None;
            }
            buf.put_bytes(0, *n as usize)
        }
        ("Varint", Value::U64(n)) => netidx_core::pack::encode_varint(*n, buf),
        ("Zigzag", Value::I64(n)) => {
            netidx_core::pack::encode_varint(netidx_core::pack::i64_zz(*n), buf)
        }
        _ => return None,
    }
    Some(())
}

fn fc_encode(args: &[Value]) -> Option<Value> {
    let Value::Array(arr) = &args[0] else { return None };
    let mut buf = BytesMut::new();
    for v in arr.iter() {
        encode_spec(&mut buf, v)?;
    }
    Some(Value::Bytes(PBytes::new(buf.freeze())))
}

#[derive(Debug, Default)]
pub(crate) struct EncodeEv;
crate::unit_image_state!(EncodeEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for EncodeEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_encode)));
    const NAME: &str = "core_buffer_encode";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_encode, from)
    }
}

pub(crate) type BufferEncode = CachedArgs<EncodeEv>;

/// One decode pass: a cursor over the buffer and the writes it makes,
/// held until the whole spec decodes.
struct Decoder<'a> {
    buf: &'a Bytes,
    pos: usize,
    chain: &'a ByRefChain,
    written: poolshark::local::LPooled<Vec<(BindId, Value)>>,
}

fn decode_err(msg: impl std::fmt::Display) -> Value {
    errf!("DecodeError", "{msg}")
}

impl<'a> Decoder<'a> {
    /// The binding a spec's reference names.
    fn target(&self, r: &Value) -> Result<BindId, Value> {
        let Value::U64(id) = r else { return Err(decode_err("not a reference")) };
        self.chain
            .get(&BindId::from(*id))
            .copied()
            .ok_or_else(|| decode_err("ref does not point to a let binding"))
    }

    /// The length a reference holds: this pass's write first, else the
    /// store; `None` when it has not arrived.
    fn len<R: Rt, E: UserEvent>(
        &self,
        ctx: &ExecCtx<'_, R, E>,
        r: &Value,
    ) -> Result<Option<usize>, Value> {
        let target = self.target(r)?;
        let v = match self.written.iter().rev().find(|(t, _)| *t == target) {
            Some((_, v)) => Some(v.clone()),
            None => ctx.rt.store_value(&target),
        };
        match v {
            None => Ok(None),
            Some(Value::U64(n)) => Ok(Some(n as usize)),
            Some(v) => Err(decode_err(format_args!("a length of {v}"))),
        }
    }

    /// The next `n` bytes, the cursor past them.
    fn take(&mut self, n: usize) -> Result<&'a [u8], Value> {
        let buf: &'a Bytes = self.buf;
        let bytes = buf.get(self.pos..self.pos.saturating_add(n));
        let bytes = bytes.ok_or_else(|| decode_err("not enough bytes"))?;
        self.pos += n;
        Ok(bytes)
    }

    fn put(&mut self, r: &Value, v: Value) -> Result<(), Value> {
        let target = self.target(r)?;
        self.written.push((target, v));
        Ok(())
    }

    fn fixed<const N: usize>(
        &mut self,
        r: &Value,
        from: fn([u8; N]) -> Value,
    ) -> Result<(), Value> {
        let bytes: [u8; N] = self.take(N)?.try_into().expect("take returns N bytes");
        self.put(r, from(bytes))
    }

    fn varint(&mut self) -> Result<u64, Value> {
        let mut cursor = &self.buf[self.pos..];
        let v = netidx_core::pack::decode_varint(&mut cursor)
            .map_err(|e| decode_err(format_args!("varint: {e}")))?;
        self.pos = self.buf.len() - cursor.len();
        Ok(v)
    }

    /// Decode one spec element: `Ok(None)` when a length it needs has
    /// not arrived.
    fn element<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        tag: &str,
        a: &[Value],
    ) -> Result<Option<()>, Value> {
        let arg = |i: usize| a.get(i).ok_or_else(|| decode_err("missing argument"));
        match tag {
            "I8" => self.fixed(arg(0)?, |b: [u8; 1]| Value::I8(i8::from_le_bytes(b))),
            "U8" => self.fixed(arg(0)?, |b: [u8; 1]| Value::U8(b[0])),
            "I16" => self.fixed(arg(0)?, |b| Value::I16(i16::from_be_bytes(b))),
            "I16LE" => self.fixed(arg(0)?, |b| Value::I16(i16::from_le_bytes(b))),
            "U16" => self.fixed(arg(0)?, |b| Value::U16(u16::from_be_bytes(b))),
            "U16LE" => self.fixed(arg(0)?, |b| Value::U16(u16::from_le_bytes(b))),
            "I32" => self.fixed(arg(0)?, |b| Value::I32(i32::from_be_bytes(b))),
            "I32LE" => self.fixed(arg(0)?, |b| Value::I32(i32::from_le_bytes(b))),
            "U32" => self.fixed(arg(0)?, |b| Value::U32(u32::from_be_bytes(b))),
            "U32LE" => self.fixed(arg(0)?, |b| Value::U32(u32::from_le_bytes(b))),
            "I64" => self.fixed(arg(0)?, |b| Value::I64(i64::from_be_bytes(b))),
            "I64LE" => self.fixed(arg(0)?, |b| Value::I64(i64::from_le_bytes(b))),
            "U64" => self.fixed(arg(0)?, |b| Value::U64(u64::from_be_bytes(b))),
            "U64LE" => self.fixed(arg(0)?, |b| Value::U64(u64::from_le_bytes(b))),
            "F32" => self.fixed(arg(0)?, |b| Value::F32(f32::from_be_bytes(b))),
            "F32LE" => self.fixed(arg(0)?, |b| Value::F32(f32::from_le_bytes(b))),
            "F64" => self.fixed(arg(0)?, |b| Value::F64(f64::from_be_bytes(b))),
            "F64LE" => self.fixed(arg(0)?, |b| Value::F64(f64::from_le_bytes(b))),
            "Bytes" | "UTF8" | "Skip" => {
                let Some(n) = self.len(ctx, arg(0)?)? else { return Ok(None) };
                let at = self.pos;
                self.take(n)?;
                let bytes = self.buf.slice(at..at + n);
                match tag {
                    "Bytes" => self.put(arg(1)?, Value::Bytes(PBytes::new(bytes)))?,
                    "UTF8" => {
                        let s = std::str::from_utf8(&bytes).map_err(|e| {
                            decode_err(format_args!("invalid UTF-8: {e}"))
                        })?;
                        self.put(arg(1)?, Value::String(ArcStr::from(s)))?
                    }
                    _ => (),
                }
                Ok(())
            }
            "Varint" => {
                let v = self.varint()?;
                self.put(arg(0)?, Value::U64(v))
            }
            "Zigzag" => {
                let v = self.varint()?;
                self.put(arg(0)?, Value::I64(netidx_core::pack::i64_uzz(v)))
            }
            _ => return Err(decode_err(format_args!("unknown spec {tag}"))),
        }
        .map(Some)
    }
}

#[derive(Debug, Default)]
pub(crate) struct DecodeEv;
crate::unit_image_state!(DecodeEv);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DecodeEv {
    // CR claude for claude: [bug] core_buffer_decode is Stateless(None), but its eval
    // writes every target with set_var. An arm holding it is judged pure, so each
    // re-selection runs it as a birth with the standing buf and spec read FIRED. In an
    // arm that sleeps, CachedArgs re-runs a stateless eval at the wake. Either way
    // every re-entry rewrites the targets though nothing the call reads fired, a
    // phantom fire to every reader, while `*r <- v` in the same arm writes once.
    // Declaring it Sync alone would leave a stale remainder at a wake, and the writes
    // would still go out from CachedArgs' eval while the last result is bottom: with a
    // later length ref absent this rewrites the earlier fields every cycle, a busy loop
    // (one-liner in the probe header). The writes belong to a fired invocation only;
    // probe: design/review-2026-10-05/repro/x-builtin-effects-02.gx
    // (x-builtin-effects-02)
    // 2026-10-07 claude: the decode now commits its writes only when the whole spec
    // decodes (no rewrite of earlier fields while a later length is absent, the busy
    // loop), and reads payloads and references by shape. Writing on a fired
    // invocation only is the open part: Sync leaves a stale remainder at a wake and
    // Stateless re-writes at a re-entry; it needs the effect class the dbg/print pair
    // (x-builtin-effects-05, core-lib-09) does.
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_buffer_decode";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        let buf = from.get::<Bytes>(0)?;
        let Value::Array(spec) = from.0.get(1)?.as_ref()? else { return None };
        let chain = ctx.env.byref_chain.clone();
        let mut d =
            Decoder { buf: &buf, pos: 0, chain: &chain, written: Default::default() };
        for elem in spec.iter() {
            let (tag, args) = variant_tag(elem)?;
            match d.element(ctx, tag, args) {
                Ok(Some(())) => (),
                Ok(None) => return None,
                Err(e) => return Some(e),
            }
        }
        for (target, v) in d.written.drain(..) {
            ctx.rt.set_var(target, v);
        }
        Some(Value::Bytes(PBytes::new(buf.slice(d.pos..))))
    }
}

pub(crate) type BufferDecode = CachedArgs<DecodeEv>;
