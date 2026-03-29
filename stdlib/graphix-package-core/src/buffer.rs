use ::bytes::{BufMut, Bytes, BytesMut};
use arcstr::ArcStr;
use graphix_compiler::{errf, BindId, ExecCtx, Rt, UserEvent};
use netidx_value::{PBytes, ValArray, Value};

use crate::{ByRefChain, CachedArgs, CachedVals, EvalCached};

#[derive(Debug, Default)]
pub(crate) struct BytesToStringEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesToStringEv {
    const NAME: &str = "core_bytes_to_string";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let b = from.get::<Bytes>(0)?;
        match String::from_utf8(b.into()) {
            Ok(s) => Some(Value::String(ArcStr::from(&s))),
            Err(e) => Some(errf!("EncodingError", "invalid UTF-8: {e}")),
        }
    }
}

pub(crate) type BytesToString = CachedArgs<BytesToStringEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesToStringLossyEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesToStringLossyEv {
    const NAME: &str = "core_bytes_to_string_lossy";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let b = from.get::<Bytes>(0)?;
        let s = String::from_utf8_lossy(&b).into_owned();
        Some(Value::String(ArcStr::from(&s)))
    }
}

pub(crate) type BytesToStringLossy = CachedArgs<BytesToStringLossyEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesFromStringEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesFromStringEv {
    const NAME: &str = "core_bytes_from_string";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let s = from.get::<ArcStr>(0)?;
        Some(Value::Bytes(PBytes::new(Bytes::copy_from_slice(s.as_bytes()))))
    }
}

pub(crate) type BytesFromString = CachedArgs<BytesFromStringEv>;

#[derive(Debug)]
pub(crate) struct BytesConcatEv {
    buf: BytesMut,
}

impl Default for BytesConcatEv {
    fn default() -> Self {
        Self { buf: BytesMut::new() }
    }
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesConcatEv {
    const NAME: &str = "core_bytes_concat";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        self.buf.clear();
        for v in from.0.iter() {
            match v {
                None => return None,
                Some(Value::Bytes(b)) => self.buf.extend_from_slice(b),
                Some(Value::Array(a)) => {
                    for elem in a.iter() {
                        match elem {
                            Value::Bytes(b) => self.buf.extend_from_slice(b),
                            _ => return None,
                        }
                    }
                }
                _ => return None,
            }
        }
        Some(Value::Bytes(PBytes::new(self.buf.split().freeze())))
    }
}

pub(crate) type BytesConcat = CachedArgs<BytesConcatEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesToArrayEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesToArrayEv {
    const NAME: &str = "core_bytes_to_array";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let b = from.get::<Bytes>(0)?;
        Some(Value::Array(ValArray::from_iter_exact(
            b.iter().map(|byte| Value::U8(*byte)),
        )))
    }
}

pub(crate) type BytesToArray = CachedArgs<BytesToArrayEv>;

#[derive(Debug)]
pub(crate) struct BytesFromArrayEv {
    buf: BytesMut,
}

impl Default for BytesFromArrayEv {
    fn default() -> Self {
        Self { buf: BytesMut::new() }
    }
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesFromArrayEv {
    const NAME: &str = "core_bytes_from_array";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let arr = match from.0.first()?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        self.buf.clear();
        self.buf.reserve(arr.len());
        for v in arr.iter() {
            match v {
                Value::U8(b) => self.buf.extend_from_slice(&[*b]),
                _ => return None,
            }
        }
        Some(Value::Bytes(PBytes::new(self.buf.split().freeze())))
    }
}

pub(crate) type BytesFromArray = CachedArgs<BytesFromArrayEv>;

#[derive(Debug, Default)]
pub(crate) struct BytesLenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BytesLenEv {
    const NAME: &str = "core_bytes_len";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let b = from.get::<Bytes>(0)?;
        Some(Value::U64(b.len() as u64))
    }
}

pub(crate) type BytesLen = CachedArgs<BytesLenEv>;

// ── Encode ────────────────────────────────────────────────────────

fn variant_tag(v: &Value) -> Option<(&ArcStr, &[Value])> {
    match v {
        Value::Array(a) if !a.is_empty() => match &a[0] {
            Value::String(tag) => Some((tag, &a[1..])),
            _ => None,
        },
        _ => None,
    }
}

/// # Safety: the type checker proves the variant payloads, so
/// get_as_unchecked is sound here.
fn encode_spec(buf: &mut BytesMut, v: &Value) -> Option<()> {
    let (tag, args) = variant_tag(v)?;
    let a = &args[0];
    // SAFETY: the graphix type checker guarantees each variant tag
    // carries the declared payload type.
    unsafe {
        match &**tag {
            "I8" => buf.put_i8(*a.get_as_unchecked::<i8>()),
            "U8" => buf.put_u8(*a.get_as_unchecked::<u8>()),
            "I16" => buf.put_i16(*a.get_as_unchecked::<i16>()),
            "I16LE" => buf.put_i16_le(*a.get_as_unchecked::<i16>()),
            "U16" => buf.put_u16(*a.get_as_unchecked::<u16>()),
            "U16LE" => buf.put_u16_le(*a.get_as_unchecked::<u16>()),
            "I32" => buf.put_i32(*a.get_as_unchecked::<i32>()),
            "I32LE" => buf.put_i32_le(*a.get_as_unchecked::<i32>()),
            "U32" => buf.put_u32(*a.get_as_unchecked::<u32>()),
            "U32LE" => buf.put_u32_le(*a.get_as_unchecked::<u32>()),
            "I64" => buf.put_i64(*a.get_as_unchecked::<i64>()),
            "I64LE" => buf.put_i64_le(*a.get_as_unchecked::<i64>()),
            "U64" => buf.put_u64(*a.get_as_unchecked::<u64>()),
            "U64LE" => buf.put_u64_le(*a.get_as_unchecked::<u64>()),
            "F32" => buf.put_f32(*a.get_as_unchecked::<f32>()),
            "F32LE" => buf.put_f32_le(*a.get_as_unchecked::<f32>()),
            "F64" => buf.put_f64(*a.get_as_unchecked::<f64>()),
            "F64LE" => buf.put_f64_le(*a.get_as_unchecked::<f64>()),
            "Bytes" => buf.put_slice(a.get_as_unchecked::<PBytes>()),
            "Pad" => buf.put_bytes(0, *a.get_as_unchecked::<u64>() as usize),
            "Varint" => {
                netidx_core::pack::encode_varint(*a.get_as_unchecked::<u64>(), buf);
            }
            "Zigzag" => {
                let val = *a.get_as_unchecked::<i64>();
                netidx_core::pack::encode_varint(netidx_core::pack::i64_zz(val), buf);
            }
            _ => return None,
        }
    }
    Some(())
}

#[derive(Debug)]
pub(crate) struct EncodeEv {
    buf: BytesMut,
}

impl Default for EncodeEv {
    fn default() -> Self {
        Self { buf: BytesMut::new() }
    }
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for EncodeEv {
    const NAME: &str = "core_buffer_encode";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let arr = match from.0.first()?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        self.buf.clear();
        for v in arr.iter() {
            encode_spec(&mut self.buf, v)?;
        }
        Some(Value::Bytes(PBytes::new(self.buf.split().freeze())))
    }
}

pub(crate) type BufferEncode = CachedArgs<EncodeEv>;

// ── Decode ────────────────────────────────────────────────────────

/// # Safety: the type checker guarantees refs are represented as U64.
fn get_bind_id(v: &Value) -> BindId {
    BindId::from(*unsafe { v.get_as_unchecked::<u64>() })
}

fn decode_err(msg: &str) -> Value {
    errf!("DecodeError", "{msg}")
}

fn resolve_ref(byref_chain: &ByRefChain, ref_id: BindId) -> Result<BindId, Value> {
    byref_chain
        .get(&ref_id)
        .copied()
        .ok_or_else(|| decode_err("ref does not point to a let binding"))
}

/// Resolve a ref BindId through the byref chain, returning the target
/// variable's current u64 value from `ctx.cached`. Returns `Err` with a
/// decode error if the ref isn't in the byref chain, `Ok(None)` if the
/// value hasn't arrived yet (bottom).
fn resolve_u64<R: Rt, E: UserEvent>(
    ctx: &ExecCtx<R, E>,
    byref_chain: &ByRefChain,
    ref_id: BindId,
) -> Result<Option<u64>, Value> {
    let target = resolve_ref(byref_chain, ref_id)?;
    Ok(ctx.cached.get(&target).map(|v| *unsafe { v.get_as_unchecked::<u64>() }))
}

macro_rules! decode_fixed {
    ($ctx:expr, $buf:expr, $pos:expr, $args:expr,
     $byref_chain:expr, $sz:expr, $ty:ty, $from_bytes:ident, $variant:ident) => {{
        if $buf.len() - $pos < $sz {
            return Some(decode_err("not enough bytes"));
        }
        // SAFETY: we checked buf.len() - pos >= $sz above, and $sz always
        // matches the byte width of $ty.
        let val =
            <$ty>::$from_bytes(unsafe { *($buf[$pos..].as_ptr() as *const [u8; $sz]) });
        let ref_id = get_bind_id(&$args[0]);
        let target = match resolve_ref(&$byref_chain, ref_id) {
            Ok(t) => t,
            Err(e) => return Some(e),
        };
        $ctx.set_var(target, Value::$variant(val));
        $pos += $sz;
    }};
}

#[derive(Debug, Default)]
pub(crate) struct DecodeEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DecodeEv {
    const NAME: &str = "core_buffer_decode";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let buf = from.get::<Bytes>(0)?;
        let spec = match from.0.get(1)?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        let byref_chain = ctx.env.byref_chain.clone();
        let mut pos = 0usize;

        for elem in spec.iter() {
            let (tag, args) = variant_tag(elem)?;
            match &**tag {
                "I8" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    1,
                    i8,
                    from_le_bytes,
                    I8
                ),
                "U8" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    1,
                    u8,
                    from_le_bytes,
                    U8
                ),
                "I16" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    2,
                    i16,
                    from_be_bytes,
                    I16
                ),
                "I16LE" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    2,
                    i16,
                    from_le_bytes,
                    I16
                ),
                "U16" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    2,
                    u16,
                    from_be_bytes,
                    U16
                ),
                "U16LE" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    2,
                    u16,
                    from_le_bytes,
                    U16
                ),
                "I32" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    4,
                    i32,
                    from_be_bytes,
                    I32
                ),
                "I32LE" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    4,
                    i32,
                    from_le_bytes,
                    I32
                ),
                "U32" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    4,
                    u32,
                    from_be_bytes,
                    U32
                ),
                "U32LE" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    4,
                    u32,
                    from_le_bytes,
                    U32
                ),
                "I64" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    8,
                    i64,
                    from_be_bytes,
                    I64
                ),
                "I64LE" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    8,
                    i64,
                    from_le_bytes,
                    I64
                ),
                "U64" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    8,
                    u64,
                    from_be_bytes,
                    U64
                ),
                "U64LE" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    8,
                    u64,
                    from_le_bytes,
                    U64
                ),
                "F32" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    4,
                    f32,
                    from_be_bytes,
                    F32
                ),
                "F32LE" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    4,
                    f32,
                    from_le_bytes,
                    F32
                ),
                "F64" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    8,
                    f64,
                    from_be_bytes,
                    F64
                ),
                "F64LE" => decode_fixed!(
                    ctx,
                    buf,
                    pos,
                    args,
                    byref_chain,
                    8,
                    f64,
                    from_le_bytes,
                    F64
                ),
                "Bytes" => {
                    let len_ref_id = get_bind_id(&args[0]);
                    let n = match resolve_u64(ctx, &byref_chain, len_ref_id) {
                        Ok(Some(n)) => n as usize,
                        Ok(None) => return None,
                        Err(e) => return Some(e),
                    };
                    if buf.len() - pos < n {
                        return Some(decode_err("not enough bytes"));
                    }
                    let dest_ref_id = get_bind_id(&args[1]);
                    let target = match resolve_ref(&byref_chain, dest_ref_id) {
                        Ok(t) => t,
                        Err(e) => return Some(e),
                    };
                    ctx.set_var(
                        target,
                        Value::Bytes(PBytes::new(buf.slice(pos..pos + n))),
                    );
                    pos += n;
                }
                "UTF8" => {
                    let len_ref_id = get_bind_id(&args[0]);
                    let n = match resolve_u64(ctx, &byref_chain, len_ref_id) {
                        Ok(Some(n)) => n as usize,
                        Ok(None) => return None,
                        Err(e) => return Some(e),
                    };
                    if buf.len() - pos < n {
                        return Some(decode_err("not enough bytes"));
                    }
                    let s = match std::str::from_utf8(&buf[pos..pos + n]) {
                        Ok(s) => s,
                        Err(e) => {
                            return Some(decode_err(&format!("invalid UTF-8: {e}")));
                        }
                    };
                    let dest_ref_id = get_bind_id(&args[1]);
                    let target = match resolve_ref(&byref_chain, dest_ref_id) {
                        Ok(t) => t,
                        Err(e) => return Some(e),
                    };
                    ctx.set_var(target, Value::String(ArcStr::from(s)));
                    pos += n;
                }
                "Skip" => {
                    let len_ref_id = get_bind_id(&args[0]);
                    let n = match resolve_u64(ctx, &byref_chain, len_ref_id) {
                        Ok(Some(n)) => n as usize,
                        Ok(None) => return None,
                        Err(e) => return Some(e),
                    };
                    if buf.len() - pos < n {
                        return Some(decode_err("not enough bytes"));
                    }
                    pos += n;
                }
                "Varint" => {
                    let mut cursor = &buf[pos..];
                    let val = match netidx_core::pack::decode_varint(&mut cursor) {
                        Ok(v) => v,
                        Err(e) => return Some(decode_err(&format!("varint: {e}"))),
                    };
                    pos += buf.len() - pos - cursor.len();
                    let ref_id = get_bind_id(&args[0]);
                    let target = match resolve_ref(&byref_chain, ref_id) {
                        Ok(t) => t,
                        Err(e) => return Some(e),
                    };
                    ctx.set_var(target, Value::U64(val));
                }
                "Zigzag" => {
                    let mut cursor = &buf[pos..];
                    let raw = match netidx_core::pack::decode_varint(&mut cursor) {
                        Ok(v) => v,
                        Err(e) => return Some(decode_err(&format!("zigzag: {e}"))),
                    };
                    pos += buf.len() - pos - cursor.len();
                    let ref_id = get_bind_id(&args[0]);
                    let target = match resolve_ref(&byref_chain, ref_id) {
                        Ok(t) => t,
                        Err(e) => return Some(e),
                    };
                    ctx.set_var(target, Value::I64(netidx_core::pack::i64_uzz(raw)));
                }
                _ => return None,
            }
        }

        let rest = buf.slice(pos..);
        Some(Value::Bytes(PBytes::new(rest)))
    }
}

pub(crate) type BufferDecode = CachedArgs<DecodeEv>;
