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

crate::fast_builtin!(pub(crate) BytesToString, BytesToStringEv, "core_bytes_to_string", fc_bytes_to_string);

fn fc_bytes_to_string_lossy(args: &[Value]) -> Option<Value> {
    let b = fast_get::<Bytes>(args, 0)?;
    Some(Value::String(ArcStr::from(&*String::from_utf8_lossy(&b))))
}

// CR claude for claude: [structure/low] BytesToStringLossyEv and its BytesToStringLossy
// alias are a hand-written copy of what crate::fast_builtin! expands to, while every
// other builtin in this file uses the macro. A change to the fast-builtin shell would
// have to be repeated here by hand. Replace the struct, the unit_image_state!, the
// EvalCached impl and the alias with `crate::fast_builtin!(pub(crate)
// BytesToStringLossy, BytesToStringLossyEv, "core_bytes_to_string_lossy",
// fc_bytes_to_string_lossy);`. (core-aux-09)
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

crate::fast_builtin!(pub(crate) BytesFromString, BytesFromStringEv, "core_bytes_from_string", fc_bytes_from_string);

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

crate::fast_builtin!(pub(crate) BytesConcat, BytesConcatEv, "core_bytes_concat", fc_bytes_concat);

fn fc_bytes_to_array(args: &[Value]) -> Option<Value> {
    let b = fast_get::<Bytes>(args, 0)?;
    Some(Value::Array(ValArray::from_iter_exact(b.iter().map(|byte| Value::U8(*byte)))))
}

crate::fast_builtin!(pub(crate) BytesToArray, BytesToArrayEv, "core_bytes_to_array", fc_bytes_to_array);

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

crate::fast_builtin!(pub(crate) BytesFromArray, BytesFromArrayEv, "core_bytes_from_array", fc_bytes_from_array);

fn fc_bytes_len(args: &[Value]) -> Option<Value> {
    let b = fast_get::<Bytes>(args, 0)?;
    Some(Value::U64(b.len() as u64))
}

crate::fast_builtin!(pub(crate) BytesLen, BytesLenEv, "core_bytes_len", fc_bytes_len);

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

crate::fast_builtin!(pub(crate) BufferEncode, EncodeEv, "core_buffer_encode", fc_encode);

/// One decode pass: a cursor over the buffer and the writes it makes,
/// held until the whole spec decodes.
struct Decoder<'a> {
    buf: &'a Bytes,
    pos: usize,
    chain: &'a ByRefChain,
    /// each write's reference, the binding it names (none for a place),
    /// and the value
    written: poolshark::local::LPooled<Vec<(BindId, Option<BindId>, Value)>>,
}

fn decode_err(msg: impl std::fmt::Display) -> Value {
    errf!("DecodeError", "{msg}")
}

impl<'a> Decoder<'a> {
    // CR claude for claude: [bug/medium] buffer::decode refuses well-typed refs: &mut
    // over a value, &place and &literal as lengths. `Decoder::target` (buffer.rs)
    // resolves a length reference only through the by-ref chain to a let binding.
    // `Decoder::put` (buffer.rs) also accepts a place, but refuses anything else. The
    // `Decode` type takes `&mut T` / `&u64`, and CLAUDE.md (References) says `&mut e`
    // over a non-place expression is a fresh writable cell its uses type, and that
    // `&s.f` is a place. So programs the checker accepts fail at run time with a
    // DecodeError. Four cases fail: (1) `` `U8(&mut u8:0) ``, a fresh cell that `*r <-
    // v` writes correctly; (2) `` `Bytes(&hdr.n, ..) `` with the length in a struct
    // field, the ordinary length-prefixed header shape; (3) `` `Skip(&u64:0) ``; (4) ``
    // `U64(fresh), `Bytes(fresh, ..) `` with `let fresh = &mut u64:0`. A place can be
    // written as a target, but its write cannot be read back as a length later in the
    // same pass: `len()` only matches written entries whose target is `Some(binding)`.
    // The test `decode_ref_to_literal` (stdlib/graphix-tests/src/lib_tests/buffer.rs)
    // pins the refusal as intended ('A ref to a literal in a decode spec is a runtime
    // decode error'), and that contradicts the reference semantics. The gxi doc ('Each
    // decoded field is written through its &mut') states no such restriction. Failure:
    // `let hdr = {n: u64:0, tag: u8:0}; let d: bytes = never(); buffer::decode(encoded,
    // [`U64(&mut hdr.n), `Bytes(&hdr.n, &mut d)])` type-checks and returns
    // error:["DecodeError", "ref does not point to a let binding"] in both engines.
    // `let r = &mut u8:0; buffer::decode(enc, [`U8(r)])` returns DecodeError, while `*r
    // <- u8:5` on the same reference works. Fix: Resolve every spec reference the way
    // `==` does (`bind::ref_target`: the place, else the by-ref chain's binding, else
    // the cell itself). Key `written` by that resolved target, so a place or a fresh
    // cell can be both written and read back as a length within one pass. Then change
    // `decode_ref_to_literal` to assert the decode succeeds and that `*r` reads the
    // decoded value. Add a pin for a length held in a struct field. Unverified: the
    // reviewer's report, not checked by a verifier. (tests-lib-b1-01)
    /// The binding a spec's reference names.
    // CR claude for claude: [bug/low] Decoder::target resolves a length reference only
    // through the byref chain of let bindings, so a place reference (`&s.n`, `&a[i]`)
    // used as a `Bytes` or `Skip` length is refused with "ref does not point to a let
    // binding". The Decode type admits any `&u64`, and Decoder::put accepts the same
    // place, so `U64(&mut s.n)` followed by `Bytes(&s.n, ..)` fails while the
    // let-binding version works. Decoder::len also cannot find a length written earlier
    // in the same pass, because a place write records `None` as its target. Fix: key
    // both the written records and the length lookups by crate::write_target (a binding
    // or a root plus path). Read a place length from this pass's writes first, else
    // from the root's store value through place::read_path. probe:
    // design/review-2026-10-12/repro/core-aux-04.gx (core-aux-07)
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
        let v = match self.written.iter().rev().find(|(_, t, _)| *t == Some(target)) {
            Some((_, _, v)) => Some(v.clone()),
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

    /// A write through `r`, to a binding or a place (`&s.f`, `&a[i]`).
    fn put<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        r: &Value,
        v: Value,
    ) -> Result<(), Value> {
        let Value::U64(id) = r else { return Err(decode_err("not a reference")) };
        let r = BindId::from(*id);
        let target = self.chain.get(&r).copied();
        if target.is_none() && ctx.rt.ref_path(&r).is_none() {
            return Err(decode_err("ref does not point to a let binding or a place"));
        }
        self.written.push((r, target, v));
        Ok(())
    }

    fn fixed<const N: usize, R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        r: &Value,
        from: fn([u8; N]) -> Value,
    ) -> Result<(), Value> {
        let bytes: [u8; N] = self.take(N)?.try_into().expect("take returns N bytes");
        self.put(ctx, r, from(bytes))
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
            "I8" => {
                self.fixed(ctx, arg(0)?, |b: [u8; 1]| Value::I8(i8::from_le_bytes(b)))
            }
            "U8" => self.fixed(ctx, arg(0)?, |b: [u8; 1]| Value::U8(b[0])),
            "I16" => self.fixed(ctx, arg(0)?, |b| Value::I16(i16::from_be_bytes(b))),
            "I16LE" => self.fixed(ctx, arg(0)?, |b| Value::I16(i16::from_le_bytes(b))),
            "U16" => self.fixed(ctx, arg(0)?, |b| Value::U16(u16::from_be_bytes(b))),
            "U16LE" => self.fixed(ctx, arg(0)?, |b| Value::U16(u16::from_le_bytes(b))),
            "I32" => self.fixed(ctx, arg(0)?, |b| Value::I32(i32::from_be_bytes(b))),
            "I32LE" => self.fixed(ctx, arg(0)?, |b| Value::I32(i32::from_le_bytes(b))),
            "U32" => self.fixed(ctx, arg(0)?, |b| Value::U32(u32::from_be_bytes(b))),
            "U32LE" => self.fixed(ctx, arg(0)?, |b| Value::U32(u32::from_le_bytes(b))),
            "I64" => self.fixed(ctx, arg(0)?, |b| Value::I64(i64::from_be_bytes(b))),
            "I64LE" => self.fixed(ctx, arg(0)?, |b| Value::I64(i64::from_le_bytes(b))),
            "U64" => self.fixed(ctx, arg(0)?, |b| Value::U64(u64::from_be_bytes(b))),
            "U64LE" => self.fixed(ctx, arg(0)?, |b| Value::U64(u64::from_le_bytes(b))),
            "F32" => self.fixed(ctx, arg(0)?, |b| Value::F32(f32::from_be_bytes(b))),
            "F32LE" => self.fixed(ctx, arg(0)?, |b| Value::F32(f32::from_le_bytes(b))),
            "F64" => self.fixed(ctx, arg(0)?, |b| Value::F64(f64::from_be_bytes(b))),
            "F64LE" => self.fixed(ctx, arg(0)?, |b| Value::F64(f64::from_le_bytes(b))),
            "Bytes" | "UTF8" | "Skip" => {
                let Some(n) = self.len(ctx, arg(0)?)? else { return Ok(None) };
                let at = self.pos;
                self.take(n)?;
                let bytes = self.buf.slice(at..at + n);
                match tag {
                    "Bytes" => {
                        self.put(ctx, arg(1)?, Value::Bytes(PBytes::new(bytes)))?
                    }
                    "UTF8" => {
                        let s = std::str::from_utf8(&bytes).map_err(|e| {
                            decode_err(format_args!("invalid UTF-8: {e}"))
                        })?;
                        self.put(ctx, arg(1)?, Value::String(ArcStr::from(s)))?
                    }
                    _ => (),
                }
                Ok(())
            }
            "Varint" => {
                let v = self.varint()?;
                self.put(ctx, arg(0)?, Value::U64(v))
            }
            "Zigzag" => {
                let v = self.varint()?;
                self.put(ctx, arg(0)?, Value::I64(netidx_core::pack::i64_uzz(v)))
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
    const EFFECT: Effect = Effect::Io(None);
    const NAME: &str = "core_buffer_decode";

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        decode(ctx, from, true)
    }

    fn quiet(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        decode(ctx, from, false)
    }
}

/// Decode `from`'s buffer by its spec, and when `commit`, write what the
/// whole spec decoded to its references.
fn decode<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    from: &CachedVals,
    commit: bool,
) -> Option<Value> {
    let buf = from.get::<Bytes>(0)?;
    let Value::Array(spec) = from.0.get(1)?.as_ref()? else { return None };
    let chain = ctx.env.byref_chain.clone();
    let mut d = Decoder { buf: &buf, pos: 0, chain: &chain, written: Default::default() };
    for elem in spec.iter() {
        let (tag, args) = variant_tag(elem)?;
        match d.element(ctx, tag, args) {
            Ok(Some(())) => (),
            Ok(None) => return None,
            Err(e) => return Some(e),
        }
    }
    if commit {
        for (r, _, v) in d.written.drain(..) {
            // a place reference fires with its root, so a place already
            // holding the value is left alone, or decode would feed itself
            let unchanged = ctx.rt.ref_path(&r).is_some_and(|(root, path)| {
                ctx.rt.store_value(root).is_some_and(|rv| {
                    graphix_compiler::node::place::read_path(&rv, path)
                        .is_ok_and(|c| c == v)
                })
            });
            if !unchanged {
                crate::write_through(ctx, r, v);
            }
        }
    }
    Some(Value::Bytes(PBytes::new(buf.slice(d.pos..))))
}

pub(crate) type BufferDecode = CachedArgs<DecodeEv>;
