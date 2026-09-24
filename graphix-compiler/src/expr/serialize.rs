//! Binary serialization of the module AST, so a package deserializes its
//! AST at load instead of re-parsing its `.gx` source.
//!
//! The codec is netidx [`Pack`]; most AST types derive it. This module
//! holds the hand-written impls (`Expr`, `AbstractId`, `TVar`, `FnType`),
//! the per-module decode setup and the `pack_module`/`unpack_module`
//! (and `_sig`) entry points.
//!
//! There is no version field and no parse fallback: the same compiler
//! build writes and reads a blob, so a decode error is an internal bug.

use crate::{
    SourcePosition,
    expr::{
        Decorations, Expr, ExprId, ExprKind, Origin, OriginScope, Sig, VfsEntry,
        get_origin,
    },
    image,
    node::NOP,
    profile::{self, Phase},
};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut, Bytes, BytesMut};
use netidx_core::{
    pack::{self, Pack, PackError},
    path::Path,
};
use poolshark::local::LPooled;
use triomphe::Arc;

/// Magic header on every packed blob.
const MAGIC: &[u8; 4] = b"GXAS";

/// Under an image session the id and origin travel with the
/// expression; the syntax codec mints a fresh id and takes the unit's
/// origin.
impl Expr {
    fn syntax_len(&self) -> usize {
        <i32 as Pack>::encoded_len(&self.pos.line)
            + <i32 as Pack>::encoded_len(&self.pos.column)
            + self.kind.encoded_len()
            + self.dec.encoded_len()
    }

    fn syntax_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        <i32 as Pack>::encode(&self.pos.line, buf)?;
        <i32 as Pack>::encode(&self.pos.column, buf)?;
        self.kind.encode(buf)?;
        self.dec.encode(buf)
    }

    fn syntax_decode(
        buf: &mut impl Buf,
        id: ExprId,
        ori: Arc<Origin>,
    ) -> Result<Self, PackError> {
        let line = <i32 as Pack>::decode(buf)?;
        let column = <i32 as Pack>::decode(buf)?;
        let kind = <ExprKind as Pack>::decode(buf)?;
        let dec = <Option<Arc<Decorations>> as Pack>::decode(buf)?;
        Ok(Expr {
            id,
            ori,
            pos: SourcePosition { line, column },
            kind,
            dec,
            str_form: Default::default(),
            end: Default::default(),
        })
    }
}

/// Under an image session an expression is an object carrying its id
/// and origin, written once and referenced afterwards (a node's spec
/// and a definition's body are clones of subtrees of one tree); the
/// syntax codec mints a fresh id and takes the unit's origin. The
/// process-static [`NOP`] is a flag alone and decodes to the reader's:
/// its id, minted at the process's first compile, would stretch the
/// image's id span back to it.
impl Pack for Expr {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() {
            image::object_len(
                &image::expr_key(self),
                |k| (*k, ()),
                |e| &mut e.exprs,
                || match self.id == NOP.id {
                    true => 1,
                    false => {
                        1 + self.id.encoded_len()
                            + image::origin_len(&self.ori)
                            + self.syntax_len()
                    }
                },
            )
        } else {
            self.syntax_len()
        }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            image::object_encode(
                &image::expr_key(self),
                |k| (*k, ()),
                |e| &mut e.exprs,
                buf,
                |buf| {
                    let nop = self.id == NOP.id;
                    nop.encode(buf)?;
                    if nop {
                        return Ok(());
                    }
                    self.id.encode(buf)?;
                    image::origin_encode(&self.ori, buf)?;
                    self.syntax_encode(buf)
                },
            )
        } else {
            self.syntax_encode(buf)
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if image::is_decoding() {
            image::object_decode(
                buf,
                |d| &mut d.exprs,
                |buf| {
                    if bool::decode(buf)? {
                        return Ok((**NOP).clone());
                    }
                    let id = ExprId::decode(buf)?;
                    let ori = image::origin_decode(buf)?;
                    Self::syntax_decode(buf, id, ori)
                },
                |b| Self::decode(b),
            )
        } else {
            Self::syntax_decode(buf, ExprId::new(), get_origin())
        }
    }
}

fn codec_error(e: PackError) -> anyhow::Error {
    anyhow::anyhow!("packed AST codec error: {e:?}")
}

fn check_magic(bytes: &mut &[u8]) -> Result<()> {
    if bytes.len() < MAGIC.len() || &bytes[..MAGIC.len()] != MAGIC {
        bail!("packed AST: bad magic header");
    }
    bytes.advance(MAGIC.len());
    Ok(())
}

/// Serialize a module's top-level expressions to a packed blob.
pub fn pack_module(exprs: &[Expr]) -> Result<Bytes> {
    let mut buf = BytesMut::new();
    buf.put_slice(MAGIC);
    pack::encode_varint(exprs.len() as u64, &mut buf);
    for e in exprs {
        e.encode(&mut buf).map_err(codec_error)?;
    }
    Ok(buf.freeze())
}

/// Deserialize a module's top-level expressions from a packed blob, setting
/// every node's `ori` to `ori` and minting fresh ids. `bytes` must come from
/// [`pack_module`] in the same compiler build.
pub fn unpack_module(mut bytes: &[u8], ori: Arc<Origin>) -> Result<Arc<[Expr]>> {
    let _profile = profile::phase(Phase::Decode);
    check_magic(&mut bytes)?;
    let _unit = OriginScope::enter(ori);
    let n = pack::decode_varint(&mut bytes).map_err(codec_error)? as usize;
    let mut v: LPooled<Vec<Expr>> = LPooled::take();
    for _ in 0..n {
        v.push(Expr::decode(&mut bytes).map_err(codec_error)?);
    }
    Ok(Arc::from_iter(v.drain(..)))
}

/// Serialize a module interface (`.gxi`) signature to a packed blob.
pub fn pack_sig(sig: &Sig) -> Result<Bytes> {
    let mut buf = BytesMut::new();
    buf.put_slice(MAGIC);
    sig.encode(&mut buf).map_err(codec_error)?;
    Ok(buf.freeze())
}

/// Deserialize a module interface signature from a packed blob.
pub fn unpack_sig(mut bytes: &[u8], ori: Arc<Origin>) -> Result<Sig> {
    let _profile = profile::phase(Phase::Decode);
    check_magic(&mut bytes)?;
    let _unit = OriginScope::enter(ori);
    Sig::decode(&mut bytes).map_err(codec_error)
}

/// Serialize a whole package's modules as one blob: a list of
/// `(vfs_path_key, source, packed_module_ast)`, each `ast` a
/// `pack_module`/`pack_sig` blob decoded lazily at module resolution.
pub fn pack_index(entries: &[(ArcStr, ArcStr, Bytes)]) -> Result<Bytes> {
    let mut buf = BytesMut::new();
    buf.put_slice(MAGIC);
    pack::encode_varint(entries.len() as u64, &mut buf);
    for (path, source, ast) in entries {
        path.encode(&mut buf).map_err(codec_error)?;
        source.encode(&mut buf).map_err(codec_error)?;
        pack::encode_varint(ast.len() as u64, &mut buf);
        buf.put_slice(ast);
    }
    Ok(buf.freeze())
}

/// Decode a package index blob (see [`pack_index`]) into `(Path, VfsEntry)`
/// pairs; each entry's AST stays packed in `VfsEntry.packed`.
/// The blob is a compiled-in static, so each AST is a view into it.
pub fn unpack_index(mut bytes: &'static [u8]) -> Result<Vec<(Path, VfsEntry)>> {
    check_magic(&mut bytes)?;
    let n = pack::decode_varint(&mut bytes).map_err(codec_error)? as usize;
    let mut result = Vec::with_capacity(n);
    for _ in 0..n {
        let path = ArcStr::decode(&mut bytes).map_err(codec_error)?;
        let source = ArcStr::decode(&mut bytes).map_err(codec_error)?;
        let ast_len = pack::decode_varint(&mut bytes).map_err(codec_error)? as usize;
        if bytes.len() < ast_len {
            bail!("packed index: truncated module AST");
        }
        let ast = Bytes::from_static(&bytes[..ast_len]);
        bytes.advance(ast_len);
        result.push((Path::from(path), VfsEntry { source, packed: Some(ast) }));
    }
    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        expr::{Source, parser},
        typ::Type,
    };

    /// `src` parsed, and its package round trip.
    fn packed(src: &str) -> (Arc<[Expr]>, Arc<[Expr]>) {
        let ori = Origin {
            parent: None,
            source: Source::Internal(ArcStr::from("test")),
            text: ArcStr::from(src),
        };
        let exprs = parser::parse(ori).unwrap_or_else(|e| panic!("parse `{src}`: {e}"));
        let packed = pack_module(&exprs).expect("pack");
        let dummy = Arc::new(Origin {
            parent: None,
            source: Source::Internal(ArcStr::from("decoded")),
            text: ArcStr::new(),
        });
        (exprs, unpack_module(&packed, dummy).expect("unpack"))
    }

    fn rt(src: &str) {
        let (exprs, unpacked) = packed(src);
        // `Expr` equality is kind-only, so this checks structure.
        assert_eq!(&exprs[..], &unpacked[..], "round-trip mismatch for: {src}");
    }

    /// A fn type's declared quantifiers, in source order, and those of the
    /// fn types inside it (none).
    #[test]
    fn quantifiers_cross_the_wire() {
        for src in [
            "let f: fn<'a: Number>(g: fn(x: 'a) -> 'a) -> 'a = h",
            "let f: fn<'b: Int, 'a: Number>(x: 'a, y: 'b) -> 'a = h",
        ] {
            let (exprs, unpacked) = packed(src);
            assert_eq!(exprs[0].to_string(), unpacked[0].to_string(), "{src}");
            let quantifiers = |e: &Expr| match &e.kind {
                ExprKind::Bind(b) => match &b.typ {
                    Some(Type::Fn(ft)) => ft.quantifiers.clone(),
                    t => panic!("{t:?}"),
                },
                k => panic!("{k:?}"),
            };
            assert_eq!(quantifiers(&exprs[0]), quantifiers(&unpacked[0]), "{src}");
        }
    }

    /// A quantifier its own constraint names packs without recursing
    /// forever.
    #[test]
    fn a_self_constrained_quantifier_packs() {
        rt("let f: fn<'a: [i64, Array<'a>]>(x: 'a) -> 'a = g");
    }

    #[test]
    fn roundtrip_surface() {
        rt("42");
        rt("f64:3.14");
        rt("\"hello [name]!\"");
        rt("[1, 2, 3]");
        rt("(1, \"two\", 3.0)");
        rt("{x: 10, y: 20}");
        rt("`Foo(42, \"x\")");
        rt("{ let x = i64:3; let y = x * x; -(y + 1) }");
        rt("|x: i64, y: f64| -> i64 x");
        rt("'a: Number |x: 'a, y: 'a| -> 'a x + y");
        rt("|#greeting = \"hi\", name| \"[greeting], [name]!\"");
        rt("select x { i64 as n => n + 1, string as s => 0, _ => 0 }");
        rt("select pair { (0, y) => y, (x, 0) => x, (x, y) => x + y }");
        rt("select arr { [x, rest..] => x, [] => 0 }");
        rt("a[1..3]");
        rt("m{\"key\"}");
        rt("let f = |x| x + 1");
        rt("// a comment\n#[native]\n(1 + 2)");
        rt("{ catch(e) 0; x? }");
        rt("let r = &v");
        rt("x <- x + 1");
        rt("*r <- 5");
        rt("use array");
        rt("cast<i64>(x)");
        rt("any(a, b, c)");
        rt("!done");
        rt("{point with x: 5, y: 6}");
        rt("type Point = {x: f64, y: f64}");
        rt("a.field");
        rt("t.0");
        rt("array::map(xs, f)");
        rt("a +? b");
        rt("p?");
        rt("v$");
        rt("seq (go ~ x) { until ready; x }");
        rt("seqq request { { let x = request; x }; 42 }");
    }

    #[test]
    fn roundtrip_preserves_dec() {
        // `Expr` equality ignores `dec`; attributes are semantic and must
        // survive, so check `dec` directly.
        let ori = Origin {
            parent: None,
            source: Source::Internal(ArcStr::from("dec")),
            text: ArcStr::from("#[native]\n// note\n42"),
        };
        let exprs = parser::parse(ori).expect("parse");
        let packed = pack_module(&exprs).expect("pack");
        let dummy = Arc::new(Origin {
            parent: None,
            source: Source::Internal(ArcStr::from("decoded")),
            text: ArcStr::new(),
        });
        let unpacked = unpack_module(&packed, dummy).expect("unpack");
        let dec = unpacked[0].dec.as_ref().expect("dec dropped on round-trip");
        assert_eq!(dec.attrs.len(), 1, "attribute lost");
        assert_eq!(&dec.attrs[0].name, "native");
        assert_eq!(&dec.comments[..], &[arcstr::literal!(" note")], "comment lost");
    }

    #[test]
    fn roundtrip_type_annotations() {
        rt("let x: Array<i64> = [1, 2]");
        rt("let m: Map<string, i64> = {\"a\" => 1}");
        rt("let f: fn(x: i64) -> string = g");
        rt("let o: [i64, null] = null");
        rt("let e: Error<`MyErr> = error(`MyErr)");
        rt("let p: {x: f64, y: f64} = point");
        rt("let v: [`Foo, `Bar(i64)] = x");
        rt("let r: &i64 = y");
        rt("let t: (i64, string) = pair");
        rt("let f: fn(x: i64) -> string throws `E = g");
        rt("let n: decimal = z");
    }
}
