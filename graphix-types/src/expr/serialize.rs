//! Binary serialization of the module AST, so a package deserializes its
//! AST at load instead of re-parsing its `.gx` source.
//!
//! The codec is netidx [`Pack`]; most AST types derive it. This module
//! holds `Expr`'s hand-written impl, the per-module decode setup and the
//! `pack_module`/`unpack_module` (and `_sig`) entry points; `TVar`'s and
//! `FnType`'s live beside their types, `AbstractId`'s is `uuid_id_codec!`.
//!
//! Every blob starts with the magic and the writing graphix-types'
//! version, and a reader of another version refuses it: a package's
//! AST-packing build-dependency and its compiler resolve separately.

use crate::{
    SourcePosition,
    expr::{
        Decorations, Expr, ExprId, ExprKind, NOP, Origin, OriginScope, Sig, SigItem,
        VfsEntry, get_origin,
    },
    image,
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
            image::object_len(&image::expr_key(self), |k| (*k, ()), |e| &mut e.exprs)
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

/// An interface item packs its position as an expression does; its
/// origin travels under an image session, and the syntax codec takes the
/// unit's.
impl Pack for SigItem {
    fn encoded_len(&self) -> usize {
        let ori = match image::is_encoding() {
            false => 0,
            true => image::origin_len(&self.ori),
        };
        self.comments.encoded_len()
            + self.eol.encoded_len()
            + self.doc.encoded_len()
            + self.kind.encoded_len()
            + <i32 as Pack>::encoded_len(&self.pos.line)
            + <i32 as Pack>::encoded_len(&self.pos.column)
            + ori
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.comments.encode(buf)?;
        self.eol.encode(buf)?;
        self.doc.encode(buf)?;
        self.kind.encode(buf)?;
        <i32 as Pack>::encode(&self.pos.line, buf)?;
        <i32 as Pack>::encode(&self.pos.column, buf)?;
        if image::is_encoding() {
            image::origin_encode(&self.ori, buf)?
        }
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let comments = Pack::decode(buf)?;
        let eol = Pack::decode(buf)?;
        let doc = Pack::decode(buf)?;
        let kind = Pack::decode(buf)?;
        let line = <i32 as Pack>::decode(buf)?;
        let column = <i32 as Pack>::decode(buf)?;
        let ori = match image::is_decoding() {
            false => get_origin(),
            true => image::origin_decode(buf)?,
        };
        Ok(SigItem {
            comments,
            eol,
            doc,
            kind,
            pos: SourcePosition { line, column },
            ori,
        })
    }
}

fn codec_error(e: PackError) -> anyhow::Error {
    anyhow::anyhow!("packed AST codec error: {e:?}")
}

/// The graphix-types version that writes and reads blobs.
const VERSION: &str = env!("CARGO_PKG_VERSION");

const fn fnv1a(mut h: u64, s: &str) -> u64 {
    let b = s.as_bytes();
    let mut i = 0;
    while i < b.len() {
        h ^= b[i] as u64;
        h = h.wrapping_mul(0x100000001b3);
        i += 1;
    }
    h
}

/// The sources of every type a blob holds: derived `Pack` writes variant
/// indices, so two builds read each other's blobs only from the same
/// definitions, whatever their version strings say.
const LAYOUT: u64 = {
    let mut h = 0xcbf29ce484222325;
    h = fnv1a(h, include_str!("mod.rs"));
    h = fnv1a(h, include_str!("binop.rs"));
    h = fnv1a(h, include_str!("modpath.rs"));
    h = fnv1a(h, include_str!("pattern.rs"));
    h = fnv1a(h, include_str!("serialize.rs"));
    h = fnv1a(h, include_str!("../typ/mod.rs"));
    h = fnv1a(h, include_str!("../typ/fntyp.rs"));
    fnv1a(h, include_str!("../typ/tvar.rs"))
};

fn put_header(buf: &mut BytesMut) {
    buf.put_slice(MAGIC);
    pack::encode_varint(VERSION.len() as u64, buf);
    buf.put_slice(VERSION.as_bytes());
    buf.put_u64(LAYOUT);
}

fn check_magic(bytes: &mut &[u8]) -> Result<()> {
    if bytes.len() < MAGIC.len() || &bytes[..MAGIC.len()] != MAGIC {
        bail!("packed AST: bad magic header");
    }
    bytes.advance(MAGIC.len());
    let n = pack::decode_varint(bytes).map_err(codec_error)? as usize;
    if bytes.len() < n {
        bail!("packed AST: truncated header");
    }
    let theirs = String::from_utf8_lossy(&bytes[..n]).into_owned();
    bytes.advance(n);
    if theirs != VERSION {
        bail!(
            "packed AST written by graphix-types {theirs}, read by {VERSION}: rebuild \
             the package with this compiler"
        )
    }
    if bytes.len() < 8 || bytes.get_u64() != LAYOUT {
        bail!(
            "packed AST written by another build of graphix-types {VERSION}: rebuild \
             the package with this compiler"
        )
    }
    Ok(())
}

/// Serialize a module's top-level expressions to a packed blob.
pub fn pack_module(exprs: &[Expr]) -> Result<Bytes> {
    let mut buf = BytesMut::new();
    put_header(&mut buf);
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
    put_header(&mut buf);
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
    put_header(&mut buf);
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
        typ::{TVar, Type},
    };

    /// `src` parsed, and its package round trip.
    fn packed(src: &str) -> (Arc<[Expr]>, Arc<[Expr]>) {
        let ori = Origin {
            parent: None,
            source: Source::Vfs(ArcStr::from("/test")),
            text: ArcStr::from(src),
        };
        let exprs = parser::parse(ori).unwrap_or_else(|e| panic!("parse `{src}`: {e}"));
        let packed = pack_module(&exprs).expect("pack");
        let dummy = Arc::new(Origin {
            parent: None,
            source: Source::Vfs(ArcStr::from("/decoded")),
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

    /// A cell met inside its own contents has no syntax: the codec refuses
    /// it, where it would write a bound that reads back without itself.
    #[test]
    fn a_cell_holding_itself_does_not_pack() {
        let tv = TVar::empty_named(arcstr::literal!("a"));
        tv.add_cell_constraint(Type::Array(Arc::new(Type::TVar(tv.clone()))));
        let t = Type::TVar(tv.clone());
        assert!(matches!(t.encode(&mut BytesMut::new()), Err(PackError::InvalidFormat)));
        tv.cell().write().constraints.clear();
    }

    /// A blob from another build of the AST types is refused, whatever
    /// version it names.
    #[test]
    fn another_layout_is_refused() {
        let e = crate::expr::parser::parse_one("1 + 2").unwrap();
        let mut packed =
            BytesMut::from(&pack_module(std::slice::from_ref(&e)).unwrap()[..]);
        let at = MAGIC.len() + 1 + VERSION.len();
        packed[at] ^= 1;
        let err = unpack_module(&packed, Arc::new(Origin::default())).unwrap_err();
        assert!(format!("{err}").contains("another build"), "{err}");
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
            source: Source::Vfs(ArcStr::from("/dec")),
            text: ArcStr::from("#[native]\n// note\n42"),
        };
        let exprs = parser::parse(ori).expect("parse");
        let packed = pack_module(&exprs).expect("pack");
        let dummy = Arc::new(Origin {
            parent: None,
            source: Source::Vfs(ArcStr::from("/decoded")),
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
