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
    LambdaId, SourcePosition,
    expr::{
        Decorations, Expr, ExprId, ExprKind, Origin, OriginScope, Sig, VfsEntry,
        get_origin,
    },
    image,
    profile::{self, Phase},
    typ::{AbstractId, FnArgType, FnType, TVar, TraitId, Type, fntyp::LambdaIds},
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

// CR claude for eric: [structure] The Pack impls of AbstractId, TraitId, TVar
// and FnType live here while Type's lives in typ/mod.rs beside the type; the
// AbstractId and TraitId impls are identical, and a varint of a uuid-derived
// u64 costs ~10 bytes where a fixed u64 costs 8. Move each impl next to its
// type (typ/mod.rs, tvar.rs, fntyp.rs) and share the id codec.
impl Pack for AbstractId {
    fn encoded_len(&self) -> usize {
        pack::varint_len(self.inner())
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        Ok(pack::encode_varint(self.inner(), buf))
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(AbstractId::from_inner(pack::decode_varint(buf)?))
    }
}

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
        let dec = <Option<Box<Decorations>> as Pack>::decode(buf)?;
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
/// syntax codec mints a fresh id and takes the unit's origin.
impl Pack for Expr {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() {
            image::object_len(
                &image::expr_key(self),
                |k| (*k, ()),
                |e| &mut e.exprs,
                || {
                    self.id.encoded_len()
                        + image::origin_len(&self.ori)
                        + self.syntax_len()
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

impl Pack for TraitId {
    fn encoded_len(&self) -> usize {
        pack::varint_len(self.inner())
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        Ok(pack::encode_varint(self.inner(), buf))
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(TraitId::from_inner(pack::decode_varint(buf)?))
    }
}

/// Under an image session the wrapper and its cell are shared objects
/// ([`image::tvar_encode`]); the syntax codec writes the cell's
/// contents and mints a fresh variable.
// CR claude for eric: [bug] suspected: the syntax path writes the cell's
// constraints inline with no cycle guard, and the parser aliases a quantifier's
// own name inside its constraint to the same cell (typexp.rs:263). So a fn
// type `fn<'a: [i64, Array<'a>]>(x: 'a) -> 'a` (accepted: the same constraint
// on a lambda runs) in a package .gx/.gxi makes pack_module/pack_sig recurse
// until the build script overflows its stack. Only the image path shares
// cells. Also: encoded_len and encode each clone the bound and `to_vec()` the
// constraints; one borrowed helper serves both.
impl Pack for TVar {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() {
            return image::tvar_len(self);
        }
        let (bound, constraints): (Option<Type>, Vec<Type>) = {
            let cell = self.read().typ.clone();
            let cell = cell.read();
            (cell.typ.clone(), cell.constraints.to_vec())
        };
        self.name.encoded_len() + bound.encoded_len() + constraints.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            return image::tvar_encode(self, buf);
        }
        self.name.encode(buf)?;
        let (bound, constraints): (Option<Type>, Vec<Type>) = {
            let cell = self.read().typ.clone();
            let cell = cell.read();
            (cell.typ.clone(), cell.constraints.to_vec())
        };
        bound.encode(buf)?;
        constraints.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if image::is_decoding() {
            return image::tvar_decode(buf);
        }
        let name = <ArcStr as Pack>::decode(buf)?;
        let bound = <Option<Type> as Pack>::decode(buf)?;
        let constraints = <Vec<Type> as Pack>::decode(buf)?;
        // A fresh id is sound: the typechecker re-aliases same-named tvars
        // within a scope.
        let tv = match bound {
            Some(t) => TVar::named(name, t),
            None => TVar::empty_named(name),
        };
        {
            let cell = tv.read().typ.clone();
            let mut cell = cell.write();
            for c in constraints {
                cell.add_constraint(c);
            }
        }
        Ok(tv)
    }
}

impl FnType {
    // The constraints wire slot is a derived view of the cells: decode
    // re-seeds its entries onto the cells (`add_cell_constraint` dedups).
    fn shape_len(&self) -> usize {
        // The full cell pairs, not the declared-quantifier view: anonymous
        // cells carry inference facts that must cross the wire.
        let constraints = self.cell_constraint_pairs();
        self.args.encoded_len()
            + self.vargs.encoded_len()
            + self.rtype.encoded_len()
            + <Vec<(TVar, Type)> as Pack>::encoded_len(&constraints)
            + self.throws.encoded_len()
            + self.explicit_throws.encoded_len()
    }

    fn shape_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.args.encode(buf)?;
        self.vargs.encode(buf)?;
        self.rtype.encode(buf)?;
        let constraints = self.cell_constraint_pairs();
        <Vec<(TVar, Type)> as Pack>::encode(&constraints, buf)?;
        self.throws.encode(buf)?;
        self.explicit_throws.encode(buf)
    }

    fn shape_decode(
        buf: &mut impl Buf,
        own: Option<LambdaId>,
    ) -> Result<Self, PackError> {
        let args = <Arc<[FnArgType]> as Pack>::decode(buf)?;
        let vargs = <Option<Type> as Pack>::decode(buf)?;
        let rtype = <Type as Pack>::decode(buf)?;
        let constraints = <Vec<(TVar, Type)> as Pack>::decode(buf)?;
        let throws = <Type as Pack>::decode(buf)?;
        let explicit_throws = <bool as Pack>::decode(buf)?;
        // CR claude for eric: [risk] `quantifiers` is not on the wire; it is
        // guessed from the single-conjunct cell pairs by name. That is wrong
        // three ways: an inner fn type naming an outer constrained quantifier
        // (`fn<'a: Number>(g: fn(x: 'a) -> 'a) -> 'a`) decodes with `['a]` where
        // it had none, so its constraint_view (Eq, Hash, contains, printing)
        // differs from the parsed type; a `+` (multi-conjunct) or unbounded
        // quantifier is dropped; source order becomes name order. Encode it.
        // Named pairs are the declared quantifiers; anonymous '_N pairs are
        // inference facts, re-seeded below.
        let quantifiers = Arc::from_iter(
            constraints
                .iter()
                .filter(|(tv, _)| !tv.name.starts_with('_'))
                .map(|(tv, _)| tv.name.clone()),
        );
        for (tv, tc) in constraints {
            tv.add_cell_constraint(tc);
        }
        // Provenance only; excluded from FnType identity.
        let lambda_ids = LambdaIds::default();
        if let Some(id) = own {
            lambda_ids.set_id(id);
        }
        Ok(FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws,
            quantifiers,
            lambda_ids,
        })
    }
}

/// Under an image session a function type is an object carrying its
/// own lambda id, written once and referenced afterwards (a binding's
/// type and its definition share one).
impl Pack for FnType {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() {
            image::fntype_len(self, || {
                self.lambda_ids.own().encoded_len() + self.shape_len()
            })
        } else {
            self.shape_len()
        }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            image::fntype_encode(self, buf, |buf| {
                self.lambda_ids.own().encode(buf)?;
                self.shape_encode(buf)
            })
        } else {
            self.shape_encode(buf)
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if image::is_decoding() {
            image::object_decode(
                buf,
                |d| &mut d.fntypes,
                |buf| {
                    let own = <Option<LambdaId> as Pack>::decode(buf)?;
                    Self::shape_decode(buf, own)
                },
                |b| Self::decode(b),
            )
        } else {
            Self::shape_decode(buf, None)
        }
    }
}

// CR claude for eric: [readability] A free fn named `map_err`, used as
// `.map_err(map_err)`, reads as a typo; `codec_error` says what it builds.
fn map_err(e: PackError) -> anyhow::Error {
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
        e.encode(&mut buf).map_err(map_err)?;
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
    let n = pack::decode_varint(&mut bytes).map_err(map_err)? as usize;
    let mut v: LPooled<Vec<Expr>> = LPooled::take();
    for _ in 0..n {
        v.push(Expr::decode(&mut bytes).map_err(map_err)?);
    }
    Ok(Arc::from_iter(v.drain(..)))
}

/// Serialize a module interface (`.gxi`) signature to a packed blob.
pub fn pack_sig(sig: &Sig) -> Result<Bytes> {
    let mut buf = BytesMut::new();
    buf.put_slice(MAGIC);
    sig.encode(&mut buf).map_err(map_err)?;
    Ok(buf.freeze())
}

/// Deserialize a module interface signature from a packed blob.
pub fn unpack_sig(mut bytes: &[u8], ori: Arc<Origin>) -> Result<Sig> {
    let _profile = profile::phase(Phase::Decode);
    check_magic(&mut bytes)?;
    let _unit = OriginScope::enter(ori);
    Sig::decode(&mut bytes).map_err(map_err)
}

/// Serialize a whole package's modules as one blob: a list of
/// `(vfs_path_key, source, packed_module_ast)`, each `ast` a
/// `pack_module`/`pack_sig` blob decoded lazily at module resolution.
pub fn pack_index(entries: &[(ArcStr, ArcStr, Bytes)]) -> Result<Bytes> {
    let mut buf = BytesMut::new();
    buf.put_slice(MAGIC);
    pack::encode_varint(entries.len() as u64, &mut buf);
    for (path, source, ast) in entries {
        path.encode(&mut buf).map_err(map_err)?;
        source.encode(&mut buf).map_err(map_err)?;
        pack::encode_varint(ast.len() as u64, &mut buf);
        buf.put_slice(ast);
    }
    Ok(buf.freeze())
}

/// Decode a package index blob (see [`pack_index`]) into `(Path, VfsEntry)`
/// pairs; each entry's AST stays packed in `VfsEntry.packed`.
// CR claude for eric: [perf] The only caller (defpackage!'s register) passes an
// `include_bytes!` blob, yet every module's AST is copied out of it
// (`Bytes::copy_from_slice`) on every start, for every package. Taking
// `&'static [u8]` and `Bytes::from_static(..)` slices keeps them zero-copy.
pub fn unpack_index(mut bytes: &[u8]) -> Result<Vec<(Path, VfsEntry)>> {
    check_magic(&mut bytes)?;
    let n = pack::decode_varint(&mut bytes).map_err(map_err)? as usize;
    let mut result = Vec::with_capacity(n);
    for _ in 0..n {
        let path = ArcStr::decode(&mut bytes).map_err(map_err)?;
        let source = ArcStr::decode(&mut bytes).map_err(map_err)?;
        let ast_len = pack::decode_varint(&mut bytes).map_err(map_err)? as usize;
        if bytes.len() < ast_len {
            bail!("packed index: truncated module AST");
        }
        let ast = Bytes::copy_from_slice(&bytes[..ast_len]);
        bytes.advance(ast_len);
        result.push((Path::from(path), VfsEntry { source, packed: Some(ast) }));
    }
    Ok(result)
}

// CR claude for eric: [risk] The round trips compare `kind` only (Expr
// equality), so `pos`, which IS packed, is never checked; pack_sig/unpack_sig,
// pack_index/unpack_index and FnType quantifiers have no test; and the inputs
// are 47 hand-picked strings while expr/test.rs has a generator over every
// kind and a `check` comparator. A proptest pack -> unpack -> `check` (plus
// pos) over that generator would have caught the quantifier guess above.
#[cfg(test)]
mod test {
    use super::*;
    use crate::expr::{Source, parser};

    fn rt(src: &str) {
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
        let unpacked = unpack_module(&packed, dummy).expect("unpack");
        // `Expr` equality is kind-only, so this checks structure.
        assert_eq!(&exprs[..], &unpacked[..], "round-trip mismatch for: {src}");
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
