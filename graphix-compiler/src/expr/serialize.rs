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
        Decorations, Expr, ExprId, ExprKind, Origin, Sig, VfsEntry, get_origin,
        swap_origin,
    },
    profile::{self, Phase},
    typ::{AbstractId, FnArgType, FnType, TVar, Type, fntyp::LambdaIds},
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

/// Brackets a decode unit: installs the module's `Origin` for decoded
/// `Expr`s and restores the previous one on `Drop`.
struct DecodeUnit {
    prev_origin: Option<Arc<Origin>>,
}

impl DecodeUnit {
    fn new(ori: Arc<Origin>) -> Self {
        let prev_origin = swap_origin(Some(ori));
        DecodeUnit { prev_origin }
    }
}

impl Drop for DecodeUnit {
    fn drop(&mut self) {
        swap_origin(self.prev_origin.take());
    }
}

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

impl Pack for Expr {
    fn encoded_len(&self) -> usize {
        <i32 as Pack>::encoded_len(&self.pos.line)
            + <i32 as Pack>::encoded_len(&self.pos.column)
            + self.kind.encoded_len()
            + self.dec.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        <i32 as Pack>::encode(&self.pos.line, buf)?;
        <i32 as Pack>::encode(&self.pos.column, buf)?;
        self.kind.encode(buf)?;
        self.dec.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let line = <i32 as Pack>::decode(buf)?;
        let column = <i32 as Pack>::decode(buf)?;
        let kind = <ExprKind as Pack>::decode(buf)?;
        let dec = <Option<Box<Decorations>> as Pack>::decode(buf)?;
        Ok(Expr {
            id: ExprId::new(),
            ori: get_origin(),
            pos: SourcePosition { line, column },
            kind,
            dec,
        })
    }
}

impl Pack for TVar {
    fn encoded_len(&self) -> usize {
        let (bound, constraints): (Option<Type>, Vec<Type>) = {
            let cell = self.read().typ.clone();
            let cell = cell.read();
            (cell.typ.clone(), cell.constraints.to_vec())
        };
        self.name.encoded_len() + bound.encoded_len() + constraints.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
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

impl Pack for FnType {
    // The constraints wire slot is a derived view of the cells: decode
    // re-seeds its entries onto the cells (`add_cell_constraint` dedups).
    fn encoded_len(&self) -> usize {
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

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.args.encode(buf)?;
        self.vargs.encode(buf)?;
        self.rtype.encode(buf)?;
        let constraints = self.cell_constraint_pairs();
        <Vec<(TVar, Type)> as Pack>::encode(&constraints, buf)?;
        self.throws.encode(buf)?;
        self.explicit_throws.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let args = <Arc<[FnArgType]> as Pack>::decode(buf)?;
        let vargs = <Option<Type> as Pack>::decode(buf)?;
        let rtype = <Type as Pack>::decode(buf)?;
        let constraints = <Vec<(TVar, Type)> as Pack>::decode(buf)?;
        let throws = <Type as Pack>::decode(buf)?;
        let explicit_throws = <bool as Pack>::decode(buf)?;
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
        Ok(FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws,
            quantifiers,
            // Provenance only; excluded from FnType identity.
            lambda_ids: LambdaIds::default(),
        })
    }
}

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
    let _unit = DecodeUnit::new(ori);
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
    let _unit = DecodeUnit::new(ori);
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
        rt("seqq request { do { let x = request; x }; 42 }");
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
