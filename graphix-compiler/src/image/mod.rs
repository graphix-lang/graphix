//! The image session: an encode or decode of compiler-session state
//! that preserves identity. Under a session, ids relocate (dense on
//! encode, offset into a reserved block on decode), origins and
//! type-variable wrappers and cells are object tables, expressions keep
//! their ids and origins, and the environment's maps keep their
//! sharing ([`crate::shared_map`]). Outside a session the same codecs
//! are the syntax codec's: fresh ids, fresh cells, no sharing.
//!
//! The decoder outlives any one session: an instance decoded later
//! resolves into what was decoded earlier, so the runtime owns an
//! [`ImageDecoder`] and opens a [`DecodeImage`] over it per read.

mod env;

use crate::{
    BindId, LambdaId, SourcePosition,
    expr::{ExprId, Origin, Source},
    ids::IdRelocation,
    shared_map,
    typ::{
        TVar,
        tvar::{TCell, TVarId},
    },
};
use ahash::AHashMap;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use parking_lot::RwLock;
use std::{
    cell::Cell, collections::HashMap, marker::PhantomData, path::PathBuf, ptr::NonNull,
};
use triomphe::Arc;

const REF: u8 = 0;
const DEF: u8 = 1;

/// How many ids of each relocated domain an image holds; the decoder
/// reserves a block of each.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct IdCounts {
    pub bind: u64,
    pub lambda: u64,
    pub expr: u64,
    pub tvar: u64,
}

impl Pack for IdCounts {
    fn encoded_len(&self) -> usize {
        let IdCounts { bind, lambda, expr, tvar } = self;
        varint_len(*bind) + varint_len(*lambda) + varint_len(*expr) + varint_len(*tvar)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let IdCounts { bind, lambda, expr, tvar } = self;
        for n in [bind, lambda, expr, tvar] {
            encode_varint(*n, buf);
        }
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(IdCounts {
            bind: decode_varint(buf)?,
            lambda: decode_varint(buf)?,
            expr: decode_varint(buf)?,
            tvar: decode_varint(buf)?,
        })
    }
}

/// The dense renumbering of each relocated id domain, kept between
/// sessions so every session over one encoder continues the numbering.
#[derive(Default)]
struct IdMaps {
    bind: HashMap<u64, u64>,
    lambda: HashMap<u64, u64>,
    expr: HashMap<u64, u64>,
    tvar: HashMap<u64, u64>,
}

pub struct ImageEncoder {
    pub(crate) maps: shared_map::EncodeTable,
    ids: IdMaps,
    origins: AHashMap<usize, u64>,
    pinned_origins: Vec<Arc<Origin>>,
    tvars: AHashMap<usize, u64>,
    pinned_tvars: Vec<TVar>,
    cells: AHashMap<usize, u64>,
    pinned_cells: Vec<Arc<RwLock<TCell>>>,
}

impl Default for ImageEncoder {
    fn default() -> Self {
        Self::new()
    }
}

impl ImageEncoder {
    pub fn new() -> Self {
        ImageEncoder {
            maps: shared_map::EncodeTable::default(),
            ids: IdMaps::default(),
            origins: AHashMap::new(),
            pinned_origins: Vec::new(),
            tvars: AHashMap::new(),
            pinned_tvars: Vec::new(),
            cells: AHashMap::new(),
            pinned_cells: Vec::new(),
        }
    }

    /// Renumber every id seen so far by its original value, so a map
    /// keyed by ids keeps its order once relocated. The image writer
    /// measures the whole image, calls this, then encodes; an id first
    /// met during the encode takes the next number, which is only
    /// order-preserving if nothing orders it against the earlier ones.
    pub fn sort_ids(&mut self) {
        fn sort(m: &mut HashMap<u64, u64>) {
            let mut seen: Vec<u64> = m.keys().copied().collect();
            seen.sort_unstable();
            for (rank, old) in seen.into_iter().enumerate() {
                m.insert(old, rank as u64);
            }
        }
        sort(&mut self.ids.bind);
        sort(&mut self.ids.lambda);
        sort(&mut self.ids.expr);
        sort(&mut self.ids.tvar);
    }

    /// The ids written so far. The image writer stores these ahead of
    /// the body; read between sessions.
    pub fn counts(&self) -> IdCounts {
        IdCounts {
            bind: self.ids.bind.len() as u64,
            lambda: self.ids.lambda.len() as u64,
            expr: self.ids.expr.len() as u64,
            tvar: self.ids.tvar.len() as u64,
        }
    }
}

pub struct ImageDecoder {
    pub(crate) maps: shared_map::DecodeTable,
    origins: Vec<Arc<Origin>>,
    tvars: Vec<TVar>,
    cells: Vec<Arc<RwLock<TCell>>>,
    bases: IdCounts,
}

impl ImageDecoder {
    /// Reserves a block of each id domain for the image's ids.
    pub fn new(counts: IdCounts) -> Self {
        ImageDecoder {
            maps: shared_map::DecodeTable::default(),
            origins: Vec::new(),
            tvars: Vec::new(),
            cells: Vec::new(),
            bases: IdCounts {
                bind: BindId::reserve(counts.bind).inner(),
                lambda: LambdaId::reserve(counts.lambda).inner(),
                expr: ExprId::reserve(counts.expr).inner(),
                tvar: TVarId::reserve(counts.tvar).inner(),
            },
        }
    }
}

thread_local! {
    static ENCODER: Cell<Option<NonNull<ImageEncoder>>> = const { Cell::new(None) };
    static DECODER: Cell<Option<NonNull<ImageDecoder>>> = const { Cell::new(None) };
}

struct Relocations {
    bind: Option<IdRelocation>,
    lambda: Option<IdRelocation>,
    expr: Option<IdRelocation>,
    tvar: Option<IdRelocation>,
}

impl Relocations {
    fn install(
        bind: Option<IdRelocation>,
        lambda: Option<IdRelocation>,
        expr: Option<IdRelocation>,
        tvar: Option<IdRelocation>,
    ) -> Self {
        Relocations {
            bind: BindId::set_relocation(bind),
            lambda: LambdaId::set_relocation(lambda),
            expr: ExprId::set_relocation(expr),
            tvar: TVarId::set_relocation(tvar),
        }
    }

    /// Restore the previous relocations, returning the ones removed.
    fn restore(self) -> Self {
        Relocations {
            bind: BindId::set_relocation(self.bind),
            lambda: LambdaId::set_relocation(self.lambda),
            expr: ExprId::set_relocation(self.expr),
            tvar: TVarId::set_relocation(self.tvar),
        }
    }
}

fn dense(r: Option<IdRelocation>) -> HashMap<u64, u64> {
    match r {
        Some(IdRelocation::Encode(dense)) => dense,
        _ => HashMap::new(),
    }
}

/// Encodes under `encoder` on this thread until dropped. Ids are
/// renumbered densely across every session over one encoder.
pub struct EncodeImage<'a> {
    encoder: NonNull<ImageEncoder>,
    prev: Option<NonNull<ImageEncoder>>,
    prev_maps: Option<NonNull<shared_map::EncodeTable>>,
    prev_ids: Relocations,
    _encoder: PhantomData<&'a mut ImageEncoder>,
}

impl<'a> EncodeImage<'a> {
    pub fn new(encoder: &'a mut ImageEncoder) -> Self {
        let ids = std::mem::take(&mut encoder.ids);
        let enc = |m: HashMap<u64, u64>| Some(IdRelocation::Encode(m));
        let prev_maps =
            shared_map::install_encode(Some(NonNull::from(&mut encoder.maps)));
        let encoder = NonNull::from(encoder);
        let prev = ENCODER.replace(Some(encoder));
        let prev_ids = Relocations::install(
            enc(ids.bind),
            enc(ids.lambda),
            enc(ids.expr),
            enc(ids.tvar),
        );
        EncodeImage { encoder, prev, prev_maps, prev_ids, _encoder: PhantomData }
    }
}

impl Drop for EncodeImage<'_> {
    fn drop(&mut self) {
        let prev = std::mem::replace(
            &mut self.prev_ids,
            Relocations { bind: None, lambda: None, expr: None, tvar: None },
        );
        let ours = prev.restore();
        let ids = IdMaps {
            bind: dense(ours.bind),
            lambda: dense(ours.lambda),
            expr: dense(ours.expr),
            tvar: dense(ours.tvar),
        };
        // The guard holds the `&mut` this pointer came from.
        unsafe { (*self.encoder.as_ptr()).ids = ids };
        ENCODER.set(self.prev);
        shared_map::install_encode(self.prev_maps);
    }
}

/// Decodes under `decoder` on this thread until dropped.
pub struct DecodeImage<'a> {
    prev: Option<NonNull<ImageDecoder>>,
    prev_maps: Option<NonNull<shared_map::DecodeTable>>,
    prev_ids: Relocations,
    _decoder: PhantomData<&'a mut ImageDecoder>,
}

impl<'a> DecodeImage<'a> {
    pub fn new(decoder: &'a mut ImageDecoder) -> Self {
        let b = decoder.bases;
        let at = |base: u64| Some(IdRelocation::Decode { base });
        let prev_maps =
            shared_map::install_decode(Some(NonNull::from(&mut decoder.maps)));
        let prev = DECODER.replace(Some(NonNull::from(decoder)));
        let prev_ids =
            Relocations::install(at(b.bind), at(b.lambda), at(b.expr), at(b.tvar));
        DecodeImage { prev, prev_maps, prev_ids, _decoder: PhantomData }
    }
}

impl Drop for DecodeImage<'_> {
    fn drop(&mut self) {
        let prev = std::mem::replace(
            &mut self.prev_ids,
            Relocations { bind: None, lambda: None, expr: None, tvar: None },
        );
        prev.restore();
        DECODER.set(self.prev);
        shared_map::install_decode(self.prev_maps);
    }
}

/// Run `f` against the installed encoder, or `None` outside a session.
pub(crate) fn encoding<R>(f: impl FnOnce(&mut ImageEncoder) -> R) -> Option<R> {
    // The session guard holds the `&mut` that produced this pointer for
    // as long as it is installed.
    ENCODER.get().map(|mut p| f(unsafe { p.as_mut() }))
}

pub(crate) fn decoding<R>(f: impl FnOnce(&mut ImageDecoder) -> R) -> Option<R> {
    DECODER.get().map(|mut p| f(unsafe { p.as_mut() }))
}

pub(crate) fn is_encoding() -> bool {
    ENCODER.get().is_some()
}

pub(crate) fn is_decoding() -> bool {
    DECODER.get().is_some()
}

pub(crate) fn pos_len(p: &SourcePosition) -> usize {
    <i32 as Pack>::encoded_len(&p.line) + <i32 as Pack>::encoded_len(&p.column)
}

pub(crate) fn pos_encode(
    p: &SourcePosition,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    <i32 as Pack>::encode(&p.line, buf)?;
    <i32 as Pack>::encode(&p.column, buf)
}

pub(crate) fn pos_decode(buf: &mut impl Buf) -> Result<SourcePosition, PackError> {
    let line = <i32 as Pack>::decode(buf)?;
    let column = <i32 as Pack>::decode(buf)?;
    Ok(SourcePosition { line, column })
}

fn source_len(s: &Source) -> usize {
    1 + match s {
        Source::File(p) => p.to_string_lossy().into_owned().encoded_len(),
        Source::Netidx(p) => p.encoded_len(),
        Source::Internal(s) => s.encoded_len(),
        Source::Unspecified => 0,
    }
}

fn source_encode(s: &Source, buf: &mut impl BufMut) -> Result<(), PackError> {
    match s {
        Source::File(p) => {
            buf.put_u8(0);
            p.to_string_lossy().into_owned().encode(buf)
        }
        Source::Netidx(p) => {
            buf.put_u8(1);
            p.encode(buf)
        }
        Source::Internal(s) => {
            buf.put_u8(2);
            s.encode(buf)
        }
        Source::Unspecified => {
            buf.put_u8(3);
            Ok(())
        }
    }
}

fn source_decode(buf: &mut impl Buf) -> Result<Source, PackError> {
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    match buf.get_u8() {
        0 => Ok(Source::File(PathBuf::from(String::decode(buf)?))),
        1 => Ok(Source::Netidx(Pack::decode(buf)?)),
        2 => Ok(Source::Internal(Pack::decode(buf)?)),
        3 => Ok(Source::Unspecified),
        _ => Err(PackError::UnknownTag),
    }
}

/// An origin is written once per image and referenced afterwards; its
/// parent chain is written first, so ids are completion order. Outside
/// a session every origin is written in full. (`Arc<Origin>` cannot
/// carry the impl itself: the orphan rule.)
pub(crate) fn origin_len(ori: &Arc<Origin>) -> usize {
    let seen =
        encoding(|e| e.origins.get(&(Arc::as_ptr(ori) as usize)).copied()).flatten();
    match seen {
        Some(id) => 1 + varint_len(id),
        None => {
            let parent = match &ori.parent {
                Some(p) => origin_len(p),
                None => 0,
            };
            1 + 1 + parent + source_len(&ori.source) + ori.text.encoded_len()
        }
    }
}

pub(crate) fn origin_encode(
    ori: &Arc<Origin>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let key = Arc::as_ptr(ori) as usize;
    if let Some(id) = encoding(|e| e.origins.get(&key).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(id, buf);
        return Ok(());
    }
    buf.put_u8(DEF);
    match &ori.parent {
        Some(p) => {
            buf.put_u8(1);
            origin_encode(p, buf)?;
        }
        None => buf.put_u8(0),
    }
    source_encode(&ori.source, buf)?;
    ori.text.encode(buf)?;
    encoding(|e| {
        let id = e.pinned_origins.len() as u64;
        e.origins.insert(key, id);
        e.pinned_origins.push(ori.clone());
    });
    Ok(())
}

pub(crate) fn origin_decode(buf: &mut impl Buf) -> Result<Arc<Origin>, PackError> {
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    match buf.get_u8() {
        REF => {
            let id = decode_varint(buf)? as usize;
            decoding(|d| d.origins.get(id).cloned())
                .flatten()
                .ok_or(PackError::InvalidFormat)
        }
        DEF => {
            if !buf.has_remaining() {
                return Err(PackError::BufferShort);
            }
            let parent = match buf.get_u8() {
                0 => None,
                1 => Some(origin_decode(buf)?),
                _ => return Err(PackError::UnknownTag),
            };
            let source = source_decode(buf)?;
            let text = Pack::decode(buf)?;
            let ori = Arc::new(Origin { parent, source, text });
            decoding(|d| d.origins.push(ori.clone()));
            Ok(ori)
        }
        _ => Err(PackError::UnknownTag),
    }
}

/// A type variable under an image: the wrapper (name, id, frozen) and
/// its cell (bound type, constraints, refusal) are separate objects,
/// each written once. Both are registered before their contents, so a
/// cell that reaches its own wrapper through a constraint decodes.
pub(crate) fn tvar_len(tv: &TVar) -> usize {
    let key = tv.wrapper_addr();
    if let Some(id) = encoding(|e| e.tvars.get(&key).copied()).flatten() {
        return 1 + varint_len(id);
    }
    let (id, frozen, cell) = tv.parts();
    let mut n = 1 + tv.name.encoded_len() + id.encoded_len() + frozen.encoded_len();
    let ckey = Arc::as_ptr(&cell) as usize;
    n += match encoding(|e| e.cells.get(&ckey).copied()).flatten() {
        Some(id) => 1 + varint_len(id),
        None => {
            let (typ, constraints, refused) = {
                let c = cell.read();
                (c.typ.clone(), c.constraints.to_vec(), c.cycle_refused)
            };
            1 + typ.encoded_len() + constraints.encoded_len() + refused.encoded_len()
        }
    };
    n
}

pub(crate) fn tvar_encode(tv: &TVar, buf: &mut impl BufMut) -> Result<(), PackError> {
    let key = tv.wrapper_addr();
    if let Some(id) = encoding(|e| e.tvars.get(&key).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(id, buf);
        return Ok(());
    }
    buf.put_u8(DEF);
    encoding(|e| {
        let id = e.pinned_tvars.len() as u64;
        e.tvars.insert(key, id);
        e.pinned_tvars.push(tv.clone());
    });
    let (id, frozen, cell) = tv.parts();
    tv.name.encode(buf)?;
    id.encode(buf)?;
    frozen.encode(buf)?;
    let ckey = Arc::as_ptr(&cell) as usize;
    if let Some(id) = encoding(|e| e.cells.get(&ckey).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(id, buf);
        return Ok(());
    }
    buf.put_u8(DEF);
    encoding(|e| {
        let id = e.pinned_cells.len() as u64;
        e.cells.insert(ckey, id);
        e.pinned_cells.push(cell.clone());
    });
    let (typ, constraints, refused) = {
        let c = cell.read();
        if c.rigid_gates != 0 {
            return Err(PackError::InvalidFormat);
        }
        (c.typ.clone(), c.constraints.to_vec(), c.cycle_refused)
    };
    typ.encode(buf)?;
    constraints.encode(buf)?;
    refused.encode(buf)
}

pub(crate) fn tvar_decode(buf: &mut impl Buf) -> Result<TVar, PackError> {
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    match buf.get_u8() {
        REF => {
            let id = decode_varint(buf)? as usize;
            decoding(|d| d.tvars.get(id).cloned())
                .flatten()
                .ok_or(PackError::InvalidFormat)
        }
        DEF => {
            let name = Pack::decode(buf)?;
            let id = TVarId::decode(buf)?;
            let frozen = bool::decode(buf)?;
            if !buf.has_remaining() {
                return Err(PackError::BufferShort);
            }
            let (cell, fill) = match buf.get_u8() {
                REF => {
                    let id = decode_varint(buf)? as usize;
                    let cell = decoding(|d| d.cells.get(id).cloned())
                        .flatten()
                        .ok_or(PackError::InvalidFormat)?;
                    (cell, false)
                }
                DEF => {
                    let cell = Arc::new(RwLock::new(TCell::default()));
                    decoding(|d| d.cells.push(cell.clone()));
                    (cell, true)
                }
                _ => return Err(PackError::UnknownTag),
            };
            let tv = TVar::from_parts(name, id, frozen, cell.clone());
            decoding(|d| d.tvars.push(tv.clone()));
            if fill {
                let typ = Pack::decode(buf)?;
                let constraints: Vec<_> = Pack::decode(buf)?;
                let refused = bool::decode(buf)?;
                let mut c = cell.write();
                c.typ = typ;
                c.constraints = constraints.into_iter().collect();
                c.cycle_refused = refused;
            }
            Ok(tv)
        }
        _ => Err(PackError::UnknownTag),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        expr::{Expr, ExprKind},
        typ::Type,
    };
    use arcstr::literal;
    use bytes::BytesMut;
    use netidx_value::Value;

    fn pack_all<T: Pack>(items: &[T], enc: &mut ImageEncoder) -> BytesMut {
        let bounds: Vec<usize> = {
            let _s = EncodeImage::new(enc);
            items.iter().map(|i| i.encoded_len()).collect()
        };
        enc.sort_ids();
        let _s = EncodeImage::new(enc);
        let mut buf = BytesMut::new();
        for (i, bound) in items.iter().zip(bounds) {
            let before = buf.len();
            i.encode(&mut buf).unwrap();
            assert!(buf.len() - before <= bound);
        }
        buf
    }

    #[test]
    fn ids_relocate_into_a_reserved_block() {
        let a = ExprId::new();
        let b = ExprId::new();
        let mut enc = ImageEncoder::new();
        let bytes = pack_all(&[a, b, a], &mut enc);
        assert_eq!(enc.counts(), IdCounts { expr: 2, ..IdCounts::default() });
        // dense: 0, 1, 0
        assert_eq!(&bytes[..], &[0, 1, 0]);
        let mut dec = ImageDecoder::new(enc.counts());
        let base = dec.bases.expr;
        let after = ExprId::new();
        let _s = DecodeImage::new(&mut dec);
        let mut b = &bytes[..];
        let (x, y, z) = (
            ExprId::decode(&mut b).unwrap(),
            ExprId::decode(&mut b).unwrap(),
            ExprId::decode(&mut b).unwrap(),
        );
        assert_eq!(x, z);
        assert_ne!(x, y);
        assert_ne!(x, a);
        assert!(x.inner() < after.inner() && y.inner() < after.inner());
        // a second decoder of the same image gets its own block
        let dec2 = ImageDecoder::new(enc.counts());
        assert!(dec2.bases.expr > base);
    }

    #[test]
    fn tvars_keep_wrapper_and_cell_sharing() {
        let a = TVar::empty_named(literal!("a"));
        let b = TVar::empty_named(literal!("b"));
        a.alias(&b);
        assert!(a.same_cell(&b));
        let c = TVar::named(literal!("c"), Type::TVar(a.clone()));
        let typ = Type::Tuple(Arc::from_iter([
            Type::TVar(a.clone()),
            Type::TVar(b.clone()),
            Type::TVar(a.clone()),
            Type::TVar(c.clone()),
        ]));
        let mut enc = ImageEncoder::new();
        let bytes = pack_all(&[typ.clone()], &mut enc);
        // an alias takes the other's id, so a and b share one
        assert_eq!(enc.counts().tvar, 2);
        let mut dec = ImageDecoder::new(enc.counts());
        let _s = DecodeImage::new(&mut dec);
        let decoded = Type::decode(&mut &bytes[..]).unwrap();
        let Type::Tuple(elems) = &decoded else { panic!("{decoded:?}") };
        let tv = |i: usize| match &elems[i] {
            Type::TVar(tv) => tv.clone(),
            other => panic!("{other:?}"),
        };
        let (a2, b2, a3, c2) = (tv(0), tv(1), tv(2), tv(3));
        assert_eq!(a2.wrapper_addr(), a3.wrapper_addr(), "one wrapper, two occurrences");
        assert_ne!(a2.wrapper_addr(), b2.wrapper_addr());
        assert!(a2.same_cell(&b2), "aliases share a cell");
        assert!(!a2.same_cell(&c2));
        // c is bound to a's wrapper, the same one the tuple holds
        let Some(Type::TVar(inner)) = c2.read().typ.read().typ.clone() else {
            panic!("c must stay bound to a tvar")
        };
        assert_eq!(inner.wrapper_addr(), a2.wrapper_addr());
        // a constraint added through one alias shows through the other
        a2.add_cell_constraint(Type::Primitive(netidx_value::Typ::I64.into()));
        assert_eq!(b2.cell_constraints().len(), 1);
        assert_eq!(b.cell_constraints().len(), 0, "the originals are untouched");
        // ids and the frozen flag survive, relocated
        assert_eq!(a2.parts().0, b2.parts().0);
        assert_ne!(a2.parts().0, c2.parts().0);
        assert_ne!(a2.parts().0, a.parts().0);
        assert!(a2.parts().1, "the alias source is frozen");
        assert!(!b2.parts().1);
    }

    #[test]
    fn expressions_keep_ids_and_share_origins() {
        let ori = Arc::new(Origin {
            parent: None,
            source: Source::Internal(literal!("test")),
            text: literal!("1; 2"),
        });
        let mk = |v: i64| Expr {
            id: ExprId::new(),
            ori: ori.clone(),
            pos: Default::default(),
            kind: ExprKind::Constant(Value::I64(v)),
            dec: None,
        };
        let (e1, e2) = (mk(1), mk(2));
        let mut enc = ImageEncoder::new();
        let bytes = pack_all(&[e1.clone(), e2.clone()], &mut enc);
        assert_eq!(enc.counts().expr, 2);
        let mut dec = ImageDecoder::new(enc.counts());
        let base = dec.bases.expr;
        let _s = DecodeImage::new(&mut dec);
        let mut b = &bytes[..];
        let d1 = Expr::decode(&mut b).unwrap();
        let d2 = Expr::decode(&mut b).unwrap();
        assert!(b.is_empty());
        assert!(Arc::ptr_eq(&d1.ori, &d2.ori), "one origin, two expressions");
        assert!(!Arc::ptr_eq(&d1.ori, &ori));
        assert_eq!(d1.ori.text, ori.text);
        assert_ne!(d1.id, d2.id);
        assert_eq!(d1.id.inner() - base, 0);
        assert_eq!(d2.id.inner() - base, 1);
        assert_eq!(d1.kind, ExprKind::Constant(Value::I64(1)));
    }
}
