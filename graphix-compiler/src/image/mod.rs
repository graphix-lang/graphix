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

mod defs;
pub(crate) mod env;
pub mod nodes;
mod registration;

pub(crate) use env::{lexical_decode, lexical_encode, lexical_len};
pub use nodes::NOT_IMAGED;
pub use registration::{NOT_QUIESCENT, ProgramRoot, REGISTRATION_FORMAT, Registration};

use crate::{
    BindId, CFlag, DynScope, ErrorHandler, FastCall, LambdaId, LambdaInstanceId, Scope,
    SourcePosition,
    expr::{Expr, ExprId, ModPath, Origin, Source},
    fusion::{
        emit::BodyRecord,
        kernel_abi::{KernelSig, SiteLeaf},
    },
    ids::IdRelocation,
    shared_map,
    typ::{
        FnType, ResolvedRef, TVar, Type,
        tvar::{TCell, TVarId},
    },
};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use bytes::{Buf, BufMut, Bytes, BytesMut};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use parking_lot::{Mutex, RwLock};
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

/// The image under construction: a byte buffer that tells the session
/// how much it has written, so an object's definition knows its offset.
pub struct ImageBuf(BytesMut);

impl ImageBuf {
    pub fn with_capacity(n: usize) -> Self {
        ImageBuf(BytesMut::with_capacity(n))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn freeze(self) -> bytes::Bytes {
        self.0.freeze()
    }

    /// Overwrite eight bytes written earlier, for a header field whose
    /// value is known only at the end.
    pub fn patch_u64(&mut self, at: usize, v: u64) {
        self.0[at..at + 8].copy_from_slice(&v.to_be_bytes());
    }
}

/// An instance body the eager part of the image skipped: written
/// after it, in the heap, at an offset the instance table records.
type Deferred = Box<dyn FnOnce(&mut ImageBuf) -> Result<(), PackError>>;

unsafe impl BufMut for ImageBuf {
    fn remaining_mut(&self) -> usize {
        self.0.remaining_mut()
    }

    unsafe fn advance_mut(&mut self, cnt: usize) {
        unsafe { self.0.advance_mut(cnt) };
        encoding(|e| e.written += cnt as u64);
    }

    fn chunk_mut(&mut self) -> &mut bytes::buf::UninitSlice {
        self.0.chunk_mut()
    }

    fn put_slice(&mut self, src: &[u8]) {
        self.0.put_slice(src);
        encoding(|e| e.written += src.len() as u64);
    }
}

pub struct ImageEncoder {
    pub(crate) maps: shared_map::EncodeTable,
    /// Bytes written through the [`ImageBuf`] so far: the offset of
    /// the next byte.
    pub(crate) written: u64,
    ids: IdMaps,
    handlers: AHashMap<usize, u64>,
    pinned_handlers: Vec<ErrorHandler>,
    paths: AHashMap<ArcStr, u64>,
    refcells: AHashMap<usize, u64>,
    pinned_refcells: Vec<RefCell>,
    /// What the length pass has measured as a definition: the first
    /// sight measures the contents, later sights a reference, as the
    /// encode will write them. Objects can reach themselves, so the
    /// length walk needs this to terminate.
    measured: AHashSet<usize>,
    origins: AHashMap<usize, u64>,
    pinned_origins: Vec<Arc<Origin>>,
    tvars: AHashMap<usize, u64>,
    pinned_tvars: Vec<TVar>,
    cells: AHashMap<usize, u64>,
    pinned_cells: Vec<Arc<RwLock<TCell>>>,
    /// Expressions and function types by address. The session cannot
    /// pin them (their codecs see a borrow, not the `Arc`), so everything
    /// a session encodes must be borrowed from the context and the root
    /// nodes for the whole session; a temporary could hand its address
    /// to a later object.
    pub(crate) exprs: AHashMap<usize, u64>,
    /// Kernel signatures, slot-chain leaves and body records by `Arc`.
    pub(crate) kernel_sigs: AHashMap<usize, u64>,
    pub(crate) site_leaves: AHashMap<usize, u64>,
    pub(crate) records: AHashMap<usize, u64>,
    /// An expression's address, or the address of the first expression
    /// seen with its id and contents: a node's spec is a clone of the
    /// tree it was compiled from.
    expr_alias: AHashMap<usize, usize>,
    exprs_by_id: AHashMap<ExprId, usize>,
    /// Types and function types by their canonical bytes, every shared
    /// leaf (a variable, a resolution cell, an origin) by identity:
    /// equal types decode to one shared value.
    pub(crate) types: ContentTable,
    pub(crate) fntypes: ContentTable,
    /// The canonical bytes of every shared type node met so far, by
    /// the address of its `Arc`, so a key walk stops at shared subtrees.
    type_keys: AHashMap<usize, Box<[u8]>>,
    /// Key buffers free for the next key walk, one per nesting level.
    key_scratch: Vec<Vec<u8>>,
    /// Whether a call site writes its instance into the heap, for a
    /// first dispatch to decode, rather than inline.
    pub(crate) defer_instances: bool,
    pub(crate) deferred: Vec<(LambdaInstanceId, Deferred)>,
    /// The measured size of every deferred body, for the length bound.
    pub(crate) deferred_len: usize,
    pub(crate) instances: AHashMap<LambdaInstanceId, u64>,
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
            written: 0,
            ids: IdMaps::default(),
            handlers: AHashMap::new(),
            pinned_handlers: Vec::new(),
            paths: AHashMap::new(),
            refcells: AHashMap::new(),
            pinned_refcells: Vec::new(),
            measured: AHashSet::new(),
            origins: AHashMap::new(),
            pinned_origins: Vec::new(),
            tvars: AHashMap::new(),
            pinned_tvars: Vec::new(),
            cells: AHashMap::new(),
            pinned_cells: Vec::new(),
            exprs: AHashMap::new(),
            kernel_sigs: AHashMap::new(),
            site_leaves: AHashMap::new(),
            records: AHashMap::new(),
            expr_alias: AHashMap::new(),
            exprs_by_id: AHashMap::new(),
            types: AHashMap::new(),
            fntypes: AHashMap::new(),
            type_keys: AHashMap::new(),
            key_scratch: Vec::new(),
            defer_instances: false,
            deferred: Vec::new(),
            deferred_len: 0,
            instances: AHashMap::new(),
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

    /// The objects defined so far.
    pub fn object_counts(&self) -> ObjectCounts {
        ObjectCounts {
            exprs: self.exprs.len() as u64,
            types: self.types.len() as u64,
            fntypes: self.fntypes.len() as u64,
            paths: self.paths.len() as u64,
            tvars: self.tvars.len() as u64,
            cells: self.cells.len() as u64,
            refcells: self.refcells.len() as u64,
            origins: self.origins.len() as u64,
            handlers: self.handlers.len() as u64,
        }
    }
}

/// The objects an image session has built, each by the offset of its
/// definition in the image, and the image itself, so a reference to
/// an object not built yet decodes it from there.
/// How many objects of each kind the eager part of an image defines,
/// so a restore sizes its tables once.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ObjectCounts {
    pub exprs: u64,
    pub types: u64,
    pub fntypes: u64,
    pub paths: u64,
    pub tvars: u64,
    pub cells: u64,
    pub refcells: u64,
    pub origins: u64,
    pub handlers: u64,
}

impl ObjectCounts {
    fn each(&self) -> [u64; 9] {
        let Self {
            exprs,
            types,
            fntypes,
            paths,
            tvars,
            cells,
            refcells,
            origins,
            handlers,
        } = *self;
        [exprs, types, fntypes, paths, tvars, cells, refcells, origins, handlers]
    }
}

impl Pack for ObjectCounts {
    fn encoded_len(&self) -> usize {
        self.each().iter().map(|n| varint_len(*n)).sum()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        for n in self.each() {
            encode_varint(n, buf);
        }
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let mut each = [0; 9];
        for n in each.iter_mut() {
            *n = decode_varint(buf)?;
        }
        let [exprs, types, fntypes, paths, tvars, cells, refcells, origins, handlers] =
            each;
        Ok(Self {
            exprs,
            types,
            fntypes,
            paths,
            tvars,
            cells,
            refcells,
            origins,
            handlers,
        })
    }
}

pub struct ImageDecoder {
    pub(crate) maps: shared_map::DecodeTable,
    image: Bytes,
    handlers: AHashMap<u64, ErrorHandler>,
    paths: AHashMap<u64, ModPath>,
    refcells: AHashMap<u64, RefCell>,
    origins: AHashMap<u64, Arc<Origin>>,
    tvars: AHashMap<u64, TVar>,
    cells: AHashMap<u64, Arc<RwLock<TCell>>>,
    pub(crate) exprs: AHashMap<u64, Expr>,
    pub(crate) types: AHashMap<u64, Type>,
    pub(crate) fntypes: AHashMap<u64, FnType>,
    pub(crate) kernel_sigs: AHashMap<u64, std::sync::Arc<KernelSig>>,
    pub(crate) site_leaves: AHashMap<u64, std::sync::Arc<SiteLeaf>>,
    pub(crate) records: AHashMap<u64, std::sync::Arc<BodyRecord>>,
    /// The builtins' fast fns by name, for a kernel constant's recipe.
    fastcalls: AHashMap<&'static str, FastCall>,
    instances: AHashMap<LambdaInstanceId, u64>,
    bases: IdCounts,
}

impl ImageDecoder {
    /// Reserves a block of each id domain for the image's ids.
    pub fn new(counts: IdCounts) -> Self {
        ImageDecoder {
            maps: shared_map::DecodeTable::default(),
            image: Bytes::new(),
            handlers: AHashMap::new(),
            paths: AHashMap::new(),
            refcells: AHashMap::new(),
            origins: AHashMap::new(),
            tvars: AHashMap::new(),
            cells: AHashMap::new(),
            exprs: AHashMap::new(),
            types: AHashMap::new(),
            fntypes: AHashMap::new(),
            kernel_sigs: AHashMap::new(),
            site_leaves: AHashMap::new(),
            records: AHashMap::new(),
            fastcalls: AHashMap::new(),
            instances: AHashMap::new(),
            bases: IdCounts {
                bind: BindId::reserve(counts.bind).inner(),
                lambda: LambdaId::reserve(counts.lambda).inner(),
                expr: ExprId::reserve(counts.expr).inner(),
                tvar: TVarId::reserve(counts.tvar).inner(),
            },
        }
    }

    /// The image every offset in the session refers into. Set before
    /// anything decodes; the session keeps it for what decodes later.
    pub fn set_image(&mut self, image: Bytes) {
        self.image = image;
    }

    pub fn image(&self) -> &Bytes {
        &self.image
    }

    pub(crate) fn set_instances(&mut self, instances: AHashMap<LambdaInstanceId, u64>) {
        self.instances = instances;
    }

    pub(crate) fn set_fastcalls(&mut self, fastcalls: AHashMap<&'static str, FastCall>) {
        self.fastcalls = fastcalls;
    }

    pub(crate) fn fastcall(&self, name: &str) -> Option<FastCall> {
        self.fastcalls.get(name).copied()
    }

    /// Size the object tables for what the eager part defines.
    pub fn reserve(&mut self, counts: ObjectCounts) {
        let ObjectCounts {
            exprs,
            types,
            fntypes,
            paths,
            tvars,
            cells,
            refcells,
            origins,
            handlers,
        } = counts;
        self.exprs.reserve(exprs as usize);
        self.types.reserve(types as usize);
        self.fntypes.reserve(fntypes as usize);
        self.paths.reserve(paths as usize);
        self.tvars.reserve(tvars as usize);
        self.cells.reserve(cells as usize);
        self.refcells.reserve(refcells as usize);
        self.origins.reserve(origins as usize);
        self.handlers.reserve(handlers as usize);
    }

    /// Where the instance's body starts in the image, when it was
    /// written to the heap.
    pub(crate) fn instance_offset(&self, id: LambdaInstanceId) -> Option<u64> {
        self.instances.get(&id).copied()
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

/// A type reference's write-once resolution cell, shared by every
/// rebuild of the reference (`TypeRef::with_params`).
pub(crate) type RefCell = Arc<Mutex<Option<Arc<ResolvedRef>>>>;

fn path_key(path: &ModPath) -> &str {
    path.0.as_ref()
}

/// A module path is written once per distinct path and referenced
/// afterwards, so a scope repeated by every binding under it costs a
/// varint and decodes to one shared string.
pub(crate) fn path_len(path: &ModPath) -> usize {
    match encoding(|e| e.paths.get(path_key(path)).copied()).flatten() {
        Some(id) => 1 + varint_len(id),
        None => 1 + path.0.encoded_len(),
    }
}

pub(crate) fn path_encode(
    path: &ModPath,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    if let Some(offset) = encoding(|e| e.paths.get(path_key(path)).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(offset, buf);
        return Ok(());
    }
    let at = encoding(|e| e.written).unwrap_or(0);
    buf.put_u8(DEF);
    encoding(|e| {
        e.paths.insert(ArcStr::from(path_key(path)), at);
    });
    path.0.encode(buf)
}

pub(crate) fn path_decode(buf: &mut impl Buf) -> Result<ModPath, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = decode_varint(sub)?;
                match decoding(|d| d.paths.get(&offset).cloned()).flatten() {
                    Some(p) => Ok(p),
                    None => decode_at(offset, |b| path_decode(b)),
                }
            }
            DEF => {
                let path = ModPath(Pack::decode(sub)?);
                decoding(|d| d.paths.insert(at, path.clone()));
                Ok(path)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

/// A resolution cell is an object: written once with its contents
/// through `resolved`, referenced afterwards, so references that share
/// a cell share it again after decode.
pub(crate) fn refcell_len(
    cell: &RefCell,
    resolved_len: impl FnOnce(&ResolvedRef) -> usize,
) -> usize {
    let key = Arc::as_ptr(cell) as usize;
    if measured_before(key, |e| e.refcells.get(&key).copied()) {
        return 1 + varint_len(key as u64);
    }
    // The resolved type can reach this cell again; the lock is not
    // reentrant, so clone out before walking.
    let resolved = cell.lock().clone();
    1 + 1 + resolved.map_or(0, |r| resolved_len(&r))
}

pub(crate) fn refcell_encode<B: BufMut>(
    cell: &RefCell,
    buf: &mut B,
    resolved_encode: impl FnOnce(&ResolvedRef, &mut B) -> Result<(), PackError>,
) -> Result<(), PackError> {
    let key = Arc::as_ptr(cell) as usize;
    if let Some(offset) = encoding(|e| e.refcells.get(&key).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(offset, buf);
        return Ok(());
    }
    let at = encoding(|e| e.written).unwrap_or(0);
    buf.put_u8(DEF);
    encoding(|e| {
        e.refcells.insert(key, at);
        e.pinned_refcells.push(cell.clone());
    });
    let resolved = cell.lock().clone();
    match resolved {
        None => buf.put_u8(0),
        Some(r) => {
            buf.put_u8(1);
            resolved_encode(&r, buf)?;
        }
    }
    Ok(())
}

pub(crate) fn refcell_decode(
    buf: &mut impl Buf,
    resolved_decode: impl Fn(&mut &[u8]) -> Result<ResolvedRef, PackError> + Copy,
) -> Result<RefCell, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = decode_varint(sub)?;
                match decoding(|d| d.refcells.get(&offset).cloned()).flatten() {
                    Some(c) => Ok(c),
                    None => decode_at(offset, |b| refcell_decode(b, resolved_decode)),
                }
            }
            DEF => {
                // Entered before its contents: the resolved type can
                // reach this cell again.
                let cell: RefCell = Arc::new(Mutex::new(None));
                decoding(|d| d.refcells.insert(at, cell.clone()));
                if !sub.has_remaining() {
                    return Err(PackError::BufferShort);
                }
                match sub.get_u8() {
                    0 => {}
                    1 => *cell.lock() = Some(Arc::new(resolved_decode(sub)?)),
                    _ => return Err(PackError::UnknownTag),
                }
                Ok(cell)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

pub(crate) fn flags_len(_flags: BitFlags<CFlag>) -> usize {
    8
}

pub(crate) fn flags_encode(
    flags: BitFlags<CFlag>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    buf.put_u64(flags.bits());
    Ok(())
}

pub(crate) fn flags_decode(buf: &mut impl Buf) -> Result<BitFlags<CFlag>, PackError> {
    if buf.remaining() < 8 {
        return Err(PackError::BufferShort);
    }
    BitFlags::from_bits(buf.get_u64()).map_err(|_| PackError::InvalidFormat)
}

/// A dynamic scope is the chain of handlers a `?` sees; each handler is
/// shared by every node under its catch, so it is an object: written
/// once, parent first, its counters pristine before any cycle.
fn dynscope_len(scope: &DynScope) -> usize {
    match scope.handler() {
        None => 1,
        Some(h) => {
            let key = h.identity();
            match encoding(|e| e.handlers.get(&key).copied()).flatten() {
                Some(id) => 1 + varint_len(id),
                None => {
                    let (bind, expr) = h.id();
                    1 + bind.encoded_len()
                        + expr.encoded_len()
                        + dynscope_len(&h.parent())
                }
            }
        }
    }
}

/// A handler outside its scope (a catch's own, a `?`'s resolved one)
/// is the same object the scope codec shares.
pub(crate) fn handler_len(h: &ErrorHandler) -> usize {
    dynscope_len(&DynScope::from_handler(h.clone()))
}

pub(crate) fn handler_encode(
    h: &ErrorHandler,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    dynscope_encode(&DynScope::from_handler(h.clone()), buf)
}

pub(crate) fn handler_decode(buf: &mut impl Buf) -> Result<ErrorHandler, PackError> {
    dynscope_decode(buf)?.handler().ok_or(PackError::InvalidFormat)
}

fn dynscope_encode(scope: &DynScope, buf: &mut impl BufMut) -> Result<(), PackError> {
    let Some(h) = scope.handler() else {
        buf.put_u8(2);
        return Ok(());
    };
    let key = h.identity();
    if let Some(offset) = encoding(|e| e.handlers.get(&key).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(offset, buf);
        return Ok(());
    }
    if h.generation() != 0 || h.has_nested_errors() {
        return Err(PackError::Application(registration::NOT_QUIESCENT));
    }
    let at = encoding(|e| e.written).unwrap_or(0);
    buf.put_u8(DEF);
    encoding(|e| {
        e.handlers.insert(key, at);
        e.pinned_handlers.push(h.clone());
    });
    let (bind, expr) = h.id();
    bind.encode(buf)?;
    expr.encode(buf)?;
    dynscope_encode(&h.parent(), buf)
}

fn dynscope_decode(buf: &mut impl Buf) -> Result<DynScope, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            2 => Ok(DynScope::root()),
            REF => {
                let offset = decode_varint(sub)?;
                match decoding(|d| d.handlers.get(&offset).cloned()).flatten() {
                    Some(h) => Ok(DynScope::from_handler(h)),
                    None => decode_at(offset, |b| dynscope_decode(b)),
                }
            }
            DEF => {
                let bind = BindId::decode(sub)?;
                let expr = ExprId::decode(sub)?;
                let parent = dynscope_decode(sub)?;
                let scope = parent.with_catch((bind, expr));
                let h = scope.handler().expect("with_catch installs a handler");
                decoding(|d| d.handlers.insert(at, h));
                Ok(scope)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

pub(crate) fn scope_len(scope: &Scope) -> usize {
    scope.lexical.encoded_len() + dynscope_len(&scope.dynamic)
}

pub(crate) fn scope_encode(
    scope: &Scope,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    scope.lexical.encode(buf)?;
    dynscope_encode(&scope.dynamic, buf)
}

pub(crate) fn scope_decode(buf: &mut impl Buf) -> Result<Scope, PackError> {
    let lexical = Pack::decode(buf)?;
    let dynamic = dynscope_decode(buf)?;
    Ok(Scope { lexical, dynamic })
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
    let key = Arc::as_ptr(ori) as usize;
    if measured_before(key, |e| e.origins.get(&key).copied()) {
        return 1 + varint_len(key as u64);
    }
    let parent = match &ori.parent {
        Some(p) => origin_len(p),
        None => 0,
    };
    1 + 1 + parent + source_len(&ori.source) + ori.text.encoded_len()
}

pub(crate) fn origin_encode(
    ori: &Arc<Origin>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let key = Arc::as_ptr(ori) as usize;
    if let Some(offset) = encoding(|e| e.origins.get(&key).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(offset, buf);
        return Ok(());
    }
    let at = encoding(|e| e.written).unwrap_or(0);
    buf.put_u8(DEF);
    encoding(|e| {
        e.origins.insert(key, at);
        e.pinned_origins.push(ori.clone());
    });
    match &ori.parent {
        Some(p) => {
            buf.put_u8(1);
            origin_encode(p, buf)?;
        }
        None => buf.put_u8(0),
    }
    source_encode(&ori.source, buf)?;
    ori.text.encode(buf)
}

pub(crate) fn origin_decode(buf: &mut impl Buf) -> Result<Arc<Origin>, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = decode_varint(sub)?;
                match decoding(|d| d.origins.get(&offset).cloned()).flatten() {
                    Some(o) => Ok(o),
                    None => decode_at(offset, |b| origin_decode(b)),
                }
            }
            DEF => {
                if !sub.has_remaining() {
                    return Err(PackError::BufferShort);
                }
                let parent = match sub.get_u8() {
                    0 => None,
                    1 => Some(origin_decode(sub)?),
                    _ => return Err(PackError::UnknownTag),
                };
                let source = source_decode(sub)?;
                let text = Pack::decode(sub)?;
                let ori = Arc::new(Origin { parent, source, text });
                decoding(|d| d.origins.insert(at, ori.clone()));
                Ok(ori)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

/// A type variable under an image: the wrapper (name, id, frozen) and
/// its cell (bound type, constraints, refusal) are separate objects,
/// each written once. Both are registered before their contents, so a
/// cell that reaches its own wrapper through a constraint decodes.
pub(crate) fn tvar_len(tv: &TVar) -> usize {
    let key = tv.wrapper_addr();
    if measured_before(key, |e| e.tvars.get(&key).copied()) {
        return 1 + varint_len(key as u64);
    }
    let (id, frozen, cell) = tv.parts();
    let mut n = 1 + tv.name.encoded_len() + id.encoded_len() + frozen.encoded_len();
    let ckey = Arc::as_ptr(&cell) as usize;
    n += if measured_before(ckey, |e| e.cells.get(&ckey).copied()) {
        1 + varint_len(ckey as u64)
    } else {
        let (typ, constraints, refused) = {
            let c = cell.read();
            (c.typ.clone(), c.constraints.to_vec(), c.cycle_refused)
        };
        1 + typ.encoded_len() + constraints.encoded_len() + refused.encoded_len()
    };
    n
}

/// Whether the length walk has already measured (or an encode has
/// written) the object at `key`; the first call marks it measured. A
/// reference's varint is bounded by the address's, which a length pass
/// may over-count.
fn measured_before(
    key: usize,
    registered: impl FnOnce(&ImageEncoder) -> Option<u64>,
) -> bool {
    encoding(|e| registered(e).is_some() || !e.measured.insert(key)).unwrap_or(false)
}

/// A boxed slice on the wire as the `Vec` it decodes to.
pub(crate) fn slice_len<T: Pack>(xs: &[T]) -> usize {
    varint_len(xs.len() as u64) + xs.iter().map(|x| x.encoded_len()).sum::<usize>()
}

pub(crate) fn slice_encode<T: Pack>(
    xs: &[T],
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    encode_varint(xs.len() as u64, buf);
    for x in xs {
        x.encode(buf)?;
    }
    Ok(())
}

/// The address of an object a session writes once and references
/// afterwards.
pub(crate) fn key<T>(object: &T) -> usize {
    (object as *const T).addr()
}

/// Run `f` over the buffer's contiguous remainder, which is a slice of
/// the image, and advance the buffer by what `f` consumed.
pub(crate) fn with_slice<T>(
    buf: &mut impl Buf,
    f: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    let mut sub = buf.chunk();
    let len = sub.len();
    let r = f(&mut sub);
    let consumed = len - sub.len();
    buf.advance(consumed);
    r
}

/// The offset in the image of the next byte `sub` reads.
pub(crate) fn position(sub: &[u8]) -> Result<u64, PackError> {
    let (base, len) = decoding(|d| (d.image.as_ptr() as usize, d.image.len()))
        .ok_or(PackError::InvalidFormat)?;
    let at = sub.as_ptr() as usize;
    if at < base || at > base + len {
        return Err(PackError::InvalidFormat);
    }
    Ok((at - base) as u64)
}

/// Decode the object defined at `offset` with `full`, its whole codec,
/// which enters it in its table as a side effect.
pub(crate) fn decode_at<T>(
    offset: u64,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    let image = decoding(|d| d.image.clone()).ok_or(PackError::InvalidFormat)?;
    let offset = offset as usize;
    if offset >= image.len() {
        return Err(PackError::InvalidFormat);
    }
    let mut sub = &image[offset..];
    full(&mut sub)
}

/// A content-keyed object's progress through the two passes. An object
/// can reach itself through a resolution cell it contains; while its
/// definition is in progress the nested occurrence is written as a
/// definition too, since a reference can only name a finished one.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ContentState {
    Measuring,
    Measured,
    Writing,
    Written(u64),
}

pub(crate) type ContentTable = AHashMap<Box<[u8]>, ContentState>;

/// The length bound of a reference: an image offset fits a `u32`, which
/// the encode enforces.
fn ref_len() -> usize {
    1 + varint_len(u32::MAX as u64)
}

/// The image length of an object keyed by its canonical bytes: its
/// `contents` at the first sight, a reference afterwards.
pub(crate) fn content_len(
    key: &[u8],
    table: impl Fn(&mut ImageEncoder) -> &mut ContentTable,
    contents: impl FnOnce() -> usize,
) -> usize {
    let state = encoding(|e| {
        let table = table(e);
        match table.get(key) {
            Some(s) => Some(*s),
            None => {
                table.insert(key.into(), ContentState::Measuring);
                None
            }
        }
    });
    match state {
        Some(Some(ContentState::Measured)) | Some(Some(ContentState::Written(_))) => {
            ref_len()
        }
        Some(None) => {
            let len = 1 + contents();
            encoding(|e| {
                if let Some(s) = table(e).get_mut(key) {
                    *s = ContentState::Measured;
                }
            });
            len
        }
        Some(Some(ContentState::Measuring))
        | Some(Some(ContentState::Writing))
        | None => 1 + contents(),
    }
}

/// Write an object keyed by its canonical bytes once and a reference to
/// its offset afterwards.
pub(crate) fn content_encode<B: BufMut>(
    key: &[u8],
    table: impl Fn(&mut ImageEncoder) -> &mut ContentTable,
    buf: &mut B,
    contents: impl FnOnce(&mut B) -> Result<(), PackError>,
) -> Result<(), PackError> {
    let state = encoding(|e| table(e).get(key).copied()).flatten();
    if let Some(ContentState::Written(offset)) = state {
        buf.put_u8(REF);
        encode_varint(offset, buf);
        return Ok(());
    }
    let at = encoding(|e| e.written).unwrap_or(0);
    if at > u32::MAX as u64 {
        return Err(PackError::InvalidFormat);
    }
    buf.put_u8(DEF);
    if state == Some(ContentState::Writing) {
        return contents(buf);
    }
    let set = |state: ContentState| {
        encoding(|e| match table(e).get_mut(key) {
            Some(s) => *s = state,
            None => {
                table(e).insert(key.into(), state);
            }
        });
    };
    set(ContentState::Writing);
    contents(buf)?;
    set(ContentState::Written(at));
    Ok(())
}

/// Append the canonical bytes of a shared type node, walking it once
/// per session.
pub(crate) fn shared_key(ptr: usize, out: &mut Vec<u8>, walk: impl FnOnce(&mut Vec<u8>)) {
    let hit = encoding(|e| match e.type_keys.get(&ptr) {
        Some(k) => {
            out.extend_from_slice(k);
            true
        }
        None => false,
    })
    .unwrap_or(false);
    if hit {
        return;
    }
    let start = out.len();
    walk(out);
    let key: Box<[u8]> = out[start..].into();
    encoding(|e| {
        e.type_keys.insert(ptr, key);
    });
}

/// Build a key in a buffer the session keeps for reuse and run `f`
/// over it; `f` may build nested keys.
fn with_key<R>(walk: impl FnOnce(&mut Vec<u8>), f: impl FnOnce(&[u8]) -> R) -> R {
    let mut key = encoding(|e| e.key_scratch.pop()).flatten().unwrap_or_default();
    key.clear();
    walk(&mut key);
    let r = f(&key);
    encoding(|e| e.key_scratch.push(key));
    r
}

pub(crate) fn type_len(t: &Type, contents: impl FnOnce() -> usize) -> usize {
    with_key(|key| t.content_key(key), |key| content_len(key, |e| &mut e.types, contents))
}

pub(crate) fn type_encode<B: BufMut>(
    t: &Type,
    buf: &mut B,
    contents: impl FnOnce(&mut B) -> Result<(), PackError>,
) -> Result<(), PackError> {
    with_key(
        |key| t.content_key(key),
        |key| content_encode(key, |e| &mut e.types, buf, contents),
    )
}

pub(crate) fn fntype_len(t: &FnType, contents: impl FnOnce() -> usize) -> usize {
    with_key(
        |key| t.content_key(key),
        |key| content_len(key, |e| &mut e.fntypes, contents),
    )
}

pub(crate) fn fntype_encode<B: BufMut>(
    t: &FnType,
    buf: &mut B,
    contents: impl FnOnce(&mut B) -> Result<(), PackError>,
) -> Result<(), PackError> {
    with_key(
        |key| t.content_key(key),
        |key| content_encode(key, |e| &mut e.fntypes, buf, contents),
    )
}

/// The address an expression is keyed by: its own, or that of the
/// first expression seen with its id and contents.
pub(crate) fn expr_key(e: &Expr) -> usize {
    let addr = key(e);
    encoding(|enc| {
        if let Some(canonical) = enc.expr_alias.get(&addr) {
            return *canonical;
        }
        let canonical = match enc.exprs_by_id.get(&e.id) {
            // Everything a session encodes stays borrowed for the
            // session, so the first expression is still there.
            Some(&first) if unsafe { &*(first as *const Expr) }.same_tree(e) => first,
            Some(_) => addr,
            None => {
                enc.exprs_by_id.insert(e.id, addr);
                addr
            }
        };
        enc.expr_alias.insert(addr, canonical);
        canonical
    })
    .unwrap_or(addr)
}

/// The image length of an object in the table `table` selects: its
/// `contents` at the first sight, a reference afterwards.
pub(crate) fn object_len(
    key: usize,
    table: impl FnOnce(&ImageEncoder) -> &AHashMap<usize, u64>,
    contents: impl FnOnce() -> usize,
) -> usize {
    if measured_before(key, |e| table(e).get(&key).copied()) {
        1 + varint_len(key as u64)
    } else {
        1 + contents()
    }
}

/// Write an object once, at an offset its table records before its
/// contents, and a reference to that offset afterwards.
pub(crate) fn object_encode<B: BufMut>(
    key: usize,
    table: impl Fn(&mut ImageEncoder) -> &mut AHashMap<usize, u64>,
    buf: &mut B,
    contents: impl FnOnce(&mut B) -> Result<(), PackError>,
) -> Result<(), PackError> {
    if let Some(offset) = encoding(|e| table(e).get(&key).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(offset, buf);
        return Ok(());
    }
    let at = encoding(|e| e.written).unwrap_or(0);
    buf.put_u8(DEF);
    encoding(|e| {
        table(e).insert(key, at);
    });
    contents(buf)
}

/// Read an object written by [`object_encode`]: a reference clones the
/// table's entry or decodes the definition at its offset with `full`;
/// a definition decodes `contents` and enters it at its offset.
pub(crate) fn object_decode<T: Clone>(
    buf: &mut impl Buf,
    table: impl Fn(&mut ImageDecoder) -> &mut AHashMap<u64, T>,
    contents: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = decode_varint(sub)?;
                match decoding(|d| table(d).get(&offset).cloned()).flatten() {
                    Some(v) => Ok(v),
                    None => decode_at(offset, full),
                }
            }
            DEF => {
                let v = contents(sub)?;
                decoding(|d| table(d).insert(at, v.clone()));
                Ok(v)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

pub(crate) fn tvar_encode(tv: &TVar, buf: &mut impl BufMut) -> Result<(), PackError> {
    let key = tv.wrapper_addr();
    if let Some(offset) = encoding(|e| e.tvars.get(&key).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(offset, buf);
        return Ok(());
    }
    let at = encoding(|e| e.written).unwrap_or(0);
    buf.put_u8(DEF);
    encoding(|e| {
        e.tvars.insert(key, at);
        e.pinned_tvars.push(tv.clone());
    });
    let (id, frozen, cell) = tv.parts();
    tv.name.encode(buf)?;
    id.encode(buf)?;
    frozen.encode(buf)?;
    cell_encode(&cell, buf)
}

fn cell_encode(
    cell: &Arc<RwLock<TCell>>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let ckey = Arc::as_ptr(cell) as usize;
    if let Some(offset) = encoding(|e| e.cells.get(&ckey).copied()).flatten() {
        buf.put_u8(REF);
        encode_varint(offset, buf);
        return Ok(());
    }
    let at = encoding(|e| e.written).unwrap_or(0);
    buf.put_u8(DEF);
    encoding(|e| {
        e.cells.insert(ckey, at);
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
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = decode_varint(sub)?;
                match decoding(|d| d.tvars.get(&offset).cloned()).flatten() {
                    Some(tv) => Ok(tv),
                    None => decode_at(offset, |b| tvar_decode(b)),
                }
            }
            DEF => {
                let name = Pack::decode(sub)?;
                let id = TVarId::decode(sub)?;
                let frozen = bool::decode(sub)?;
                let cell = cell_decode(sub)?;
                let tv = TVar::from_parts(name, id, frozen, cell);
                decoding(|d| d.tvars.insert(at, tv.clone()));
                Ok(tv)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

fn cell_decode(buf: &mut impl Buf) -> Result<Arc<RwLock<TCell>>, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = decode_varint(sub)?;
                match decoding(|d| d.cells.get(&offset).cloned()).flatten() {
                    Some(c) => Ok(c),
                    None => decode_at(offset, |b| cell_decode(b)),
                }
            }
            DEF => {
                // Entered before its contents: a bound type can reach
                // the cell again.
                let cell = Arc::new(RwLock::new(TCell::default()));
                decoding(|d| d.cells.insert(at, cell.clone()));
                let typ = Pack::decode(sub)?;
                let constraints: Vec<_> = Pack::decode(sub)?;
                let refused = bool::decode(sub)?;
                let mut c = cell.write();
                c.typ = typ;
                c.constraints = constraints.into_iter().collect();
                c.cycle_refused = refused;
                drop(c);
                Ok(cell)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        expr::{Expr, ExprKind},
        typ::Type,
    };
    use arcstr::literal;
    use bytes::Bytes;
    use netidx_value::Value;

    fn pack_all<T: Pack>(items: &[T], enc: &mut ImageEncoder) -> Bytes {
        let bounds: Vec<usize> = {
            let _s = EncodeImage::new(enc);
            items.iter().map(|i| i.encoded_len()).collect()
        };
        enc.sort_ids();
        let _s = EncodeImage::new(enc);
        let mut buf = ImageBuf::with_capacity(0);
        for (i, bound) in items.iter().zip(bounds) {
            let before = buf.len();
            i.encode(&mut buf).unwrap();
            assert!(buf.len() - before <= bound);
        }
        buf.freeze()
    }

    fn decoder(counts: IdCounts, image: &Bytes) -> ImageDecoder {
        let mut dec = ImageDecoder::new(counts);
        dec.set_image(image.clone());
        dec
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
        let mut dec = decoder(enc.counts(), &bytes);
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
        let mut dec = decoder(enc.counts(), &bytes);
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
    fn types_share_by_content() {
        use netidx_value::Typ;
        let a = TVar::empty_named(literal!("a"));
        let tuple = |tv: &TVar| {
            Type::Tuple(Arc::from_iter([
                Type::Primitive(Typ::I64.into()),
                Type::TVar(tv.clone()),
            ]))
        };
        let (t1, t2) = (tuple(&a), tuple(&a));
        let other = tuple(&TVar::empty_named(literal!("a")));
        assert!(!Arc::ptr_eq(
            match &t1 {
                Type::Tuple(x) => x,
                _ => unreachable!(),
            },
            match &t2 {
                Type::Tuple(x) => x,
                _ => unreachable!(),
            }
        ));
        let mut enc = ImageEncoder::new();
        let bytes = pack_all(&[t1, t2, other], &mut enc);
        // the two tuples, the other tuple, the primitive and two variables
        assert_eq!(enc.types.len(), 5);
        let mut dec = decoder(enc.counts(), &bytes);
        let _s = DecodeImage::new(&mut dec);
        let mut b = &bytes[..];
        let d1 = Type::decode(&mut b).unwrap();
        let d2 = Type::decode(&mut b).unwrap();
        let d3 = Type::decode(&mut b).unwrap();
        assert!(!b.has_remaining());
        assert_eq!(d1, d2);
        let (Type::Tuple(x1), Type::Tuple(x2), Type::Tuple(x3)) = (&d1, &d2, &d3) else {
            panic!("{d1:?} {d2:?} {d3:?}")
        };
        assert!(Arc::ptr_eq(x1, x2), "equal types decode to one value");
        assert!(!Arc::ptr_eq(x1, x3));
        let (Type::TVar(v1), Type::TVar(v3)) = (&x1[1], &x3[1]) else { panic!() };
        assert_ne!(
            v1.wrapper_addr(),
            v3.wrapper_addr(),
            "a different variable is a different type"
        );
    }

    #[test]
    fn expression_clones_share_one_definition() {
        let ori = Arc::new(Origin {
            parent: None,
            source: Source::Internal(literal!("test")),
            text: literal!("1 + 2"),
        });
        let child = Expr {
            id: ExprId::new(),
            ori: ori.clone(),
            pos: Default::default(),
            kind: ExprKind::Constant(Value::I64(1)),
            dec: None,
        };
        let parent = Expr {
            id: ExprId::new(),
            ori: ori.clone(),
            pos: Default::default(),
            kind: ExprKind::Array { args: Arc::from_iter([child]) },
            dec: None,
        };
        let clone = parent.clone();
        let ExprKind::Array { args } = &parent.kind else { unreachable!() };
        let child_clone = args[0].clone();
        let mut enc = ImageEncoder::new();
        let bytes = pack_all(&[parent.clone(), clone, child_clone], &mut enc);
        // the parent and its child are the only definitions
        assert_eq!(enc.exprs.len(), 2);
        let mut dec = decoder(enc.counts(), &bytes);
        let _s = DecodeImage::new(&mut dec);
        let mut b = &bytes[..];
        let d1 = Expr::decode(&mut b).unwrap();
        let d2 = Expr::decode(&mut b).unwrap();
        let d3 = Expr::decode(&mut b).unwrap();
        assert!(!b.has_remaining());
        assert_eq!(d1.id, d2.id);
        assert_eq!(d1, d2);
        let ExprKind::Array { args } = &d1.kind else { panic!("{d1:?}") };
        assert_eq!(args[0].id, d3.id);
        assert_eq!(args[0], d3);
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
        let mut dec = decoder(enc.counts(), &bytes);
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
