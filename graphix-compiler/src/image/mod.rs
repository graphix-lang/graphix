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
pub use nodes::{NOT_IMAGED, decode_node, decode_nodes, encode_nodes, nodes_len};
pub use registration::{NOT_QUIESCENT, ProgramRoot, REGISTRATION_FORMAT, Registration};

use crate::{
    BindId, CFlag, DynScope, ErrorHandler, FastCall, LambdaId, LambdaInstanceId, Scope,
    SourcePosition,
    expr::{Expr, ExprId, ModPath, Origin, Source, WrittenAt},
    fusion::{
        emit::BodyRecord,
        kernel_abi::{KernelSig, SiteLeaf},
    },
    ids::{IdRelocation, IdSpan},
    shared_map,
    typ::{
        FnType, ResolvedRef, TVar, Type,
        tvar::{TCell, TVarId},
    },
};
use ahash::AHashMap;
use arcstr::ArcStr;
use bytes::{Buf, BufMut, Bytes, BytesMut};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use parking_lot::{Mutex, RwLock};
use smallvec::SmallVec;
use std::{cell::Cell, marker::PhantomData, path::PathBuf, ptr::NonNull};
use triomphe::Arc;

pub(crate) const REF: u8 = 0;
pub(crate) const DEF: u8 = 1;
/// A dynamic scope's tag for the root, beside [`REF`] and [`DEF`].
const ROOT: u8 = 2;

/// One value per relocated id domain.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PerDomain<T> {
    pub bind: T,
    pub lambda: T,
    pub instance: T,
    pub expr: T,
    pub tvar: T,
}

impl<T: Copy> PerDomain<T> {
    fn each(&self) -> [T; 5] {
        let PerDomain { bind, lambda, instance, expr, tvar } = *self;
        [bind, lambda, instance, expr, tvar]
    }

    fn from_each([bind, lambda, instance, expr, tvar]: [T; 5]) -> Self {
        PerDomain { bind, lambda, instance, expr, tvar }
    }

    fn map<U: Copy>(&self, f: impl FnMut(T) -> U) -> PerDomain<U> {
        PerDomain::from_each(self.each().map(f))
    }
}

/// The span of each relocated id domain an image holds; the decoder
/// reserves a block of each.
pub type IdCounts = PerDomain<IdSpan>;

impl Pack for IdCounts {
    fn encoded_len(&self) -> usize {
        self.each().iter().map(|s| varint_len(s.floor) + varint_len(s.extent)).sum()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        for s in self.each() {
            encode_varint(s.floor, buf);
            encode_varint(s.extent, buf);
        }
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let mut each = [IdSpan::default(); 5];
        for s in each.iter_mut() {
            *s = IdSpan { floor: decode_varint(buf)?, extent: decode_varint(buf)? };
        }
        Ok(IdCounts::from_each(each))
    }
}

/// Each domain's relocation on this thread.
type Relocations = PerDomain<Option<IdRelocation>>;

impl Relocations {
    /// Install these relocations and return the ones they replace.
    fn install(self) -> Self {
        PerDomain {
            bind: BindId::set_relocation(self.bind),
            lambda: LambdaId::set_relocation(self.lambda),
            instance: LambdaInstanceId::set_relocation(self.instance),
            expr: ExprId::set_relocation(self.expr),
            tvar: TVarId::set_relocation(self.tvar),
        }
    }
}

/// The image under construction, and the buffer an object's definition
/// is written into before it joins the definitions area.
#[derive(Default)]
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

// XCR claude for eric: a lifetime on the encoder cannot reach the node borrow:
// a call site finds the encoder through the thread-local, which erases it. The
// session now drops what it deferred when it ends (`EncodeImage::drop`), so a
// closure lives no longer than the borrows the session contract requires.
/// An instance body the eager part of the image skipped: written
/// after it, in the heap, at an offset the instance table records.
type Deferred = Box<dyn FnOnce(&mut ImageBuf) -> Result<(), PackError>>;

unsafe impl BufMut for ImageBuf {
    fn remaining_mut(&self) -> usize {
        self.0.remaining_mut()
    }

    unsafe fn advance_mut(&mut self, cnt: usize) {
        unsafe { self.0.advance_mut(cnt) }
    }

    fn chunk_mut(&mut self) -> &mut bytes::buf::UninitSlice {
        self.0.chunk_mut()
    }

    fn put_slice(&mut self, src: &[u8]) {
        self.0.put_slice(src)
    }
}

/// An address-keyed object's table entry holds the object (`P`), so
/// its address names it alone for the session.
pub(crate) type Table<K, P = ()> = AHashMap<K, Slot<P>>;

#[derive(Default)]
pub struct ImageEncoder {
    /// Persistent map nodes by identity.
    pub(crate) map_nodes: Table<usize, Box<dyn std::any::Any + Send + Sync>>,
    ids: IdCounts,
    /// The next ordinal an object meets at its first sight.
    next_ordinal: u32,
    /// Every definition written, each where its offset says: appended
    /// to the image after everything that references it (`finish`).
    defs: ImageBuf,
    /// Each definition's offset in `defs` by ordinal.
    offsets: Vec<Option<u64>>,
    /// What the definitions measured at first sight add up to.
    pub(crate) defs_len: usize,
    /// Buffers for definitions being written, one per nesting level.
    scratch: Vec<ImageBuf>,
    handlers: Table<usize, ErrorHandler>,
    paths: Table<ArcStr>,
    refcells: Table<usize, RefCell>,
    origins: Table<usize, Arc<Origin>>,
    tvars: Table<usize, TVar>,
    cells: Table<usize, Arc<RwLock<TCell>>>,
    /// Expressions by the address of the session's clone
    /// ([`expr_key`]).
    pub(crate) exprs: Table<usize>,
    /// Kernel signatures, slot-chain leaves and body records by `Arc`.
    pub(crate) kernel_sigs: Table<usize, triomphe::Arc<KernelSig>>,
    pub(crate) site_leaves: Table<usize, triomphe::Arc<SiteLeaf>>,
    pub(crate) records: Table<usize, triomphe::Arc<BodyRecord>>,
    /// The distinct trees seen with each id, as the session's own
    /// clones (a clone shares its children): an expression is keyed by
    /// the address of the clone it matches, so a caller's address is
    /// never a key and a reused one names nothing.
    exprs_by_id: AHashMap<ExprId, SmallVec<[Box<Expr>; 1]>>,
    /// Types and function types by their canonical bytes, every shared
    /// leaf (a variable, a resolution cell, an origin) by identity:
    /// equal types decode to one shared value.
    pub(crate) types: ContentTable,
    pub(crate) fntypes: ContentTable,
    /// The canonical bytes of every shared type node met so far, by
    /// the address of its `Arc`, so a key walk stops at shared subtrees.
    type_keys: AHashMap<usize, (KeyedNode, Box<[u8]>)>,
    /// Key buffers free for the next key walk, one per nesting level.
    key_scratch: Vec<Vec<u8>>,
    /// Whether a call site writes its instance into the heap, for a
    /// first dispatch to decode, rather than inline.
    pub(crate) defer_instances: bool,
    pub(crate) deferred: Vec<(LambdaInstanceId, Deferred)>,
    /// The measured size of every deferred body, for the length bound.
    pub(crate) deferred_len: usize,
    pub(crate) instances: AHashMap<LambdaInstanceId, u64>,
    /// Every deferred instance's reference summary, walked once: the
    /// measure passes and the encode all write it.
    pub(crate) instance_refs:
        AHashMap<LambdaInstanceId, crate::node::callsite::RefsSummary>,
}

impl ImageEncoder {
    pub fn new() -> Self {
        Self::default()
    }

    /// The span of the ids written so far, per domain. The image
    /// writer stores these ahead of the body; read between sessions.
    pub fn counts(&self) -> IdCounts {
        self.ids
    }

    /// Append the definitions to `buf`, the image, and return each
    /// one's offset in it by ordinal, for the trailer; `u64::MAX` for
    /// an object measured and never written.
    pub fn finish(&mut self, buf: &mut ImageBuf) -> Vec<u64> {
        let base = buf.len() as u64;
        buf.put_slice(&self.defs.0);
        self.defs.0.clear();
        self.offsets.drain(..).map(|at| at.map_or(u64::MAX, |at| base + at)).collect()
    }

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

// XCR claude for eric: the encoder derives Default; the decoder keeps its
// explicit `new`, since a defaulted decoder would decode ids unrelocated onto
// ids this process minted.
/// The objects an image session has built, each by the offset of its
/// definition in the image, and the image itself, so a reference to
/// an object not built yet decodes it from there.
pub struct ImageDecoder {
    pub(crate) maps: shared_map::DecodeTable,
    image: Bytes,
    /// Every definition's offset by ordinal, from the trailer.
    offsets: Vec<u64>,
    /// How many objects the session has entered in its tables.
    built: usize,
    /// Each definition being decoded, by offset, with [`Self::built`] at
    /// its latest entry (see [`decode_at`]).
    active: AHashMap<u64, usize>,
    handlers: AHashMap<u64, ErrorHandler>,
    paths: AHashMap<u64, ModPath>,
    refcells: AHashMap<u64, RefCell>,
    origins: AHashMap<u64, Arc<Origin>>,
    tvars: AHashMap<u64, TVar>,
    cells: AHashMap<u64, Arc<RwLock<TCell>>>,
    pub(crate) exprs: AHashMap<u64, Expr>,
    pub(crate) types: AHashMap<u64, Type>,
    pub(crate) fntypes: AHashMap<u64, FnType>,
    pub(crate) kernel_sigs: AHashMap<u64, triomphe::Arc<KernelSig>>,
    pub(crate) site_leaves: AHashMap<u64, triomphe::Arc<SiteLeaf>>,
    pub(crate) records: AHashMap<u64, triomphe::Arc<BodyRecord>>,
    /// The builtins' fast fns by name, for a kernel constant's recipe.
    fastcalls: AHashMap<&'static str, FastCall>,
    instances: AHashMap<LambdaInstanceId, u64>,
    relocations: Relocations,
}

impl ImageDecoder {
    /// Enter the object defined at `at` in its table.
    fn enter<T>(
        &mut self,
        table: impl Fn(&mut Self) -> &mut AHashMap<u64, T>,
        at: u64,
        v: T,
    ) {
        self.built += 1;
        table(self).insert(at, v);
    }

    /// Reserves a block of each id domain for the image's ids; refuses
    /// a span no block can hold.
    pub fn new(counts: IdCounts) -> Result<Self, PackError> {
        let reserve =
            |r: Option<IdRelocation>| r.ok_or(PackError::InvalidFormat).map(Some);
        let relocations = PerDomain {
            bind: reserve(BindId::reserve(counts.bind))?,
            lambda: reserve(LambdaId::reserve(counts.lambda))?,
            instance: reserve(LambdaInstanceId::reserve(counts.instance))?,
            expr: reserve(ExprId::reserve(counts.expr))?,
            tvar: reserve(TVarId::reserve(counts.tvar))?,
        };
        Ok(ImageDecoder {
            maps: shared_map::DecodeTable::default(),
            image: Bytes::new(),
            offsets: Vec::new(),
            built: 0,
            active: AHashMap::new(),
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
            relocations,
        })
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

    pub fn set_offsets(&mut self, offsets: Vec<u64>) {
        self.offsets = offsets;
    }

    pub(crate) fn set_fastcalls(&mut self, fastcalls: AHashMap<&'static str, FastCall>) {
        self.fastcalls = fastcalls;
    }

    pub(crate) fn fastcall(&self, name: &str) -> Option<FastCall> {
        self.fastcalls.get(name).copied()
    }

    /// Size the object tables for what the eager part defines; no
    /// count can exceed the image's bytes, one per definition at least.
    pub fn reserve(&mut self, counts: ObjectCounts) {
        let n = |c: u64| c.min(self.image.len() as u64) as usize;
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
        let (exprs, types, fntypes, paths) = (n(exprs), n(types), n(fntypes), n(paths));
        let (tvars, cells, refcells) = (n(tvars), n(cells), n(refcells));
        let (origins, handlers) = (n(origins), n(handlers));
        self.exprs.reserve(exprs);
        self.types.reserve(types);
        self.fntypes.reserve(fntypes);
        self.paths.reserve(paths);
        self.tvars.reserve(tvars);
        self.cells.reserve(cells);
        self.refcells.reserve(refcells);
        self.origins.reserve(origins);
        self.handlers.reserve(handlers);
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

fn span(r: Option<IdRelocation>) -> IdSpan {
    match r {
        Some(IdRelocation::Encode(span)) => span,
        _ => IdSpan::default(),
    }
}

/// An encode session: `encoder` is installed on this thread for the
/// closure [`EncodeImage::with`] runs. The id spans accumulate across
/// every session over one encoder. Sessions nest, innermost wins.
pub struct EncodeImage<'a> {
    encoder: NonNull<ImageEncoder>,
    prev: Option<NonNull<ImageEncoder>>,
    prev_ids: Relocations,
    _encoder: PhantomData<&'a mut ImageEncoder>,
}

impl<'a> EncodeImage<'a> {
    /// Run `f` with `encoder` installed.
    pub fn with<T>(encoder: &'a mut ImageEncoder, f: impl FnOnce() -> T) -> T {
        let _session = Self::new(encoder);
        f()
    }

    fn new(encoder: &'a mut ImageEncoder) -> Self {
        let ids = encoder.ids.map(|s| Some(IdRelocation::Encode(s)));
        let encoder = NonNull::from(encoder);
        let prev = ENCODER.replace(Some(encoder));
        let prev_ids = ids.install();
        EncodeImage { encoder, prev, prev_ids, _encoder: PhantomData }
    }
}

impl Drop for EncodeImage<'_> {
    fn drop(&mut self) {
        let ours = self.prev_ids.install();
        ENCODER.set(self.prev);
        // The guard holds the `&mut` this pointer came from.
        let encoder = unsafe { &mut *self.encoder.as_ptr() };
        encoder.ids = ours.map(span);
        encoder.deferred.clear();
    }
}

/// A decode session: `decoder` is installed on this thread for the
/// closure [`DecodeImage::with`] runs.
pub struct DecodeImage<'a> {
    prev: Option<NonNull<ImageDecoder>>,
    prev_ids: Relocations,
    _decoder: PhantomData<&'a mut ImageDecoder>,
}

impl<'a> DecodeImage<'a> {
    /// Run `f` with `decoder` installed.
    pub fn with<T>(decoder: &'a mut ImageDecoder, f: impl FnOnce() -> T) -> T {
        let _session = Self::new(decoder);
        f()
    }

    fn new(decoder: &'a mut ImageDecoder) -> Self {
        let relocations = decoder.relocations;
        let prev = DECODER.replace(Some(NonNull::from(decoder)));
        let prev_ids = relocations.install();
        DecodeImage { prev, prev_ids, _decoder: PhantomData }
    }
}

impl Drop for DecodeImage<'_> {
    fn drop(&mut self) {
        self.prev_ids.install();
        DECODER.set(self.prev);
    }
}

/// Run `f` against the installed encoder, or `None` outside a session.
/// The encoder is out of its slot while `f` runs, so a nested call sees
/// no session and never a second `&mut`.
pub(crate) fn encoding<R>(f: impl FnOnce(&mut ImageEncoder) -> R) -> Option<R> {
    let mut p = ENCODER.take()?;
    // The session guard holds the `&mut` that produced this pointer for
    // as long as it is installed.
    let r = f(unsafe { p.as_mut() });
    ENCODER.set(Some(p));
    Some(r)
}

/// [`encoding`] for the installed decoder.
pub(crate) fn decoding<R>(f: impl FnOnce(&mut ImageDecoder) -> R) -> Option<R> {
    let mut p = DECODER.take()?;
    let r = f(unsafe { p.as_mut() });
    DECODER.set(Some(p));
    Some(r)
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
    object_len(
        path_key(path),
        |k| (ArcStr::from(k), ()),
        |e| &mut e.paths,
        || path.0.encoded_len(),
    )
}

pub(crate) fn path_encode(
    path: &ModPath,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    object_encode(
        path_key(path),
        |k| (ArcStr::from(k), ()),
        |e| &mut e.paths,
        buf,
        |buf| path.0.encode(buf),
    )
}

pub(crate) fn path_decode(buf: &mut impl Buf) -> Result<ModPath, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = ref_offset(sub)?;
                match decoding(|d| d.paths.get(&offset).cloned()).flatten() {
                    Some(p) => Ok(p),
                    None => decode_at(offset, |b| path_decode(b)),
                }
            }
            DEF => {
                let path = ModPath(Pack::decode(sub)?);
                decoding(|d| d.enter(|d| &mut d.paths, at, path.clone()));
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
    object_len(
        &key,
        |k| (*k, cell.clone()),
        |e| &mut e.refcells,
        || {
            // The resolved type can reach this cell again; the lock is not
            // reentrant, so clone out before walking.
            let resolved = cell.lock().clone();
            1 + resolved.map_or(0, |r| resolved_len(&r))
        },
    )
}

pub(crate) fn refcell_encode<B: BufMut>(
    cell: &RefCell,
    buf: &mut B,
    resolved_encode: impl FnOnce(&ResolvedRef, &mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError> {
    let key = Arc::as_ptr(cell) as usize;
    object_encode(
        &key,
        |k| (*k, cell.clone()),
        |e| &mut e.refcells,
        buf,
        |buf| {
            let resolved = cell.lock().clone();
            match resolved {
                None => buf.put_u8(0),
                Some(r) => {
                    buf.put_u8(1);
                    resolved_encode(&r, buf)?;
                }
            }
            Ok(())
        },
    )
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
                let offset = ref_offset(sub)?;
                match decoding(|d| d.refcells.get(&offset).cloned()).flatten() {
                    Some(c) => Ok(c),
                    None => decode_at(offset, |b| refcell_decode(b, resolved_decode)),
                }
            }
            DEF => {
                // Entered before its contents: the resolved type can
                // reach this cell again.
                let cell: RefCell = Arc::new(Mutex::new(None));
                decoding(|d| d.enter(|d| &mut d.refcells, at, cell.clone()));
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
            object_len(
                &key,
                |k| (*k, h.clone()),
                |e| &mut e.handlers,
                || {
                    let (bind, expr) = h.id();
                    bind.encoded_len()
                        + expr.encoded_len()
                        + h.is_machine().encoded_len()
                        + dynscope_len(&h.parent())
                },
            )
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
        buf.put_u8(ROOT);
        return Ok(());
    };
    let key = h.identity();
    object_encode(
        &key,
        |k| (*k, h.clone()),
        |e| &mut e.handlers,
        buf,
        |buf| {
            if h.generation() != 0 || h.has_nested_errors() {
                return Err(PackError::Application(registration::NOT_QUIESCENT));
            }
            let (bind, expr) = h.id();
            bind.encode(buf)?;
            expr.encode(buf)?;
            h.is_machine().encode(buf)?;
            dynscope_encode(&h.parent(), buf)
        },
    )
}

fn dynscope_decode(buf: &mut impl Buf) -> Result<DynScope, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            ROOT => Ok(DynScope::root()),
            REF => {
                let offset = ref_offset(sub)?;
                match decoding(|d| d.handlers.get(&offset).cloned()).flatten() {
                    Some(h) => Ok(DynScope::from_handler(h)),
                    None => decode_at(offset, |b| dynscope_decode(b)),
                }
            }
            DEF => {
                let bind = BindId::decode(sub)?;
                let expr = ExprId::decode(sub)?;
                let machine = bool::decode(sub)?;
                let parent = dynscope_decode(sub)?;
                let scope = parent.with_catch((bind, expr), machine);
                let h = scope.handler().expect("with_catch installs a handler");
                decoding(|d| d.enter(|d| &mut d.handlers, at, h));
                Ok(scope)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

pub fn scope_len(scope: &Scope) -> usize {
    scope.lexical.encoded_len() + dynscope_len(&scope.dynamic)
}

pub fn scope_encode(scope: &Scope, buf: &mut impl BufMut) -> Result<(), PackError> {
    scope.lexical.encode(buf)?;
    dynscope_encode(&scope.dynamic, buf)
}

pub fn scope_decode(buf: &mut impl Buf) -> Result<Scope, PackError> {
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

/// A file path as its bytes, so a path that is not UTF-8 survives.
#[cfg(unix)]
fn path_bytes(p: &std::path::Path) -> Result<&[u8], PackError> {
    use std::os::unix::ffi::OsStrExt;
    Ok(p.as_os_str().as_bytes())
}

#[cfg(not(unix))]
fn path_bytes(p: &std::path::Path) -> Result<&[u8], PackError> {
    p.to_str().map(str::as_bytes).ok_or(PackError::InvalidFormat)
}

#[cfg(unix)]
fn path_from_bytes(b: &[u8]) -> Result<PathBuf, PackError> {
    use std::os::unix::ffi::OsStrExt;
    Ok(PathBuf::from(std::ffi::OsStr::from_bytes(b)))
}

#[cfg(not(unix))]
fn path_from_bytes(b: &[u8]) -> Result<PathBuf, PackError> {
    std::str::from_utf8(b).map(PathBuf::from).map_err(|_| PackError::InvalidFormat)
}

fn source_len(s: &Source) -> usize {
    1 + match s {
        Source::File(p) => {
            path_bytes(p).map_or(0, |b| varint_len(b.len() as u64) + b.len())
        }
        Source::Netidx(p) => p.encoded_len(),
        Source::Internal(s) => s.encoded_len(),
        Source::Unspecified => 0,
    }
}

fn source_encode(s: &Source, buf: &mut impl BufMut) -> Result<(), PackError> {
    match s {
        Source::File(p) => {
            let b = path_bytes(p)?;
            buf.put_u8(0);
            encode_varint(b.len() as u64, buf);
            buf.put_slice(b);
            Ok(())
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
        0 => {
            let n = decode_varint(buf)? as usize;
            if buf.remaining() < n || buf.chunk().len() < n {
                return Err(PackError::BufferShort);
            }
            let p = path_from_bytes(&buf.chunk()[..n])?;
            buf.advance(n);
            Ok(Source::File(p))
        }
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
    object_len(
        &key,
        |k| (*k, ori.clone()),
        |e| &mut e.origins,
        || {
            let parent = match &ori.parent {
                Some(p) => origin_len(p),
                None => 0,
            };
            1 + parent + source_len(&ori.source) + ori.text.encoded_len()
        },
    )
}

pub(crate) fn origin_encode(
    ori: &Arc<Origin>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let key = Arc::as_ptr(ori) as usize;
    object_encode(
        &key,
        |k| (*k, ori.clone()),
        |e| &mut e.origins,
        buf,
        |buf| {
            match &ori.parent {
                Some(p) => {
                    buf.put_u8(1);
                    origin_encode(p, buf)?;
                }
                None => buf.put_u8(0),
            }
            source_encode(&ori.source, buf)?;
            ori.text.encode(buf)
        },
    )
}

pub(crate) fn origin_decode(buf: &mut impl Buf) -> Result<Arc<Origin>, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = ref_offset(sub)?;
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
                decoding(|d| d.enter(|d| &mut d.origins, at, ori.clone()));
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
    object_len(
        &key,
        |k| (*k, tv.clone()),
        |e| &mut e.tvars,
        || {
            let (id, frozen, cell) = tv.parts();
            tv.name.encoded_len()
                + id.encoded_len()
                + frozen.encoded_len()
                + cell_len(&cell)
        },
    )
}

fn cell_len(cell: &Arc<RwLock<TCell>>) -> usize {
    let key = Arc::as_ptr(cell) as usize;
    object_len(
        &key,
        |k| (*k, cell.clone()),
        |e| &mut e.cells,
        || {
            let (typ, constraints, refused) = {
                let c = cell.read();
                (c.binding.clone(), c.constraints.to_vec(), c.cycle_refused)
            };
            typ.encoded_len() + slice_len(&constraints) + refused.encoded_len()
        },
    )
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
/// which enters it in its table as a side effect. A decode that meets
/// the object it is inside builds it again from here. Every cycle of a
/// valid image passes through a kind entered before its contents, so it
/// enters an object before it meets a definition again; an entry with
/// nothing entered since that definition's last is a cycle in the image.
pub(crate) fn decode_at<T>(
    offset: u64,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    let image = decoding(|d| d.image.clone()).ok_or(PackError::InvalidFormat)?;
    let at = usize::try_from(offset)
        .ok()
        .filter(|at| *at < image.len())
        .ok_or(PackError::InvalidFormat)?;
    let prev = decoding(|d| {
        let built = d.built;
        match d.active.insert(offset, built) {
            Some(prev) if prev == built => None,
            prev => Some(prev),
        }
    })
    .flatten()
    .ok_or(PackError::InvalidFormat)?;
    let r = crate::stack::ensure_sufficient(|| full(&mut &image[at..]));
    decoding(|d| match prev {
        Some(p) => d.active.insert(offset, p),
        None => d.active.remove(&offset),
    });
    r
}

/// An object's place in the session: the ordinal every occurrence
/// names, assigned at first sight, whether its definition has been
/// written, and what keeps the object alive for the session.
pub(crate) struct Slot<P = ()> {
    ord: u32,
    defined: bool,
    _pin: P,
}

pub(crate) type ContentTable = Table<Box<[u8]>>;

fn new_ordinal(e: &mut ImageEncoder) -> u32 {
    let ord = e.next_ordinal;
    e.next_ordinal += 1;
    e.offsets.push(None);
    ord
}

/// The image length of the object at `key`: a reference, always, so a
/// length is exact whatever was measured or written before it. At the
/// first sight `owned` builds the table's key and pin, and `contents`
/// measures the definition, which is written elsewhere. An image
/// object has no encoding outside a session.
pub(crate) fn object_len<K, Q, P>(
    key: &Q,
    owned: impl FnOnce(&Q) -> (K, P),
    table: impl Fn(&mut ImageEncoder) -> &mut Table<K, P>,
    contents: impl FnOnce() -> usize,
) -> usize
where
    K: std::hash::Hash + Eq + std::borrow::Borrow<Q>,
    Q: std::hash::Hash + Eq + ?Sized,
{
    let seen = encoding(|e| match table(e).get(key) {
        Some(s) => (s.ord, false),
        None => {
            let ord = new_ordinal(e);
            let (k, _pin) = owned(key);
            table(e).insert(k, Slot { ord, defined: false, _pin });
            (ord, true)
        }
    });
    let Some((ord, first)) = seen else { return 0 };
    if first {
        let len = 1 + crate::stack::ensure_sufficient(contents);
        encoding(|e| e.defs_len += len);
    }
    1 + varint_len(ord as u64)
}

/// Write a reference to the object and, the first time, its definition
/// to the definitions area. The object is defined before its contents
/// are written, so an occurrence of it inside them is a reference too.
/// Outside a session, an error.
pub(crate) fn object_encode<K, Q, P, B: BufMut>(
    key: &Q,
    owned: impl FnOnce(&Q) -> (K, P),
    table: impl Fn(&mut ImageEncoder) -> &mut Table<K, P>,
    buf: &mut B,
    contents: impl FnOnce(&mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError>
where
    K: std::hash::Hash + Eq + std::borrow::Borrow<Q>,
    Q: std::hash::Hash + Eq + ?Sized,
{
    let seen = encoding(|e| match table(e).get_mut(key) {
        Some(s) => (s.ord, !std::mem::replace(&mut s.defined, true)),
        None => {
            let ord = new_ordinal(e);
            let (k, _pin) = owned(key);
            table(e).insert(k, Slot { ord, defined: true, _pin });
            (ord, true)
        }
    });
    let (ord, first) = seen.ok_or(PackError::InvalidFormat)?;
    buf.put_u8(REF);
    encode_varint(ord as u64, buf);
    if !first {
        return Ok(());
    }
    let mut def = encoding(|e| e.scratch.pop()).flatten().unwrap_or_default();
    def.put_u8(DEF);
    let written = crate::stack::ensure_sufficient(|| contents(&mut def));
    encoding(|e| {
        if written.is_ok() {
            e.offsets[ord as usize] = Some(e.defs.len() as u64);
            e.defs.put_slice(&def.0);
        }
        def.0.clear();
        e.scratch.push(def);
    });
    written
}

/// The definition offset a reference names.
fn ref_offset(sub: &mut &[u8]) -> Result<u64, PackError> {
    let ord = decode_varint(sub)? as usize;
    decoding(|d| d.offsets.get(ord).copied()).flatten().ok_or(PackError::InvalidFormat)
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
                let offset = ref_offset(sub)?;
                match decoding(|d| table(d).get(&offset).cloned()).flatten() {
                    Some(v) => Ok(v),
                    None => decode_at(offset, full),
                }
            }
            DEF => {
                let v = contents(sub)?;
                decoding(|d| d.enter(&table, at, v.clone()));
                Ok(v)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

/// A type node whose key the session holds: kept alive with it, so its
/// address names it alone for the session. An encode can measure a
/// node built on the fly (a function type's normalized constraints) and
/// drop it; its address must not come back as another node's key.
#[allow(dead_code)]
pub(crate) enum KeyedNode {
    Types(Arc<[Type]>),
    Type(Arc<Type>),
    Fn(Arc<FnType>),
    Fields(Arc<[(ArcStr, Type, WrittenAt)]>),
}

/// Append the canonical bytes of the shared type node at `ptr`, walking
/// it once per session; `keep` is the node, held with its key.
pub(crate) fn shared_key(
    ptr: usize,
    keep: impl FnOnce() -> KeyedNode,
    out: &mut Vec<u8>,
    walk: impl FnOnce(&mut Vec<u8>),
) {
    let hit = encoding(|e| match e.type_keys.get(&ptr) {
        Some((_, k)) => {
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
    crate::stack::ensure_sufficient(|| walk(out));
    let key: Box<[u8]> = out[start..].into();
    encoding(|e| {
        e.type_keys.insert(ptr, (keep(), key));
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

/// A content table's key is its own bytes; nothing to pin.
fn content_owned(k: &[u8]) -> (Box<[u8]>, ()) {
    (Box::from(k), ())
}

pub(crate) fn type_len(t: &Type, contents: impl FnOnce() -> usize) -> usize {
    with_key(
        |key| t.content_key(key),
        |key| object_len(key, content_owned, |e| &mut e.types, contents),
    )
}

pub(crate) fn type_encode<B: BufMut>(
    t: &Type,
    buf: &mut B,
    contents: impl FnOnce(&mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError> {
    with_key(
        |key| t.content_key(key),
        |key| object_encode(key, content_owned, |e| &mut e.types, buf, contents),
    )
}

pub(crate) fn fntype_len(t: &FnType, contents: impl FnOnce() -> usize) -> usize {
    with_key(
        |key| t.content_key(key),
        |key| object_len(key, content_owned, |e| &mut e.fntypes, contents),
    )
}

pub(crate) fn fntype_encode<B: BufMut>(
    t: &FnType,
    buf: &mut B,
    contents: impl FnOnce(&mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError> {
    with_key(
        |key| t.content_key(key),
        |key| object_encode(key, content_owned, |e| &mut e.fntypes, buf, contents),
    )
}

/// The address an expression is keyed by: that of the session's clone
/// of the first expression seen with its id and contents (a node's
/// spec is a clone of the tree it was compiled from). Outside a
/// session, its own.
pub(crate) fn expr_key(e: &Expr) -> usize {
    encoding(|enc| {
        let seen = enc.exprs_by_id.entry(e.id).or_default();
        match seen.iter().find(|kept| kept.same_tree(e)) {
            Some(kept) => key(&**kept),
            None => {
                seen.push(Box::new(e.clone()));
                key(&**seen.last().expect("just pushed"))
            }
        }
    })
    .unwrap_or_else(|| key(e))
}

pub(crate) fn tvar_encode(tv: &TVar, buf: &mut impl BufMut) -> Result<(), PackError> {
    let key = tv.wrapper_addr();
    object_encode(
        &key,
        |k| (*k, tv.clone()),
        |e| &mut e.tvars,
        buf,
        |buf| {
            let (id, frozen, cell) = tv.parts();
            tv.name.encode(buf)?;
            id.encode(buf)?;
            frozen.encode(buf)?;
            cell_encode(&cell, buf)
        },
    )
}

fn cell_encode(
    cell: &Arc<RwLock<TCell>>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let key = Arc::as_ptr(cell) as usize;
    object_encode(
        &key,
        |k| (*k, cell.clone()),
        |e| &mut e.cells,
        buf,
        |buf| {
            let (typ, constraints, refused) = {
                let c = cell.read();
                if c.rigid_gates != 0 {
                    log::warn!(
                        "a tvar cell with {} open rigid gate(s) cannot be imaged: typ={:?} constraints={:?}",
                        c.rigid_gates,
                        c.binding,
                        c.constraints
                    );
                    return Err(PackError::InvalidFormat);
                }
                (c.binding.clone(), c.constraints.to_vec(), c.cycle_refused)
            };
            typ.encode(buf)?;
            slice_encode(&constraints, buf)?;
            refused.encode(buf)
        },
    )
}

pub(crate) fn tvar_decode(buf: &mut impl Buf) -> Result<TVar, PackError> {
    with_slice(buf, |sub| {
        let at = position(sub)?;
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let offset = ref_offset(sub)?;
                match decoding(|d| d.tvars.get(&offset).cloned()).flatten() {
                    Some(tv) => Ok(tv),
                    None => decode_at(offset, |b| tvar_decode(b)),
                }
            }
            DEF => {
                let name = Pack::decode(sub)?;
                let id = TVarId::decode(sub)?;
                let frozen = bool::decode(sub)?;
                // Entered over a placeholder before its cell: the cell's
                // bound type or a constraint can reach this wrapper.
                let placeholder = Arc::new(RwLock::new(TCell::default()));
                let tv = TVar::from_parts(name, id, frozen, placeholder);
                decoding(|d| d.enter(|d| &mut d.tvars, at, tv.clone()));
                let cell = cell_decode(sub)?;
                tv.write().cell = cell;
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
                let offset = ref_offset(sub)?;
                match decoding(|d| d.cells.get(&offset).cloned()).flatten() {
                    Some(c) => Ok(c),
                    None => decode_at(offset, |b| cell_decode(b)),
                }
            }
            DEF => {
                // Entered before its contents: a bound type can reach
                // the cell again.
                let cell = Arc::new(RwLock::new(TCell::default()));
                decoding(|d| d.enter(|d| &mut d.cells, at, cell.clone()));
                let typ = Pack::decode(sub)?;
                let constraints: Vec<_> = Pack::decode(sub)?;
                let refused = bool::decode(sub)?;
                let mut c = cell.write();
                c.binding = typ;
                c.constraints = constraints.into_iter().collect();
                c.cycle_refused = refused;
                drop(c);
                Ok(cell)
            }
            _ => Err(PackError::UnknownTag),
        }
    })
}

/// An image a test wrote: its body, then the definitions.
#[cfg(test)]
pub(crate) struct Packed {
    pub(crate) image: Bytes,
    body: usize,
    offsets: Vec<u64>,
}

#[cfg(test)]
impl Packed {
    /// Close the image `buf` holds the body of.
    pub(crate) fn new(enc: &mut ImageEncoder, mut buf: ImageBuf) -> Self {
        let body = buf.len();
        let offsets = enc.finish(&mut buf);
        Packed { image: buf.freeze(), body, offsets }
    }

    pub(crate) fn body(&self) -> &[u8] {
        &self.image[..self.body]
    }

    /// The objects defined.
    pub(crate) fn definitions(&self) -> usize {
        self.offsets.iter().filter(|at| **at != u64::MAX).count()
    }

    pub(crate) fn decoder(&self, enc: &ImageEncoder) -> ImageDecoder {
        let mut dec = ImageDecoder::new(enc.counts()).expect("a reservable span");
        dec.set_image(self.image.clone());
        dec.set_offsets(self.offsets.clone());
        dec
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
    use netidx_value::Value;

    fn expr_base(dec: &ImageDecoder) -> u64 {
        match dec.relocations.expr {
            Some(IdRelocation::Decode { base, .. }) => base,
            r => panic!("{r:?}"),
        }
    }

    /// Measure `items`, then write each, asserting it is as long as it
    /// measured.
    fn pack_all<T: Pack>(items: &[T], enc: &mut ImageEncoder) -> Packed {
        let measure = |enc: &mut ImageEncoder| -> Vec<usize> {
            EncodeImage::with(enc, || items.iter().map(|i| i.encoded_len()).collect())
        };
        let bounds = measure(enc);
        let buf = EncodeImage::with(enc, || {
            let mut buf = ImageBuf::with_capacity(0);
            for (i, bound) in items.iter().zip(bounds) {
                let before = buf.len();
                i.encode(&mut buf).unwrap();
                assert_eq!(buf.len() - before, bound);
            }
            buf
        });
        Packed::new(enc, buf)
    }

    /// A derived `Pack` frame measures every field before writing any,
    /// and its header is what the fields then write, whatever else was
    /// measured first: another measurement of the frame, or of its
    /// fields out of order.
    #[test]
    fn a_frame_measures_what_it_writes() {
        use crate::{expr::parser::parse_type, typ::Type};
        #[derive(Debug, netidx_derive::Pack)]
        struct Pair {
            a: Type,
            b: Type,
        }
        let pair =
            Pair { a: parse_type("Array<i64>").unwrap(), b: parse_type("i64").unwrap() };
        let mut enc = ImageEncoder::new();
        let measured = EncodeImage::with(&mut enc, || pair.encoded_len());
        let buf = EncodeImage::with(&mut enc, || {
            assert_eq!(pair.encoded_len(), measured);
            let _ = (pair.b.encoded_len(), pair.a.encoded_len());
            let mut buf = ImageBuf::with_capacity(measured);
            pair.encode(&mut buf).unwrap();
            assert_eq!(buf.len(), measured);
            assert_eq!(pair.encoded_len(), measured);
            12345_u64.encode(&mut buf).unwrap();
            buf
        });
        let packed = Packed::new(&mut enc, buf);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let mut input = packed.body();
            let decoded = Pair::decode(&mut input).unwrap();
            assert_eq!(decoded.a, pair.a);
            assert_eq!(decoded.b, pair.b);
            assert_eq!(u64::decode(&mut input).unwrap(), 12345);
            assert!(input.is_empty());
        });
    }

    /// A type measured and written on the fly, then dropped, leaves no
    /// key behind for the next type built at its address.
    #[test]
    fn a_dropped_type_frees_no_key() {
        use crate::expr::parser::parse_type;
        let texts: Vec<String> =
            (0..64).map(|i| format!("fn(x: Array<i64>) -> [`T{i}, null]")).collect();
        let mut enc = ImageEncoder::new();
        let buf = EncodeImage::with(&mut enc, || {
            let mut buf = ImageBuf::with_capacity(0);
            for text in &texts {
                let t = parse_type(text).unwrap();
                let len = t.encoded_len();
                let before = buf.len();
                t.encode(&mut buf).unwrap();
                assert_eq!(buf.len() - before, len);
            }
            buf
        });
        let packed = Packed::new(&mut enc, buf);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let mut b = packed.body();
            for text in &texts {
                assert_eq!(Type::decode(&mut b).unwrap(), parse_type(text).unwrap());
            }
            assert!(b.is_empty());
        });
    }

    /// An input address reused for a different expression within a
    /// session names the new expression, not the old one.
    #[test]
    fn reused_address_is_not_an_alias() {
        use crate::expr::parser::parse_one;
        let mut slot = Box::new(parse_one("1").unwrap());
        let expected = parse_one("2").unwrap();
        let mut enc = ImageEncoder::new();
        let buf = EncodeImage::with(&mut enc, || {
            let mut buf = ImageBuf::with_capacity(0);
            slot.encode(&mut buf).unwrap();
            *slot = expected.clone();
            slot.encode(&mut buf).unwrap();
            buf
        });
        let packed = Packed::new(&mut enc, buf);
        let mut dec = packed.decoder(&enc);
        let actual = DecodeImage::with(&mut dec, || {
            let mut b = packed.body();
            Expr::decode(&mut b).unwrap();
            Expr::decode(&mut b).unwrap()
        });
        assert_eq!(actual, expected);
    }

    #[test]
    fn ids_relocate_into_a_reserved_block() {
        let a = ExprId::new();
        let b = ExprId::new();
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&[a, b, a], &mut enc);
        // as minted; the span runs from the smallest to one past the largest
        let span = IdSpan {
            floor: a.inner().min(b.inner()),
            extent: a.inner().max(b.inner()) + 1,
        };
        assert_eq!(enc.counts(), IdCounts { expr: span, ..IdCounts::default() });
        let mut raw = ImageBuf::with_capacity(0);
        for id in [a, b, a] {
            encode_varint(id.inner(), &mut raw);
        }
        assert_eq!(&packed.image[..], &raw.freeze()[..]);
        let mut dec = packed.decoder(&enc);
        let base = expr_base(&dec);
        let after = ExprId::new();
        DecodeImage::with(&mut dec, || {
            let mut b = packed.body();
            let (x, y, z) = (
                ExprId::decode(&mut b).unwrap(),
                ExprId::decode(&mut b).unwrap(),
                ExprId::decode(&mut b).unwrap(),
            );
            assert_eq!(x, z);
            assert_ne!(x, y);
            assert_ne!(x, a);
            assert!(x.inner() < after.inner() && y.inner() < after.inner());
            // the block holds the span, floor first
            assert_eq!(x.inner().min(y.inner()), base.wrapping_add(span.floor));
            // above every id the image wrote, so a scope component minted
            // from a relocated id never spells one the image's text holds
            assert!(x.inner().min(y.inner()) >= span.extent);
            // a second decoder of the same image gets its own block
            let dec2 = ImageDecoder::new(enc.counts()).unwrap();
            assert!(expr_base(&dec2) > base);
        });
    }

    /// An id outside the span the image recorded fails the read rather
    /// than relocating onto an id this process minted.
    #[test]
    fn an_id_outside_the_span_is_refused() {
        let a = ExprId::new();
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&[a], &mut enc);
        let mut dec = packed.decoder(&enc);
        let mut bad = ImageBuf::with_capacity(0);
        encode_varint(a.inner() + 1, &mut bad);
        let bad = bad.freeze();
        DecodeImage::with(&mut dec, || {
            assert!(ExprId::decode(&mut packed.body()).is_ok());
            assert!(ExprId::decode(&mut &bad[..]).is_err());
        });
    }

    /// A span no block can hold is refused before anything reserves.
    #[test]
    fn an_unreservable_span_is_refused() {
        let huge = IdSpan { floor: 0, extent: u64::MAX };
        assert!(
            ImageDecoder::new(IdCounts { bind: huge, ..IdCounts::default() }).is_err()
        );
    }

    /// Instance ids are a relocated domain like the others.
    #[test]
    fn instance_ids_relocate() {
        let a = LambdaInstanceId::new();
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&[a], &mut enc);
        assert_eq!(enc.counts().instance.len(), 1);
        let mut dec = packed.decoder(&enc);
        let decoded = DecodeImage::with(&mut dec, || {
            LambdaInstanceId::decode(&mut packed.body()).unwrap()
        });
        let after = LambdaInstanceId::new();
        assert_ne!(decoded, a);
        assert!(decoded.inner() < after.inner());
    }

    /// A definition that references its own ordinal fails the read
    /// instead of recursing until the stack runs out.
    #[test]
    fn a_self_referencing_definition_is_refused() {
        // ordinal 0 is defined at offset 0 as a reference to ordinal 0
        let image = Bytes::from_static(&[REF, 0]);
        let mut dec = ImageDecoder::new(IdCounts::default()).unwrap();
        dec.set_image(image.clone());
        dec.set_offsets(vec![0]);
        DecodeImage::with(&mut dec, || {
            assert!(Expr::decode(&mut &image[..]).is_err());
        });
    }

    /// A variable whose constraint holds the variable itself decodes to
    /// one wrapper, inside and out.
    #[test]
    fn a_self_constrained_tvar_is_one_wrapper() {
        let a = TVar::empty_named(literal!("a"));
        a.add_cell_constraint(Type::Array(Arc::new(Type::TVar(a.clone()))));
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&[Type::TVar(a)], &mut enc);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let Type::TVar(outer) = &Type::decode(&mut packed.body()).unwrap() else {
                panic!("a tvar")
            };
            let cons = outer.cell_constraints();
            let Some(Type::Array(elt)) = cons.first() else { panic!("{cons:?}") };
            let Type::TVar(inner) = &**elt else { panic!("{elt:?}") };
            assert_eq!(inner.wrapper_addr(), outer.wrapper_addr());
        });
    }

    /// Decoding an expression nested far deeper than a small stack holds
    /// grows the stack as the compile that built it did.
    #[test]
    fn a_deep_expression_decodes_on_a_small_stack() {
        use crate::expr::parser::parse_one;
        let text = vec!["1"; 900].join(" + ");
        let e = parse_one(&text).unwrap();
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&[e.clone()], &mut enc);
        let mut dec = packed.decoder(&enc);
        let ok = std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn(move || {
                DecodeImage::with(&mut dec, || Expr::decode(&mut packed.body()).is_ok())
            })
            .unwrap()
            .join()
            .unwrap();
        assert!(ok);
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
        let packed = pack_all(&[typ.clone()], &mut enc);
        // an alias takes the other's id, so a and b share one
        let id = |tv: &TVar| tv.parts().0.inner();
        assert_eq!(enc.counts().tvar.extent, id(&a).max(id(&c)) + 1);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let decoded = Type::decode(&mut packed.body()).unwrap();
            let Type::Tuple(elems) = &decoded else { panic!("{decoded:?}") };
            let tv = |i: usize| match &elems[i] {
                Type::TVar(tv) => tv.clone(),
                other => panic!("{other:?}"),
            };
            let (a2, b2, a3, c2) = (tv(0), tv(1), tv(2), tv(3));
            assert_eq!(
                a2.wrapper_addr(),
                a3.wrapper_addr(),
                "one wrapper, two occurrences"
            );
            assert_ne!(a2.wrapper_addr(), b2.wrapper_addr());
            assert!(a2.same_cell(&b2), "aliases share a cell");
            assert!(!a2.same_cell(&c2));
            // c is bound to a's wrapper, the same one the tuple holds
            let binding = c2.binding();
            let Some(Type::TVar(inner)) = &binding else {
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
        });
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
        let packed = pack_all(&[t1, t2, other], &mut enc);
        // the two tuples, the other tuple, the primitive and two variables
        assert_eq!(enc.types.len(), 5);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let mut b = packed.body();
            let d1 = Type::decode(&mut b).unwrap();
            let d2 = Type::decode(&mut b).unwrap();
            let d3 = Type::decode(&mut b).unwrap();
            assert!(!b.has_remaining());
            assert_eq!(d1, d2);
            let (Type::Tuple(x1), Type::Tuple(x2), Type::Tuple(x3)) = (&d1, &d2, &d3)
            else {
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
        });
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
            str_form: Default::default(),
            end: Default::default(),
        };
        let parent = Expr {
            id: ExprId::new(),
            ori: ori.clone(),
            pos: Default::default(),
            kind: ExprKind::Array { args: Arc::from_iter([child]) },
            dec: None,
            str_form: Default::default(),
            end: Default::default(),
        };
        let clone = parent.clone();
        let ExprKind::Array { args } = &parent.kind else { unreachable!() };
        let child_clone = args[0].clone();
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&[parent.clone(), clone, child_clone], &mut enc);
        // the parent and its child are the only definitions
        assert_eq!(enc.exprs.len(), 2);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let mut b = packed.body();
            let d1 = Expr::decode(&mut b).unwrap();
            let d2 = Expr::decode(&mut b).unwrap();
            let d3 = Expr::decode(&mut b).unwrap();
            assert!(!b.has_remaining());
            assert_eq!(d1.id, d2.id);
            assert_eq!(d1, d2);
            let ExprKind::Array { args } = &d1.kind else { panic!("{d1:?}") };
            assert_eq!(args[0].id, d3.id);
            assert_eq!(args[0], d3);
        });
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
            str_form: Default::default(),
            end: Default::default(),
        };
        let (e1, e2) = (mk(1), mk(2));
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&[e1.clone(), e2.clone()], &mut enc);
        assert_eq!(
            enc.counts().expr,
            IdSpan { floor: e1.id.inner(), extent: e2.id.inner() + 1 }
        );
        let mut dec = packed.decoder(&enc);
        let base = expr_base(&dec);
        DecodeImage::with(&mut dec, || {
            let mut b = packed.body();
            let d1 = Expr::decode(&mut b).unwrap();
            let d2 = Expr::decode(&mut b).unwrap();
            assert!(b.is_empty());
            assert!(Arc::ptr_eq(&d1.ori, &d2.ori), "one origin, two expressions");
            assert!(!Arc::ptr_eq(&d1.ori, &ori));
            assert_eq!(d1.ori.text, ori.text);
            assert_ne!(d1.id, d2.id);
            assert_eq!(d1.id.inner().wrapping_sub(base), e1.id.inner());
            assert_eq!(d2.id.inner().wrapping_sub(base), e2.id.inner());
            assert_eq!(d1.kind, ExprKind::Constant(Value::I64(1)));
        });
    }
}
