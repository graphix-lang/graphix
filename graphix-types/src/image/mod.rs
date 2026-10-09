//! The image session: an encode or decode that preserves identity.
//! Under a session, ids relocate (dense on encode, offset into a
//! reserved block on decode), origins and type-variable wrappers and
//! cells are object tables, expressions keep their ids and origins, and
//! the environment's maps keep their sharing ([`crate::shared_map`]).
//! Outside a session the same codecs are the syntax codec's: fresh ids,
//! fresh cells, no sharing. Objects of types the core does not know
//! ride [`Obj::Any`] and the encoder's [`SessionExt`].
//!
//! The decoder outlives any one session: an instance decoded later
//! resolves into what was decoded earlier, so the runtime owns an
//! [`ImageDecoder`] and opens a [`DecodeImage`] over it per read.

mod env;

pub use env::{lexical_decode, lexical_encode};

use crate::{
    BindId, CFlag, LambdaId, LambdaInstanceId, SourcePosition,
    expr::{Expr, ExprId, ModPath, Origin, Source, WrittenAt},
    ids::{IdRelocation, IdSpan, IdSpans},
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
use std::{
    any::Any,
    cell::Cell,
    marker::PhantomData,
    path::PathBuf,
    ptr::NonNull,
    sync::{self, Weak},
};
use triomphe::Arc;

#[doc(hidden)]
pub const REF: u8 = 0;
#[doc(hidden)]
pub const DEF: u8 = 1;

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

/// The spans of each relocated id domain an image holds; the decoder
/// reserves a block for each domain.
pub type IdCounts = PerDomain<IdSpans>;

impl Pack for IdCounts {
    fn encoded_len(&self) -> usize {
        self.each()
            .iter()
            .flat_map(|s| [s.reserved, s.minted])
            .map(|s| varint_len(s.floor) + varint_len(s.extent))
            .sum()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        for s in self.each().iter().flat_map(|s| [s.reserved, s.minted]) {
            encode_varint(s.floor, buf);
            encode_varint(s.extent, buf);
        }
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let mut span = || -> Result<IdSpan, PackError> {
            Ok(IdSpan { floor: decode_varint(buf)?, extent: decode_varint(buf)? })
        };
        let mut each = [IdSpans::default(); 5];
        for s in each.iter_mut() {
            *s = IdSpans { reserved: span()?, minted: span()? };
            if !s.valid() {
                return Err(PackError::InvalidFormat);
            }
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
#[doc(hidden)]
pub type Table<K, P = ()> = AHashMap<K, Slot<P>>;

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
    /// Buffers for definitions being written, one per nesting level.
    scratch: Vec<ImageBuf>,
    paths: Table<ArcStr>,
    refcells: Table<usize, RefCell>,
    resolveds: Table<usize, Resolved>,
    origins: Table<usize, Arc<Origin>>,
    tvars: Table<usize, TVar>,
    cells: Table<usize, Arc<RwLock<TCell>>>,
    /// Expressions by the address of the session's clone
    /// ([`expr_key`]).
    pub(crate) exprs: Table<usize>,
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
    /// The interned key of every shared type node met so far, by the
    /// address of its `Arc`, so a key walk stops at shared subtrees.
    type_keys: AHashMap<usize, (KeyedNode, u64)>,
    /// A shared node's own key, its shared children by their interned
    /// keys, to its interned key: equal contents intern alike, and each
    /// node's entry is the size of the node, not of its subtree.
    key_ids: AHashMap<Box<[u8]>, u64>,
    /// Key buffers free for the next key walk, one per nesting level.
    key_scratch: Vec<Vec<u8>>,
    /// What the session's user keeps beside the core's tables.
    ext: Option<Box<dyn SessionExt>>,
}

/// State an encoder's user keeps in it for the objects the core does not
/// know. An encoder holds one extension type.
pub trait SessionExt: Any {
    /// A session over the encoder ended.
    fn end_session(&mut self);
}

impl ImageEncoder {
    pub fn new() -> Self {
        Self::default()
    }

    /// The span of the ids written so far, per domain; read between
    /// sessions.
    pub fn counts(&self) -> IdCounts {
        self.ids
    }

    /// The encoder's extension, made at its first use.
    pub fn ext<T: SessionExt + Default>(&mut self) -> &mut T {
        let ext = self.ext.get_or_insert_with(|| Box::new(T::default()));
        (&mut **ext as &mut dyn Any)
            .downcast_mut()
            .expect("an encoder holds one extension type")
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
}

/// A decoded object in its ordinal's slot of the store.
#[doc(hidden)]
pub enum Obj {
    Path(ModPath),
    RefCell(RefCell),
    Resolved(Resolved),
    Origin(Arc<Origin>),
    TVar(TVar),
    Cell(Arc<RwLock<TCell>>),
    Expr(Box<Expr>),
    Type(Type),
    FnType(Box<FnType>),
    /// An object of a type the core does not know, by its own `Object`
    /// impl ([`Foreign`]).
    Any(Box<dyn Any + Send + Sync>),
}

/// A type the store holds objects of.
#[doc(hidden)]
pub trait Object: Clone {
    fn into_obj(self) -> Obj;
    fn of(obj: &Obj) -> Option<&Self>;
}

macro_rules! object {
    ($t:ty, $v:ident) => {
        impl Object for $t {
            fn into_obj(self) -> Obj {
                Obj::$v(self)
            }
            fn of(obj: &Obj) -> Option<&Self> {
                match obj {
                    Obj::$v(o) => Some(o),
                    _ => None,
                }
            }
        }
    };
    (box $t:ty, $v:ident) => {
        impl Object for $t {
            fn into_obj(self) -> Obj {
                Obj::$v(Box::new(self))
            }
            fn of(obj: &Obj) -> Option<&Self> {
                match obj {
                    Obj::$v(o) => Some(&**o),
                    _ => None,
                }
            }
        }
    };
}

object!(ModPath, Path);
object!(RefCell, RefCell);
object!(Resolved, Resolved);
object!(Arc<Origin>, Origin);
object!(TVar, TVar);
object!(Arc<RwLock<TCell>>, Cell);
object!(box Expr, Expr);
object!(Type, Type);
object!(box FnType, FnType);

/// An object of a type outside the core, as the store holds it.
pub(crate) fn any_obj<T: Send + Sync + 'static>(object: T) -> Obj {
    Obj::Any(Box::new(object))
}

/// The object of type `T` the store holds as [`Obj::Any`].
pub(crate) fn any_of<T: 'static>(obj: &Obj) -> Option<&T> {
    match obj {
        Obj::Any(o) => o.downcast_ref(),
        _ => None,
    }
}

/// An object of a type outside the core, as a session writes and
/// reads it: the store holds it as [`Obj::Any`].
#[derive(Clone)]
pub struct Foreign<T>(pub T);

impl<T: Clone + Send + Sync + 'static> Object for Foreign<T> {
    fn into_obj(self) -> Obj {
        any_obj(self)
    }

    fn of(obj: &Obj) -> Option<&Self> {
        any_of(obj)
    }
}

/// [`object_decode`] for an object of a type outside the core.
pub fn foreign_decode<T: Clone + Send + Sync + 'static>(
    buf: &mut impl Buf,
    contents: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    object_decode(buf, |b| contents(b).map(Foreign), |b| full(b).map(Foreign))
        .map(|f| f.0)
}

/// [`object_at`] for an object of a type outside the core.
pub fn foreign_at<T: Clone + Send + Sync + 'static>(
    ord: u32,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    object_at(ord, |b| full(b).map(Foreign)).map(|f| f.0)
}

/// The objects an image session has built, each in the slot of its
/// ordinal, and the image itself, so a reference to an object not built
/// yet decodes it from its definition.
pub struct ImageDecoder {
    image: Bytes,
    /// Every definition's offset by ordinal, from the trailer.
    offsets: Vec<u64>,
    objects: Vec<Option<Obj>>,
    /// How many objects the session has entered into an empty slot.
    built: usize,
    /// Each definition being decoded, by ordinal: [`Self::built`] at its
    /// latest entry and how many of its entries are open (see
    /// [`decode_at`]).
    active: AHashMap<u32, (usize, u8)>,
    /// The definitions being decoded, innermost last: an object enters
    /// the slot of the innermost.
    defining: Vec<u32>,
    /// Typedef definitions being built, which a cell inside one reaches
    /// weakly.
    resolving: AHashMap<u32, Weak<ResolvedRef>>,
    relocations: Relocations,
    /// What the session's user keeps beside the core's objects.
    ext: Option<Box<dyn Any + Send>>,
    /// The handle the session holds this decoder by, for a definition
    /// that decodes part of the image later.
    shared: Weak<Mutex<ImageDecoder>>,
}

/// A session's decoder as the context and its restored definitions
/// hold it.
pub type SharedDecoder = sync::Arc<Mutex<ImageDecoder>>;

impl ImageDecoder {
    /// Enter `obj` in the slot of the definition being decoded. A value
    /// a cycle built twice keeps its first.
    fn enter(&mut self, obj: Obj) -> Result<(), PackError> {
        let ord = *self.defining.last().ok_or(PackError::InvalidFormat)?;
        let slot = self.objects.get_mut(ord as usize).ok_or(PackError::InvalidFormat)?;
        if slot.is_none() {
            self.built += 1;
            *slot = Some(obj);
        }
        Ok(())
    }

    fn get<T: Object>(&self, ord: u32) -> Option<T> {
        self.objects.get(ord as usize)?.as_ref().and_then(T::of).cloned()
    }

    /// A decoder of `image`, whose definitions start at `offsets`, by
    /// ordinal: both are fixed for the session, which is what makes
    /// [`decode_at`]'s slice of the image sound. Reserves a block of
    /// each id domain for the image's ids; refuses a span no block can
    /// hold.
    pub fn new(
        counts: IdCounts,
        image: Bytes,
        offsets: Vec<u64>,
    ) -> Result<Self, PackError> {
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
            image,
            objects: offsets.iter().map(|_| None).collect(),
            offsets,
            built: 0,
            active: AHashMap::new(),
            defining: Vec::new(),
            resolving: AHashMap::new(),
            relocations,
            ext: None,
            shared: Weak::new(),
        })
    }

    /// The decoder as the session holds it.
    pub fn share(self) -> SharedDecoder {
        let shared = sync::Arc::new(Mutex::new(self));
        shared.lock().shared = sync::Arc::downgrade(&shared);
        shared
    }

    #[doc(hidden)]
    pub fn shared(&self) -> Weak<Mutex<ImageDecoder>> {
        self.shared.clone()
    }

    /// The image every offset in the session refers into. Set before
    /// anything decodes; the session keeps it for what decodes later.
    pub fn image(&self) -> &Bytes {
        &self.image
    }

    /// The decoder's extension, made at its first use. A decoder holds
    /// one extension type.
    pub fn ext<T: Any + Send + Default>(&mut self) -> &mut T {
        let ext = self.ext.get_or_insert_with(|| Box::new(T::default()));
        ext.downcast_mut().expect("a decoder holds one extension type")
    }

    /// The decoder's extension, if it was made.
    pub fn ext_ref<T: Any>(&self) -> Option<&T> {
        self.ext.as_ref().and_then(|e| e.downcast_ref())
    }
}

thread_local! {
    static ENCODER: Cell<Option<NonNull<ImageEncoder>>> = const { Cell::new(None) };
    static DECODER: Cell<Option<NonNull<ImageDecoder>>> = const { Cell::new(None) };
}

fn span(r: Option<IdRelocation>) -> IdSpans {
    match r {
        Some(IdRelocation::Encode(spans)) => spans,
        _ => IdSpans::default(),
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
        if let Some(ext) = &mut encoder.ext {
            ext.end_session();
        }
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
#[doc(hidden)]
pub fn encoding<R>(f: impl FnOnce(&mut ImageEncoder) -> R) -> Option<R> {
    let mut p = ENCODER.take()?;
    // The session guard holds the `&mut` that produced this pointer for
    // as long as it is installed.
    let r = f(unsafe { p.as_mut() });
    ENCODER.set(Some(p));
    Some(r)
}

/// [`encoding`] for the installed decoder.
#[doc(hidden)]
pub fn decoding<R>(f: impl FnOnce(&mut ImageDecoder) -> R) -> Option<R> {
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
pub(crate) type RefCell = Arc<Mutex<crate::typ::Resolution>>;

/// A typedef's definition, held by its `TypeDef` and weakly by every
/// cell naming it: an object by identity, so all of them decode to one.
pub(crate) type Resolved = sync::Arc<ResolvedRef>;

fn path_key(path: &ModPath) -> &str {
    path.0.as_ref()
}

/// A module path is written once per distinct path and referenced
/// afterwards, so a scope repeated by every binding under it costs a
/// varint and decodes to one shared string.
pub(crate) fn path_len(path: &ModPath) -> usize {
    object_len(path_key(path), |k| (ArcStr::from(k), ()), |e| &mut e.paths)
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
    object_decode(buf, |sub| Ok(ModPath(Pack::decode(sub)?)), |b| path_decode(b))
}

/// A resolution cell is an object: written once, referenced afterwards,
/// so references that share a cell share it again after decode. Its
/// contents are a tag, `CELL_EMPTY`, `CELL_LIVE` and the definition,
/// `CELL_DEAD` for a definition gone before the write, or `CELL_TRAIT`
/// and the trait's id.
const CELL_EMPTY: u8 = 0;
const CELL_LIVE: u8 = 1;
const CELL_DEAD: u8 = 2;
const CELL_TRAIT: u8 = 3;

/// The cell's state, cloned out: the definition can reach this cell
/// again and the lock is not reentrant.
enum CellState {
    Empty,
    Live(Resolved),
    Dead,
    Trait(crate::typ::TraitId),
}

fn cell_state(cell: &RefCell) -> CellState {
    use crate::typ::Resolution;
    match &*cell.lock() {
        Resolution::Open => CellState::Empty,
        Resolution::Trait(id) => CellState::Trait(*id),
        Resolution::Def(w) => match w.upgrade() {
            Some(r) => CellState::Live(r),
            None => CellState::Dead,
        },
    }
}

pub(crate) fn refcell_len(cell: &RefCell) -> usize {
    let key = Arc::as_ptr(cell) as usize;
    object_len(&key, |k| (*k, cell.clone()), |e| &mut e.refcells)
}

pub(crate) fn refcell_encode<B: BufMut>(
    cell: &RefCell,
    buf: &mut B,
) -> Result<(), PackError> {
    let key = Arc::as_ptr(cell) as usize;
    object_encode(
        &key,
        |k| (*k, cell.clone()),
        |e| &mut e.refcells,
        buf,
        |buf| match cell_state(cell) {
            CellState::Empty => Ok(buf.put_u8(CELL_EMPTY)),
            CellState::Dead => Ok(buf.put_u8(CELL_DEAD)),
            CellState::Live(r) => {
                buf.put_u8(CELL_LIVE);
                resolved_encode(&r, buf)
            }
            CellState::Trait(id) => {
                buf.put_u8(CELL_TRAIT);
                id.inner().encode(buf)
            }
        },
    )
}

pub(crate) fn refcell_decode(buf: &mut impl Buf) -> Result<RefCell, PackError> {
    shared_decode(
        buf,
        built::<RefCell>,
        |sub| {
            // Entered before its contents: the definition can reach this
            // cell again.
            use crate::typ::{Resolution, TraitId};
            let cell: RefCell = Arc::new(Mutex::new(Resolution::Open));
            enter(Obj::RefCell(cell.clone()))?;
            if !sub.has_remaining() {
                return Err(PackError::BufferShort);
            }
            match sub.get_u8() {
                CELL_EMPTY => {}
                CELL_LIVE => *cell.lock() = Resolution::Def(resolved_decode_weak(sub)?),
                CELL_DEAD => *cell.lock() = Resolution::Def(Weak::new()),
                CELL_TRAIT => {
                    *cell.lock() =
                        Resolution::Trait(TraitId::from_inner(u64::decode(sub)?))
                }
                _ => return Err(PackError::UnknownTag),
            }
            Ok(cell)
        },
        |b| refcell_decode(b),
        unknown_tag,
    )
}

pub(crate) fn resolved_len(r: &Resolved) -> usize {
    let key = sync::Arc::as_ptr(r) as usize;
    object_len(&key, |k| (*k, r.clone()), |e| &mut e.resolveds)
}

#[doc(hidden)]
pub fn resolved_encode<B: BufMut>(r: &Resolved, buf: &mut B) -> Result<(), PackError> {
    let key = sync::Arc::as_ptr(r) as usize;
    object_encode(
        &key,
        |k| (*k, r.clone()),
        |e| &mut e.resolveds,
        buf,
        |buf| crate::typ::resolved_encode(r, buf),
    )
}

/// A definition decoded, or one still being decoded (only a cell
/// inside it can meet it then).
enum ResolvedRead {
    Built(Resolved),
    Building(Weak<ResolvedRef>),
}

fn resolved_read(buf: &mut impl Buf) -> Result<ResolvedRead, PackError> {
    shared_decode(
        buf,
        |ord| {
            decoding(|d| match d.get::<Resolved>(ord) {
                Some(r) => Some(ResolvedRead::Built(r)),
                None => d.resolving.get(&ord).cloned().map(ResolvedRead::Building),
            })
            .flatten()
        },
        |sub| {
            let at = decoding(|d| d.defining.last().copied())
                .flatten()
                .ok_or(PackError::InvalidFormat)?;
            // Built cyclic: the cells in its body take the weak half
            // before the definition exists.
            let mut failed = None;
            let r = sync::Arc::new_cyclic(|w| {
                decoding(|d| d.resolving.insert(at, w.clone()));
                crate::typ::resolved_decode(sub).unwrap_or_else(|e| {
                    failed = Some(e);
                    ResolvedRef::new(
                        ModPath::root(),
                        Default::default(),
                        Arc::default(),
                        Arc::from_iter([]),
                        Type::Bottom,
                    )
                })
            });
            decoding(|d| d.resolving.remove(&at));
            if let Some(e) = failed {
                return Err(e);
            }
            enter(Obj::Resolved(r.clone()))?;
            Ok(ResolvedRead::Built(r))
        },
        |b| resolved_read(b),
        unknown_tag,
    )
}

/// A typedef's definition; one still being decoded is a malformed image,
/// since no definition contains its own typedef.
#[doc(hidden)]
pub fn resolved_decode(buf: &mut impl Buf) -> Result<Resolved, PackError> {
    match resolved_read(buf)? {
        ResolvedRead::Built(r) => Ok(r),
        ResolvedRead::Building(_) => Err(PackError::InvalidFormat),
    }
}

fn resolved_decode_weak(buf: &mut impl Buf) -> Result<Weak<ResolvedRef>, PackError> {
    Ok(match resolved_read(buf)? {
        ResolvedRead::Built(r) => sync::Arc::downgrade(&r),
        ResolvedRead::Building(w) => w,
    })
}

#[doc(hidden)]
pub fn flags_encode(
    flags: BitFlags<CFlag>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    buf.put_u64(flags.bits());
    Ok(())
}

/// A count an image declares for what follows it: at most the bytes
/// left, each counted item taking at least one, so a corrupt count fails
/// the read instead of sizing an allocation.
#[doc(hidden)]
pub fn count_decode(buf: &mut impl Buf) -> Result<usize, PackError> {
    let n = decode_varint(buf)? as usize;
    if n > buf.remaining() {
        return Err(PackError::TooBig);
    }
    Ok(n)
}

#[doc(hidden)]
pub fn flags_decode(buf: &mut impl Buf) -> Result<BitFlags<CFlag>, PackError> {
    if buf.remaining() < 8 {
        return Err(PackError::BufferShort);
    }
    BitFlags::from_bits(buf.get_u64()).map_err(|_| PackError::InvalidFormat)
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
    object_len(&key, |k| (*k, ori.clone()), |e| &mut e.origins)
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
    object_decode(
        buf,
        |sub| {
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
            Ok(Arc::new(Origin { parent, source, text }))
        },
        |b| origin_decode(b),
    )
}

/// A type variable under an image: the wrapper (name, id, frozen) and
/// its cell (bound type, constraints, refusal) are separate objects,
/// each written once. Both are registered before their contents, so a
/// cell that reaches its own wrapper through a constraint decodes.
pub(crate) fn tvar_len(tv: &TVar) -> usize {
    let key = tv.wrapper_addr();
    object_len(&key, |k| (*k, tv.clone()), |e| &mut e.tvars)
}

/// A boxed slice on the wire as the `Vec` it decodes to.
#[doc(hidden)]
pub fn slice_len<T: Pack>(xs: &[T]) -> usize {
    varint_len(xs.len() as u64) + xs.iter().map(|x| x.encoded_len()).sum::<usize>()
}

#[doc(hidden)]
pub fn slice_encode<T: Pack>(xs: &[T], buf: &mut impl BufMut) -> Result<(), PackError> {
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
#[doc(hidden)]
pub fn with_slice<T>(
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

/// Decode the object defined for `ord` with `full`, its whole codec,
/// which enters it in the store as a side effect. A decode that meets
/// the object it is inside builds it again from here. Every cycle of a
/// valid image passes through a kind entered before its contents, so it
/// enters an object before it meets a definition again, and so enters a
/// definition at most twice; an entry with nothing entered since that
/// definition's last, or a third, is a cycle in the image.
#[doc(hidden)]
pub fn decode_at<T>(
    ord: u32,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    let (image, at, prev) = decoding(|d| {
        let at = d
            .offsets
            .get(ord as usize)
            .and_then(|at| usize::try_from(*at).ok())
            .filter(|at| *at < d.image.len())?;
        let built = d.built;
        let open = match d.active.get(&ord) {
            None => 0,
            Some((at, open)) if *at != built && *open < 2 => *open,
            Some(_) => return None,
        };
        let prev = d.active.insert(ord, (built, open + 1));
        d.defining.push(ord);
        Some(((d.image.as_ptr(), d.image.len()), at, prev))
    })
    .flatten()
    .ok_or(PackError::InvalidFormat)?;
    // The session's decoder holds the image, and nothing replaces it
    // while the session is installed.
    let image = unsafe { std::slice::from_raw_parts(image.0, image.1) };
    let r = crate::stack::ensure_sufficient(|| full(&mut &image[at..]));
    decoding(|d| {
        d.defining.pop();
        match prev {
            Some(p) => d.active.insert(ord, p),
            None => d.active.remove(&ord),
        }
    });
    r
}

/// An object's place in the session: the ordinal every occurrence
/// names, assigned at first sight, whether its definition has been
/// written, and what keeps the object alive for the session.
#[doc(hidden)]
pub struct Slot<P = ()> {
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
/// first sight `owned` builds the table's key and pin and the object
/// takes its ordinal; its definition is written elsewhere. An image
/// object has no encoding outside a session.
#[doc(hidden)]
pub fn object_len<K, Q, P>(
    key: &Q,
    owned: impl FnOnce(&Q) -> (K, P),
    table: impl Fn(&mut ImageEncoder) -> &mut Table<K, P>,
) -> usize
where
    K: std::hash::Hash + Eq + std::borrow::Borrow<Q>,
    Q: std::hash::Hash + Eq + ?Sized,
{
    let ord = encoding(|e| match table(e).get(key) {
        Some(s) => s.ord,
        None => {
            let ord = new_ordinal(e);
            let (k, _pin) = owned(key);
            table(e).insert(k, Slot { ord, defined: false, _pin });
            ord
        }
    });
    ord.map_or(0, |ord| 1 + varint_len(ord as u64))
}

/// Write a reference to the object and, the first time, its definition
/// to the definitions area. The object is defined before its contents
/// are written, so an occurrence of it inside them is a reference too.
/// Outside a session, an error.
#[doc(hidden)]
pub fn object_encode<K, Q, P, B: BufMut>(
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

/// The ordinal the object reference at the head of `buf` names, read
/// without decoding the object, for [`object_at`] later.
#[doc(hidden)]
pub fn object_ref(buf: &mut impl Buf) -> Result<u32, PackError> {
    with_slice(buf, |sub| {
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => ref_ord(sub),
            _ => Err(PackError::InvalidFormat),
        }
    })
}

/// The object `ord` names: the session's, or decoded with `full`.
pub(crate) fn object_at<T: Object>(
    ord: u32,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    match built::<T>(ord) {
        Some(t) => Ok(t),
        None => decode_at(ord, full),
    }
}

/// The ordinal a reference names.
#[doc(hidden)]
pub fn ref_ord(sub: &mut &[u8]) -> Result<u32, PackError> {
    u32::try_from(decode_varint(sub)?).map_err(|_| PackError::InvalidFormat)
}

/// The object built for `ord`, if it has been.
#[doc(hidden)]
pub fn built<T: Object>(ord: u32) -> Option<T> {
    decoding(|d| d.get::<T>(ord)).flatten()
}

/// Enter `obj` in the slot of the definition being decoded.
#[doc(hidden)]
pub fn enter(obj: Obj) -> Result<(), PackError> {
    decoding(|d| d.enter(obj)).unwrap_or(Err(PackError::InvalidFormat))
}

/// Read an object written by [`object_encode`]: a reference clones the
/// store's object or decodes its definition with `full`; a definition
/// decodes `contents` and enters it.
pub(crate) fn object_decode<T: Object>(
    buf: &mut impl Buf,
    contents: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
) -> Result<T, PackError> {
    shared_decode(
        buf,
        built::<T>,
        |sub| {
            let v = contents(sub)?;
            enter(v.clone().into_obj())?;
            Ok(v)
        },
        full,
        unknown_tag,
    )
}

/// The frame every shared object decodes through: a REF is the object
/// `built` finds, else its definition decoded again from its offset by
/// `full`; a DEF is `def`'s, which enters the object; any other tag is
/// `other`'s.
#[doc(hidden)]
pub fn shared_decode<T>(
    buf: &mut impl Buf,
    built: impl FnOnce(u32) -> Option<T>,
    def: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
    full: impl FnOnce(&mut &[u8]) -> Result<T, PackError>,
    other: impl FnOnce(u8) -> Result<T, PackError>,
) -> Result<T, PackError> {
    with_slice(buf, |sub| {
        if !sub.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match sub.get_u8() {
            REF => {
                let ord = ref_ord(sub)?;
                match built(ord) {
                    Some(v) => Ok(v),
                    None => decode_at(ord, full),
                }
            }
            DEF => def(sub),
            tag => other(tag),
        }
    })
}

#[doc(hidden)]
pub fn unknown_tag<T>(_: u8) -> Result<T, PackError> {
    Err(PackError::UnknownTag)
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

/// Append the interned key of the shared type node at `ptr`, walking
/// it once per session; `keep` is the node, held with its key.
pub(crate) fn shared_key(
    ptr: usize,
    keep: impl FnOnce() -> KeyedNode,
    out: &mut Vec<u8>,
    walk: impl FnOnce(&mut Vec<u8>),
) {
    let known = encoding(|e| e.type_keys.get(&ptr).map(|(_, id)| *id)).flatten();
    let id = match known {
        Some(id) => id,
        None => {
            let start = out.len();
            crate::stack::ensure_sufficient(|| walk(out));
            // outside a session the node's key stays written out whole
            let Some(id) = encoding(|e| {
                let next = e.key_ids.len() as u64;
                let id = match e.key_ids.get(&out[start..]) {
                    Some(id) => *id,
                    None => {
                        e.key_ids.insert(out[start..].into(), next);
                        next
                    }
                };
                e.type_keys.insert(ptr, (keep(), id));
                id
            }) else {
                return;
            };
            out.truncate(start);
            id
        }
    };
    encode_varint(id, out);
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

pub(crate) fn type_len(t: &Type) -> usize {
    with_key(
        |key| t.content_key(key),
        |key| object_len(key, content_owned, |e| &mut e.types),
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

pub(crate) fn fntype_len(t: &FnType) -> usize {
    with_key(
        |key| t.content_key(key),
        |key| object_len(key, content_owned, |e| &mut e.fntypes),
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
            let (typ, constraints, flags) = {
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
                (
                    c.binding.clone(),
                    c.constraints.to_vec(),
                    (c.cycle_refused, c.bottom_fed, c.level),
                )
            };
            typ.encode(buf)?;
            slice_encode(&constraints, buf)?;
            flags.0.encode(buf)?;
            flags.1.encode(buf)?;
            flags.2.encode(buf)
        },
    )
}

pub(crate) fn tvar_decode(buf: &mut impl Buf) -> Result<TVar, PackError> {
    shared_decode(
        buf,
        built::<TVar>,
        |sub| {
            let name = Pack::decode(sub)?;
            let id = TVarId::decode(sub)?;
            let frozen = bool::decode(sub)?;
            // Entered over a placeholder before its cell: the cell's
            // bound type or a constraint can reach this wrapper.
            let placeholder = Arc::new(RwLock::new(TCell::default()));
            let tv = TVar::from_parts(name, id, frozen, placeholder);
            enter(Obj::TVar(tv.clone()))?;
            let cell = cell_decode(sub)?;
            tv.write().cell = cell;
            Ok(tv)
        },
        |b| tvar_decode(b),
        unknown_tag,
    )
}

fn cell_decode(buf: &mut impl Buf) -> Result<Arc<RwLock<TCell>>, PackError> {
    shared_decode(
        buf,
        built::<Arc<RwLock<TCell>>>,
        |sub| {
            // Entered before its contents: a bound type can reach the
            // cell again.
            let cell = Arc::new(RwLock::new(TCell::default()));
            enter(Obj::Cell(cell.clone()))?;
            let typ = Pack::decode(sub)?;
            let constraints: Vec<_> = Pack::decode(sub)?;
            let refused = bool::decode(sub)?;
            let bottom_fed = bool::decode(sub)?;
            let level = Pack::decode(sub)?;
            let mut c = cell.write();
            c.level = level;
            c.binding = typ;
            c.constraints = constraints.into_iter().collect();
            c.cycle_refused = refused;
            c.bottom_fed = bottom_fed;
            drop(c);
            Ok(cell)
        },
        |b| cell_decode(b),
        unknown_tag,
    )
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
        ImageDecoder::new(enc.counts(), self.image.clone(), self.offsets.clone())
            .expect("a reservable span")
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

    /// Counts no image writes are refused at decode, before a reader
    /// reserves anything or lifts a counter.
    #[test]
    fn id_counts_out_of_their_regions_are_refused() {
        use crate::ids::{IdSpan, IdSpans, MINT_BASE};
        let empty = IdSpans::default();
        let good = IdSpans {
            reserved: IdSpan { floor: 0, extent: 10 },
            minted: IdSpan { floor: MINT_BASE, extent: MINT_BASE + 10 },
        };
        let bad = [
            IdSpans { reserved: IdSpan { floor: 0, extent: MINT_BASE + 1 }, ..empty },
            IdSpans { reserved: IdSpan { floor: 5, extent: 2 }, ..empty },
            IdSpans { minted: IdSpan { floor: 0, extent: u64::MAX - 1 }, ..empty },
            IdSpans {
                minted: IdSpan { floor: MINT_BASE, extent: 3 * MINT_BASE },
                ..empty
            },
        ];
        let round = |s: IdSpans| {
            let counts = IdCounts::from_each([s, empty, empty, empty, empty]);
            let mut buf = BytesMut::new();
            counts.encode(&mut buf).unwrap();
            IdCounts::decode(&mut buf.freeze())
        };
        assert!(round(empty).is_ok() && round(good).is_ok());
        for b in bad {
            assert!(round(b).is_err(), "{b:?}");
        }
    }

    fn expr_start(dec: &ImageDecoder) -> u64 {
        match dec.relocations.expr {
            Some(IdRelocation::Decode { start, .. }) => start,
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

    /// A corrupt image decodes to an answer, never an unbounded
    /// recursion: every byte of a type's image rewritten to every small
    /// value, some of which make a definition reach back into its own
    /// decode.
    #[test]
    fn a_corrupt_image_fails_the_read() {
        use crate::expr::parser::parse_type;
        let t = parse_type(
            "Array<(Array<string>, Foo<(Array<u8>, Bar<Array<bool>>, Array<i32>)>, Array<i64>)>",
        )
        .unwrap();
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&[t.clone()], &mut enc);
        let ords = packed.offsets.len() as u8;
        let mut refused = 0;
        for at in 0..packed.image.len() {
            for v in 0..ords + 4 {
                let mut image = packed.image.to_vec();
                image[at] = v;
                let bad = Packed {
                    image: Bytes::from(image),
                    body: packed.body,
                    offsets: packed.offsets.clone(),
                };
                let mut dec = bad.decoder(&enc);
                let r = DecodeImage::with(&mut dec, || Type::decode(&mut bad.body()));
                refused += r.is_err() as usize;
            }
        }
        assert!(refused > 0, "no rewrite was refused");
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
        // minted; the span runs from the smallest to one past the largest
        let span = IdSpan {
            floor: a.inner().min(b.inner()),
            extent: a.inner().max(b.inner()) + 1,
        };
        let spans = IdSpans { minted: span, ..IdSpans::default() };
        assert_eq!(enc.counts(), IdCounts { expr: spans, ..IdCounts::default() });
        // each written as its offset from the first minted id
        let mut raw = ImageBuf::with_capacity(0);
        for id in [a, b, a] {
            encode_varint(((id.inner() - crate::ids::MINT_BASE) << 1) | 1, &mut raw);
        }
        assert_eq!(&packed.image[..], &raw.freeze()[..]);
        let mut dec = packed.decoder(&enc);
        let start = expr_start(&dec);
        let after = ExprId::new();
        let a_first = a.inner() < b.inner();
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
            // the block holds the span, floor first, in order
            assert_eq!(x.inner().min(y.inner()), start);
            assert_eq!(x.inner() < y.inner(), a_first);
            // in the reserved region, so a scope component minted from a
            // relocated id never spells one the image's text holds
            assert!(x.inner().max(y.inner()) < crate::ids::MINT_BASE);
            // a second decoder of the same image gets its own block
            let dec2 = ImageDecoder::new(enc.counts(), Bytes::new(), Vec::new()).unwrap();
            assert!(expr_start(&dec2) >= start + spans.len());
        });
    }

    /// A runtime restored from an image mints after other runtimes
    /// restored theirs, and writes an image of its own: the span it
    /// records holds its own ids, never the blocks the others reserved,
    /// so a chain of such images stays small and reservable.
    #[test]
    fn a_chain_of_restores_keeps_its_span() {
        let mut carried = ExprId::new();
        for round in 0..100 {
            let fresh = ExprId::new();
            let mut enc = ImageEncoder::new();
            let packed = pack_all(&[carried, fresh], &mut enc);
            let len = enc.counts().expr.len();
            assert!(len < 1 << 20, "round {round}: the image spans {len} ids");
            let mut dec = packed.decoder(&enc);
            let _concurrent = ImageDecoder::new(enc.counts(), Bytes::new(), Vec::new())
                .expect("a reservable span");
            carried = DecodeImage::with(&mut dec, || {
                ExprId::decode(&mut packed.body()).unwrap()
            });
        }
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
        encode_varint(crate::ids::to_wire(a.inner() + 1), &mut bad);
        let bad = bad.freeze();
        DecodeImage::with(&mut dec, || {
            assert!(ExprId::decode(&mut packed.body()).is_ok());
            assert!(ExprId::decode(&mut &bad[..]).is_err());
        });
    }

    /// A span no block can hold is refused before anything reserves.
    #[test]
    fn an_unreservable_span_is_refused() {
        let huge = IdSpans {
            reserved: IdSpan { floor: 0, extent: crate::ids::MINT_BASE },
            ..IdSpans::default()
        };
        assert!(
            ImageDecoder::new(
                IdCounts { bind: huge, ..IdCounts::default() },
                Bytes::new(),
                Vec::new()
            )
            .is_err()
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
        let mut dec =
            ImageDecoder::new(IdCounts::default(), image.clone(), vec![0]).unwrap();
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
        assert_eq!(enc.counts().tvar.minted.extent, id(&a).max(id(&c)) + 1);
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

    /// A recursive typedef's cells decode onto the definition the decoded
    /// typedef owns, the one being built included, and nothing else
    /// keeps it: dropping the decoded defs and the session frees it.
    #[test]
    fn typedef_cells_decode_onto_their_definition() {
        use crate::env::{Env, TypeDef};
        let mut env = Env::default();
        let scope = ModPath::root();
        for src in [
            "type L = [`Nil, `Cons(i64, L)]",
            "type A = [`End, `A(B)]",
            "type B = [`End, `B(A)]",
        ] {
            let expr = crate::expr::parser::parse_one(src).unwrap();
            let ExprKind::TypeDef(td) = &expr.kind else { unreachable!() };
            env.deftype(
                &scope,
                &td.name,
                td.params.clone(),
                &td.body,
                true,
                None,
                expr.pos,
                expr.pos,
                expr.ori.clone(),
            )
            .unwrap();
        }
        env.seed_typedef_refs();
        let defs = env.typedefs.get(&scope).unwrap();
        let tds: Vec<TypeDef> =
            ["L", "A", "B"].iter().map(|n| defs.get(*n).unwrap().clone()).collect();
        let mut enc = ImageEncoder::new();
        let packed = pack_all(&tds, &mut enc);
        fn target(t: &Type, name: &str) -> Option<Resolved> {
            let mut found = match t {
                Type::Ref(tr) if tr.name.ends_with(name) => tr.resolved(),
                _ => None,
            };
            t.for_each_child(&mut |c| found = found.take().or_else(|| target(c, name)));
            found
        }
        let mut dec = packed.decoder(&enc);
        drop(enc);
        let decoded: Vec<TypeDef> = DecodeImage::with(&mut dec, || {
            let mut body = packed.body();
            tds.iter().map(|_| TypeDef::decode(&mut body).unwrap()).collect()
        });
        let (l, a, b) = (&decoded[0], &decoded[1], &decoded[2]);
        for (from, name, to) in [(l, "L", l), (a, "B", b), (b, "A", a)] {
            let r = target(from.typ(), name).expect("a live cell");
            assert!(sync::Arc::ptr_eq(&r, &to.def), "{name} decoded twice");
            assert!(!sync::Arc::ptr_eq(&r, &tds[0].def), "decoded onto the original");
        }
        let held: Vec<Weak<ResolvedRef>> =
            decoded.iter().map(|td| sync::Arc::downgrade(&td.def)).collect();
        drop(decoded);
        drop(dec);
        assert!(
            held.iter().all(|w| w.upgrade().is_none()),
            "a decoded definition leaked"
        );
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
            enc.counts().expr.minted,
            IdSpan { floor: e1.id.inner(), extent: e2.id.inner() + 1 }
        );
        let mut dec = packed.decoder(&enc);
        let start = expr_start(&dec);
        DecodeImage::with(&mut dec, || {
            let mut b = packed.body();
            let d1 = Expr::decode(&mut b).unwrap();
            let d2 = Expr::decode(&mut b).unwrap();
            assert!(b.is_empty());
            assert!(Arc::ptr_eq(&d1.ori, &d2.ori), "one origin, two expressions");
            assert!(!Arc::ptr_eq(&d1.ori, &ori));
            assert_eq!(d1.ori.text, ori.text);
            assert_ne!(d1.id, d2.id);
            assert_eq!(d1.id.inner() - start, 0);
            assert_eq!(d2.id.inner() - start, e2.id.inner() - e1.id.inner());
            assert_eq!(d1.kind, ExprKind::Constant(Value::I64(1)));
        });
    }
}
