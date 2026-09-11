//! What a defined JIT function is made of, kept so a program image can
//! carry it: the machine code, its relocations by symbolic target, and
//! the recipes of the constants the code refers to by address. The
//! cold run installs a function from its record exactly as a warm run
//! does, through `define_function_bytes`.

use crate::{
    FastCall, FastFn, TypedFastFn,
    expr::{Expr, ExprId},
    fusion::{
        kernel_abi::{self, KernelSig, SiteLeaf},
        lowering::cast_typed,
    },
    image,
    node::error::QopSite,
    typ::Type,
};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use cranelift_codegen::{
    binemit::Reloc,
    ir::{GlobalValue, LibCall},
};
use cranelift_module::DataId;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::Value;
use parking_lot::Mutex;
use std::{collections::HashMap, str::FromStr, sync::Arc};

/// The addresses the module resolves imported constant symbols to,
/// shared with its symbol lookup fn.
pub(crate) type SymbolTable = Arc<Mutex<HashMap<String, usize>>>;

/// A pointee the code refers to by address, owned here when it is one,
/// with what a loader needs to recreate it. Every occurrence in a body
/// is an imported data symbol whose address is the pointer itself.
pub enum KernelConst {
    Str(Box<ArcStr>),
    Value(Box<Value>),
    Type(Box<Type>),
    QopSite(Box<QopSite>),
    /// A builtin's plain fast fn, by the builtin's name.
    FastFn {
        name: ArcStr,
        f: FastFn,
    },
    /// A builtin's typed fast fn, by the builtin's name.
    TypedFn {
        name: ArcStr,
        f: TypedFastFn,
    },
    /// The cast pseudo-site's fn.
    Cast(TypedFastFn),
    SiteLeaf(Arc<SiteLeaf>),
    /// The owning kernel's `site_block_words` cell.
    SiteBlockWords,
}

impl KernelConst {
    /// The address the symbol resolves to; `kernel` is the body's own.
    pub fn pointer(&self, kernel: &KernelSig) -> usize {
        match self {
            KernelConst::Str(b) => &**b as *const ArcStr as usize,
            KernelConst::Value(b) => &**b as *const Value as usize,
            KernelConst::Type(b) => &**b as *const Type as usize,
            KernelConst::QopSite(b) => &**b as *const QopSite as usize,
            KernelConst::FastFn { f, .. } => *f as usize,
            KernelConst::TypedFn { f, .. } | KernelConst::Cast(f) => *f as usize,
            KernelConst::SiteLeaf(l) => Arc::as_ptr(l) as *const u8 as usize,
            KernelConst::SiteBlockWords => {
                &kernel.site_block_words as *const std::sync::atomic::AtomicU64 as usize
            }
        }
    }

    /// Whether one symbol can serve both: equal strings and values, the
    /// same leaf, the same fn. Types and `?` sites are never shared.
    pub(super) fn same_as(&self, other: &KernelConst) -> bool {
        match (self, other) {
            (KernelConst::Str(a), KernelConst::Str(b)) => a == b,
            (KernelConst::Value(a), KernelConst::Value(b)) => a == b,
            (KernelConst::FastFn { f: a, .. }, KernelConst::FastFn { f: b, .. }) => {
                *a as usize == *b as usize
            }
            (KernelConst::TypedFn { f: a, .. }, KernelConst::TypedFn { f: b, .. })
            | (KernelConst::Cast(a), KernelConst::Cast(b)) => *a as usize == *b as usize,
            (KernelConst::SiteLeaf(a), KernelConst::SiteLeaf(b)) => Arc::ptr_eq(a, b),
            (KernelConst::SiteBlockWords, KernelConst::SiteBlockWords) => true,
            _ => false,
        }
    }
}

/// A constant while its body is being emitted: the recipe, its data
/// symbol and the function-local global value the code reads it through.
pub(super) struct EmitConst {
    pub(super) recipe: KernelConst,
    pub(super) data: DataId,
    pub(super) gv: GlobalValue,
}

/// What a relocation names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelocTarget {
    Helper(ArcStr),
    /// A record in the body's `callees`.
    Callee(u32),
    /// The record itself, or for a thunk the body it serves.
    Owner,
    /// The body's own spill thunk.
    Thunk,
    LibCall(LibCall),
    /// A record in the body's `consts`.
    Const(u32),
}

#[derive(Debug, Clone)]
pub struct RecordReloc {
    pub offset: u32,
    pub kind: Reloc,
    pub target: RelocTarget,
    pub addend: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecordKind {
    /// A kernel body; its signature derives from its `KernelSig`.
    Kernel,
    /// The `(args, out)` thunk a self-recursive body re-enters through.
    Thunk,
    /// A region's `(args, out)` wrapper.
    Wrapper,
}

/// One defined function: its code, relocations and constants, plus the
/// records it refers to. Callees are recorded before their callers, so
/// the records form a tree below a region's wrapper; a self reference
/// and the spill thunk stay inside the record.
pub struct BodyRecord {
    pub kind: RecordKind,
    /// The symbol's base name; a loader mints a fresh suffix.
    pub label: ArcStr,
    pub bytes: Box<[u8]>,
    pub align: u64,
    pub relocs: Vec<RecordReloc>,
    pub consts: Vec<KernelConst>,
    pub callees: Vec<Arc<BodyRecord>>,
    /// The kernel a body or thunk belongs to; a wrapper's is its body's.
    pub kernel: Arc<KernelSig>,
    pub thunk: Option<Arc<BodyRecord>>,
}

impl std::fmt::Debug for BodyRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BodyRecord")
            .field("kind", &self.kind)
            .field("label", &self.label)
            .field("bytes", &self.bytes.len())
            .field("relocs", &self.relocs.len())
            .field("consts", &self.consts.len())
            .field("callees", &self.callees.len())
            .finish()
    }
}

mod tag {
    pub const STR: u8 = 0;
    pub const VALUE: u8 = 1;
    pub const TYPE: u8 = 2;
    pub const QOP_SITE: u8 = 3;
    pub const FAST_FN: u8 = 4;
    pub const TYPED_FN: u8 = 5;
    pub const CAST: u8 = 6;
    pub const SITE_LEAF: u8 = 7;
    pub const SITE_BLOCK_WORDS: u8 = 8;

    pub const HELPER: u8 = 0;
    pub const CALLEE: u8 = 1;
    pub const OWNER: u8 = 2;
    pub const THUNK: u8 = 3;
    pub const LIBCALL: u8 = 4;
    pub const CONST: u8 = 5;

    pub const KERNEL: u8 = 0;
    pub const THUNK_KIND: u8 = 1;
    pub const WRAPPER: u8 = 2;
}

/// The relocation kinds a host emits; any other fails the record.
fn reloc_tag(r: Reloc) -> Result<u8, PackError> {
    Ok(match r {
        Reloc::Abs4 => 0,
        Reloc::Abs8 => 1,
        Reloc::X86PCRel4 => 2,
        Reloc::X86CallPCRel4 => 3,
        Reloc::X86CallPLTRel4 => 4,
        Reloc::X86GOTPCRel4 => 5,
        Reloc::Arm64Call => 6,
        _ => return Err(PackError::InvalidFormat),
    })
}

fn reloc_of(tag: u8) -> Result<Reloc, PackError> {
    Ok(match tag {
        0 => Reloc::Abs4,
        1 => Reloc::Abs8,
        2 => Reloc::X86PCRel4,
        3 => Reloc::X86CallPCRel4,
        4 => Reloc::X86CallPLTRel4,
        5 => Reloc::X86GOTPCRel4,
        6 => Reloc::Arm64Call,
        _ => return Err(PackError::UnknownTag),
    })
}

impl Pack for RecordReloc {
    fn encoded_len(&self) -> usize {
        let RecordReloc { offset, kind: _, target, addend } = self;
        let target = match target {
            RelocTarget::Helper(n) => n.encoded_len(),
            RelocTarget::Callee(i) | RelocTarget::Const(i) => i.encoded_len(),
            RelocTarget::Owner | RelocTarget::Thunk => 0,
            RelocTarget::LibCall(lc) => lc.to_string().encoded_len(),
        };
        offset.encoded_len() + 1 + 1 + target + addend.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let RecordReloc { offset, kind, target, addend } = self;
        offset.encode(buf)?;
        buf.put_u8(reloc_tag(*kind)?);
        match target {
            RelocTarget::Helper(n) => {
                buf.put_u8(tag::HELPER);
                n.encode(buf)?;
            }
            RelocTarget::Callee(i) => {
                buf.put_u8(tag::CALLEE);
                i.encode(buf)?;
            }
            RelocTarget::Owner => buf.put_u8(tag::OWNER),
            RelocTarget::Thunk => buf.put_u8(tag::THUNK),
            RelocTarget::LibCall(lc) => {
                buf.put_u8(tag::LIBCALL);
                lc.to_string().encode(buf)?;
            }
            RelocTarget::Const(i) => {
                buf.put_u8(tag::CONST);
                i.encode(buf)?;
            }
        }
        addend.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let offset = u32::decode(buf)?;
        let kind = reloc_of(u8::decode(buf)?)?;
        let target = match u8::decode(buf)? {
            tag::HELPER => RelocTarget::Helper(ArcStr::decode(buf)?),
            tag::CALLEE => RelocTarget::Callee(u32::decode(buf)?),
            tag::OWNER => RelocTarget::Owner,
            tag::THUNK => RelocTarget::Thunk,
            tag::LIBCALL => {
                let name = String::decode(buf)?;
                RelocTarget::LibCall(
                    LibCall::from_str(&name).map_err(|_| PackError::InvalidFormat)?,
                )
            }
            tag::CONST => RelocTarget::Const(u32::decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        let addend = i64::decode(buf)?;
        Ok(RecordReloc { offset, kind, target, addend })
    }
}

/// A fast fn resolves by the builtin's name through the decoder's
/// registry snapshot.
fn fastcall_of(name: &ArcStr) -> Result<FastCall, PackError> {
    image::decoding(|d| d.fastcall(name)).flatten().ok_or(PackError::InvalidFormat)
}

impl Pack for KernelConst {
    fn encoded_len(&self) -> usize {
        1 + match self {
            KernelConst::Str(s) => s.encoded_len(),
            KernelConst::Value(v) => v.encoded_len(),
            KernelConst::Type(t) => t.encoded_len(),
            KernelConst::QopSite(q) => {
                image::handler_len(&q.handler)
                    + q.own_top.encoded_len()
                    + q.spec.encoded_len()
            }
            KernelConst::FastFn { name, .. } | KernelConst::TypedFn { name, .. } => {
                name.encoded_len()
            }
            KernelConst::Cast(_) | KernelConst::SiteBlockWords => 0,
            KernelConst::SiteLeaf(l) => kernel_abi::site_leaf_len(l),
        }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        match self {
            KernelConst::Str(s) => {
                buf.put_u8(tag::STR);
                s.encode(buf)
            }
            KernelConst::Value(v) => {
                buf.put_u8(tag::VALUE);
                v.encode(buf)
            }
            KernelConst::Type(t) => {
                buf.put_u8(tag::TYPE);
                t.encode(buf)
            }
            KernelConst::QopSite(q) => {
                buf.put_u8(tag::QOP_SITE);
                image::handler_encode(&q.handler, buf)?;
                q.own_top.encode(buf)?;
                q.spec.encode(buf)
            }
            KernelConst::FastFn { name, .. } => {
                buf.put_u8(tag::FAST_FN);
                name.encode(buf)
            }
            KernelConst::TypedFn { name, .. } => {
                buf.put_u8(tag::TYPED_FN);
                name.encode(buf)
            }
            KernelConst::Cast(_) => Ok(buf.put_u8(tag::CAST)),
            KernelConst::SiteLeaf(l) => {
                buf.put_u8(tag::SITE_LEAF);
                kernel_abi::site_leaf_encode(l, buf)
            }
            KernelConst::SiteBlockWords => Ok(buf.put_u8(tag::SITE_BLOCK_WORDS)),
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(match u8::decode(buf)? {
            tag::STR => KernelConst::Str(Box::new(ArcStr::decode(buf)?)),
            tag::VALUE => KernelConst::Value(Box::new(Value::decode(buf)?)),
            tag::TYPE => KernelConst::Type(Box::new(Type::decode(buf)?)),
            tag::QOP_SITE => {
                let handler = image::handler_decode(buf)?;
                let own_top = ExprId::decode(buf)?;
                let spec = Expr::decode(buf)?;
                KernelConst::QopSite(Box::new(QopSite { handler, own_top, spec }))
            }
            tag::FAST_FN => {
                let name = ArcStr::decode(buf)?;
                match fastcall_of(&name)? {
                    FastCall::Plain(f) => KernelConst::FastFn { name, f },
                    FastCall::Typed(_) => return Err(PackError::InvalidFormat),
                }
            }
            tag::TYPED_FN => {
                let name = ArcStr::decode(buf)?;
                match fastcall_of(&name)? {
                    FastCall::Typed(f) => KernelConst::TypedFn { name, f },
                    FastCall::Plain(_) => return Err(PackError::InvalidFormat),
                }
            }
            tag::CAST => KernelConst::Cast(cast_typed),
            tag::SITE_LEAF => KernelConst::SiteLeaf(kernel_abi::site_leaf_decode(buf)?),
            tag::SITE_BLOCK_WORDS => KernelConst::SiteBlockWords,
            _ => return Err(PackError::UnknownTag),
        })
    }
}

fn kind_tag(k: RecordKind) -> u8 {
    match k {
        RecordKind::Kernel => tag::KERNEL,
        RecordKind::Thunk => tag::THUNK_KIND,
        RecordKind::Wrapper => tag::WRAPPER,
    }
}

/// A record is an image object: a body shared by several regions is
/// written once, and its callees before it.
pub(crate) fn record_len(r: &Arc<BodyRecord>) -> usize {
    image::object_len(
        Arc::as_ptr(r) as usize,
        |e| &e.records,
        || {
            let BodyRecord {
                kind: _,
                label,
                bytes,
                align,
                relocs,
                consts,
                callees,
                kernel,
                thunk,
            } = &**r;
            1 + label.encoded_len()
                + varint_len(bytes.len() as u64)
                + bytes.len()
                + varint_len(*align)
                + image::slice_len(relocs)
                + image::slice_len(consts)
                + varint_len(callees.len() as u64)
                + callees.iter().map(record_len).sum::<usize>()
                + kernel_abi::kernel_sig_len(kernel)
                + 1
                + thunk.as_ref().map_or(0, record_len)
        },
    )
}

pub(crate) fn record_encode(
    r: &Arc<BodyRecord>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    image::object_encode(
        Arc::as_ptr(r) as usize,
        |e| &mut e.records,
        buf,
        |buf| {
            let BodyRecord {
                kind,
                label,
                bytes,
                align,
                relocs,
                consts,
                callees,
                kernel,
                thunk,
            } = &**r;
            buf.put_u8(kind_tag(*kind));
            label.encode(buf)?;
            encode_varint(bytes.len() as u64, buf);
            buf.put_slice(bytes);
            encode_varint(*align, buf);
            image::slice_encode(relocs, buf)?;
            image::slice_encode(consts, buf)?;
            encode_varint(callees.len() as u64, buf);
            for c in callees {
                record_encode(c, buf)?;
            }
            kernel_abi::kernel_sig_encode(kernel, buf)?;
            match thunk {
                None => Ok(buf.put_u8(0)),
                Some(t) => {
                    buf.put_u8(1);
                    record_encode(t, buf)
                }
            }
        },
    )
}

pub(crate) fn record_decode(buf: &mut impl Buf) -> Result<Arc<BodyRecord>, PackError> {
    image::object_decode(
        buf,
        |d| &mut d.records,
        |buf| {
            let kind = match u8::decode(buf)? {
                tag::KERNEL => RecordKind::Kernel,
                tag::THUNK_KIND => RecordKind::Thunk,
                tag::WRAPPER => RecordKind::Wrapper,
                _ => return Err(PackError::UnknownTag),
            };
            let label = ArcStr::decode(buf)?;
            let n = decode_varint(buf)? as usize;
            if buf.remaining() < n {
                return Err(PackError::BufferShort);
            }
            let mut bytes = vec![0u8; n];
            buf.copy_to_slice(&mut bytes);
            let align = decode_varint(buf)?;
            let relocs: Vec<RecordReloc> = Pack::decode(buf)?;
            let consts: Vec<KernelConst> = Pack::decode(buf)?;
            let n = decode_varint(buf)? as usize;
            let mut callees = Vec::with_capacity(n.min(64));
            for _ in 0..n {
                callees.push(record_decode(buf)?);
            }
            let kernel = kernel_abi::kernel_sig_decode(buf)?;
            let thunk = match u8::decode(buf)? {
                0 => None,
                1 => Some(record_decode(buf)?),
                _ => return Err(PackError::UnknownTag),
            };
            Ok(Arc::new(BodyRecord {
                kind,
                label,
                bytes: bytes.into(),
                align,
                relocs,
                consts,
                callees,
                kernel,
                thunk,
            }))
        },
        |b| record_decode(b),
    )
}
