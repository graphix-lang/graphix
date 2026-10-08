//! The registration image: a compiler session's state after the
//! package root modules compiled and before any cycle ran, so a warm
//! start restores it instead of compiling. Layout: magic, version, the
//! ISA description, the heap, trailer and id-count offsets, then the
//! environment, the definitions, the context's tables, the root nodes,
//! the root scope and the program root, all under one image session,
//! then the instance heap, the shared objects' definitions, the trailer
//! (the instance table, the eager object counts and every definition's
//! offset) and last the id counts, known once the session ends. The
//! writer encodes in one pass; the reader takes the counts before
//! anything decodes.

use super::{
    Compiled, DecodeImage, EncodeImage, IdCounts, ImageBuf, ImageDecoder, ImageEncoder,
    Restored, SharedDecoder, defs, nodes, scope_decode, scope_encode,
};
use crate::{
    BindId, BuiltinBindInfo, CompileCtx, ExecCtx, ExecState, LambdaId, LambdaInstanceId,
    Node, Rt, Saved, Scope, UserEvent,
    env::Env,
    expr::{ExprId, ModPath},
    image,
    node::lambda::LambdaDef,
    profile::{self, Phase},
    typ::Type,
};
use ahash::AHashMap;
use arcstr::ArcStr;
use bytes::{Buf, BufMut, Bytes};
use compact_str::CompactString;
use log::{info, warn};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};

const MAGIC: &[u8; 4] = b"GXIM";

/// The registration image's format; a cache key includes it.
pub const REGISTRATION_FORMAT: u8 = 38;

/// `PackError::Application` payload: the session holds state the
/// image cannot carry (a pending settle, an open gate, a kernel).
pub const NOT_QUIESCENT: u64 = 2;

/// The program a session compiled after its packages, when it did: the
/// runtime hands its embedder the root's id, output flag and type.
#[derive(Debug, Clone)]
pub struct ProgramRoot {
    pub id: ExprId,
    pub output: bool,
    pub typ: Type,
}

impl Pack for ProgramRoot {
    fn encoded_len(&self) -> usize {
        self.id.encoded_len() + self.output.encoded_len() + self.typ.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.id.encode(buf)?;
        self.output.encode(buf)?;
        self.typ.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(ProgramRoot {
            id: Pack::decode(buf)?,
            output: Pack::decode(buf)?,
            typ: Pack::decode(buf)?,
        })
    }
}

/// The root nodes and scope a restored registration hands back to the
/// runtime, which owns them exactly as it owns compiled ones.
pub struct Registration<R: Rt, E: UserEvent> {
    pub nodes: Vec<(ExprId, Node<R, E>)>,
    pub scope: Scope,
    pub program: Option<ProgramRoot>,
}

/// The context's tables that registration fills and later compiles
/// read; each is a plain list on the wire.
struct Tables<'a, R: Rt, E: UserEvent> {
    defs: Vec<&'a LambdaDef<R, E>>,
    bind_to_lambda: Vec<(BindId, LambdaId)>,
    builtin_bindings: Vec<(ModPath, CompactString, BuiltinBindInfo)>,
    fn_forward_resolutions: Vec<(BindId, LambdaId)>,
    connect_targets: Vec<BindId>,
    batch_connect_targets: Vec<BindId>,
    tags: Vec<ArcStr>,
}

impl<'a, R: Rt, E: UserEvent> Tables<'a, R, E> {
    fn collect(ctx: &'a CompileCtx<R, E>) -> Result<Self, PackError> {
        let mut defs: Vec<&LambdaDef<R, E>> = ctx
            .lambda_defs
            .iter()
            .map(|(_, v)| {
                v.downcast_ref::<LambdaDef<R, E>>().ok_or(PackError::InvalidFormat)
            })
            .collect::<Result<_, _>>()?;
        defs.sort_by_key(|d| d.id);
        let mut bind_to_lambda: Vec<(BindId, LambdaId)> = ctx
            .bind_to_lambda
            .iter()
            .map(|(b, v)| {
                let d = v
                    .downcast_ref::<LambdaDef<R, E>>()
                    .ok_or(PackError::InvalidFormat)?;
                Ok((*b, d.id))
            })
            .collect::<Result<_, PackError>>()?;
        bind_to_lambda.sort();
        let mut builtin_bindings: Vec<_> = ctx
            .builtin_bindings
            .iter()
            .map(|((s, n), i)| (s.clone(), n.clone(), i.clone()))
            .collect();
        builtin_bindings.sort_by(|a, b| (&a.0, &a.1).cmp(&(&b.0, &b.1)));
        let mut fn_forward_resolutions: Vec<_> =
            ctx.fn_forward_resolutions.iter().map(|(b, l)| (*b, *l)).collect();
        fn_forward_resolutions.sort();
        let mut connect_targets: Vec<_> = ctx.connect_targets.iter().copied().collect();
        connect_targets.sort();
        let mut batch_connect_targets: Vec<_> =
            ctx.batch_connect_targets.iter().copied().collect();
        batch_connect_targets.sort();
        let mut tags: Vec<_> = ctx.tags.iter().cloned().collect();
        tags.sort();
        Ok(Tables {
            defs,
            bind_to_lambda,
            builtin_bindings,
            fn_forward_resolutions,
            connect_targets,
            batch_connect_targets,
            tags,
        })
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        encode_varint(self.defs.len() as u64, buf);
        for d in &self.defs {
            defs::def_encode(d, buf)?;
        }
        self.bind_to_lambda.encode(buf)?;
        self.builtin_bindings.encode(buf)?;
        self.fn_forward_resolutions.encode(buf)?;
        self.connect_targets.encode(buf)?;
        self.batch_connect_targets.encode(buf)?;
        self.tags.encode(buf)
    }
}

fn restore_tables<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    buf: &mut impl Buf,
) -> Result<(), PackError> {
    let n = decode_varint(buf)? as usize;
    for _ in 0..n {
        defs::def_decode(ctx, buf)?;
    }
    let bind_to_lambda: Vec<(BindId, LambdaId)> = Pack::decode(buf)?;
    for (b, l) in bind_to_lambda {
        let v = ctx.lambda_defs.get(&l).ok_or(PackError::InvalidFormat)?.clone();
        ctx.bind_to_lambda.insert(b, v);
    }
    let builtin_bindings: Vec<(ModPath, CompactString, BuiltinBindInfo)> =
        Pack::decode(buf)?;
    for (s, n, i) in builtin_bindings {
        ctx.builtin_bindings.insert((s, n), i);
    }
    ctx.fn_forward_resolutions.extend(<Vec<(BindId, LambdaId)> as Pack>::decode(buf)?);
    ctx.connect_targets.extend(<Vec<BindId> as Pack>::decode(buf)?);
    ctx.batch_connect_targets.extend(<Vec<BindId> as Pack>::decode(buf)?);
    ctx.tags.extend(<Vec<ArcStr> as Pack>::decode(buf)?);
    Ok(())
}

impl<R: Rt, E: UserEvent> ExecState<R, E> {
    /// The registration image of this session with `nodes` as the root
    /// nodes, or an error naming what the image cannot carry.
    pub fn write_registration(
        &self,
        nodes: &[(ExprId, &Node<R, E>)],
        scope: &Scope,
        program: Option<&ProgramRoot>,
    ) -> Result<Bytes, PackError> {
        let busy = [
            ("pending settles", !self.pending_settles.iter().all(|s| s.is_empty())),
            ("an open definition gate", self.def_gate_depth != 0),
            ("lambdas resolving", !self.resolving_lambdas.lock().is_empty()),
            ("core hook sites", !self.core_hook_sites.lock().is_empty()),
        ];
        if let Some((what, _)) = busy.iter().find(|(_, b)| *b) {
            warn!("the session is not quiescent: {what}");
            return Err(PackError::Application(NOT_QUIESCENT));
        }
        let tables = Tables::collect(self)?;
        let mut enc = ImageEncoder::new();
        enc.ext::<Compiled>().defer_instances = program.is_some();
        let isa = crate::fusion::emit::isa_description();
        let mut buf = ImageBuf::default();
        buf.put_slice(MAGIC);
        buf.put_u8(REGISTRATION_FORMAT);
        isa.encode(&mut buf)?;
        let offsets_at = buf.len();
        buf.put_u64(0);
        buf.put_u64(0);
        buf.put_u64(0);
        let body_at = buf.len();
        EncodeImage::with(&mut enc, || -> Result<(), PackError> {
            let p = profile::phase(Phase::ImageEncode);
            self.env.encode(&mut buf)?;
            let env_bytes = buf.len() - body_at;
            tables.encode(&mut buf)?;
            let defs_bytes = buf.len() - body_at - env_bytes;
            encode_varint(nodes.len() as u64, &mut buf);
            for (id, n) in nodes {
                id.encode(&mut buf)?;
                n.image_encode(&mut buf)?;
            }
            let nodes_bytes = buf.len() - body_at - env_bytes - defs_bytes;
            scope_encode(scope, &mut buf)?;
            program.is_some().encode(&mut buf)?;
            if let Some(p) = program {
                p.encode(&mut buf)?;
            }
            let heap_at = buf.len();
            drop(p);
            let p = profile::phase(Phase::ImageHeap);
            loop {
                let Some((id, body)) =
                    image::encoding(|e| e.ext::<Compiled>().deferred.pop()).flatten()
                else {
                    break;
                };
                let at = buf.len() as u64;
                body(&mut buf)?;
                image::encoding(|e| e.ext::<Compiled>().instances.insert(id, at));
            }
            let defs_at = buf.len();
            let offsets = image::encoding(|e| e.finish(&mut buf))
                .ok_or(PackError::InvalidFormat)?;
            let table_at = buf.len();
            drop(p);
            let _p = profile::phase(Phase::ImageTrailer);
            let instances =
                image::encoding(|e| std::mem::take(&mut e.ext::<Compiled>().instances))
                    .unwrap_or_default();
            encode_varint(instances.len() as u64, &mut buf);
            for (id, at) in instances {
                id.encode(&mut buf)?;
                encode_varint(at, &mut buf);
            }
            encode_varint(offsets.len() as u64, &mut buf);
            for at in offsets {
                encode_varint(at, &mut buf);
            }
            buf.patch_u64(offsets_at, heap_at as u64);
            buf.patch_u64(offsets_at + 8, table_at as u64);
            info!(
                "registration image: env {env_bytes} defs {defs_bytes} nodes {nodes_bytes} \
                 heap {} objects {} total {} bytes",
                defs_at - heap_at,
                table_at - defs_at,
                buf.len()
            );
            Ok(())
        })?;
        let counts_at = buf.len();
        enc.counts().encode(&mut buf)?;
        buf.patch_u64(offsets_at + 16, counts_at as u64);
        Ok(buf.freeze())
    }
}

impl<R: Rt, E: UserEvent> ExecCtx<'_, R, E> {
    /// Restore a registration image into this session, which must have
    /// its builtins registered and nothing compiled. The decoder stays
    /// with the session for anything decoded later. A bad image fails
    /// the read and leaves the session as it was, so it can compile
    /// cold.
    pub fn read_registration(
        &mut self,
        image: Bytes,
    ) -> Result<Registration<R, E>, PackError> {
        let saved = Saved::take(self);
        let mut nodes = Vec::new();
        match self.decode_registration(image, &mut nodes) {
            Ok((dec, scope, program)) => {
                let fresh = self.image_decoder.set(dec).is_ok();
                debug_assert!(fresh, "a session restores one registration image");
                self.apply_deferred();
                Ok(Registration { nodes, scope, program })
            }
            Err(e) => {
                for (_, mut n) in nodes {
                    n.delete(self);
                }
                self.drop_deferred();
                saved.restore(self);
                Err(e)
            }
        }
    }

    fn decode_registration(
        &mut self,
        image: Bytes,
        nodes: &mut Vec<(ExprId, Node<R, E>)>,
    ) -> Result<(SharedDecoder, Scope, Option<ProgramRoot>), PackError> {
        let mut bytes: &[u8] = &image;
        // CR claude for eric: [risk] Nothing checks an entry's contents. Past the
        // magic, format, ISA, header offsets and id counts, every byte is trusted,
        // kernel machine code included (define_function_bytes installs it as stored).
        // One flipped bit in a cached entry can make a warm run print a changed string
        // literal with exit 0, make a fused fact() return 1, rename a builtin so every
        // program calling it fails on every run, or crash in restored JIT code
        // (SIGSEGV/SIGILL). An entry that fails the read is never written again
        // (graphix-shell/src/lib.rs:287-310), and entries are renamed into place with
        // no fsync (graphix-shell/src/cache.rs:198), so a crash that leaves an empty
        // entry means cold starts until the next build. A checksum over each part the
        // restore reads (the eager part, each heap instance, each kernel record),
        // checked before use, with a mismatch handled as a failed read that rewrites
        // the entry, would close this. probe:
        // design/review-2026-10-05/repro/x-image-03.sh (x-image-03)
        if bytes.len() < MAGIC.len() + 1 || &bytes[..MAGIC.len()] != MAGIC {
            return Err(PackError::InvalidFormat);
        }
        bytes.advance(MAGIC.len());
        if bytes.get_u8() != REGISTRATION_FORMAT {
            return Err(PackError::InvalidFormat);
        }
        let isa = String::decode(&mut bytes)?;
        if isa != crate::fusion::emit::isa_description() {
            warn!("the image was written for another isa: {isa}");
            return Err(PackError::InvalidFormat);
        }
        if bytes.remaining() < 24 {
            return Err(PackError::BufferShort);
        }
        let heap_at = bytes.get_u64() as usize;
        let table_at = bytes.get_u64() as usize;
        let counts_at = bytes.get_u64() as usize;
        if heap_at > table_at || table_at > counts_at || counts_at > image.len() {
            return Err(PackError::InvalidFormat);
        }
        let mut counts_bytes = &image[counts_at..];
        let counts = IdCounts::decode(&mut counts_bytes)?;
        if counts_bytes.has_remaining() {
            return Err(PackError::InvalidFormat);
        }
        // the instance table's ids relocate inside the session; its
        // offsets, after it, are plain varints the decoder is made with
        let mut table = &image[table_at..counts_at];
        let n_instances = decode_varint(&mut table)? as usize;
        let instances_at = table;
        for _ in 0..n_instances {
            decode_varint(&mut table)?;
            decode_varint(&mut table)?;
        }
        let n = decode_varint(&mut table)? as usize;
        // every offset is one varint at least
        let mut offsets = Vec::with_capacity(n.min(table.len()));
        for _ in 0..n {
            offsets.push(decode_varint(&mut table)?);
        }
        if table.has_remaining() {
            return Err(PackError::InvalidFormat);
        }
        let shared = ImageDecoder::new(counts, image.clone(), offsets)?.share();
        let mut dec = shared.lock();
        dec.ext::<Restored>().fastcalls = self
            .registry
            .builtins
            .iter()
            .filter_map(|(name, b)| b.effect.fastcall().map(|f| (*name, f)))
            .collect();
        let (scope, program) =
            DecodeImage::with(&mut dec, || -> Result<_, PackError> {
                let mut table = instances_at;
                // every entry is two varints at least
                let mut instances =
                    AHashMap::with_capacity(n_instances.min(table.len() / 2));
                for _ in 0..n_instances {
                    let id = LambdaInstanceId::decode(&mut table)?;
                    let at = decode_varint(&mut table)?;
                    if !(heap_at as u64..table_at as u64).contains(&at) {
                        return Err(PackError::InvalidFormat);
                    }
                    instances.insert(id, at);
                }
                image::decoding(|d| d.ext::<Restored>().instances = instances);
                let p = profile::phase(Phase::ImageEnv);
                let env: Env = Pack::decode(&mut bytes)?;
                self.env = Env { ide: std::mem::take(&mut self.env.ide), ..env };
                drop(p);
                let p = profile::phase(Phase::ImageDefs);
                restore_tables(self, &mut bytes)?;
                drop(p);
                let _p = profile::phase(Phase::ImageNodes);
                // every node is one tag byte at least
                let n = decode_varint(&mut bytes)? as usize;
                nodes.reserve(n.min(bytes.len()));
                for _ in 0..n {
                    let id = ExprId::decode(&mut bytes)?;
                    let node = nodes::decode_node(self, &mut bytes)?;
                    nodes.push((id, node));
                }
                let scope = scope_decode(&mut bytes)?;
                let program = match bool::decode(&mut bytes)? {
                    true => Some(ProgramRoot::decode(&mut bytes)?),
                    false => None,
                };
                Ok((scope, program))
            })?;
        drop(dec);
        if image.len() - bytes.remaining() != heap_at {
            return Err(PackError::InvalidFormat);
        }
        Ok((shared, scope, program))
    }
}
