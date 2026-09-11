//! The registration image: a compiler session's state after the
//! package root modules compiled and before any cycle ran, so a warm
//! start restores it instead of compiling. Layout: magic, version, the
//! id counts, then the environment, the definitions, the context's
//! tables, the root nodes and the root scope, all under one image
//! session. The writer measures everything first so relocated ids can
//! be sorted, then encodes.

use super::{
    DecodeImage, EncodeImage, IdCounts, ImageDecoder, ImageEncoder, defs, nodes,
    scope_decode, scope_encode, scope_len,
};
use crate::{
    BindId, BuiltinBindInfo, ExecCtx, LambdaId, Node, Rt, Scope, UserEvent,
    expr::{ExprId, ModPath},
    node::lambda::LambdaDef,
    profile::{self, Phase},
    typ::Type,
};
use arcstr::ArcStr;
use bytes::{Buf, BufMut, Bytes, BytesMut};
use compact_str::CompactString;
use log::{info, warn};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};

const MAGIC: &[u8; 4] = b"GXIM";

/// The registration image's format; a cache key includes it.
pub const REGISTRATION_FORMAT: u8 = 2;

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
    rec_defs: Vec<LambdaId>,
    fn_forward_resolutions: Vec<(BindId, LambdaId)>,
    connect_targets: Vec<BindId>,
    batch_connect_targets: Vec<BindId>,
    predeclared_mods: Vec<ModPath>,
    tags: Vec<ArcStr>,
}

impl<'a, R: Rt, E: UserEvent> Tables<'a, R, E> {
    fn collect(ctx: &'a ExecCtx<R, E>) -> Result<Self, PackError> {
        let mut defs: Vec<&LambdaDef<R, E>> = ctx
            .lambda_defs
            .values()
            .map(|v| v.downcast_ref::<LambdaDef<R, E>>().ok_or(PackError::InvalidFormat))
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
        let sorted = |it: &mut dyn Iterator<Item = LambdaId>| {
            let mut v: Vec<_> = it.collect();
            v.sort();
            v
        };
        let rec_defs = sorted(&mut ctx.rec_defs.iter().copied());
        let mut fn_forward_resolutions: Vec<_> =
            ctx.fn_forward_resolutions.iter().map(|(b, l)| (*b, *l)).collect();
        fn_forward_resolutions.sort();
        let mut connect_targets: Vec<_> = ctx.connect_targets.iter().copied().collect();
        connect_targets.sort();
        let mut batch_connect_targets: Vec<_> =
            ctx.batch_connect_targets.iter().copied().collect();
        batch_connect_targets.sort();
        let mut predeclared_mods: Vec<_> = ctx.predeclared_mods.iter().cloned().collect();
        predeclared_mods.sort();
        let mut tags: Vec<_> = ctx.tags.iter().cloned().collect();
        tags.sort();
        Ok(Tables {
            defs,
            bind_to_lambda,
            builtin_bindings,
            rec_defs,
            fn_forward_resolutions,
            connect_targets,
            batch_connect_targets,
            predeclared_mods,
            tags,
        })
    }

    fn len(&self) -> usize {
        varint_len(self.defs.len() as u64)
            + self.defs.iter().map(|d| defs::def_len(d)).sum::<usize>()
            + self.bind_to_lambda.encoded_len()
            + self.builtin_bindings.encoded_len()
            + self.rec_defs.encoded_len()
            + self.fn_forward_resolutions.encoded_len()
            + self.connect_targets.encoded_len()
            + self.batch_connect_targets.encoded_len()
            + self.predeclared_mods.encoded_len()
            + self.tags.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        encode_varint(self.defs.len() as u64, buf);
        for d in &self.defs {
            defs::def_encode(d, buf)?;
        }
        self.bind_to_lambda.encode(buf)?;
        self.builtin_bindings.encode(buf)?;
        self.rec_defs.encode(buf)?;
        self.fn_forward_resolutions.encode(buf)?;
        self.connect_targets.encode(buf)?;
        self.batch_connect_targets.encode(buf)?;
        self.predeclared_mods.encode(buf)?;
        self.tags.encode(buf)
    }
}

fn restore_tables<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
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
    ctx.rec_defs.extend(<Vec<LambdaId> as Pack>::decode(buf)?);
    ctx.fn_forward_resolutions.extend(<Vec<(BindId, LambdaId)> as Pack>::decode(buf)?);
    ctx.connect_targets.extend(<Vec<BindId> as Pack>::decode(buf)?);
    ctx.batch_connect_targets.extend(<Vec<BindId> as Pack>::decode(buf)?);
    ctx.predeclared_mods.extend(<Vec<ModPath> as Pack>::decode(buf)?);
    ctx.tags.extend(<Vec<ArcStr> as Pack>::decode(buf)?);
    Ok(())
}

impl<R: Rt, E: UserEvent> ExecCtx<R, E> {
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
            ("active lambdas", !self.active_lambdas.is_empty()),
            ("kernels", !self.fusion.kernels.lock().is_empty()),
            ("core hook sites", !self.core_hook_sites.is_empty()),
        ];
        if let Some((what, _)) = busy.iter().find(|(_, b)| *b) {
            warn!("the session is not quiescent: {what}");
            return Err(PackError::Application(NOT_QUIESCENT));
        }
        let tables = Tables::collect(self)?;
        let mut enc = ImageEncoder::new();
        let body_bound = {
            let _s = EncodeImage::new(&mut enc);
            let env_len = self.env.encoded_len();
            let tables_len = tables.len();
            let nodes_len = varint_len(nodes.len() as u64)
                + nodes
                    .iter()
                    .map(|(id, n)| id.encoded_len() + n.image_len())
                    .sum::<usize>();
            info!(
                "registration image bounds: env {env_len} defs {tables_len} nodes {nodes_len} bytes"
            );
            env_len
                + tables_len
                + nodes_len
                + scope_len(scope)
                + 1
                + program.map_or(0, |p| p.encoded_len())
        };
        enc.sort_ids();
        let counts = enc.counts();
        let mut buf =
            BytesMut::with_capacity(MAGIC.len() + 1 + counts.encoded_len() + body_bound);
        buf.put_slice(MAGIC);
        buf.put_u8(REGISTRATION_FORMAT);
        counts.encode(&mut buf)?;
        {
            let _s = EncodeImage::new(&mut enc);
            self.env.encode(&mut buf)?;
            tables.encode(&mut buf)?;
            encode_varint(nodes.len() as u64, &mut buf);
            for (id, n) in nodes {
                id.encode(&mut buf)?;
                n.image_encode(&mut buf)?;
            }
            scope_encode(scope, &mut buf)?;
            program.is_some().encode(&mut buf)?;
            if let Some(p) = program {
                p.encode(&mut buf)?;
            }
        }
        Ok(buf.freeze())
    }

    /// Restore a registration image into this session, which must have
    /// its builtins registered and nothing compiled. The decoder stays
    /// with the session for anything decoded later.
    pub fn read_registration(
        &mut self,
        mut bytes: &[u8],
    ) -> Result<Registration<R, E>, PackError> {
        if bytes.len() < MAGIC.len() + 1 || &bytes[..MAGIC.len()] != MAGIC {
            return Err(PackError::InvalidFormat);
        }
        bytes.advance(MAGIC.len());
        if bytes.get_u8() != REGISTRATION_FORMAT {
            return Err(PackError::InvalidFormat);
        }
        let counts = IdCounts::decode(&mut bytes)?;
        let mut dec = ImageDecoder::new(counts);
        let restored = {
            let _s = DecodeImage::new(&mut dec);
            let p = profile::phase(Phase::ImageEnv);
            self.env = Pack::decode(&mut bytes)?;
            drop(p);
            let p = profile::phase(Phase::ImageDefs);
            restore_tables(self, &mut bytes)?;
            drop(p);
            let _p = profile::phase(Phase::ImageNodes);
            let n = decode_varint(&mut bytes)? as usize;
            let mut nodes = Vec::with_capacity(n);
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
            Registration { nodes, scope, program }
        };
        if bytes.has_remaining() {
            return Err(PackError::InvalidFormat);
        }
        self.image_decoder = Some(dec);
        Ok(restored)
    }
}
