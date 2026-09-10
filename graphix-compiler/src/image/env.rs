//! The environment under an image: every map with its sharing, the
//! records that carry a position and an origin by hand (a foreign
//! position type and a shared origin), the rest derived.

use super::{origin_decode, origin_encode, origin_len, pos_decode, pos_encode, pos_len};
use crate::{
    BindId,
    env::{Bind, Env, ImplDef, ImportEntry, Map, TraitDef, TypeDef},
    expr::ModPath,
    shared_map::{self, SharedMap, SharedSet},
};
use bytes::{Buf, BufMut};
use compact_str::CompactString;
use netidx_core::pack::{Pack, PackError};

impl Pack for Bind {
    fn encoded_len(&self) -> usize {
        let Bind { id, export, typ, doc, scope, name, pos, ori, pattern, facet } = self;
        id.encoded_len()
            + export.encoded_len()
            + typ.encoded_len()
            + doc.encoded_len()
            + scope.encoded_len()
            + name.encoded_len()
            + pos_len(pos)
            + origin_len(ori)
            + pattern.encoded_len()
            + facet.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let Bind { id, export, typ, doc, scope, name, pos, ori, pattern, facet } = self;
        id.encode(buf)?;
        export.encode(buf)?;
        typ.encode(buf)?;
        doc.encode(buf)?;
        scope.encode(buf)?;
        name.encode(buf)?;
        pos_encode(pos, buf)?;
        origin_encode(ori, buf)?;
        pattern.encode(buf)?;
        facet.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(Bind {
            id: Pack::decode(buf)?,
            export: Pack::decode(buf)?,
            typ: Pack::decode(buf)?,
            doc: Pack::decode(buf)?,
            scope: Pack::decode(buf)?,
            name: Pack::decode(buf)?,
            pos: pos_decode(buf)?,
            ori: origin_decode(buf)?,
            pattern: Pack::decode(buf)?,
            facet: Pack::decode(buf)?,
        })
    }
}

impl Pack for TypeDef {
    fn encoded_len(&self) -> usize {
        let TypeDef { params, typ, rep, doc, pos, ori } = self;
        params.encoded_len()
            + typ.encoded_len()
            + rep.encoded_len()
            + doc.encoded_len()
            + pos_len(pos)
            + origin_len(ori)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let TypeDef { params, typ, rep, doc, pos, ori } = self;
        params.encode(buf)?;
        typ.encode(buf)?;
        rep.encode(buf)?;
        doc.encode(buf)?;
        pos_encode(pos, buf)?;
        origin_encode(ori, buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(TypeDef {
            params: Pack::decode(buf)?,
            typ: Pack::decode(buf)?,
            rep: Pack::decode(buf)?,
            doc: Pack::decode(buf)?,
            pos: pos_decode(buf)?,
            ori: origin_decode(buf)?,
        })
    }
}

impl Pack for ImportEntry {
    fn encoded_len(&self) -> usize {
        let ImportEntry { scope, name, keyword_anchored, pos, ori } = self;
        scope.encoded_len()
            + name.encoded_len()
            + keyword_anchored.encoded_len()
            + pos_len(pos)
            + origin_len(ori)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let ImportEntry { scope, name, keyword_anchored, pos, ori } = self;
        scope.encode(buf)?;
        name.encode(buf)?;
        keyword_anchored.encode(buf)?;
        pos_encode(pos, buf)?;
        origin_encode(ori, buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(ImportEntry {
            scope: Pack::decode(buf)?,
            name: Pack::decode(buf)?,
            keyword_anchored: Pack::decode(buf)?,
            pos: pos_decode(buf)?,
            ori: origin_decode(buf)?,
        })
    }
}

impl Pack for TraitDef {
    fn encoded_len(&self) -> usize {
        let TraitDef { id, name, scope, path, methods, hole, doc, pos, ori } = self;
        id.encoded_len()
            + name.encoded_len()
            + scope.encoded_len()
            + path.encoded_len()
            + methods.encoded_len()
            + hole.encoded_len()
            + doc.encoded_len()
            + pos_len(pos)
            + origin_len(ori)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let TraitDef { id, name, scope, path, methods, hole, doc, pos, ori } = self;
        id.encode(buf)?;
        name.encode(buf)?;
        scope.encode(buf)?;
        path.encode(buf)?;
        methods.encode(buf)?;
        hole.encode(buf)?;
        doc.encode(buf)?;
        pos_encode(pos, buf)?;
        origin_encode(ori, buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(TraitDef {
            id: Pack::decode(buf)?,
            name: Pack::decode(buf)?,
            scope: Pack::decode(buf)?,
            path: Pack::decode(buf)?,
            methods: Pack::decode(buf)?,
            hole: Pack::decode(buf)?,
            doc: Pack::decode(buf)?,
            pos: pos_decode(buf)?,
            ori: origin_decode(buf)?,
        })
    }
}

impl Pack for ImplDef {
    fn encoded_len(&self) -> usize {
        let ImplDef { trait_id, target, params, scope, methods, declared, pos, ori } =
            self;
        trait_id.encoded_len()
            + target.encoded_len()
            + params.encoded_len()
            + scope.encoded_len()
            + SharedMap(methods.clone()).encoded_len()
            + declared.encoded_len()
            + pos_len(pos)
            + origin_len(ori)
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let ImplDef { trait_id, target, params, scope, methods, declared, pos, ori } =
            self;
        trait_id.encode(buf)?;
        target.encode(buf)?;
        params.encode(buf)?;
        scope.encode(buf)?;
        SharedMap(methods.clone()).encode(buf)?;
        declared.encode(buf)?;
        pos_encode(pos, buf)?;
        origin_encode(ori, buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(ImplDef {
            trait_id: Pack::decode(buf)?,
            target: Pack::decode(buf)?,
            params: Pack::decode(buf)?,
            scope: Pack::decode(buf)?,
            methods: SharedMap::decode(buf)?.0,
            declared: Pack::decode(buf)?,
            pos: pos_decode(buf)?,
            ori: origin_decode(buf)?,
        })
    }
}

/// A scope-keyed map of name-keyed maps, shared at both levels.
type Nested<V> = Map<ModPath, Map<CompactString, V>>;

fn nested_len<V>(m: &Nested<V>) -> usize
where
    V: Pack + Clone + Send + Sync + 'static,
{
    shared_map::map_len(m, &mut |scope, inner| {
        scope.encoded_len()
            + shared_map::map_len(inner, &mut |name, v| {
                name.encoded_len() + v.encoded_len()
            })
    })
}

fn nested_encode<V, B: BufMut>(m: &Nested<V>, buf: &mut B) -> Result<(), PackError>
where
    V: Pack + Clone + Send + Sync + 'static,
{
    shared_map::map_encode(m, buf, &mut |scope, inner, buf| {
        scope.encode(buf)?;
        shared_map::map_encode(inner, buf, &mut |name, v, buf| {
            name.encode(buf)?;
            v.encode(buf)
        })
    })
}

fn nested_decode<V, B: Buf>(buf: &mut B) -> Result<Nested<V>, PackError>
where
    V: Pack + Clone + Send + Sync + 'static,
{
    shared_map::map_decode(buf, &mut |buf| {
        let scope = ModPath::decode(buf)?;
        let inner = shared_map::map_decode(buf, &mut |buf| {
            Ok((CompactString::decode(buf)?, V::decode(buf)?))
        })?;
        Ok((scope, inner))
    })
}

/// The IDE side-channel is process state and is not in the image; a
/// restored environment starts with none.
impl Pack for Env {
    fn encoded_len(&self) -> usize {
        let Env {
            by_id,
            byref_chain,
            binds,
            modules,
            typedefs,
            names,
            abstract_reps,
            traits,
            trait_defs,
            trait_methods,
            impls,
            poly_binds,
            package_roots,
            ide_binds,
            lsp_mode,
            ide: _,
        } = self;
        SharedMap(by_id.clone()).encoded_len()
            + SharedMap(byref_chain.clone()).encoded_len()
            + nested_len(binds)
            + SharedSet(modules.clone()).encoded_len()
            + nested_len(typedefs)
            + SharedMap(names.clone()).encoded_len()
            + SharedMap(abstract_reps.clone()).encoded_len()
            + nested_len(traits)
            + SharedMap(trait_defs.clone()).encoded_len()
            + SharedMap(trait_methods.clone()).encoded_len()
            + SharedMap(impls.clone()).encoded_len()
            + SharedSet(poly_binds.clone()).encoded_len()
            + SharedSet(package_roots.clone()).encoded_len()
            + nested_len(ide_binds)
            + lsp_mode.encoded_len()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let Env {
            by_id,
            byref_chain,
            binds,
            modules,
            typedefs,
            names,
            abstract_reps,
            traits,
            trait_defs,
            trait_methods,
            impls,
            poly_binds,
            package_roots,
            ide_binds,
            lsp_mode,
            ide: _,
        } = self;
        SharedMap(by_id.clone()).encode(buf)?;
        SharedMap(byref_chain.clone()).encode(buf)?;
        nested_encode(binds, buf)?;
        SharedSet(modules.clone()).encode(buf)?;
        nested_encode(typedefs, buf)?;
        SharedMap(names.clone()).encode(buf)?;
        SharedMap(abstract_reps.clone()).encode(buf)?;
        nested_encode(traits, buf)?;
        SharedMap(trait_defs.clone()).encode(buf)?;
        SharedMap(trait_methods.clone()).encode(buf)?;
        SharedMap(impls.clone()).encode(buf)?;
        SharedSet(poly_binds.clone()).encode(buf)?;
        SharedSet(package_roots.clone()).encode(buf)?;
        nested_encode(ide_binds, buf)?;
        lsp_mode.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(Env {
            by_id: SharedMap::<BindId, Bind>::decode(buf)?.0,
            byref_chain: SharedMap::decode(buf)?.0,
            binds: nested_decode(buf)?,
            modules: SharedSet::decode(buf)?.0,
            typedefs: nested_decode(buf)?,
            names: SharedMap::decode(buf)?.0,
            abstract_reps: SharedMap::decode(buf)?.0,
            traits: nested_decode(buf)?,
            trait_defs: SharedMap::decode(buf)?.0,
            trait_methods: SharedMap::decode(buf)?.0,
            impls: SharedMap::decode(buf)?.0,
            poly_binds: SharedSet::decode(buf)?.0,
            package_roots: SharedSet::decode(buf)?.0,
            ide_binds: nested_decode(buf)?,
            lsp_mode: Pack::decode(buf)?,
            ide: None,
        })
    }
}
