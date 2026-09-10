//! `Pack` for the environment's persistent maps that preserves their
//! sharing. A persistent update copies the path from the root and
//! shares every other node, so an environment's snapshots are mostly
//! one tree; packed by contents they would decode to as many trees as
//! there are snapshots. [`SharedMap`] and [`SharedSet`] walk the tree
//! through the chunkmap's structural API and write each node once, and
//! a reference to it afterwards, so the decoded forest shares exactly
//! what the encoded one did. A definition is written before its
//! subtrees and numbered after them, so a reference names a node by
//! the rank at which the decoder completed it.
//!
//! The table of nodes seen belongs to the caller: an image writer for
//! encoding, the runtime for decoding, so an instance decoded later
//! resolves references into nodes decoded earlier. A session guard
//! installs the table for the duration of a call; without one, a call
//! is self-contained.
//!
//! `encoded_len` is an upper bound, not the exact length: which nodes
//! are references depends on what has been written by the time the
//! encode runs, and a length walk counts every node not yet written as
//! a definition. A container that length-prefixes one of these values
//! reserves the bound and patches the prefix after encoding; the
//! packer's own length-wrapped derives must not hold one.

use crate::env::{Map, Set};
use ahash::AHashMap;
use bytes::{Buf, BufMut};
use immutable_chunkmap::map::{NodeHandle, NodeRef};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use std::{
    any::{Any, TypeId},
    cell::Cell,
    marker::PhantomData,
    ptr::NonNull,
};

const SIZE: usize = 16;
const EMPTY: u8 = 0;
const REF: u8 = 1;
const NODE: u8 = 2;

/// A map packed with its sharing. The wrapped map is the environment's
/// own (an `Arc` clone), so wrapping is free.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedMap<K: Ord + Clone, V: Clone>(pub Map<K, V>);

/// A set packed with its sharing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedSet<K: Ord + Clone>(pub Set<K>);

struct Seen<K: Ord + Clone, V: Clone> {
    id: u64,
    /// Pins the identity while the table holds it.
    _handle: NodeHandle<K, V, SIZE>,
}

struct EncodeNodes<K: Ord + Clone, V: Clone> {
    by_identity: AHashMap<usize, Seen<K, V>>,
}

/// The nodes an encoder has seen, across every map and set packed
/// under one [`EncodeSession`]. One per image.
#[derive(Default)]
pub struct EncodeTable {
    by_type: AHashMap<TypeId, Box<dyn Any + Send + Sync>>,
}

/// The nodes a decoder has built, in definition order, across every
/// map and set unpacked under a [`DecodeSession`] over it. One per
/// runtime, since an instance decoded later refers into it.
#[derive(Default)]
pub struct DecodeTable {
    by_type: AHashMap<TypeId, Box<dyn Any + Send + Sync>>,
}

impl EncodeTable {
    fn nodes<K, V>(&mut self) -> &mut EncodeNodes<K, V>
    where
        K: Ord + Clone + Send + Sync + 'static,
        V: Clone + Send + Sync + 'static,
    {
        self.by_type
            .entry(TypeId::of::<(K, V)>())
            .or_insert_with(|| {
                Box::new(EncodeNodes::<K, V> { by_identity: AHashMap::new() })
            })
            .downcast_mut()
            .expect("encode table entry keyed by its own type")
    }
}

impl DecodeTable {
    fn nodes<K, V>(&mut self) -> &mut Vec<NodeHandle<K, V, SIZE>>
    where
        K: Ord + Clone + Send + Sync + 'static,
        V: Clone + Send + Sync + 'static,
    {
        self.by_type
            .entry(TypeId::of::<(K, V)>())
            .or_insert_with(|| Box::new(Vec::<NodeHandle<K, V, SIZE>>::new()))
            .downcast_mut()
            .expect("decode table entry keyed by its own type")
    }
}

thread_local! {
    static ENCODE: Cell<Option<NonNull<EncodeTable>>> = const { Cell::new(None) };
    static DECODE: Cell<Option<NonNull<DecodeTable>>> = const { Cell::new(None) };
}

/// Installs `table` for every [`SharedMap`]/[`SharedSet`] encoded on
/// this thread until dropped. Sessions nest; the inner one wins.
pub struct EncodeSession<'a> {
    prev: Option<NonNull<EncodeTable>>,
    _table: PhantomData<&'a mut EncodeTable>,
}

impl<'a> EncodeSession<'a> {
    pub fn new(table: &'a mut EncodeTable) -> Self {
        let prev = install_encode(Some(NonNull::from(table)));
        EncodeSession { prev, _table: PhantomData }
    }
}

impl Drop for EncodeSession<'_> {
    fn drop(&mut self) {
        install_encode(self.prev);
    }
}

/// Install `table` for this thread and return the previous one; the
/// caller's guard must hold the `&mut` it came from until it restores
/// the previous pointer.
pub(crate) fn install_encode(
    table: Option<NonNull<EncodeTable>>,
) -> Option<NonNull<EncodeTable>> {
    ENCODE.replace(table)
}

pub(crate) fn install_decode(
    table: Option<NonNull<DecodeTable>>,
) -> Option<NonNull<DecodeTable>> {
    DECODE.replace(table)
}

/// Installs `table` for every [`SharedMap`]/[`SharedSet`] decoded on
/// this thread until dropped.
pub struct DecodeSession<'a> {
    prev: Option<NonNull<DecodeTable>>,
    _table: PhantomData<&'a mut DecodeTable>,
}

impl<'a> DecodeSession<'a> {
    pub fn new(table: &'a mut DecodeTable) -> Self {
        let prev = install_decode(Some(NonNull::from(table)));
        DecodeSession { prev, _table: PhantomData }
    }
}

impl Drop for DecodeSession<'_> {
    fn drop(&mut self) {
        install_decode(self.prev);
    }
}

/// Run `f` against the installed encode table, or a fresh one for
/// this call when none is installed.
fn with_encode<R>(f: impl FnOnce(&mut EncodeTable) -> R) -> R {
    match ENCODE.get() {
        // The session guard holds the `&mut` that produced this
        // pointer for as long as it is installed, and nothing else
        // touches the table while it is.
        Some(mut p) => f(unsafe { p.as_mut() }),
        None => {
            let mut table = EncodeTable::default();
            let _session = EncodeSession::new(&mut table);
            with_encode(f)
        }
    }
}

fn with_decode<R>(f: impl FnOnce(&mut DecodeTable) -> R) -> R {
    match DECODE.get() {
        Some(mut p) => f(unsafe { p.as_mut() }),
        None => {
            let mut table = DecodeTable::default();
            let _session = DecodeSession::new(&mut table);
            with_decode(f)
        }
    }
}

impl<K: Ord + Clone, V: Clone> EncodeNodes<K, V> {
    fn written(&self, node: &NodeRef<'_, K, V, SIZE>) -> Option<u64> {
        self.by_identity.get(&node.identity()).map(|seen| seen.id)
    }

    /// Ids are assigned as definitions complete, after their subtrees,
    /// which is the order a decoder completes them in.
    fn complete(&mut self, node: &NodeRef<'_, K, V, SIZE>) {
        let id = self.by_identity.len() as u64;
        self.by_identity.insert(node.identity(), Seen { id, _handle: node.keep() });
    }
}

fn tree_len<K, V>(
    table: &mut EncodeTable,
    node: Option<NodeRef<'_, K, V, SIZE>>,
    pair_len: &mut impl FnMut(&K, &V) -> usize,
) -> usize
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    let Some(node) = node else { return 1 };
    match table.nodes::<K, V>().written(&node) {
        Some(id) => 1 + varint_len(id),
        None => {
            let pairs: usize = node.pairs().map(|(k, v)| pair_len(k, v)).sum();
            1 + varint_len(node.len() as u64)
                + pairs
                + tree_len(table, node.left(), pair_len)
                + tree_len(table, node.right(), pair_len)
        }
    }
}

/// A definition is its pairs, then its left and right subtrees.
fn tree_encode<K, V, B: BufMut>(
    table: &mut EncodeTable,
    node: Option<NodeRef<'_, K, V, SIZE>>,
    buf: &mut B,
    pair_encode: &mut impl FnMut(&K, &V, &mut B) -> Result<(), PackError>,
) -> Result<(), PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    let Some(node) = node else {
        buf.put_u8(EMPTY);
        return Ok(());
    };
    match table.nodes::<K, V>().written(&node) {
        Some(id) => {
            buf.put_u8(REF);
            encode_varint(id, buf);
            Ok(())
        }
        None => {
            buf.put_u8(NODE);
            encode_varint(node.len() as u64, buf);
            for (k, v) in node.pairs() {
                pair_encode(k, v, buf)?;
            }
            tree_encode(table, node.left(), buf, pair_encode)?;
            tree_encode(table, node.right(), buf, pair_encode)?;
            table.nodes::<K, V>().complete(&node);
            Ok(())
        }
    }
}

fn tree_decode<K, V, B: Buf>(
    table: &mut DecodeTable,
    buf: &mut B,
    pair_decode: &mut impl FnMut(&mut B) -> Result<(K, V), PackError>,
) -> Result<Option<NodeHandle<K, V, SIZE>>, PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    match buf.get_u8() {
        EMPTY => Ok(None),
        REF => {
            let id = decode_varint(buf)? as usize;
            let nodes = table.nodes::<K, V>();
            nodes.get(id).cloned().map(Some).ok_or(PackError::InvalidFormat)
        }
        NODE => {
            let n = decode_varint(buf)? as usize;
            if n == 0 || n > SIZE {
                return Err(PackError::InvalidFormat);
            }
            let mut pairs = Vec::with_capacity(n);
            for _ in 0..n {
                pairs.push(pair_decode(buf)?);
            }
            let left = tree_decode(table, buf, pair_decode)?;
            let right = tree_decode(table, buf, pair_decode)?;
            // The stream is the encoder's walk of a map the chunkmap
            // built, read back in the same order.
            let node = unsafe { NodeHandle::create(left, pairs, right) };
            table.nodes::<K, V>().push(node.clone());
            Ok(Some(node))
        }
        _ => Err(PackError::UnknownTag),
    }
}

/// The walkers behind the `Pack` impls, for a map whose values need
/// their own codec: a map of maps shares at both levels only when the
/// inner maps go through these too.
pub(crate) fn map_len<K, V>(
    map: &Map<K, V>,
    pair_len: &mut impl FnMut(&K, &V) -> usize,
) -> usize
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    with_encode(|t| tree_len(t, map.root(), pair_len))
}

pub(crate) fn map_encode<K, V, B: BufMut>(
    map: &Map<K, V>,
    buf: &mut B,
    pair_encode: &mut impl FnMut(&K, &V, &mut B) -> Result<(), PackError>,
) -> Result<(), PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    with_encode(|t| tree_encode(t, map.root(), buf, pair_encode))
}

pub(crate) fn map_decode<K, V, B: Buf>(
    buf: &mut B,
    pair_decode: &mut impl FnMut(&mut B) -> Result<(K, V), PackError>,
) -> Result<Map<K, V>, PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    Ok(Map::from_root(with_decode(|t| tree_decode(t, buf, pair_decode))?))
}

impl<K, V> Pack for SharedMap<K, V>
where
    K: Ord + Clone + Pack + Send + Sync + 'static,
    V: Clone + Pack + Send + Sync + 'static,
{
    fn encoded_len(&self) -> usize {
        map_len(&self.0, &mut |k: &K, v: &V| k.encoded_len() + v.encoded_len())
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        map_encode(&self.0, buf, &mut |k: &K, v: &V, buf| {
            k.encode(buf)?;
            v.encode(buf)
        })
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(SharedMap(map_decode(buf, &mut |buf| Ok((K::decode(buf)?, V::decode(buf)?)))?))
    }
}

impl<K> Pack for SharedSet<K>
where
    K: Ord + Clone + Pack + Send + Sync + 'static,
{
    fn encoded_len(&self) -> usize {
        with_encode(|t| tree_len(t, self.0.root(), &mut |k: &K, _: &()| k.encoded_len()))
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        with_encode(|t| {
            tree_encode(t, self.0.root(), buf, &mut |k: &K, _: &(), buf| k.encode(buf))
        })
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let tree =
            with_decode(|t| tree_decode(t, buf, &mut |buf| Ok((K::decode(buf)?, ()))));
        Ok(SharedSet(Set::from_root(tree?)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bytes::BytesMut;
    use compact_str::CompactString;

    type M = Map<i64, i64>;

    fn forest() -> Vec<M> {
        let m0: M = (0..2000).map(|i| (i, i * 3)).collect();
        let (m1, _) = m0.insert(1000, -1);
        let (m2, _) = m1.insert(-5, 5);
        vec![m0, m1, m2]
    }

    fn encode_all(maps: &[M], table: &mut EncodeTable) -> BytesMut {
        let _session = EncodeSession::new(table);
        let mut buf = BytesMut::new();
        for m in maps {
            let shared = SharedMap(m.clone());
            let len = shared.encoded_len();
            let before = buf.len();
            shared.encode(&mut buf).unwrap();
            assert!(buf.len() - before <= len, "encoded_len is an upper bound");
        }
        buf
    }

    fn decode_all(mut buf: &[u8], n: usize, table: &mut DecodeTable) -> Vec<M> {
        let _session = DecodeSession::new(table);
        let out: Vec<M> =
            (0..n).map(|_| SharedMap::<i64, i64>::decode(&mut buf).unwrap().0).collect();
        assert!(buf.is_empty());
        out
    }

    fn node_tags(bytes: &[u8]) -> usize {
        // every NODE tag is preceded by two subtree tags in the stream;
        // count definitions by walking the same grammar
        let mut buf = bytes;
        let mut defs = 0;
        while buf.has_remaining() {
            match buf.get_u8() {
                EMPTY => {}
                REF => {
                    decode_varint(&mut buf).unwrap();
                }
                NODE => {
                    defs += 1;
                    let n = decode_varint(&mut buf).unwrap();
                    for _ in 0..n {
                        i64::decode(&mut buf).unwrap();
                        i64::decode(&mut buf).unwrap();
                    }
                }
                t => panic!("tag {t}"),
            }
        }
        defs
    }

    #[test]
    fn round_trip_shares_nodes() {
        let maps = forest();
        let mut enc = EncodeTable::default();
        let bytes = encode_all(&maps, &mut enc);
        let mut dec = DecodeTable::default();
        let decoded = decode_all(&bytes, maps.len(), &mut dec);
        for (m, d) in maps.iter().zip(&decoded) {
            assert_eq!(m, d);
        }
        // the decoded forest shares as the original did: re-encoding
        // it yields the same bytes, and definitions are counted once
        let mut again = EncodeTable::default();
        let bytes2 = encode_all(&decoded, &mut again);
        assert_eq!(bytes, bytes2);
        fn walk(
            n: Option<NodeRef<'_, i64, i64, SIZE>>,
            ids: &mut std::collections::HashSet<usize>,
        ) {
            if let Some(n) = n
                && ids.insert(n.identity())
            {
                walk(n.left(), ids);
                walk(n.right(), ids);
            }
        }
        let distinct: std::collections::HashSet<usize> = {
            let mut ids = std::collections::HashSet::new();
            for m in &decoded {
                walk(m.root(), &mut ids);
            }
            ids
        };
        assert_eq!(node_tags(&bytes), distinct.len());
        let m0_nodes = {
            let mut ids = std::collections::HashSet::new();
            walk(maps[0].root(), &mut ids);
            ids.len()
        };
        assert!(distinct.len() < 2 * m0_nodes, "two one-key updates share the tree");
        let (d3, prev) = decoded[2].insert(1000, 9);
        assert_eq!(prev, Some(-1));
        assert_eq!(d3.get(&1000), Some(&9));
        assert_eq!(decoded[1].get(&1000), Some(&-1));
    }

    #[test]
    fn length_bounds_the_encoding() {
        let maps = forest();
        let mut table = EncodeTable::default();
        let _session = EncodeSession::new(&mut table);
        let shared: Vec<_> = maps.iter().map(|m| SharedMap(m.clone())).collect();
        // measured before anything is written, every node is a definition
        let lens: Vec<usize> = shared.iter().map(|s| s.encoded_len()).collect();
        assert_eq!(lens, shared.iter().map(|s| s.encoded_len()).collect::<Vec<_>>());
        assert_eq!(lens[0], lens[1]);
        let mut buf = BytesMut::new();
        let mut written = Vec::new();
        for s in &shared {
            let before = buf.len();
            s.encode(&mut buf).unwrap();
            written.push(buf.len() - before);
        }
        assert_eq!(written[0], lens[0]);
        assert!(written[1] < lens[1] / 4, "the second map is mostly references");
        // once written, the length is exact: a map encoded again is
        // one reference to its root
        let len = shared[0].encoded_len();
        let before = buf.len();
        shared[0].encode(&mut buf).unwrap();
        assert_eq!(buf.len() - before, len);
        assert_eq!(len, 1 + varint_len(0));
    }

    #[test]
    fn later_decode_resolves_into_the_table() {
        let maps = forest();
        let mut enc = EncodeTable::default();
        let bytes = encode_all(&maps, &mut enc);
        let mut table = DecodeTable::default();
        // decode the first map now, the rest under a later session
        let mut buf = &bytes[..];
        let first = {
            let _s = DecodeSession::new(&mut table);
            SharedMap::<i64, i64>::decode(&mut buf).unwrap().0
        };
        assert_eq!(first, maps[0]);
        let rest = {
            let _s = DecodeSession::new(&mut table);
            let a = SharedMap::<i64, i64>::decode(&mut buf).unwrap().0;
            let b = SharedMap::<i64, i64>::decode(&mut buf).unwrap().0;
            [a, b]
        };
        assert_eq!(rest[0], maps[1]);
        assert_eq!(rest[1], maps[2]);
        // a map decoded without the maps it refers into dangles
        let first_len = {
            let mut t = EncodeTable::default();
            let _s = EncodeSession::new(&mut t);
            SharedMap(maps[0].clone()).encoded_len()
        };
        let mut fresh = DecodeTable::default();
        let mut buf = &bytes[first_len..];
        let _s = DecodeSession::new(&mut fresh);
        assert!(SharedMap::<i64, i64>::decode(&mut buf).is_err());
    }

    #[test]
    fn no_session_is_self_contained() {
        let maps = forest();
        let mut buf = BytesMut::new();
        for m in &maps {
            SharedMap(m.clone()).encode(&mut buf).unwrap();
        }
        let mut b = &buf[..];
        for m in &maps {
            assert_eq!(SharedMap::<i64, i64>::decode(&mut b).unwrap().0, *m);
        }
        let empty = SharedMap(M::new());
        let mut buf = BytesMut::new();
        empty.encode(&mut buf).unwrap();
        assert_eq!(buf.len(), empty.encoded_len());
        assert_eq!(SharedMap::<i64, i64>::decode(&mut &buf[..]).unwrap(), empty);
    }

    #[test]
    fn nested_maps_and_sets_share_at_both_levels() {
        type Inner = Map<CompactString, i64>;
        let inner: Inner =
            (0..300).map(|i| (CompactString::from(format!("k{i}")), i)).collect();
        let (inner2, _) = inner.insert(CompactString::from("k7"), -7);
        let outer: Map<i64, SharedMap<CompactString, i64>> = [
            (1, SharedMap(inner.clone())),
            (2, SharedMap(inner2.clone())),
            (3, SharedMap(inner)),
        ]
        .into_iter()
        .collect();
        let set: Set<i64> = (0..500).collect();
        let (set2, _) = set.insert(-1);
        let mut enc = EncodeTable::default();
        let mut buf = BytesMut::new();
        {
            let _s = EncodeSession::new(&mut enc);
            let o = SharedMap(outer.clone());
            let s1 = SharedSet(set.clone());
            let s2 = SharedSet(set2.clone());
            let len = o.encoded_len() + s1.encoded_len() + s2.encoded_len();
            o.encode(&mut buf).unwrap();
            s1.encode(&mut buf).unwrap();
            s2.encode(&mut buf).unwrap();
            assert!(buf.len() <= len);
        }
        let mut dec = DecodeTable::default();
        let _s = DecodeSession::new(&mut dec);
        let mut b = &buf[..];
        let o =
            SharedMap::<i64, SharedMap<CompactString, i64>>::decode(&mut b).unwrap().0;
        let s1 = SharedSet::<i64>::decode(&mut b).unwrap().0;
        let s2 = SharedSet::<i64>::decode(&mut b).unwrap().0;
        assert!(b.is_empty());
        assert_eq!(o, outer);
        assert_eq!(s1, set);
        assert_eq!(s2, set2);
        // the two inner maps under keys 1 and 3 are one tree
        let a = o.get(&1).unwrap().0.root().unwrap().identity();
        let c = o.get(&3).unwrap().0.root().unwrap().identity();
        assert_eq!(a, c);
    }
}
