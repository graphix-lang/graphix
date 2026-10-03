//! `Pack` for the environment's persistent maps that preserves their
//! sharing. A persistent update copies the path from the root and
//! shares every other node, so an environment's snapshots are mostly
//! one tree; packed by contents they would decode to as many trees as
//! there are snapshots. [`SharedMap`] and [`SharedSet`] walk the tree
//! through imhm's node API and write each node as an image object
//! ([`image::object_encode`]): a definition at its first sight, a
//! reference to its ordinal afterwards, so the decoded forest shares
//! exactly what the encoded one did, and a length under a session is
//! exact like any other image object's. imhm rebuilds each decoded
//! node only if the map could have built it.
//!
//! The codecs are image codecs: they run under an [`image::EncodeImage`]
//! / [`image::DecodeImage`] session, whose encoder and decoder hold the
//! nodes seen (one per image, one per runtime, so an instance decoded
//! later resolves references into nodes decoded earlier). Outside a
//! session an encode or a decode fails with `InvalidFormat`.

use crate::{
    env::{Hasher, Map, Set},
    image::{self, ImageBuf},
};
use bytes::{Buf, BufMut};
use imhm::{Contents, NewSlot, NodeHandle, NodeRef, SlotRef};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use std::{any::Any, hash::Hash};

/// A map packed with its sharing. The wrapped map is the environment's
/// own (an `Arc` clone), so wrapping is free. Its keys must decode to
/// the values they were written as: a node is rebuilt where it was, and
/// a key that decodes to another value (a relocated `image_id!`) hashes
/// to another place, which fails the read. Such a map is a
/// [`RelocatedMap`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedMap<K: Hash + Eq + Clone, V: Clone>(pub Map<K, V>);

/// A set packed with its sharing, keyed as a [`SharedMap`] is.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedSet<K: Hash + Eq + Clone>(pub Set<K>);

/// A map keyed by a relocated id: packed as its pairs and rebuilt by
/// inserting them where the decoded keys hash.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelocatedMap<K: Hash + Eq + Clone, V: Clone>(pub Map<K, V>);

/// A set keyed as a [`RelocatedMap`] is.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelocatedSet<K: Hash + Eq + Clone>(pub Set<K>);

/// What a map's keys and values must be to travel in an image.
pub(crate) trait Item: Hash + Eq + Clone + Send + Sync + 'static {}
impl<T: Hash + Eq + Clone + Send + Sync + 'static> Item for T {}

impl<K, V> image::Object for NodeHandle<K, V>
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    fn into_obj(self) -> image::Obj {
        image::any_obj(self)
    }

    fn of(obj: &image::Obj) -> Option<&Self> {
        image::any_of(obj)
    }
}

/// A node's table key and pin: held for the session, so its identity
/// names it alone.
fn pinned<K, V>(
    node: &NodeRef<'_, K, V>,
) -> impl FnOnce(&usize) -> (usize, Box<dyn Any + Send + Sync>)
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    let keep = node.keep();
    move |k| (*k, Box::new(keep))
}

fn tree_len<K, V>(node: Option<NodeRef<'_, K, V>>) -> usize
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    1 + node.map_or(0, |n| node_len(&n))
}

fn node_len<K, V>(node: &NodeRef<'_, K, V>) -> usize
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    image::object_len(&node.identity(), pinned(node), |e| &mut e.map_nodes)
}

fn tree_encode<K, V, B: BufMut>(
    node: Option<NodeRef<'_, K, V>>,
    buf: &mut B,
    pair_encode: &mut impl FnMut(&K, &V, &mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError>
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    let Some(node) = node else { return false.encode(buf) };
    true.encode(buf)?;
    node_encode(node, buf, pair_encode)
}

/// A definition is the node's depth and kind, then a leaf's pairs, or
/// an inner node's slots, each a pair or a child node.
fn node_encode<K, V, B: BufMut>(
    node: NodeRef<'_, K, V>,
    buf: &mut B,
    pair_encode: &mut impl FnMut(&K, &V, &mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError>
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    image::object_encode(
        &node.identity(),
        pinned(&node),
        |e| &mut e.map_nodes,
        buf,
        |buf| {
            node.depth().encode(buf)?;
            match node.contents() {
                Contents::Leaf(pairs) => {
                    true.encode(buf)?;
                    encode_varint(pairs.len() as u64, buf);
                    for (k, v) in pairs {
                        pair_encode(k, v, buf)?;
                    }
                }
                Contents::Inner(slots) => {
                    false.encode(buf)?;
                    encode_varint(slots.len() as u64, buf);
                    for slot in slots {
                        match slot {
                            SlotRef::Entry(k, v) => {
                                false.encode(buf)?;
                                pair_encode(k, v, buf)?;
                            }
                            SlotRef::Node(n) => {
                                true.encode(buf)?;
                                node_encode(n, buf, pair_encode)?;
                            }
                        }
                    }
                }
            }
            Ok(())
        },
    )
}

fn tree_decode<K, V>(
    buf: &mut &[u8],
    pair_decode: &mut impl FnMut(&mut &[u8]) -> Result<(K, V), PackError>,
) -> Result<Option<NodeHandle<K, V>>, PackError>
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    if !bool::decode(buf)? {
        return Ok(None);
    }
    node_decode(buf, pair_decode).map(Some)
}

/// A node the map could not have built (a corrupt image, or one hashed
/// by another build) fails the read.
fn node_decode<K, V>(
    buf: &mut &[u8],
    pair_decode: &mut impl FnMut(&mut &[u8]) -> Result<(K, V), PackError>,
) -> Result<NodeHandle<K, V>, PackError>
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    // one of the two closures runs, so the shared cell is never
    // borrowed twice
    let pd = std::cell::RefCell::new(pair_decode);
    image::object_decode(
        buf,
        |sub| {
            let pair_decode = &mut **pd.borrow_mut();
            let depth = u8::decode(sub)?;
            let leaf = bool::decode(sub)?;
            let n = decode_varint(sub)? as usize;
            if !leaf && n > 32 {
                return Err(PackError::InvalidFormat);
            }
            let hasher = Hasher::default();
            let node = if leaf {
                let mut pairs = Vec::with_capacity(n.min(sub.len()));
                for _ in 0..n {
                    pairs.push(pair_decode(sub)?);
                }
                NodeHandle::leaf(&hasher, depth, pairs)
            } else {
                let mut slots = Vec::with_capacity(n);
                for _ in 0..n {
                    slots.push(match bool::decode(sub)? {
                        false => {
                            let (k, v) = pair_decode(sub)?;
                            NewSlot::Entry(k, v)
                        }
                        true => NewSlot::Node(node_decode(sub, pair_decode)?),
                    });
                }
                NodeHandle::inner(&hasher, depth, slots)
            };
            node.map_err(|_| PackError::InvalidFormat)
        },
        |sub| node_decode(sub, &mut **pd.borrow_mut()),
    )
}

/// The walkers behind the `Pack` impls, for a map whose values need
/// their own codec: a map of maps shares at both levels only when the
/// inner maps go through these too.
pub(crate) fn map_len<K, V>(map: &Map<K, V>) -> usize
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    tree_len(map.root())
}

pub(crate) fn map_encode<K, V, B: BufMut>(
    map: &Map<K, V>,
    buf: &mut B,
    pair_encode: &mut impl FnMut(&K, &V, &mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError>
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    tree_encode(map.root(), buf, pair_encode)
}

pub(crate) fn map_decode<K, V>(
    buf: &mut impl Buf,
    pair_decode: &mut impl FnMut(&mut &[u8]) -> Result<(K, V), PackError>,
) -> Result<Map<K, V>, PackError>
where
    K: Item,
    V: Clone + Send + Sync + 'static,
{
    let tree = image::with_slice(buf, |sub| tree_decode(sub, pair_decode))?;
    Map::from_root(tree, Hasher::default()).map_err(|_| PackError::InvalidFormat)
}

impl<K, V> Pack for SharedMap<K, V>
where
    K: Item + Pack,
    V: Clone + Pack + Send + Sync + 'static,
{
    fn encoded_len(&self) -> usize {
        map_len(&self.0)
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
    K: Item + Pack,
{
    fn encoded_len(&self) -> usize {
        tree_len(self.0.root())
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        tree_encode(self.0.root(), buf, &mut |k: &K, _: &(), buf| k.encode(buf))
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let tree = image::with_slice(buf, |sub| {
            tree_decode(sub, &mut |b| Ok((K::decode(b)?, ())))
        })?;
        let set = Set::from_root(tree, Hasher::default())
            .map_err(|_| PackError::InvalidFormat)?;
        Ok(SharedSet(set))
    }
}

impl<K, V> Pack for RelocatedMap<K, V>
where
    K: Hash + Eq + Clone + Pack,
    V: Clone + Pack,
{
    fn encoded_len(&self) -> usize {
        let pairs: usize =
            self.0.iter().map(|(k, v)| k.encoded_len() + v.encoded_len()).sum();
        netidx_core::pack::varint_len(self.0.len() as u64) + pairs
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        encode_varint(self.0.len() as u64, buf);
        for (k, v) in self.0.iter() {
            k.encode(buf)?;
            v.encode(buf)?;
        }
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let n = decode_varint(buf)? as usize;
        let mut map = Map::default();
        for _ in 0..n {
            let k = K::decode(buf)?;
            if map.insert_cow(k, V::decode(buf)?).is_some() {
                return Err(PackError::InvalidFormat);
            }
        }
        Ok(Self(map))
    }
}

impl<K> Pack for RelocatedSet<K>
where
    K: Hash + Eq + Clone + Pack,
{
    fn encoded_len(&self) -> usize {
        let keys: usize = self.0.iter().map(|k| k.encoded_len()).sum();
        netidx_core::pack::varint_len(self.0.len() as u64) + keys
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        encode_varint(self.0.len() as u64, buf);
        for k in self.0.iter() {
            k.encode(buf)?;
        }
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let n = decode_varint(buf)? as usize;
        let mut set = Set::default();
        for _ in 0..n {
            if set.insert_cow(K::decode(buf)?) {
                return Err(PackError::InvalidFormat);
            }
        }
        Ok(Self(set))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::image::{
        DecodeImage, EncodeImage, ImageBuf, ImageDecoder, ImageEncoder, Packed,
    };
    use compact_str::CompactString;

    type M = Map<i64, i64>;

    fn forest() -> Vec<M> {
        let m0: M = (0..2000).map(|i| (i, i * 3)).collect();
        let (m1, _) = m0.insert(1000, -1);
        let (m2, _) = m1.insert(-5, 5);
        vec![m0, m1, m2]
    }

    /// Encode `items` under one session, asserting each one's measured
    /// length is exactly what it wrote.
    fn encode_exact<T: Pack>(items: &[T], enc: &mut ImageEncoder) -> Packed {
        let buf = EncodeImage::with(enc, || {
            let mut buf = ImageBuf::with_capacity(0);
            for i in items {
                let len = i.encoded_len();
                let before = buf.len();
                i.encode(&mut buf).unwrap();
                assert_eq!(buf.len() - before, len, "encoded_len is exact");
            }
            buf
        });
        Packed::new(enc, buf)
    }

    fn encode_all(maps: &[M], enc: &mut ImageEncoder) -> Packed {
        let shared: Vec<_> = maps.iter().map(|m| SharedMap(m.clone())).collect();
        encode_exact(&shared, enc)
    }

    fn decode_all(packed: &Packed, n: usize, dec: &mut ImageDecoder) -> Vec<M> {
        DecodeImage::with(dec, || {
            let mut buf = packed.body();
            let out: Vec<M> = (0..n)
                .map(|_| SharedMap::<i64, i64>::decode(&mut buf).unwrap().0)
                .collect();
            assert!(buf.is_empty());
            out
        })
    }

    fn walk(
        n: Option<NodeRef<'_, i64, i64>>,
        ids: &mut std::collections::HashSet<usize>,
    ) {
        if let Some(n) = n
            && ids.insert(n.identity())
            && let Contents::Inner(slots) = n.contents()
        {
            for slot in slots {
                if let SlotRef::Node(c) = slot {
                    walk(Some(c), ids)
                }
            }
        }
    }

    #[test]
    fn round_trip_shares_nodes() {
        let maps = forest();
        let mut enc = ImageEncoder::new();
        let packed = encode_all(&maps, &mut enc);
        let mut dec = packed.decoder(&enc);
        let decoded = decode_all(&packed, maps.len(), &mut dec);
        for (m, d) in maps.iter().zip(&decoded) {
            assert_eq!(m, d);
        }
        let mut again = ImageEncoder::new();
        let packed2 = encode_all(&decoded, &mut again);
        assert_eq!(packed.image, packed2.image);
        let distinct: std::collections::HashSet<usize> = {
            let mut ids = std::collections::HashSet::new();
            for m in &decoded {
                walk(m.root(), &mut ids);
            }
            ids
        };
        assert_eq!(packed.definitions(), distinct.len());
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

    /// A length pass over the forest plans every node once; the encode
    /// pass writes exactly what was measured.
    #[test]
    fn length_is_exact() {
        let maps = forest();
        let mut enc = ImageEncoder::new();
        let shared: Vec<_> = maps.iter().map(|m| SharedMap(m.clone())).collect();
        let lens: Vec<usize> = EncodeImage::with(&mut enc, || {
            shared.iter().map(|s| s.encoded_len()).collect()
        });
        EncodeImage::with(&mut enc, || {
            let mut buf = ImageBuf::with_capacity(0);
            for (s, len) in shared.iter().zip(&lens) {
                assert_eq!(s.encoded_len(), *len);
                let before = buf.len();
                s.encode(&mut buf).unwrap();
                assert_eq!(buf.len() - before, *len);
            }
        });
    }

    /// Two values sharing one inner map measure as a definition and a
    /// reference, and write the same.
    #[test]
    fn shared_inner_map_is_exact() {
        let mut inner = Map::new();
        inner.insert_cow(1_u64, 2_u64);
        let inner = SharedMap(inner);
        let mut outer = Map::new();
        outer.insert_cow(1_u64, inner.clone());
        outer.insert_cow(2_u64, inner);
        let outer = SharedMap(outer);
        let mut enc = ImageEncoder::new();
        let packed = encode_exact(&[outer.clone()], &mut enc);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let d = SharedMap::<u64, SharedMap<u64, u64>>::decode(&mut packed.body())
                .unwrap();
            assert_eq!(d, outer);
            let a = d.0.get(&1).unwrap().0.root().unwrap().identity();
            let b = d.0.get(&2).unwrap().0.root().unwrap().identity();
            assert_eq!(a, b);
        });
    }

    /// A node whose keys do not hash to its place (a corrupt image, or
    /// another hasher) fails the read rather than building a map whose
    /// lookups are wrong.
    #[test]
    fn misplaced_keys_are_refused() {
        let maps = forest();
        let mut enc = ImageEncoder::new();
        let packed = encode_all(&maps[..1], &mut enc);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let mut buf = packed.body();
            let negated = map_decode::<i64, i64>(&mut buf, &mut |b| {
                Ok((-i64::decode(b)?, i64::decode(b)?))
            });
            assert!(negated.is_err());
        });
    }

    /// Outside a session a map decodes as nothing.
    #[test]
    fn decode_needs_a_session() {
        let mut m = Map::new();
        m.insert_cow(1_u64, 2_u64);
        let mut enc = ImageEncoder::new();
        let packed = encode_exact(&[SharedMap(m)], &mut enc);
        assert!(SharedMap::<u64, u64>::decode(&mut packed.body()).is_err());
    }

    /// A map decoded on its own resolves the nodes it shares from
    /// their offsets, in any order and under any later session.
    #[test]
    fn later_decode_resolves_by_offset() {
        let maps = forest();
        let mut enc = ImageEncoder::new();
        let packed = encode_all(&maps, &mut enc);
        let first_len = {
            let mut e = ImageEncoder::new();
            EncodeImage::with(&mut e, || SharedMap(maps[0].clone()).encoded_len())
        };
        let mut dec = packed.decoder(&enc);
        let mut buf = &packed.body()[first_len..];
        let rest = {
            DecodeImage::with(&mut dec, || {
                let a = SharedMap::<i64, i64>::decode(&mut buf).unwrap().0;
                let b = SharedMap::<i64, i64>::decode(&mut buf).unwrap().0;
                [a, b]
            })
        };
        assert_eq!(rest[0], maps[1]);
        assert_eq!(rest[1], maps[2]);
        let first = {
            DecodeImage::with(&mut dec, || {
                SharedMap::<i64, i64>::decode(&mut packed.body()).unwrap().0
            })
        };
        assert_eq!(first, maps[0]);
        // the trees decoded out of order still share their nodes
        assert_eq!(
            first.root().unwrap().identity() == rest[0].root().unwrap().identity(),
            maps[0].root().unwrap().identity() == maps[1].root().unwrap().identity()
        );
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
        let mut enc = ImageEncoder::new();
        let buf = EncodeImage::with(&mut enc, || {
            let o = SharedMap(outer.clone());
            let s1 = SharedSet(set.clone());
            let s2 = SharedSet(set2.clone());
            let len = o.encoded_len() + s1.encoded_len() + s2.encoded_len();
            let mut buf = ImageBuf::with_capacity(0);
            o.encode(&mut buf).unwrap();
            s1.encode(&mut buf).unwrap();
            s2.encode(&mut buf).unwrap();
            assert_eq!(buf.len(), len);
            buf
        });
        let packed = Packed::new(&mut enc, buf);
        let mut dec = packed.decoder(&enc);
        DecodeImage::with(&mut dec, || {
            let mut b = packed.body();
            let o = SharedMap::<i64, SharedMap<CompactString, i64>>::decode(&mut b)
                .unwrap()
                .0;
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
        });
    }
}
