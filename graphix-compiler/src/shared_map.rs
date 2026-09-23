//! `Pack` for the environment's persistent maps that preserves their
//! sharing. A persistent update copies the path from the root and
//! shares every other node, so an environment's snapshots are mostly
//! one tree; packed by contents they would decode to as many trees as
//! there are snapshots. [`SharedMap`] and [`SharedSet`] walk the tree
//! through the chunkmap's structural API and write each node as an
//! image object ([`image::object_encode`]): a definition at its first
//! sight, a reference to its ordinal afterwards, so the decoded forest
//! shares exactly what the encoded one did, and a length under a
//! session is exact like any other image object's.
//!
//! The codecs are image codecs: they run under an [`image::EncodeImage`]
//! / [`image::DecodeImage`] session, whose encoder and decoder hold the
//! nodes seen (one per image, one per runtime, so an instance decoded
//! later resolves references into nodes decoded earlier). Outside a
//! session an encode writes every node as a definition and a decode
//! fails with `InvalidFormat`.

use crate::{
    env::{Map, Set},
    image::{self, ImageBuf},
};
use ahash::AHashMap;
use bytes::{Buf, BufMut};
use immutable_chunkmap::map::{NodeHandle, NodeRef};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use std::any::{Any, TypeId};

// CR claude for eric: [style] Repeats the chunk size env.rs:19 hard-codes in
// `Map<K, V, 16>`; export one constant from env.rs and use it in both.
const SIZE: usize = 16;

/// A map packed with its sharing. The wrapped map is the environment's
/// own (an `Arc` clone), so wrapping is free.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedMap<K: Ord + Clone, V: Clone>(pub Map<K, V>);

/// A set packed with its sharing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedSet<K: Ord + Clone>(pub Set<K>);

/// The nodes a decoder has built, by the offset of their definition,
/// across every map and set unpacked under one decoder.
#[derive(Default)]
pub struct DecodeTable {
    by_type: AHashMap<TypeId, Box<dyn Any + Send + Sync>>,
}

impl DecodeTable {
    fn nodes<K, V>(&mut self) -> &mut AHashMap<u64, NodeHandle<K, V, SIZE>>
    where
        K: Ord + Clone + Send + Sync + 'static,
        V: Clone + Send + Sync + 'static,
    {
        self.by_type
            .entry(TypeId::of::<(K, V)>())
            .or_insert_with(|| Box::new(AHashMap::<u64, NodeHandle<K, V, SIZE>>::new()))
            .downcast_mut()
            .expect("decode table entry keyed by its own type")
    }
}

/// Keep `node` alive for the session, so its identity names it alone.
fn pin<K, V>(node: &NodeRef<'_, K, V, SIZE>)
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    image::encoding(|e| e.pinned_map_nodes.push(Box::new(node.keep())));
}

fn tree_len<K, V>(
    node: Option<NodeRef<'_, K, V, SIZE>>,
    pair_len: &mut impl FnMut(&K, &V) -> usize,
) -> usize
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    let Some(node) = node else { return 1 };
    1 + image::object_len(
        &node.identity(),
        |k| *k,
        |e| &mut e.map_nodes,
        || {
            // CR claude for eric: [perf] A node measured here and then written
            // runs `pin` twice (again in tree_encode's definition closure, since
            // the length pass leaves the slot undefined): two boxed keeps per
            // map node per image. Pin once, at the slot's first sight.
            pin(&node);
            let pairs: usize = node.pairs().map(|(k, v)| pair_len(k, v)).sum();
            varint_len(node.len() as u64)
                + pairs
                + tree_len(node.left(), pair_len)
                + tree_len(node.right(), pair_len)
        },
    )
}

/// A definition is its pairs, then its left and right subtrees.
fn tree_encode<K, V, B: BufMut>(
    node: Option<NodeRef<'_, K, V, SIZE>>,
    buf: &mut B,
    pair_encode: &mut impl FnMut(&K, &V, &mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    let Some(node) = node else { return false.encode(buf) };
    true.encode(buf)?;
    image::object_encode(
        &node.identity(),
        |k| *k,
        |e| &mut e.map_nodes,
        buf,
        |buf| {
            pin(&node);
            encode_varint(node.len() as u64, buf);
            for (k, v) in node.pairs() {
                pair_encode(k, v, buf)?;
            }
            tree_encode(node.left(), buf, pair_encode)?;
            tree_encode(node.right(), buf, pair_encode)
        },
    )
}

fn tree_decode<K, V>(
    buf: &mut &[u8],
    pair_decode: &mut impl FnMut(&mut &[u8]) -> Result<(K, V), PackError>,
) -> Result<Option<NodeHandle<K, V, SIZE>>, PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    if !bool::decode(buf)? {
        return Ok(None);
    }
    node_decode(buf, pair_decode).map(Some)
}

fn node_decode<K, V>(
    buf: &mut &[u8],
    pair_decode: &mut impl FnMut(&mut &[u8]) -> Result<(K, V), PackError>,
) -> Result<NodeHandle<K, V, SIZE>, PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    // one of the two closures runs, so the shared cell is never
    // borrowed twice
    let pd = std::cell::RefCell::new(pair_decode);
    image::object_decode(
        buf,
        |d| d.maps.nodes::<K, V>(),
        |sub| {
            let pair_decode = &mut **pd.borrow_mut();
            let n = decode_varint(sub)? as usize;
            if n == 0 || n > SIZE {
                return Err(PackError::InvalidFormat);
            }
            let mut pairs = Vec::with_capacity(n);
            for _ in 0..n {
                pairs.push(pair_decode(sub)?);
            }
            let left = tree_decode(sub, pair_decode)?;
            let right = tree_decode(sub, pair_decode)?;
            // The stream is the encoder's walk of a map the chunkmap
            // built, read back in the same order.
            // CR claude for eric: [risk] This contract rests on the image being
            // intact, and a cache image carries no checksum (image/mod.rs): a
            // corrupted file whose keys still decode builds a map whose lookups
            // are silently wrong. The keys are in hand; checking strict order
            // within `pairs` and against the subtrees' bounds is cheap.
            Ok(unsafe { NodeHandle::create(left, pairs, right) })
        },
        |sub| node_decode(sub, &mut **pd.borrow_mut()),
    )
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
    tree_len(map.root(), pair_len)
}

pub(crate) fn map_encode<K, V, B: BufMut>(
    map: &Map<K, V>,
    buf: &mut B,
    pair_encode: &mut impl FnMut(&K, &V, &mut ImageBuf) -> Result<(), PackError>,
) -> Result<(), PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    tree_encode(map.root(), buf, pair_encode)
}

pub(crate) fn map_decode<K, V>(
    buf: &mut impl Buf,
    pair_decode: &mut impl FnMut(&mut &[u8]) -> Result<(K, V), PackError>,
) -> Result<Map<K, V>, PackError>
where
    K: Ord + Clone + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    Ok(Map::from_root(image::with_slice(buf, |sub| tree_decode(sub, pair_decode))?))
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
        tree_len(self.0.root(), &mut |k: &K, _: &()| k.encoded_len())
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        tree_encode(self.0.root(), buf, &mut |k: &K, _: &(), buf| k.encode(buf))
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let tree = image::with_slice(buf, |sub| {
            tree_decode(sub, &mut |b| Ok((K::decode(b)?, ())))
        })?;
        Ok(SharedSet(Set::from_root(tree)))
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
