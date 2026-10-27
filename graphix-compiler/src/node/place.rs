//! Places: a reference into a value (design/place_references.md). A
//! reference to `root[i].field` is the root binding plus a path of
//! accessors; a read applies the path to the root's value, a write
//! rebuilds the root's value along it.

use crate::{BindId, abstract_value, expr::Expr, rep};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::{ValError, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::ops::{Deref, DerefMut};
use triomphe::Arc;

/// One accessor of a path: an array or tuple index (negative from the
/// end, as `a[-1]` reads; `0` is also an error's or an abstract
/// value's payload), a struct field, or a map key.
#[derive(Debug, Clone, PartialEq, netidx_derive::Pack)]
pub enum Step {
    Index(i64),
    Field(ArcStr),
    Key(Value),
}

/// What a place reference's cell stands for.
#[derive(Debug, Clone, PartialEq)]
pub enum RefPath {
    /// The element at the path in the root binding's value.
    At(BindId, Path),
    /// Nothing, while the place's address is bottom: a write through the
    /// reference does not happen, and a read finds the cell's bottom.
    Nowhere,
}

/// A place's accessors, from its root binding's value.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Path(SmallVec<[Step; 2]>);

impl Path {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Deref for Path {
    type Target = SmallVec<[Step; 2]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Path {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<&[Step]> for Path {
    fn from(steps: &[Step]) -> Self {
        Self(SmallVec::from(steps))
    }
}

impl Pack for Path {
    fn encoded_len(&self) -> usize {
        varint_len(self.len() as u64)
            + self.iter().map(|s| s.encoded_len()).sum::<usize>()
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        encode_varint(self.len() as u64, buf);
        for s in self.iter() {
            s.encode(buf)?;
        }
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let n = decode_varint(buf)? as usize;
        let mut path = Path::new();
        for _ in 0..n {
            path.push(Step::decode(buf)?);
        }
        Ok(path)
    }
}

/// A queued write to a bound variable: the whole value, or a patch
/// through a path applied to the value as it stands when the write is
/// delivered — so two patches to one root in one cycle land in order,
/// each on the other's result, never on a stale whole.
#[derive(Debug, Clone)]
pub enum VarUpdate {
    Set(Value),
    /// The path, the value, and the writer, which a dropped patch's log
    /// names (an embedder's write has none).
    Patch(Path, Value, Option<Arc<Expr>>),
}

fn index_of(len: usize, i: i64) -> Result<usize> {
    super::array::index(len, i)
        .ok_or_else(|| anyhow!("index {i} out of range for length {len}"))
}

/// The position and value of the struct field `name`.
fn field_of<'a>(v: &'a Value, name: &str) -> Result<(usize, &'a Value)> {
    rep::struct_fields(v)
        .and_then(|fields| rep::field_named(fields, name))
        .ok_or_else(|| anyhow!("no field {name}"))
}

/// The value at `path` inside `root`. A map step compares keys, so the
/// caller runs this under `coretraits::with_hooks`.
pub fn read_path(root: &Value, path: &[Step]) -> Result<Value> {
    let mut cur = root;
    for step in path {
        cur = match (step, cur) {
            (Step::Index(i), v) if let Some(a) = rep::indexed(v) => {
                &a[index_of(a.len(), *i)?]
            }
            (Step::Index(0), Value::Error(e)) => e,
            (Step::Index(0), v) if abstract_value::get(v).is_some() => {
                &abstract_value::get(v).unwrap().payload
            }
            (Step::Field(name), v) => field_of(v, name)?.1,
            (Step::Key(k), Value::Map(m)) => {
                m.get(k).ok_or_else(|| anyhow!("no key {k}"))?
            }
            (step, v) => bail!("cannot apply {step:?} to {}", crate::typ::Naked(v)),
        };
    }
    Ok(cur.clone())
}

/// `root` with the value at `path` replaced by `v`. A map step compares
/// keys, so the caller runs this under `coretraits::with_hooks`.
// CR claude for claude: [bug/medium] write_path recurses once per path step with no
// stack guard, and paths compose: `&mut (*r)[0]` extends r's path, so a flat chain of
// lets builds a path as long as the program. A 10000-step chain overflows the tokio
// worker at the write and aborts. Each reference also clones its whole prefix, so the
// chain's paths take O(n^2) memory: n = 20000 is OOM-killed at 6 GB. Fix: make
// write_path iterative, collecting the spine as read_path walks it and rebuilding
// bottom-up (or guard each level with ensure_sufficient), and share path prefixes
// instead of copying them. probe:
// design/review-2026-10-26/repro/x-stack-place-path-chain.py (x-stack-03)
// 2026-10-27 claude: The stack half is fixed: write_path walks the path in a loop,
// recording a Hole per step (the container and the position it takes), and rebuilds
// bottom-up by folding the holes; the probe at n = 10000 prints 1 and exits 0. Pin:
// node::place::tests::deep_write_keeps_a_flat_stack (a 50000-step write on a 512 KB
// stack; aborts on overflow with the recursive version). Still open: the O(n^2)
// paths. Place::update clears and rebuilds its whole path from root_place's prefix at
// every update, so each place of a chain holds a copy of its prefix. Sharing prefixes
// needs a persistent path (an Arc'd parent link per place) that every reader
// (read_path, write_path, the RefPath equality in Deref's registration, branch
// patch_level) walks root-first, and Place::update would then allocate a node per
// step per update unless it reuses an unchanged one; left for a later batch.
pub fn write_path(root: &Value, path: &[Step], v: Value) -> Result<Value> {
    let mut spine: LPooled<Vec<Hole>> = LPooled::take();
    let mut cur = root;
    for (i, step) in path.iter().enumerate() {
        if let (Step::Index(0), Some(g)) = (step, abstract_value::get(cur)) {
            spine.push(Hole::Payload(cur));
            cur = &g.payload;
            continue;
        }
        cur = match (step, cur) {
            (Step::Index(i), c) if let Some(a) = rep::indexed(c) => {
                let j = index_of(a.len(), *i)?;
                spine.push(Hole::Indexed(c, j));
                &a[j]
            }
            (Step::Index(0), Value::Error(e)) => {
                spine.push(Hole::Error);
                e
            }
            (Step::Field(name), c) => {
                let (j, f) = field_of(c, name)?;
                spine.push(Hole::Field(c, j));
                f
            }
            (Step::Key(k), Value::Map(m)) => {
                spine.push(Hole::Key(m, k));
                match m.get(k) {
                    Some(c) => c,
                    None if i + 1 == path.len() => break,
                    None => bail!("no key {k}"),
                }
            }
            (step, v) => bail!("cannot apply {step:?} to {}", crate::typ::Naked(v)),
        };
    }
    Ok(spine.drain(..).rev().fold(v, |inner, hole| hole.fill(inner)))
}

/// Where one step of a write took its value from, which the write
/// rebuilds around the new value.
enum Hole<'a> {
    /// An abstract value's payload.
    Payload(&'a Value),
    /// An array or tuple element.
    Indexed(&'a Value, usize),
    /// An error's payload.
    Error,
    /// A struct field.
    Field(&'a Value, usize),
    /// A map entry, present or not.
    Key(&'a netidx_value::Map, &'a Value),
}

impl Hole<'_> {
    fn fill(self, inner: Value) -> Value {
        match self {
            Hole::Payload(c) => {
                let g = abstract_value::get(c).expect("an abstract value");
                abstract_value::wrap(g.id, g.name.clone(), g.params.clone(), inner)
            }
            Hole::Indexed(c, j) => rep::replace_indexed(c, j, inner).expect("indexed"),
            Hole::Error => Value::Error(ValError::new(inner)),
            Hole::Field(c, j) => rep::replace_field(c, j, inner).expect("a struct"),
            Hole::Key(m, k) => Value::Map(m.insert(k.clone(), inner).0),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use netidx_value::ValArray;

    /// A path is as long as a chain of place references, so a write
    /// along it must not take a frame per step.
    #[test]
    fn deep_write_keeps_a_flat_stack() {
        const DEPTH: usize = 50_000;
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(|| {
                let mut root =
                    Value::Array(ValArray::from_iter_exact(std::iter::empty()));
                for _ in 0..DEPTH {
                    root = Value::Array(ValArray::from_iter_exact(std::iter::once(root)));
                }
                let path: Path = vec![Step::Index(0); DEPTH].as_slice().into();
                let written = write_path(&root, &path, Value::I64(7)).unwrap();
                assert_eq!(read_path(&written, &path).unwrap(), Value::I64(7));
            })
            .expect("spawn")
            .join()
            .expect("the write panicked");
    }
}
