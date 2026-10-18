//! Places: a reference into a value (design/place_references.md). A
//! reference to `root[i].field` is the root binding plus a path of
//! accessors; a read applies the path to the root's value, a write
//! rebuilds the root's value along it.

use crate::{abstract_value, expr::Expr, rep};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::{ValError, Value};
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
            (step, v) => bail!("cannot apply {step:?} to {v}"),
        };
    }
    Ok(cur.clone())
}

/// `root` with the value at `path` replaced by `v`. A map step compares
/// keys, so the caller runs this under `coretraits::with_hooks`.
pub fn write_path(root: &Value, path: &[Step], v: Value) -> Result<Value> {
    let Some((step, rest)) = path.split_first() else { return Ok(v) };
    if let (Step::Index(0), Some(g)) = (step, abstract_value::get(root)) {
        let payload = write_path(&g.payload, rest, v)?;
        return Ok(abstract_value::wrap(g.id, g.name.clone(), g.params.clone(), payload));
    }
    match (step, root) {
        (Step::Index(i), root) if let Some(a) = rep::indexed(root) => {
            let j = index_of(a.len(), *i)?;
            let inner = write_path(&a[j], rest, v)?;
            Ok(rep::replace_indexed(root, j, inner).expect("indexed"))
        }
        (Step::Index(0), Value::Error(e)) => {
            Ok(Value::Error(ValError::new(write_path(e, rest, v)?)))
        }
        (Step::Field(name), root) => {
            let (j, cur) = field_of(root, name)?;
            let inner = write_path(cur, rest, v)?;
            Ok(rep::replace_field(root, j, inner).expect("a struct"))
        }
        (Step::Key(k), Value::Map(m)) => {
            let inner = match m.get(k) {
                Some(cur) => write_path(cur, rest, v)?,
                None if rest.is_empty() => v,
                None => bail!("no key {k}"),
            };
            Ok(Value::Map(m.insert(k.clone(), inner).0))
        }
        (step, v) => bail!("cannot apply {step:?} to {v}"),
    }
}
