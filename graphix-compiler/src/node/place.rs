//! Places: a reference into a value (design/place_references.md). A
//! reference to `root[i].field` is the root binding plus a path of
//! accessors; a read applies the path to the root's value, a write
//! rebuilds the root's value along it.

use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use netidx_value::{ValArray, Value};
use smallvec::SmallVec;

/// One accessor of a path: an array or tuple index (negative from the
/// end, as `a[-1]` reads), a struct field, or a map key.
#[derive(Debug, Clone, PartialEq)]
pub enum Step {
    Index(i64),
    Field(ArcStr),
    Key(Value),
}

pub type Path = SmallVec<[Step; 2]>;

/// A queued write to a bound variable: the whole value, or a patch
/// through a path applied to the value as it stands when the write is
/// delivered — so two patches to one root in one cycle land in order,
/// each on the other's result, never on a stale whole.
#[derive(Debug, Clone)]
pub enum VarUpdate {
    Set(Value),
    Patch(Path, Value),
}

fn index_of(len: usize, i: i64) -> Result<usize> {
    let j = if i < 0 { len as i64 + i } else { i };
    if j < 0 || j as usize >= len {
        bail!("index {i} out of range for length {len}")
    }
    Ok(j as usize)
}

/// A struct value is an array of `[name, value]` pairs.
fn field_of(pairs: &ValArray, name: &str) -> Result<usize> {
    pairs
        .iter()
        .position(|p| match p {
            Value::Array(kv) if kv.len() == 2 => {
                matches!(&kv[0], Value::String(n) if &**n == name)
            }
            _ => false,
        })
        .ok_or_else(|| anyhow!("no field {name}"))
}

/// The value at `path` inside `root`.
pub fn read_path(root: &Value, path: &[Step]) -> Result<Value> {
    let mut cur = root;
    for step in path {
        cur = match (step, cur) {
            (Step::Index(i), Value::Array(a)) => &a[index_of(a.len(), *i)?],
            (Step::Field(name), Value::Array(pairs)) => {
                match &pairs[field_of(pairs, name)?] {
                    Value::Array(kv) => &kv[1],
                    _ => unreachable!(),
                }
            }
            (Step::Key(k), Value::Map(m)) => {
                m.get(k).ok_or_else(|| anyhow!("no key {k}"))?
            }
            (step, v) => bail!("cannot apply {step:?} to {v}"),
        };
    }
    Ok(cur.clone())
}

/// `root` with the value at `path` replaced by `v`.
pub fn write_path(root: &Value, path: &[Step], v: Value) -> Result<Value> {
    let Some((step, rest)) = path.split_first() else { return Ok(v) };
    match (step, root) {
        (Step::Index(i), Value::Array(a)) => {
            let j = index_of(a.len(), *i)?;
            let inner = write_path(&a[j], rest, v)?;
            Ok(Value::Array(ValArray::from_iter_exact(
                a.iter()
                    .enumerate()
                    .map(|(k, e)| if k == j { inner.clone() } else { e.clone() }),
            )))
        }
        (Step::Field(name), Value::Array(pairs)) => {
            let j = field_of(pairs, name)?;
            let inner = match &pairs[j] {
                Value::Array(kv) => write_path(&kv[1], rest, v)?,
                _ => unreachable!(),
            };
            Ok(Value::Array(ValArray::from_iter_exact(pairs.iter().enumerate().map(
                |(k, e)| {
                    if k == j {
                        Value::Array(ValArray::from_iter_exact(
                            [Value::String(name.clone()), inner.clone()].into_iter(),
                        ))
                    } else {
                        e.clone()
                    }
                },
            ))))
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
