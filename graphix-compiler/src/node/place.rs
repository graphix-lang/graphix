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
use triomphe::Arc;

/// One accessor of a path: an array or tuple index (negative from the
/// end, as `a[-1]` reads; `0` is also an error's or an abstract
/// value's payload), a struct field, or a map key.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub enum Step {
    Index(i64),
    Field(ArcStr),
    /// The key and whether its static type can hold a reference.
    Key(Value, bool),
}

impl PartialEq for Step {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Index(a), Self::Index(b)) => a == b,
            (Self::Field(a), Self::Field(b)) => a == b,
            (Self::Key(a, _), Self::Key(b, _)) => a == b,
            _ => false,
        }
    }
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

/// A place's accessors, sharing the prefix of a composed reference.
#[derive(Clone, Default)]
pub struct Path(Option<Arc<PathLink>>);

struct PathLink {
    prefix: Path,
    suffix: SmallVec<[Step; 2]>,
    len: usize,
}

impl Drop for PathLink {
    fn drop(&mut self) {
        let mut prefix = self.prefix.0.take();
        while let Some(link) = prefix {
            match Arc::try_unwrap(link) {
                Ok(mut link) => prefix = link.prefix.0.take(),
                Err(_) => break,
            }
        }
    }
}

impl std::fmt::Debug for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.same_link(other)
            || (self.len() == other.len() && self.iter().eq(other.iter()))
    }
}

impl Path {
    pub const fn new() -> Self {
        Self(None)
    }

    pub(super) fn empty() -> &'static Self {
        static EMPTY: Path = Path::new();
        &EMPTY
    }

    fn same_link(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (None, None) => true,
            (Some(a), Some(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }

    pub fn len(&self) -> usize {
        self.0.as_ref().map_or(0, |link| link.len)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    pub fn push(&mut self, step: Step) {
        let prefix = std::mem::take(self);
        self.set(&prefix, &[step]);
    }

    pub(super) fn set(&mut self, prefix: &Self, suffix: &[Step]) {
        if suffix.is_empty() {
            *self = prefix.clone();
        } else if !self.0.as_ref().is_some_and(|link| {
            link.prefix.same_link(prefix) && link.suffix.as_slice() == suffix
        }) {
            *self = Self(Some(Arc::new(PathLink {
                prefix: prefix.clone(),
                len: prefix.len() + suffix.len(),
                suffix: SmallVec::from(suffix),
            })));
        }
    }

    pub(super) fn suffix(&self) -> &[Step] {
        self.0.as_ref().map_or(&[], |link| &link.suffix)
    }

    pub fn iter(&self) -> PathIter<'_> {
        let mut parts: LPooled<Vec<&[Step]>> = LPooled::take();
        let mut path = self;
        while let Some(link) = &path.0 {
            parts.push(&link.suffix);
            path = &link.prefix;
        }
        PathIter { parts, current: [].iter(), left: self.len() }
    }
}

pub struct PathIter<'a> {
    parts: LPooled<Vec<&'a [Step]>>,
    current: std::slice::Iter<'a, Step>,
    left: usize,
}

impl<'a> Iterator for PathIter<'a> {
    type Item = &'a Step;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(step) = self.current.next() {
                self.left -= 1;
                return Some(step);
            }
            self.current = self.parts.pop()?.iter();
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

impl ExactSizeIterator for PathIter<'_> {}

impl<'a> IntoIterator for &'a Path {
    type Item = &'a Step;
    type IntoIter = PathIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl From<&[Step]> for Path {
    fn from(steps: &[Step]) -> Self {
        let mut path = Self::new();
        path.set(&Self::new(), steps);
        path
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
        let mut steps: LPooled<Vec<Step>> = LPooled::take();
        for _ in 0..n {
            steps.push(Step::decode(buf)?);
        }
        let mut path = Path::new();
        path.set(&Path::new(), &steps);
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
pub fn read_path<'a>(
    root: &Value,
    path: impl IntoIterator<Item = &'a Step>,
) -> Result<Value> {
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
            (Step::Key(k, check), Value::Map(m)) => {
                anyhow::ensure!(
                    !check || crate::reference::orderable(k),
                    "references have no order, including under Any"
                );
                m.get(k).ok_or_else(|| anyhow!("no key {k}"))?
            }
            (step, v) => bail!("cannot apply {step:?} to {}", crate::typ::Naked(v)),
        };
    }
    Ok(cur.clone())
}

/// `root` with the value at `path` replaced by `v`. A map step compares
/// keys, so the caller runs this under `coretraits::with_hooks`.
// XCR claude for claude: [bug/medium] write_path recurses once per path step with no
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
// 2026-10-27 codex: Paths now hold shared Arc prefix links with an inline local
// suffix. Place::update rebuilds only its own suffix and reuses the existing link
// when that suffix and its parent link are unchanged. Dereference registration and
// queued patches clone the shared path, readers/writers iterate root-first using
// pooled scratch, equality compares contents, and link teardown is iterative.
// ExecCtx::ref_target necessarily returns &Path rather than a slice (one-line lib.rs
// change); a repository search found no ../netidx caller. Pin
// node::place::tests::retained_path_prefixes_use_linear_memory retained 3000 prefixes
// and failed on the original representation at 80600 KiB; it passes below the 32 MiB
// ceiling with sharing. All four node::place::tests pass, including
// unchanged_path_reuses_its_link_and_pack_order,
// deep_shared_path_drops_on_a_small_stack and the existing
// deep_write_keeps_a_flat_stack. Path's encoded count and root-first Step sequence
// are unchanged: no image format bump. Full Graphix runtime chain probe not yet run;
// the regression measured the actual Path representation directly. Initial
// compilation found temporary empty-path references; fixed with a static empty path
// before the passing run.
// 2026-10-27 codex: Further verification with the newly rebuilt target/debug/graphix
// --no-cache and the CR's x-stack-place-path-chain.py: n=10000 exits 0 in 3.018 s at
// 179840 KiB peak RSS; n=20000 exits 0 in 9.730 s at 292288 KiB peak RSS (both print
// 1 before and after the write). Measured with Python time.monotonic and
// resource.getrusage because /usr/bin/time is unavailable. Place's temporary local
// suffix is pooled, so even a long unchanged suffix does not allocate every update.
pub fn write_path<'a>(
    root: &Value,
    path: impl IntoIterator<Item = &'a Step>,
    v: Value,
) -> Result<Value> {
    let mut spine: LPooled<Vec<Hole>> = LPooled::take();
    let mut cur = root;
    let mut steps = path.into_iter().peekable();
    while let Some(step) = steps.next() {
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
            (Step::Key(k, check), Value::Map(m)) => {
                anyhow::ensure!(
                    !check || crate::reference::orderable(k),
                    "references have no order, including under Any"
                );
                spine.push(Hole::Key(m, k));
                match m.get(k) {
                    Some(c) => c,
                    None if steps.peek().is_none() => break,
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

    #[test]
    fn key_type_check_does_not_change_place_identity() {
        assert_eq!(Step::Key(Value::I64(1), false), Step::Key(Value::I64(1), true));
    }

    #[test]
    fn map_paths_refuse_reference_keys() {
        let key = crate::reference::reference(BindId::new(), None);
        let root = Value::Map(netidx_value::Map::new());
        let path = [Step::Key(key, true)];
        assert!(
            read_path(&root, &path)
                .unwrap_err()
                .to_string()
                .contains("references have no order")
        );
        assert!(
            write_path(&root, &path, Value::I64(1))
                .unwrap_err()
                .to_string()
                .contains("references have no order")
        );
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn retained_path_prefixes_use_linear_memory() {
        const CHILD: &str = "GRAPHIX_PATH_MEMORY_CHILD";
        if std::env::var_os(CHILD).is_none() {
            let out = std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "node::place::tests::retained_path_prefixes_use_linear_memory",
                    "--nocapture",
                ])
                .env(CHILD, "1")
                .output()
                .unwrap();
            assert!(
                out.status.success(),
                "{}\n{}",
                String::from_utf8_lossy(&out.stdout),
                String::from_utf8_lossy(&out.stderr)
            );
            return;
        }
        let rss = || {
            let status = std::fs::read_to_string("/proc/self/status").unwrap();
            status
                .lines()
                .find_map(|l| l.strip_prefix("VmRSS:"))
                .unwrap()
                .split_whitespace()
                .next()
                .unwrap()
                .parse::<usize>()
                .unwrap()
        };
        let before = rss();
        let mut paths: LPooled<Vec<Path>> = LPooled::take();
        let mut path = Path::new();
        for _ in 0..3_000 {
            path.push(Step::Index(0));
            paths.push(path.clone());
        }
        let used = rss().saturating_sub(before);
        assert!(used < 32 * 1024, "3000 retained prefixes used {used} KiB");
        assert_eq!(paths.last().unwrap().len(), 3_000);
    }

    #[test]
    fn unchanged_path_reuses_its_link_and_pack_order() {
        let mut prefix = Path::new();
        prefix.push(Step::Index(2));
        let suffix = [Step::Field(arcstr::literal!("a")), Step::Index(-1)];
        let mut path = Path::new();
        path.set(&prefix, &suffix);
        let previous = path.clone();
        path.set(&prefix, &suffix);
        assert!(path.same_link(&previous));
        let flat: Path =
            [Step::Index(2), Step::Field(arcstr::literal!("a")), Step::Index(-1)]
                .as_slice()
                .into();
        assert_eq!(path, flat);
        let mut encoded = bytes::BytesMut::new();
        path.encode(&mut encoded).unwrap();
        assert_eq!(encoded.len(), path.encoded_len());
        let mut expected = bytes::BytesMut::new();
        flat.encode(&mut expected).unwrap();
        assert_eq!(encoded, expected);
        assert_eq!(Path::decode(&mut encoded.freeze()).unwrap(), path);
    }

    #[test]
    fn deep_shared_path_drops_on_a_small_stack() {
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(|| {
                let mut path = Path::new();
                for _ in 0..50_000 {
                    path.push(Step::Index(0));
                }
                assert_eq!(path.iter().count(), 50_000);
                drop(path);
            })
            .unwrap()
            .join()
            .unwrap();
    }

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
