use crate::{
    PrintFlag,
    expr::{
        At, CouldNotResolve, Expr, ExprId, ExprKind, ModPath, ModuleKind, Name, Origin,
        Sig, SigItem, SigKind, Source, UseItem, parser, serialize,
    },
    format_with_flags,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use combine::stream::position::SourcePosition;
use compact_str::{CompactString, format_compact};
use futures::future::try_join_all;
use indexmap::IndexMap;
use log::info;
use netidx_core::path::Path;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    fmt::{self, Write as _},
    path::PathBuf,
    pin::Pin,
    str::FromStr,
    sync::Arc as SArc,
};
use tokio::{task, time::Instant};
use triomphe::Arc;

pub type BufferOverrides = Arc<Mutex<AHashMap<PathBuf, ArcStr>>>;

/// A VFS module entry: its source text plus, optionally, the pre-parsed AST
/// as a packed blob (see [`super::serialize`]), which the resolver decodes
/// instead of re-parsing `source`. `source` is always kept for `Origin`
/// reconstruction and error snippets.
#[derive(Debug, Clone)]
pub struct VfsEntry {
    pub source: ArcStr,
    pub packed: Option<Bytes>,
}

impl From<ArcStr> for VfsEntry {
    fn from(source: ArcStr) -> Self {
        VfsEntry { source, packed: None }
    }
}

/// A module source loader. The compiler ships [`VfsResolver`] and
/// [`FilesResolver`]; loaders for other transports live in the package
/// that owns the transport and are threaded in by the embedder.
pub trait ModuleResolver: std::fmt::Debug + Send + Sync {
    /// Try to resolve module `name` in `scope` under `parent`. Return
    /// [`Resolution::TryNextMethod`] (pushing any diagnostic into
    /// `errors`) to let the next resolver in the list try.
    fn resolve<'a>(
        &'a self,
        scope: &'a ModPath,
        parent: &'a Arc<Origin>,
        name: &'a Path,
        errors: &'a mut Vec<anyhow::Error>,
    ) -> Pin<Box<dyn Future<Output = Resolution> + Send + Sync + 'a>>;

    /// Derive a resolver for the submodules of a module whose
    /// implementation came from `source`, when this resolver understands
    /// that source kind. The compiler handles `Source::File` itself.
    fn for_source(&self, _source: &Source) -> Option<ResolverRef> {
        None
    }

    /// Fetch a single top-level source this resolver's transport
    /// understands. `None` (the default) means "not my transport".
    fn fetch_source<'a>(
        &'a self,
        _source: &'a Source,
    ) -> Option<Pin<Box<dyn Future<Output = Result<ArcStr>> + Send + Sync + 'a>>> {
        None
    }

    /// The LSP buffer overrides this resolver carries, if any; inherited by
    /// the directory-based resolvers derived for relative includes.
    fn overrides(&self) -> Option<BufferOverrides> {
        None
    }
}

/// A shared resolver handle (`std::sync::Arc`: triomphe cannot unsize to
/// trait objects).
pub type ResolverRef = SArc<dyn ModuleResolver>;

/// Resolvers threaded through module resolution, tried in order.
pub type Resolvers = SArc<[ResolverRef]>;

/// Constructs a resolver from the payload of a `scheme:` entry in
/// GRAPHIX_MODPATH. Registered by the embedder per scheme; `file` is built
/// in. Receives the context's [`LibState`](crate::LibState) so a package resolver can share
/// state with its package's builtins.
pub type ResolverFactory =
    SArc<dyn Fn(&mut crate::LibState, &str) -> Result<ResolverRef> + Send + Sync>;

/// In-memory module store — the stdlib packages and test sources.
#[derive(Debug, Clone)]
pub struct VfsResolver {
    pub vfs: AHashMap<Path, VfsEntry>,
    /// The registered packages' root modules: what the registration
    /// loads, never a file's or a netidx path's own `mod` of that name.
    pub packages: AHashSet<ArcStr>,
}

impl VfsResolver {
    pub fn new(vfs: AHashMap<Path, VfsEntry>) -> ResolverRef {
        Self::with_packages(vfs, [])
    }

    pub fn with_packages(
        vfs: AHashMap<Path, VfsEntry>,
        packages: impl IntoIterator<Item = ArcStr>,
    ) -> ResolverRef {
        SArc::new(VfsResolver { vfs, packages: packages.into_iter().collect() })
    }
}

impl ModuleResolver for VfsResolver {
    fn resolve<'a>(
        &'a self,
        scope: &'a ModPath,
        parent: &'a Arc<Origin>,
        name: &'a Path,
        _errors: &'a mut Vec<anyhow::Error>,
    ) -> Pin<Box<dyn Future<Output = Resolution> + Send + Sync + 'a>> {
        Box::pin(async move { resolve_from_vfs(scope, parent, name, self) })
    }
}

/// Filesystem loader rooted at `base`, with optional LSP buffer
/// overrides.
#[derive(Debug, Clone)]
pub struct FilesResolver {
    pub base: PathBuf,
    pub overrides: Option<BufferOverrides>,
}

impl FilesResolver {
    pub fn new(base: PathBuf, overrides: Option<BufferOverrides>) -> ResolverRef {
        SArc::new(FilesResolver { base, overrides })
    }
}

impl ModuleResolver for FilesResolver {
    fn resolve<'a>(
        &'a self,
        _scope: &'a ModPath,
        parent: &'a Arc<Origin>,
        name: &'a Path,
        errors: &'a mut Vec<anyhow::Error>,
    ) -> Pin<Box<dyn Future<Output = Resolution> + Send + Sync + 'a>> {
        Box::pin(async move {
            resolve_from_files(parent, name, &self.base, self.overrides.as_ref(), errors)
                .await
        })
    }

    fn overrides(&self) -> Option<BufferOverrides> {
        self.overrides.clone()
    }
}

/// `s` split at each `sep` that `esc` does not escape, into at most `n`
/// fields, the last holding the rest; the fields keep their escapes. An
/// escape escapes the one char after it, a separator included.
pub fn split_escaped(
    s: &str,
    esc: char,
    sep: char,
    n: usize,
) -> impl Iterator<Item = &str> {
    let mut rest = Some(s);
    let mut fields = 0;
    std::iter::from_fn(move || {
        let r = rest.take()?;
        fields += 1;
        if fields < n {
            let mut escaped = false;
            for (i, c) in r.char_indices() {
                if escaped {
                    escaped = false
                } else if c == esc {
                    escaped = true
                } else if c == sep {
                    rest = Some(&r[i + c.len_utf8()..]);
                    return Some(&r[..i]);
                }
            }
        }
        Some(r)
    })
}

/// Parse a GRAPHIX_MODPATH-style list (`scheme:payload,{...}`) into
/// resolvers. `file:` is built in, other schemes look up `factories`,
/// an entry with no registered scheme is a file path, and an empty
/// entry (a trailing comma's) is nothing.
pub fn parse_modpath(
    factories: &AHashMap<ArcStr, ResolverFactory>,
    libstate: &mut crate::LibState,
    s: &str,
) -> Result<Vec<ResolverRef>> {
    let mut res: Vec<ResolverRef> = vec![];
    for l in split_escaped(s, '\\', ',', usize::MAX) {
        // only the separator is escaped: a Windows path keeps its `\`
        let l = l.trim().replace("\\,", ",");
        if l.is_empty() {
            continue;
        }
        let scheme = l
            .split_once(':')
            .and_then(|(scheme, rest)| factories.get(scheme).map(|f| f(libstate, rest)));
        match scheme {
            Some(r) => res.push(r.with_context(|| format_compact!("{l}"))?),
            None => {
                let path = l.strip_prefix("file:").unwrap_or(&l);
                res.push(FilesResolver::new(PathBuf::from_str(path)?, None))
            }
        }
    }
    Ok(res)
}

/// `GRAPHIX_DISABLE_PACKED_AST=1` forces module resolution to parse source
/// even when a packed AST is available.
fn packed_ast_disabled() -> bool {
    use std::sync::LazyLock;
    static DISABLED: LazyLock<bool> =
        LazyLock::new(|| std::env::var_os("GRAPHIX_DISABLE_PACKED_AST").is_some());
    *DISABLED
}

/// One source file of a resolved module, with the packed pre-parsed AST
/// (see [`super::serialize`]) when the VFS entry carried one.
pub struct Unit {
    pub ori: Origin,
    pub packed: Option<Bytes>,
}

impl Unit {
    fn parsed(ori: Origin) -> Self {
        Unit { ori, packed: None }
    }
}

/// The result of one resolver's attempt — the [`ModuleResolver`]
/// trait's currency.
pub enum Resolution {
    Resolved {
        interface: Option<Unit>,
        implementation: Unit,
    },
    /// Not this resolver's module; the next one tries.
    TryNextMethod,
    /// The module is here but cannot be read: no later resolver's
    /// module of the same name stands in for it.
    Broken(anyhow::Error),
}

impl Resolution {
    /// A parse-always resolution (no packed AST).
    pub fn parsed(interface: Option<Origin>, implementation: Origin) -> Self {
        Resolution::Resolved {
            interface: interface.map(Unit::parsed),
            implementation: Unit::parsed(implementation),
        }
    }
}

/// The files a module `name` may be, tried in order beside its parent's
/// base: `{name}.gx` or `{name}/mod.gx`, each paired with the interface
/// beside it. Every resolver lays modules out this way.
pub const MODULE_LAYOUTS: [(&str, &str); 2] = [(".gx", ".gxi"), ("/mod.gx", "/mod.gxi")];

/// Where the submodules of the module at `path` (its implementation or
/// its interface) live: `{stem}/` beside `{stem}.gx`, its own directory
/// for `mod.gx`.
pub fn submodule_base(path: &str) -> &str {
    let stem =
        path.strip_suffix(".gxi").or_else(|| path.strip_suffix(".gx")).unwrap_or(path);
    stem.strip_suffix("/mod").unwrap_or(stem)
}

fn resolve_from_vfs(
    scope: &ModPath,
    parent: &Arc<Origin>,
    name: &Path,
    r: &VfsResolver,
) -> Resolution {
    let vfs = &r.vfs;
    let unit = |e: &VfsEntry| Unit {
        ori: Origin {
            parent: Some(parent.clone()),
            source: Source::Internal(name.clone().into()),
            text: e.source.clone(),
        },
        packed: e.packed.clone(),
    };
    // a program's own `mod str` is its module, not the str package
    if matches!(parent.source, Source::File(_) | Source::Netidx(_))
        && scope.0 == Path::root()
        && r.packages.contains(&**name)
    {
        return Resolution::TryNextMethod;
    }
    let path = |file: &str| scope.append(&format_compact!("{name}{file}"));
    let at = |file: &str| vfs.get(&path(file));
    // an interface pairs with the implementation beside it, as on disk
    let [(imp0, intf0), (imp1, intf1)] = MODULE_LAYOUTS;
    match (at(imp0), at(imp1)) {
        (Some(_), Some(_)) => Resolution::Broken(anyhow::anyhow!(
            "{} and {} both define module {name}",
            path(imp0),
            path(imp1)
        )),
        (Some(_), None) if at(intf1).is_some() => {
            Resolution::Broken(stray_interface(&path(intf1), &path(imp0)))
        }
        (None, Some(_)) if at(intf0).is_some() => {
            Resolution::Broken(stray_interface(&path(intf0), &path(imp1)))
        }
        (Some(e), None) => Resolution::Resolved {
            interface: at(intf0).map(unit),
            implementation: unit(e),
        },
        (None, Some(e)) => Resolution::Resolved {
            interface: at(intf1).map(unit),
            implementation: unit(e),
        },
        (None, None) => Resolution::TryNextMethod,
    }
}

/// An interface left beside the layout its module does not use: it
/// would be dropped and the module's every item public.
fn stray_interface(intf: &dyn fmt::Display, imp: &dyn fmt::Display) -> anyhow::Error {
    anyhow::anyhow!(
        "{intf} is no interface: the module is {imp}, and its interface goes beside it"
    )
}

async fn resolve_from_files(
    parent: &Arc<Origin>,
    name: &Path,
    base: &PathBuf,
    overrides: Option<&BufferOverrides>,
    errors: &mut Vec<anyhow::Error>,
) -> Resolution {
    async fn read(
        overrides: Option<&BufferOverrides>,
        path: &PathBuf,
    ) -> Result<Option<ArcStr>> {
        match overrides.and_then(|o| o.lock().get(path).cloned()) {
            Some(s) => Ok(Some(s)),
            None => read_optional(path).await,
        }
    }
    let unit = |text: ArcStr, path: PathBuf| {
        Unit::parsed(Origin {
            parent: Some(parent.clone()),
            source: Source::File(path),
            text,
        })
    };
    let rel = Path::parts(&name).collect::<Vec<_>>().join("/");
    let mut found: SmallVec<[(PathBuf, Option<ArcStr>, PathBuf, Option<ArcStr>); 2]> =
        SmallVec::new();
    for (imp, intf) in MODULE_LAYOUTS {
        let (imp, intf) =
            (base.join(format!("{rel}{imp}")), base.join(format!("{rel}{intf}")));
        let imp_text = match read(overrides, &imp).await {
            Ok(t) => t,
            Err(e) => return Resolution::Broken(e),
        };
        let intf_text = match read(overrides, &intf).await {
            Ok(t) => t,
            Err(e) => return Resolution::Broken(e),
        };
        found.push((imp, imp_text, intf, intf_text));
    }
    let [(imp0, t0, intf0, i0), (imp1, t1, intf1, i1)] =
        <[_; 2]>::try_from(found.into_vec()).ok().expect("two layouts");
    match (t0, t1) {
        (Some(_), Some(_)) => {
            return Resolution::Broken(anyhow::anyhow!(
                "{} and {} both define module {name}",
                imp0.display(),
                imp1.display()
            ));
        }
        (Some(_), None) if i1.is_some() => {
            return Resolution::Broken(stray_interface(
                &intf1.display(),
                &imp0.display(),
            ));
        }
        (None, Some(_)) if i0.is_some() => {
            return Resolution::Broken(stray_interface(
                &intf0.display(),
                &imp1.display(),
            ));
        }
        (Some(text), None) => {
            return Resolution::Resolved {
                interface: i0.map(|text| unit(text, intf0)),
                implementation: unit(text, imp0),
            };
        }
        (None, Some(text)) => {
            return Resolution::Resolved {
                interface: i1.map(|text| unit(text, intf1)),
                implementation: unit(text, imp1),
            };
        }
        (None, None) => (),
    }
    let [(file, _), (mod_file, _)] = MODULE_LAYOUTS;
    errors.push(anyhow::anyhow!(
        "{} or {}: no such file",
        base.join(format!("{rel}{file}")).display(),
        base.join(format!("{rel}{mod_file}")).display()
    ));
    Resolution::TryNextMethod
}

/// A spliceable declaration's identity: a module, a type or a trait by
/// name, a `use` by its names.
#[derive(Clone, PartialEq, Eq, Hash)]
enum SpliceKey {
    Module(ArcStr),
    TypeDef(ArcStr),
    Trait(ArcStr),
    Use(bool, Arc<[UseItem]>),
}

/// What an interface item is placed after: a name the item before it
/// in the interface declares.
#[derive(Clone, PartialEq, Eq, Hash)]
enum Anchor {
    Bind(ArcStr),
    Module(ArcStr),
    TypeDef(ArcStr),
    Trait(ArcStr),
    Use(UseItem),
}

impl SpliceKey {
    fn of_sig(kind: &SigKind) -> Option<Self> {
        match kind {
            SigKind::Module(name) => Some(Self::Module(name.name.clone())),
            SigKind::TypeDef(td) => Some(Self::TypeDef(td.name.name.clone())),
            SigKind::Trait(t) => Some(Self::Trait(t.name.name.clone())),
            SigKind::Use { reexport, names } => Some(Self::Use(*reexport, names.clone())),
            SigKind::Bind(_) | SigKind::Impl(_) => None,
        }
    }

    fn of_expr(kind: &ExprKind) -> Option<Self> {
        match kind {
            ExprKind::Module { name, .. } => Some(Self::Module(name.name.clone())),
            ExprKind::TypeDef(td) => Some(Self::TypeDef(td.name.name.clone())),
            ExprKind::Trait(t) => Some(Self::Trait(t.name.name.clone())),
            ExprKind::Use { reexport, names } => {
                Some(Self::Use(*reexport, names.clone()))
            }
            _ => None,
        }
    }
}

impl Anchor {
    fn of_sig(kind: &SigKind) -> SmallVec<[Self; 1]> {
        match kind {
            SigKind::Bind(v) => [Self::Bind(v.name.name.clone())].into(),
            SigKind::Module(m) => [Self::Module(m.name.clone())].into(),
            SigKind::TypeDef(td) => [Self::TypeDef(td.name.name.clone())].into(),
            SigKind::Trait(t) => [Self::Trait(t.name.name.clone())].into(),
            SigKind::Use { names, .. } => names.iter().cloned().map(Self::Use).collect(),
            SigKind::Impl(_) => SmallVec::new(),
        }
    }

    fn of_expr(kind: &ExprKind) -> SmallVec<[Self; 1]> {
        match kind {
            ExprKind::Bind(b) => {
                let mut names = SmallVec::new();
                b.pattern.with_names(&mut |n| names.push(Self::Bind(n.clone())));
                names
            }
            ExprKind::Module { name, .. } => [Self::Module(name.name.clone())].into(),
            ExprKind::TypeDef(td) => [Self::TypeDef(td.name.name.clone())].into(),
            ExprKind::Trait(t) => [Self::Trait(t.name.name.clone())].into(),
            ExprKind::Use { names, .. } => names.iter().cloned().map(Self::Use).collect(),
            _ => SmallVec::new(),
        }
    }
}

/// A declaration that reads no value of its module: a type (its body
/// resolves lazily) or a trait with no default body. It goes ahead of the
/// body, where every statement can name it.
fn needs_no_value(kind: &SigKind) -> bool {
    match kind {
        SigKind::TypeDef(_) => true,
        SigKind::Trait(t) => t.methods.iter().all(|m| m.default.is_none()),
        _ => false,
    }
}

/// `exprs` with the modules, types, traits and uses only `sig` declares
/// spliced in, keeping their relative location and order; a declaration
/// without an origin of its own (a packed interface's) takes `ori`, the
/// interface's.
pub fn add_interface_modules(
    exprs: Arc<[Expr]>,
    sig: &Sig,
    ori: &Arc<Origin>,
) -> Arc<[Expr]> {
    let synth = |si: &SigItem| {
        let kind = match &si.kind {
            SigKind::Module(name) => ExprKind::Module {
                name: name.clone(),
                value: ModuleKind::Unresolved { from_interface: true },
            },
            SigKind::TypeDef(td) => ExprKind::TypeDef(td.clone()),
            SigKind::Trait(t) => ExprKind::Trait(t.clone()),
            SigKind::Use { reexport, names } => {
                ExprKind::Use { reexport: *reexport, names: names.clone() }
            }
            SigKind::Bind(_) | SigKind::Impl(_) => return None,
        };
        Some(Expr {
            id: ExprId::new(),
            ori: si.ori.as_ref().unwrap_or(ori).clone(),
            pos: si.pos,
            kind,
            dec: None,
            str_form: Default::default(),
            end: Default::default(),
        })
    };
    // the interface-only declarations in interface order: what needs no
    // value before it first, each other one keyed after the declaration
    // before it
    let mut pending: LPooled<IndexMap<SpliceKey, &SigItem>> = LPooled::take();
    let mut after: LPooled<AHashMap<Anchor, SpliceKey>> = LPooled::take();
    let mut front: LPooled<Vec<SpliceKey>> = LPooled::take();
    let mut first: Option<SpliceKey> = None;
    let mut last: Option<&SigItem> = None;
    for si in sig.items.iter() {
        if let Some(key) = SpliceKey::of_sig(&si.kind) {
            if needs_no_value(&si.kind) {
                front.push(key.clone());
                pending.insert(key, si);
                continue;
            }
            match last {
                None => first = Some(key.clone()),
                Some(prev) => {
                    for a in Anchor::of_sig(&prev.kind) {
                        after.insert(a, key.clone());
                    }
                }
            }
            pending.insert(key, si);
        }
        // an `impl` declaration is never spliced, so it anchors nothing
        if !matches!(si.kind, SigKind::Impl(_)) {
            last = Some(si);
        }
    }
    for e in exprs.iter() {
        if let Some(key) = SpliceKey::of_expr(&e.kind) {
            pending.shift_remove(&key);
        }
    }
    if pending.is_empty() {
        return exprs;
    }
    let mut res: LPooled<Vec<Expr>> = LPooled::take();
    for key in front.drain(..) {
        if let Some(si) = pending.shift_remove(&key) {
            res.extend(synth(si));
        }
    }
    // the item keyed at `next`, then each one keyed after it in turn
    let mut splice = |mut next: Option<SpliceKey>,
                      after: &mut AHashMap<Anchor, SpliceKey>,
                      res: &mut Vec<Expr>| {
        while let Some(si) = next.take().and_then(|k| pending.shift_remove(&k)) {
            res.extend(synth(si));
            next = Anchor::of_sig(&si.kind).iter().find_map(|a| after.remove(a));
        }
    };
    splice(first, &mut after, &mut res);
    for e in exprs.iter() {
        res.push(e.clone());
        for a in Anchor::of_expr(&e.kind) {
            let next = after.remove(&a);
            splice(next, &mut after, &mut res);
        }
    }
    drop(splice);
    res.extend(pending.drain(..).filter_map(|(_, si)| synth(si)));
    Arc::from_iter(res.drain(..))
}

/// Parse (or unpack) a module's implementation and interface, with the
/// interface's modules, types, traits and uses spliced into the body.
async fn parse_module(
    interface: Option<&Unit>,
    implementation: &Unit,
) -> Result<(Arc<[Expr]>, Option<Sig>)> {
    let naming = |ori: &Origin| {
        format_with_flags(PrintFlag::NoSource | PrintFlag::NoParents, || {
            format!("parsing {ori}")
        })
    };
    // Decode and parse both run on a blocking thread; `unpack_module`
    // sets its per-module thread-locals on the thread that decodes.
    let exprs = {
        let ori = implementation.ori.clone();
        match implementation.packed.clone().filter(|_| !packed_ast_disabled()) {
            Some(bytes) => task::spawn_blocking(move || {
                serialize::unpack_module(&bytes, Arc::new(ori))
            }),
            None => task::spawn_blocking(move || parser::parse(ori)),
        }
    };
    let sig = match interface {
        None => None,
        Some(unit) => {
            let ori = Arc::new(unit.ori.clone());
            let sig_ori = ori.clone();
            let sig = match unit.packed.clone().filter(|_| !packed_ast_disabled()) {
                Some(bytes) => {
                    task::spawn_blocking(move || serialize::unpack_sig(&bytes, sig_ori))
                }
                None => {
                    task::spawn_blocking(move || parser::parse_sig((*sig_ori).clone()))
                }
            }
            .await?
            .with_context(|| naming(&unit.ori))?;
            Some((sig, ori))
        }
    };
    let exprs = exprs.await?.with_context(|| naming(&implementation.ori))?;
    let exprs = match &sig {
        Some((sig, ori)) => add_interface_modules(exprs, sig, ori),
        None => exprs,
    };
    Ok((exprs, sig.map(|(sig, _)| sig)))
}

/// A root source file: a program, or a package's `mod.gx`.
pub struct RootFile {
    pub ori: Origin,
    pub exprs: Arc<[Expr]>,
    pub sig: Option<Sig>,
}

impl RootFile {
    /// Load `file` and the `.gxi` beside it, open buffers first. With
    /// no buffers the file is named by its canonical path, so its
    /// modules resolve beside the real file; with them the caller's
    /// names rule, since they key the buffers.
    pub async fn load(
        file: &PathBuf,
        overrides: Option<&BufferOverrides>,
    ) -> Result<Self> {
        let buffer = |p: &PathBuf| overrides.and_then(|o| o.lock().get(p).cloned());
        let file = match overrides {
            Some(_) => file.clone(),
            None => tokio::fs::canonicalize(file)
                .await
                .with_context(|| format!("{}", file.display()))?,
        };
        let text = match buffer(&file) {
            Some(text) => text,
            None => read_to_arcstr(&file).await?,
        };
        let text = parser::without_shebang(&text);
        let intf = file.with_extension("gxi");
        let interface = match file.extension().and_then(|s| s.to_str()) {
            Some("gx") => match buffer(&intf) {
                Some(text) => Some(text),
                None => read_optional(&intf).await?,
            },
            Some(_) | None => None,
        };
        let interface = interface.map(|text| Origin {
            parent: None,
            source: Source::File(intf),
            text,
        });
        let implementation =
            Unit::parsed(Origin { parent: None, source: Source::File(file), text });
        let interface = interface.map(Unit::parsed);
        let (exprs, sig) = parse_module(interface.as_ref(), &implementation).await?;
        Ok(Self { ori: implementation.ori, exprs, sig })
    }

    /// The file as the body of module `name`: what a package's root is.
    pub fn into_module(self, name: ArcStr) -> Expr {
        let name = Name::from(name);
        let Self { ori, exprs, sig } = self;
        let value = ModuleKind::Resolved { exprs, sig, from_interface: false };
        let mut e = ExprKind::Module { name, value }.to_expr(SourcePosition::default());
        e.ori = Arc::new(ori);
        e
    }
}

/// Resolve the unresolved `mod` statement `stmt` in `scope`: the
/// statement with its body, from the first resolver that has it.
async fn resolve(
    scope: ModPath,
    prepend: Option<ResolverRef>,
    resolvers: Resolvers,
    stmt: &Expr,
    module: &Name,
    from_interface: bool,
) -> Result<Expr> {
    let ts = Instant::now();
    let name = Path::from(module.name.clone());
    let mut errors: LPooled<Vec<anyhow::Error>> = LPooled::take();
    // a body with a source of its own (a file, a netidx path) keeps its
    // submodules beside it; only a root's and a VFS body's go to the chain
    let chain: &[ResolverRef] = match &prepend {
        Some(_) => &[],
        None => &resolvers,
    };
    for r in prepend.iter().chain(chain.iter()) {
        let (interface, implementation) =
            match r.resolve(&scope, &stmt.ori, &name, &mut errors).await {
                Resolution::TryNextMethod => continue,
                Resolution::Broken(e) => return Err(e),
                Resolution::Resolved { interface, implementation } => {
                    (interface, implementation)
                }
            };
        let (exprs, sig) = parse_module(interface.as_ref(), &implementation).await?;
        info!(
            "load and parse {:?} and {:?} {:?}",
            implementation.ori.source,
            interface.as_ref().map(|i| &i.ori.source),
            ts.elapsed()
        );
        let value = ModuleKind::Resolved { exprs, sig, from_interface };
        let kind = ExprKind::Module { name: module.clone(), value };
        return Ok(stmt.rekind(kind));
    }
    let mut why = CompactString::new("");
    for (i, e) in errors.iter().enumerate() {
        let _ = write!(&mut why, "{}{e}", if i == 0 { ": " } else { "; " });
    }
    Err(CouldNotResolve { path: ModPath(scope.append(&*name)), why }.into())
}

impl Expr {
    /// Resolve external modules referenced in the expression, trying each
    /// resolver in order until one succeeds.
    pub async fn resolve_modules<'a>(&'a self, resolvers: &'a Resolvers) -> Result<Expr> {
        self.resolve_modules_in_scope(&ModPath::root(), resolvers).await
    }

    /// Like `resolve_modules` but starts at a non-root scope.
    pub async fn resolve_modules_in_scope<'a>(
        &'a self,
        scope: &'a ModPath,
        resolvers: &'a Resolvers,
    ) -> Result<Expr> {
        if !self.holds_unresolved() {
            return Ok(self.clone());
        }
        let e = self.resolve_modules_int(scope, &None, &None, resolvers).await?;
        Ok(e.unwrap_or_else(|| self.clone()))
    }

    /// Whether an unresolved `mod` sits at or beneath this expression.
    fn holds_unresolved(&self) -> bool {
        crate::stack::ensure_sufficient(|| match &self.kind {
            ExprKind::Module { value: ModuleKind::Unresolved { .. }, .. } => true,
            _ => {
                let mut found = false;
                self.for_each_child(&mut |c| found = found || c.holds_unresolved());
                found
            }
        })
    }

    /// Each of `exprs` with its modules resolved, or `None` when none
    /// was; only a child holding an unresolved module is walked.
    async fn resolve_children<'a>(
        exprs: impl IntoIterator<Item = &'a Expr>,
        scope: &'a ModPath,
        prepend: &'a Option<ResolverRef>,
        chain: &'a Option<Arc<LoadChain>>,
        resolvers: &'a Resolvers,
    ) -> Result<Option<SmallVec<[Option<Expr>; 4]>>> {
        let resolved = try_join_all(exprs.into_iter().map(|e| async move {
            match e.holds_unresolved() {
                true => e.resolve_modules_int(scope, prepend, chain, resolvers).await,
                false => Ok(None),
            }
        }))
        .await?;
        Ok(resolved.iter().any(|r| r.is_some()).then(|| resolved.into_iter().collect()))
    }

    /// `Some` iff a module under `self` was resolved: the tree with it
    /// resolved; an unchanged subtree is neither rebuilt nor cloned.
    fn resolve_modules_int<'a>(
        &'a self,
        scope: &'a ModPath,
        prepend: &'a Option<ResolverRef>,
        chain: &'a Option<Arc<LoadChain>>,
        resolvers: &'a Resolvers,
    ) -> Pin<Box<dyn Future<Output = Result<Option<Expr>>> + Send + Sync + 'a>> {
        // each level of a module chain polls the next: the poll is guarded
        // like any program-driven recursion
        Box::pin(Guarded(self.resolve_modules_level(scope, prepend, chain, resolvers)))
    }

    fn resolve_modules_level<'a>(
        &'a self,
        scope: &'a ModPath,
        prepend: &'a Option<ResolverRef>,
        chain: &'a Option<Arc<LoadChain>>,
        resolvers: &'a Resolvers,
    ) -> Pin<Box<dyn Future<Output = Result<Option<Expr>>> + Send + Sync + 'a>> {
        match &self.kind {
            ExprKind::Module {
                value: ModuleKind::Unresolved { from_interface },
                name,
            } => Box::pin(async move {
                let e = resolve(
                    scope.clone(),
                    prepend.clone(),
                    resolvers.clone(),
                    self,
                    name,
                    *from_interface,
                )
                .await
                .at(self)?;
                let scope = ModPath(scope.append(&**name));
                let r = e.resolve_modules_int(&scope, prepend, chain, resolvers).await?;
                Ok(Some(r.unwrap_or(e)))
            }),
            ExprKind::Module {
                value: ModuleKind::Resolved { exprs, sig, from_interface },
                name,
            } => Box::pin(async move {
                let source = self_kind_origin(&self.kind).map(|o| &o.source);
                // a file or a netidx path is one place; an `Internal`
                // source is a VFS module's bare name, and a VFS lookup is
                // scoped by the module path, so it cannot come round again
                let chain = match source {
                    Some(s @ (Source::File(_) | Source::Netidx(_))) => {
                        Some(LoadChain::push(chain, s, name).at(self)?)
                    }
                    _ => chain.clone(),
                };
                let files = |base: PathBuf| {
                    let overrides = resolvers.iter().find_map(|m| m.overrides());
                    SArc::new(FilesResolver { base, overrides }) as ResolverRef
                };
                // Sub-modules resolve beside the body's own source: relative
                // to the implementation file's directory (`<dir>/foo/` for
                // `foo.gx`, `<dir>/` for `foo/mod.gx`), by the module path
                // for a VFS body, through the transport for a netidx one.
                let prepend = match source {
                    Some(Source::File(p)) => {
                        let Some(p) = p.to_str() else { return Ok(None) };
                        Some(files(PathBuf::from(submodule_base(p))))
                    }
                    Some(Source::Internal(_) | Source::Unspecified) => None,
                    Some(s) => resolvers.iter().find_map(|m| m.for_source(s)),
                    None => match &self.ori.source {
                        Source::Unspecified | Source::Internal(_) => None,
                        Source::File(p) => p.parent().map(|p| files(p.into())),
                        s => resolvers.iter().find_map(|m| m.for_source(s)),
                    },
                };
                let Some(resolved) = Self::resolve_children(
                    exprs.iter(),
                    scope,
                    &prepend,
                    &chain,
                    resolvers,
                )
                .await?
                else {
                    return Ok(None);
                };
                let exprs = resolved
                    .into_iter()
                    .zip(exprs.iter())
                    .map(|(r, e)| r.unwrap_or_else(|| e.clone()));
                let value = ModuleKind::Resolved {
                    exprs: Arc::from_iter(exprs),
                    sig: sig.clone(),
                    from_interface: *from_interface,
                };
                Ok(Some(self.rekind(ExprKind::Module { value, name: name.clone() })))
            }),
            _ => Box::pin(async move {
                let mut children: SmallVec<[&Expr; 4]> = SmallVec::new();
                self.for_each_child(&mut |c| children.push(c));
                let Some(resolved) =
                    Self::resolve_children(children, scope, prepend, chain, resolvers)
                        .await?
                else {
                    return Ok(None);
                };
                let mut resolved = resolved.into_iter();
                let mut e = self.map_children(&mut |c| {
                    resolved
                        .next()
                        .expect("map_children follows for_each_child")
                        .unwrap_or_else(|| c.clone())
                });
                e.id = self.id;
                Ok(Some(e))
            }),
        }
    }
}

/// The implementation's origin of a resolved module expression.
fn self_kind_origin(kind: &ExprKind) -> Option<&Arc<Origin>> {
    match kind {
        ExprKind::Module { value, .. } => value.implementation_origin(),
        _ => None,
    }
}

/// A future polled under the stack guard.
struct Guarded<F>(F);

impl<F: Future + Unpin> Future for Guarded<F> {
    type Output = F::Output;

    fn poll(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<F::Output> {
        crate::stack::ensure_sufficient(|| Pin::new(&mut self.0).poll(cx))
    }
}

/// The sources on the path of module loads that led here, innermost
/// first.
struct LoadChain {
    source: Source,
    name: Name,
    prev: Option<Arc<LoadChain>>,
}

impl LoadChain {
    fn push(
        chain: &Option<Arc<LoadChain>>,
        source: &Source,
        name: &Name,
    ) -> Result<Arc<Self>> {
        let mut cur = chain.as_ref();
        while let Some(c) = cur {
            if c.source == *source {
                let mut names: SmallVec<[&ArcStr; 4]> = SmallVec::new();
                let mut back = chain.as_ref();
                while let Some(b) = back {
                    names.push(&b.name.name);
                    if b.source == *source {
                        break;
                    }
                    back = b.prev.as_ref();
                }
                let mut msg = format_compact!("import cycle:");
                for n in names.iter().rev() {
                    let _ = write!(&mut msg, " {n} ->");
                }
                bail!("{msg} {} ({source:?})", name.name);
            }
            cur = c.prev.as_ref();
        }
        Ok(Arc::new(LoadChain {
            source: source.clone(),
            name: name.clone(),
            prev: chain.clone(),
        }))
    }
}

/// Read a file to an ArcStr with minimal allocation.
pub async fn read_to_arcstr(path: impl AsRef<std::path::Path>) -> Result<ArcStr> {
    let path = path.as_ref();
    read_optional(path)
        .await?
        .ok_or_else(|| anyhow::anyhow!("{}: no such file", path.display()))
}

/// Read a file that may not exist: `None` when it does not, an error
/// for any other failure (unreadable, not UTF-8).
pub async fn read_optional(path: impl AsRef<std::path::Path>) -> Result<Option<ArcStr>> {
    use tokio::io::AsyncReadExt;
    let path = path.as_ref();
    let mut f = match tokio::fs::File::open(path).await {
        Ok(f) => f,
        // a path through a file names nothing either
        Err(e)
            if matches!(
                e.kind(),
                std::io::ErrorKind::NotFound | std::io::ErrorKind::NotADirectory
            ) =>
        {
            return Ok(None);
        }
        Err(e) => return Err(anyhow::Error::from(e).context(path.display().to_string())),
    };
    let mut buf: LPooled<Vec<u8>> = LPooled::take();
    f.read_to_end(&mut *buf).await.with_context(|| path.display().to_string())?;
    let s = str::from_utf8(&*buf).with_context(|| path.display().to_string())?;
    Ok(Some(ArcStr::from(s)))
}

#[cfg(test)]
mod test {
    #[test]
    fn submodule_base_follows_the_layout() {
        assert_eq!(submodule_base("/d/m.gx"), "/d/m");
        assert_eq!(submodule_base("/d/m.gxi"), "/d/m");
        assert_eq!(submodule_base("/d/m/mod.gx"), "/d/m");
        assert_eq!(submodule_base("/d/m/mod.gxi"), "/d/m");
    }

    use super::*;
    use crate::expr::WrittenAt;
    use arcstr::literal;

    fn vfs(files: &[(&str, &str)]) -> ResolverRef {
        VfsResolver::new(AHashMap::from_iter(files.iter().map(|(p, t)| {
            (Path::from(ArcStr::from(*p)), VfsEntry::from(ArcStr::from(*t)))
        })))
    }

    fn file(path: PathBuf, text: &str) -> Origin {
        Origin { parent: None, source: Source::File(path), text: ArcStr::from(text) }
    }

    async fn resolve_first(ori: Origin, resolvers: &[ResolverRef]) -> Result<Expr> {
        let resolvers: Resolvers = SArc::from(resolvers);
        parser::parse(ori)?[0].resolve_modules(&resolvers).await
    }

    fn body(e: &Expr) -> (&Arc<[Expr]>, &Option<Sig>) {
        match &e.kind {
            ExprKind::Module {
                value: ModuleKind::Resolved { exprs, sig, .. }, ..
            } => (exprs, sig),
            k => panic!("not a resolved module: {k:?}"),
        }
    }

    /// Only the separator is escaped in a module path list.
    #[test]
    fn modpath_unescapes_the_separator() {
        let factories = AHashMap::default();
        let mut libstate = crate::LibState::default();
        let r = parse_modpath(&factories, &mut libstate, r"file:/a\,b,file:C:\gx\lib")
            .unwrap();
        assert_eq!(r.len(), 2);
        assert!(format!("{:?}", r[0]).contains(r#""/a,b""#), "{:?}", r[0]);
        assert!(format!("{:?}", r[1]).contains(r#"C:\\gx\\lib"#), "{:?}", r[1]);
    }

    /// A VFS module's submodules resolve by its module path, never
    /// beside the file that declared it; the statement keeps its end.
    #[tokio::test]
    async fn a_vfs_body_resolves_its_submodules_in_the_vfs() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("x.gx"), "let k = 2").unwrap();
        let lib = vfs(&[("/m.gx", "mod x; let v = x::k"), ("/m/x.gx", "let k = 1")]);
        let main = file(dir.path().join("main.gx"), "mod m;");
        let files = FilesResolver::new(dir.path().to_path_buf(), None);
        let e = resolve_first(main, &[lib, files]).await.unwrap();
        assert_ne!(e.end.0, WrittenAt::NOWHERE.0);
        let (m, _) = body(&e);
        let (x, _) = body(&m[0]);
        assert!(matches!(x[0].ori.source, Source::Internal(_)), "{:?}", x[0].ori.source);
    }

    /// An interface pairs with the implementation beside it only: one
    /// beside the other layout is refused, naming both files.
    #[tokio::test]
    async fn a_vfs_interface_pairs_beside_its_implementation() {
        let lib = vfs(&[("/m/mod.gx", "let x = 1"), ("/m.gxi", "val y: i64")]);
        let main = Origin { text: literal!("mod m;"), ..Origin::default() };
        let e = format!("{:#}", resolve_first(main, &[lib]).await.unwrap_err());
        assert!(e.contains("/m.gxi is no interface: the module is /m/mod.gx"), "{e}");
    }

    /// A program's own `mod` of a package's name is its module, never the
    /// package root the VFS holds for the registration.
    #[tokio::test]
    async fn a_package_root_is_not_a_files_module() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("str.gx"), "let mine = 1").unwrap();
        let mut map = AHashMap::default();
        map.insert(Path::from("/str/mod.gx"), VfsEntry::from(literal!("let len = 2")));
        let lib = VfsResolver::with_packages(map, [literal!("str")]);
        let main = file(dir.path().join("main.gx"), "mod str;");
        let files = FilesResolver::new(dir.path().to_path_buf(), None);
        let e = resolve_first(main, &[lib, files]).await.unwrap();
        let (b, _) = body(&e);
        assert!(matches!(b[0].ori.source, Source::File(_)), "{:?}", b[0].ori.source);
    }

    /// A module file that is there but cannot be read fails the load;
    /// a later resolver's module of the same name does not stand in.
    #[tokio::test]
    async fn an_unreadable_module_fails() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("m.gx")).unwrap();
        let files = FilesResolver::new(dir.path().to_path_buf(), None);
        let lib = vfs(&[("/m.gx", "let x = 1")]);
        let main = Origin { text: literal!("mod m;"), ..Origin::default() };
        assert!(resolve_first(main, &[files, lib]).await.is_err());
    }

    /// A parse error names the file, not the text of every file above.
    #[tokio::test]
    async fn a_bad_interface_names_its_file() {
        let lib = vfs(&[("/m.gx", "let x = 1"), ("/m.gxi", "val x i64")]);
        let main = Origin { text: literal!("mod m;"), ..Origin::default() };
        let e = resolve_first(main, &[lib]).await.unwrap_err();
        let msg = format!("{e:#}");
        assert!(!msg.contains("Origin {"), "{msg}");
    }
}

#[cfg(test)]
mod split_escaped_tests {
    use super::split_escaped;

    fn split(s: &str, n: usize) -> Vec<&str> {
        split_escaped(s, '\\', ',', n).collect()
    }

    #[test]
    fn an_escape_escapes_one_char() {
        assert_eq!(split(r"a\,,b", usize::MAX), [r"a\,", "b"]);
        assert_eq!(split(r"a\,\,b", usize::MAX), [r"a\,\,b"]);
        assert_eq!(split(r"a\\,b", usize::MAX), [r"a\\", "b"]);
        assert_eq!(split("a,b,c", 2), ["a", "b,c"]);
        assert_eq!(split("", usize::MAX), [""]);
    }
}
