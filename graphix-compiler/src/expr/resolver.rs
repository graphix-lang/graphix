use crate::{
    PrintFlag,
    expr::TraitExpr,
    expr::{
        CouldNotResolve, Expr, ExprId, ExprKind, ModPath, ModuleKind, Name, Origin, Sig,
        SigItem, SigKind, Source, StructurePattern, TypeDefExpr, UseItem, parser,
        read_optional, read_to_arcstr, serialize,
    },
    format_with_flags,
};
use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use futures::future::try_join_all;
use indexmap::IndexSet;
use log::info;
use netidx_core::path::Path;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{hash::Hash, path::PathBuf, pin::Pin, str::FromStr};
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
pub type ResolverRef = std::sync::Arc<dyn ModuleResolver>;

/// Resolvers threaded through module resolution, tried in order.
pub type Resolvers = std::sync::Arc<[ResolverRef]>;

/// Constructs a resolver from the payload of a `scheme:` entry in
/// GRAPHIX_MODPATH. Registered by the embedder per scheme; `file` is built
/// in. Receives the context's [`LibState`] so a package resolver can share
/// state with its package's builtins.
pub type ResolverFactory = std::sync::Arc<
    dyn Fn(&mut crate::LibState, &str) -> Result<ResolverRef> + Send + Sync,
>;

/// In-memory module store — the stdlib packages and test sources.
#[derive(Debug, Clone)]
pub struct VfsResolver(pub AHashMap<Path, VfsEntry>);

impl VfsResolver {
    pub fn new(vfs: AHashMap<Path, VfsEntry>) -> ResolverRef {
        std::sync::Arc::new(VfsResolver(vfs))
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
        Box::pin(async move { resolve_from_vfs(scope, parent, name, &self.0) })
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
        std::sync::Arc::new(FilesResolver { base, overrides })
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

/// Parse a GRAPHIX_MODPATH-style list (`scheme:payload,{...}`) into
/// resolvers. `file:` is built in; other schemes look up `factories`.
pub fn parse_modpath(
    factories: &AHashMap<ArcStr, ResolverFactory>,
    libstate: &mut crate::LibState,
    s: &str,
) -> Result<Vec<ResolverRef>> {
    let mut res: Vec<ResolverRef> = vec![];
    for l in escaping::split(s, '\\', ',') {
        let l = l.trim();
        if let Some(s) = l.strip_prefix("file:") {
            let base = PathBuf::from_str(s)?;
            res.push(std::sync::Arc::new(FilesResolver { base, overrides: None }));
        } else {
            match l.split_once(':').and_then(|(scheme, rest)| {
                factories.get(scheme).map(|f| f(libstate, rest))
            }) {
                Some(r) => res.push(r?),
                None => {
                    bail!("no resolver for {l}: expected file: or a registered scheme")
                }
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

/// The result of one resolver's attempt — the [`ModuleResolver`]
/// trait's currency.
pub enum Resolution {
    Resolved {
        interface: Option<Origin>,
        implementation: Origin,
        // Packed pre-parsed AST for impl/interface, when the VFS entry
        // carried one.
        impl_packed: Option<Bytes>,
        intf_packed: Option<Bytes>,
    },
    TryNextMethod,
}

impl Resolution {
    /// A parse-always resolution (no packed AST).
    pub fn parsed(interface: Option<Origin>, implementation: Origin) -> Self {
        Resolution::Resolved {
            interface,
            implementation,
            impl_packed: None,
            intf_packed: None,
        }
    }
}

fn resolve_from_vfs(
    scope: &ModPath,
    parent: &Arc<Origin>,
    name: &Path,
    vfs: &AHashMap<Path, VfsEntry>,
) -> Resolution {
    macro_rules! ori {
        ($e:expr) => {
            Origin {
                parent: Some(parent.clone()),
                source: Source::Internal(name.clone().into()),
                text: $e.source.clone(),
            }
        };
    }
    let scoped_intf = scope.append(&format_compact!("{name}.gxi"));
    let scoped_impl = scope.append(&format_compact!("{name}.gx"));
    let (implementation, impl_packed) = match vfs.get(&scoped_impl) {
        Some(e) => (ori!(e), e.packed.clone()),
        None => {
            let mod_impl = scope.append(&format_compact!("{name}/mod.gx"));
            match vfs.get(&mod_impl) {
                Some(e) => (ori!(e), e.packed.clone()),
                None => return Resolution::TryNextMethod,
            }
        }
    };
    let (interface, intf_packed) = match vfs.get(&scoped_intf).or_else(|| {
        let mod_intf = scope.append(&format_compact!("{name}/mod.gxi"));
        vfs.get(&mod_intf)
    }) {
        Some(e) => (Some(ori!(e)), e.packed.clone()),
        None => (None, None),
    };
    Resolution::Resolved { interface, implementation, impl_packed, intf_packed }
}

async fn resolve_from_files(
    parent: &Arc<Origin>,
    name: &Path,
    base: &PathBuf,
    overrides: Option<&BufferOverrides>,
    errors: &mut Vec<anyhow::Error>,
) -> Resolution {
    macro_rules! ori {
        ($s:expr, $path:expr) => {
            Origin {
                parent: Some(parent.clone()),
                source: Source::File($path),
                text: ArcStr::from($s),
            }
        };
    }
    async fn read(
        overrides: Option<&BufferOverrides>,
        path: &PathBuf,
    ) -> Result<Option<ArcStr>> {
        match overrides.and_then(|o| o.lock().get(path).cloned()) {
            Some(s) => Ok(Some(s)),
            None => read_optional(path).await,
        }
    }
    let mut impl_path = base.clone();
    for part in Path::parts(&name) {
        impl_path.push(part);
    }
    impl_path.set_extension("gx");
    let mut intf_path = impl_path.with_extension("gxi");
    let implementation = match read(overrides, &impl_path).await {
        Ok(Some(s)) => ori!(s, impl_path),
        Ok(None) => {
            impl_path.set_extension("");
            impl_path.push("mod.gx");
            intf_path.set_extension("");
            intf_path.push("mod.gxi");
            match read(overrides, &impl_path).await {
                Ok(Some(s)) => ori!(s, impl_path.clone()),
                Ok(None) => {
                    errors.push(anyhow::anyhow!("{}: no such file", impl_path.display()));
                    return Resolution::TryNextMethod;
                }
                Err(e) => {
                    errors.push(e);
                    return Resolution::TryNextMethod;
                }
            }
        }
        Err(e) => {
            errors.push(e);
            return Resolution::TryNextMethod;
        }
    };
    let interface = match read(overrides, &intf_path).await {
        Ok(Some(s)) => Some(ori!(s, intf_path)),
        Ok(None) => None,
        Err(e) => {
            errors.push(e);
            return Resolution::TryNextMethod;
        }
    };
    Resolution::Resolved {
        interface,
        implementation,
        impl_packed: None,
        intf_packed: None,
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
    #[derive(Clone, Copy)]
    struct Item<'a> {
        kind: ItemKind<'a>,
        pos: SourcePosition,
        ori: Option<&'a Arc<Origin>>,
    }
    #[derive(Clone, Copy)]
    enum ItemKind<'a> {
        Module(&'a Name),
        TypeDef(&'a TypeDefExpr),
        Trait(&'a Arc<TraitExpr>),
        Use(bool, &'a Arc<[UseItem]>),
    }
    impl<'a> PartialEq for Item<'a> {
        fn eq(&self, other: &Self) -> bool {
            match (&self.kind, &other.kind) {
                (ItemKind::Module(a), ItemKind::Module(b)) => a == b,
                (ItemKind::TypeDef(a), ItemKind::TypeDef(b)) => a.name == b.name,
                (ItemKind::Trait(a), ItemKind::Trait(b)) => a.name == b.name,
                (ItemKind::Use(ra, a), ItemKind::Use(rb, b)) => ra == rb && a == b,
                (_, _) => false,
            }
        }
    }
    impl<'a> Eq for Item<'a> {}
    impl<'a> Hash for Item<'a> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            match &self.kind {
                ItemKind::Module(m) => {
                    0u8.hash(state);
                    m.hash(state);
                }
                ItemKind::TypeDef(td) => {
                    1u8.hash(state);
                    td.name.hash(state);
                }
                ItemKind::Trait(t) => {
                    3u8.hash(state);
                    t.name.hash(state);
                }
                ItemKind::Use(r, m) => {
                    2u8.hash(state);
                    r.hash(state);
                    m.hash(state);
                }
            }
        }
    }
    impl<'a> Item<'a> {
        fn synth(self, ori: &Arc<Origin>) -> Expr {
            let kind = match self.kind {
                ItemKind::Module(name) => ExprKind::Module {
                    name: name.clone(),
                    value: ModuleKind::Unresolved { from_interface: true },
                },
                ItemKind::TypeDef(td) => ExprKind::TypeDef(td.clone()),
                ItemKind::Trait(t) => ExprKind::Trait(Arc::clone(t)),
                ItemKind::Use(reexport, m) => {
                    ExprKind::Use { reexport, names: Arc::clone(m) }
                }
            };
            let ori = self.ori.unwrap_or(ori).clone();
            Expr {
                id: ExprId::new(),
                ori,
                pos: self.pos,
                kind,
                dec: None,
                str_form: Default::default(),
                end: Default::default(),
            }
        }
    }
    let mut in_sig: LPooled<IndexSet<Item>> = LPooled::take();
    let mut after_bind: LPooled<AHashMap<&str, Item>> = LPooled::take();
    let mut after_td: LPooled<AHashMap<&str, Item>> = LPooled::take();
    let mut after_trait: LPooled<AHashMap<&str, Item>> = LPooled::take();
    let mut after_mod: LPooled<AHashMap<&str, Item>> = LPooled::take();
    let mut after_use: LPooled<AHashMap<&UseItem, Item>> = LPooled::take();
    let mut first: Option<Item> = None;
    let mut last: Option<&SigItem> = None;
    macro_rules! push {
        ($kind:ident, $name:expr, $si:expr) => {{
            let name = Item {
                kind: ItemKind::$kind($name),
                pos: $si.pos,
                ori: $si.ori.as_ref(),
            };
            in_sig.insert(name);
            match last {
                None => first = Some(name),
                Some(si) => {
                    match &si.kind {
                        SigKind::Bind(v) => after_bind.insert(v.name.as_str(), name),
                        SigKind::Module(m) => after_mod.insert(m.as_str(), name),
                        SigKind::TypeDef(td) => after_td.insert(td.name.as_str(), name),
                        SigKind::Trait(t) => after_trait.insert(t.name.as_str(), name),
                        SigKind::Impl(_) => None,
                        SigKind::Use { names: n, .. } => {
                            n.iter().map(|p| after_use.insert(p, name)).last().flatten()
                        }
                    };
                }
            }
        }};
    }
    for si in &*sig.items {
        match &si.kind {
            SigKind::Module(name) => push!(Module, name, si),
            SigKind::TypeDef(td) => push!(TypeDef, td, si),
            SigKind::Trait(t) => push!(Trait, t, si),
            SigKind::Use { reexport, names } => {
                let name = Item {
                    kind: ItemKind::Use(*reexport, names),
                    pos: si.pos,
                    ori: si.ori.as_ref(),
                };
                in_sig.insert(name);
                match last {
                    None => first = Some(name),
                    Some(psi) => {
                        match &psi.kind {
                            SigKind::Bind(v) => after_bind.insert(v.name.as_str(), name),
                            SigKind::Module(m) => after_mod.insert(m.as_str(), name),
                            SigKind::TypeDef(td) => {
                                after_td.insert(td.name.as_str(), name)
                            }
                            SigKind::Trait(t) => {
                                after_trait.insert(t.name.as_str(), name)
                            }
                            SigKind::Impl(_) => None,
                            SigKind::Use { names: n, .. } => n
                                .iter()
                                .map(|p| after_use.insert(p, name))
                                .last()
                                .flatten(),
                        };
                    }
                }
            }
            SigKind::Bind(_) | SigKind::Impl(_) => (),
        }
        // An `impl` declaration is never spliced, so it anchors nothing;
        // the next interface-only item keeps the last spliceable anchor.
        if !matches!(si.kind, SigKind::Impl(_)) {
            last = Some(si);
        }
    }
    for e in &*exprs {
        if let ExprKind::Module { name, .. } = &e.kind {
            let probe = Item {
                kind: ItemKind::Module(name),
                pos: SourcePosition::default(),
                ori: None,
            };
            in_sig.shift_remove(&probe);
        }
        if let ExprKind::TypeDef(td) = &e.kind {
            let probe = Item {
                kind: ItemKind::TypeDef(td),
                pos: SourcePosition::default(),
                ori: None,
            };
            in_sig.shift_remove(&probe);
        }
        if let ExprKind::Trait(t) = &e.kind {
            let probe = Item {
                kind: ItemKind::Trait(t),
                pos: SourcePosition::default(),
                ori: None,
            };
            in_sig.shift_remove(&probe);
        }
        if let ExprKind::Use { reexport, names } = &e.kind {
            let probe = Item {
                kind: ItemKind::Use(*reexport, names),
                pos: SourcePosition::default(),
                ori: None,
            };
            in_sig.shift_remove(&probe);
        }
    }
    if in_sig.is_empty() {
        drop(in_sig);
        drop(after_bind);
        drop(after_td);
        drop(after_trait);
        drop(after_mod);
        drop(after_use);
        return exprs;
    }
    let mut res: LPooled<Vec<Expr>> = LPooled::take();
    if let Some(name) = first.take() {
        if in_sig.shift_remove(&name) {
            res.push(name.synth(ori));
        }
    }
    let mut iter = exprs.iter();
    loop {
        match res.last().map(|e| &e.kind) {
            Some(ExprKind::Bind(v)) => match &v.pattern {
                StructurePattern::Bind(n) => {
                    if let Some(name) = after_bind.remove(n.as_str())
                        && in_sig.shift_remove(&name)
                    {
                        res.push(name.synth(ori));
                        continue;
                    }
                }
                _ => (),
            },
            Some(ExprKind::TypeDef(td)) => {
                if let Some(name) = after_td.remove(td.name.as_str())
                    && in_sig.shift_remove(&name)
                {
                    res.push(name.synth(ori));
                    continue;
                }
            }
            Some(ExprKind::Trait(t)) => {
                if let Some(name) = after_trait.remove(t.name.as_str())
                    && in_sig.shift_remove(&name)
                {
                    res.push(name.synth(ori));
                    continue;
                }
            }
            Some(ExprKind::Module { name, .. }) => {
                if let Some(name) = after_mod.remove(name.as_str())
                    && in_sig.shift_remove(&name)
                {
                    res.push(name.synth(ori));
                    continue;
                }
            }
            Some(ExprKind::Use { names, .. }) => {
                if let Some(name) = names.iter().find_map(|n| after_use.remove(n))
                    && in_sig.shift_remove(&name)
                {
                    res.push(name.synth(ori));
                    continue;
                }
            }
            _ => (),
        };
        match iter.next() {
            None => break,
            Some(e) => res.push(e.clone()),
        }
    }
    for name in in_sig.drain(..) {
        res.push(name.synth(ori));
    }
    Arc::from_iter(res.drain(..))
}

/// Parse (or unpack) a module's implementation and interface, with the
/// interface's modules, types, traits and uses spliced into the body.
async fn parse_module(
    interface: &Option<Origin>,
    implementation: &Origin,
    impl_packed: Option<Bytes>,
    intf_packed: Option<Bytes>,
) -> Result<(Arc<[Expr]>, Option<Sig>)> {
    // Decode and parse both run on a blocking thread; `unpack_module`
    // sets its per-module thread-locals on the thread that decodes.
    let exprs = {
        let ori = implementation.clone();
        match impl_packed.filter(|_| !packed_ast_disabled()) {
            Some(bytes) => task::spawn_blocking(move || {
                serialize::unpack_module(&bytes, Arc::new(ori))
            }),
            None => task::spawn_blocking(move || parser::parse(ori)),
        }
    };
    let sig = match interface {
        None => None,
        Some(ori) => {
            let ori = Arc::new(ori.clone());
            let unit = ori.clone();
            let sig = match intf_packed.filter(|_| !packed_ast_disabled()) {
                Some(bytes) => {
                    task::spawn_blocking(move || serialize::unpack_sig(&bytes, unit))
                }
                None => task::spawn_blocking(move || parser::parse_sig((*unit).clone())),
            }
            .await?
            .with_context(|| format!("parsing file {interface:?}"))?;
            Some((sig, ori))
        }
    };
    let exprs =
        exprs.await?.with_context(|| format!("parsing file {implementation:?}"))?;
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
            None => tokio::fs::canonicalize(file).await?,
        };
        let text = match buffer(&file) {
            Some(text) => text,
            None => read_to_arcstr(&file).await?,
        };
        let text = match text.find('\n') {
            Some(i) if text.starts_with("#!") => ArcStr::from(&text[i..]),
            Some(_) | None => text,
        };
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
        let ori = Origin { parent: None, source: Source::File(file), text };
        let (exprs, sig) = parse_module(&interface, &ori, None, None).await?;
        Ok(Self { ori, exprs, sig })
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

async fn resolve(
    scope: ModPath,
    prepend: Option<ResolverRef>,
    resolvers: Resolvers,
    id: ExprId,
    parent: Arc<Origin>,
    pos: SourcePosition,
    module: Name,
    from_interface: bool,
) -> Result<Expr> {
    macro_rules! check {
        ($res:expr) => {
            match $res {
                Resolution::TryNextMethod => continue,
                Resolution::Resolved {
                    interface,
                    implementation,
                    impl_packed,
                    intf_packed,
                } => (interface, implementation, impl_packed, intf_packed),
            }
        };
    }
    let ts = Instant::now();
    let name = Path::from(module.name.clone());
    let mut errors: LPooled<Vec<anyhow::Error>> = LPooled::take();
    for r in prepend.iter().map(|r| &**r).chain(resolvers.iter().map(|r| &**r)) {
        let (interface, implementation, impl_packed, intf_packed) =
            check!(r.resolve(&scope, &parent, &name, &mut errors).await);
        let (exprs, sig) =
            parse_module(&interface, &implementation, impl_packed, intf_packed).await?;
        let value = ModuleKind::Resolved { exprs, sig, from_interface };
        let kind = ExprKind::Module { name: module, value };
        format_with_flags(PrintFlag::NoSource | PrintFlag::NoParents, || {
            info!(
                "load and parse {implementation:?} and {interface:?} {:?}",
                ts.elapsed()
            )
        });
        let _ = implementation; // implementation lives on the inner exprs
        return Ok(Expr {
            id,
            ori: parent,
            pos,
            kind,
            dec: None,
            str_form: Default::default(),
            end: Default::default(),
        });
    }
    let mut msg = format_compact!("module {name} could not be found");
    use std::fmt::Write as _;
    let mut first = true;
    for e in errors.iter() {
        let _ = write!(&mut msg, "{}{e}", if first { ": " } else { "; " });
        first = false;
    }
    bail!("{msg}")
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
        let e = self.resolve_modules_int(scope, &None, &None, resolvers).await?;
        Ok(e.unwrap_or_else(|| self.clone()))
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
        macro_rules! expr {
            ($kind:expr) => {
                Ok(Some(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: $kind,
                    dec: self.dec.clone(),
                    str_form: self.str_form,
                    end: self.end,
                }))
            };
        }
        match &self.kind {
            ExprKind::Module {
                value: ModuleKind::Unresolved { from_interface },
                name,
            } => {
                let (id, pos, prepend, resolvers, from_interface) = (
                    self.id,
                    self.pos,
                    prepend.clone(),
                    std::sync::Arc::clone(resolvers),
                    *from_interface,
                );
                Box::pin(async move {
                    let e = resolve(
                        scope.clone(),
                        prepend.clone(),
                        resolvers.clone(),
                        id,
                        self.ori.clone(),
                        pos,
                        name.clone(),
                        from_interface,
                    )
                    .await
                    .with_context(|| CouldNotResolve(name.name.clone()))?;
                    let scope = ModPath(scope.append(&**name));
                    let r = e
                        .resolve_modules_int(&scope, &prepend, chain, &resolvers)
                        .await?;
                    Ok(Some(r.unwrap_or(e)))
                })
            }
            ExprKind::Module {
                value: ModuleKind::Resolved { exprs, sig, from_interface },
                name,
            } => Box::pin(async move {
                let source = exprs.iter().find_map(|e| match &e.ori.source {
                    Source::Unspecified => None,
                    s => Some(s),
                });
                // a file or a netidx path is one place; an `Internal`
                // source is a VFS module's bare name, and a VFS lookup is
                // scoped by the module path, so it cannot come round again
                let chain = match source {
                    Some(s @ (Source::File(_) | Source::Netidx(_))) => {
                        Some(LoadChain::push(chain, s, name)?)
                    }
                    _ => chain.clone(),
                };
                let chain = &chain;
                // Sub-modules resolve relative to the implementation file's
                // directory (`<dir>/foo/` for `foo.gx`, `<dir>/` for
                // `foo/mod.gx`); the body's exprs carry that file as their ori.
                let impl_path: Option<&std::path::Path> = match source {
                    Some(Source::File(p)) => Some(p.as_path()),
                    _ => None,
                };
                let prepend = match impl_path {
                    Some(p) => {
                        let parent = match p.parent() {
                            Some(par) => par,
                            None => return Ok(None),
                        };
                        let dir = match p.file_stem().and_then(|s| s.to_str()) {
                            Some("mod") => parent.to_path_buf(),
                            Some(stem) => parent.join(stem),
                            None => parent.to_path_buf(),
                        };
                        let overrides = resolvers.iter().find_map(|m| m.overrides());
                        Some(std::sync::Arc::new(FilesResolver { base: dir, overrides })
                            as ResolverRef)
                    }
                    None => match &self.ori.source {
                        Source::Unspecified | Source::Internal(_) => None,
                        Source::File(p) => p.parent().map(|p| {
                            let overrides = resolvers.iter().find_map(|m| m.overrides());
                            std::sync::Arc::new(FilesResolver {
                                base: p.into(),
                                overrides,
                            }) as ResolverRef
                        }),
                        source => resolvers.iter().find_map(|m| m.for_source(source)),
                    },
                };
                let resolved = try_join_all(exprs.iter().map(|e| async {
                    e.resolve_modules_int(&scope, &prepend, chain, resolvers).await
                }))
                .await?;
                if resolved.iter().all(|r| r.is_none()) {
                    return Ok(None);
                }
                let exprs = resolved
                    .into_iter()
                    .zip(exprs.iter())
                    .map(|(r, e)| r.unwrap_or_else(|| e.clone()));
                expr!(ExprKind::Module {
                    value: ModuleKind::Resolved {
                        exprs: Arc::from_iter(exprs),
                        sig: sig.clone(),
                        from_interface: *from_interface,
                    },
                    name: name.clone(),
                })
            }),
            _ => Box::pin(async move {
                let mut children: SmallVec<[&Expr; 4]> = SmallVec::new();
                self.for_each_child(&mut |c| children.push(c));
                let resolved =
                    try_join_all(children.iter().map(|c| {
                        c.resolve_modules_int(scope, prepend, chain, resolvers)
                    }))
                    .await?;
                if resolved.iter().all(|r| r.is_none()) {
                    return Ok(None);
                }
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
                use std::fmt::Write as _;
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
