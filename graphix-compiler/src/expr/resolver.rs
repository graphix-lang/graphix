use crate::{
    PrintFlag,
    expr::{
        ApplyExpr, BindExpr, CouldNotResolve, Expr, ExprId, ExprKind, LambdaExpr,
        ModPath, ModuleKind, Origin, Pattern, SelectExpr, Sig, SigItem, SigKind, Source,
        StructExpr, StructWithExpr, StructurePattern, TryCatchExpr, TypeDefExpr, parser,
        read_to_arcstr, serialize,
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
use netidx_core::{path::Path, utils::Either};
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{hash::Hash, path::PathBuf, pin::Pin, str::FromStr};
use tokio::{task, time::Instant, try_join};
use triomphe::Arc;

pub type BufferOverrides = Arc<Mutex<AHashMap<PathBuf, ArcStr>>>;

/// A VFS module entry: its `.gx`/`.gxi` source text, plus optionally the
/// pre-parsed AST as a packed blob (see [`super::serialize`]). When `packed`
/// is present the resolver decodes it instead of re-parsing `source` — the
/// startup win for stdlib packages. `source` is always kept (for `Origin`
/// reconstruction and error-message snippets). Loose-source entries (tests,
/// `eval`, the REPL) leave `packed` `None` and parse as before.
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
/// that owns the transport (e.g. the netidx loader in
/// graphix-package-sys) and are threaded in by the embedder — the core
/// has no knowledge of any network.
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

    /// Derive a resolver for the SUBMODULES of a module whose
    /// implementation came from `source`, when this resolver
    /// understands that source kind (e.g. the netidx loader answers
    /// `Source::Netidx(p)` with a clone based at `p`). The compiler
    /// handles `Source::File` itself; everything else is offered to
    /// the resolver list in order.
    fn for_source(&self, _source: &Source) -> Option<ResolverRef> {
        None
    }

    /// Fetch a single top-level source this resolver's transport
    /// understands (the runtime's `Source::Netidx` script-file load).
    /// `None` (the default) means "not my transport".
    fn fetch_source<'a>(
        &'a self,
        _source: &'a Source,
    ) -> Option<Pin<Box<dyn Future<Output = Result<ArcStr>> + Send + Sync + 'a>>> {
        None
    }

    /// The LSP buffer-overrides this resolver carries, if any — used
    /// when the compiler derives directory-based prepend resolvers for
    /// relative includes.
    fn overrides(&self) -> Option<BufferOverrides> {
        None
    }
}

/// A shared resolver handle. `std::sync::Arc`: resolvers are
/// cold-path configuration objects and triomphe cannot unsize to
/// trait objects.
pub type ResolverRef = std::sync::Arc<dyn ModuleResolver>;

/// Resolvers threaded through module resolution, tried in order.
pub type Resolvers = std::sync::Arc<[ResolverRef]>;

/// Constructs a resolver from the payload of a `scheme:` entry in
/// GRAPHIX_MODPATH (the part after the colon). Registered by the
/// embedder per scheme — the shell registers the sys package's
/// `netidx` factory; `file` is built in. The factory receives the
/// context's [`LibState`] so package resolvers can share state (e.g.
/// config and connection handles) with their package's builtins.
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
/// even when a packed AST is available — for differential testing (packed vs
/// parsed must compile identically) and as an escape hatch.
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
        // Packed pre-parsed AST for impl/interface, when the VFS entry carried
        // one. `None` for loose source and for the file/netidx resolvers (which
        // always parse). The decode happens at the parse splice in `resolve`.
        impl_packed: Option<Bytes>,
        intf_packed: Option<Bytes>,
    },
    TryNextMethod,
}

impl Resolution {
    /// Build a parse-always resolution (no packed AST) — the shape
    /// every non-VFS loader returns.
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
            // try {name}/mod.gx fallback (consistent with file resolver)
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
    async fn read(overrides: Option<&BufferOverrides>, path: &PathBuf) -> Result<ArcStr> {
        match overrides.and_then(|o| o.lock().get(path).cloned()) {
            Some(s) => Ok(s),
            None => read_to_arcstr(path).await,
        }
    }
    let mut impl_path = base.clone();
    for part in Path::parts(&name) {
        impl_path.push(part);
    }
    impl_path.set_extension("gx");
    let mut intf_path = impl_path.with_extension("gxi");
    let implementation = match read(overrides, &impl_path).await {
        Ok(s) => ori!(s, impl_path),
        Err(_) => {
            impl_path.set_extension("");
            impl_path.push("mod.gx");
            intf_path.set_extension("");
            intf_path.push("mod.gxi");
            match read(overrides, &impl_path).await {
                Ok(s) => ori!(s, impl_path.clone()),
                Err(e) => {
                    errors.push(anyhow::Error::from(e));
                    return Resolution::TryNextMethod;
                }
            }
        }
    };
    let interface = match read(overrides, &intf_path).await {
        Ok(s) => Some(ori!(s, intf_path)),
        Err(_) => None,
    };
    // file/netidx resolvers always parse — never packed.
    Resolution::Resolved {
        interface,
        implementation,
        impl_packed: None,
        intf_packed: None,
    }
}

// add modules that are only mentioned in the interface to the implementation
// keep their relative location and order intact
pub fn add_interface_modules(exprs: Arc<[Expr]>, sig: &Sig) -> Arc<[Expr]> {
    #[derive(Clone, Copy)]
    struct Item<'a> {
        kind: ItemKind<'a>,
        pos: SourcePosition,
        ori: Option<&'a Arc<Origin>>,
    }
    #[derive(Clone, Copy)]
    enum ItemKind<'a> {
        Module(&'a ArcStr),
        TypeDef(&'a TypeDefExpr),
        Use(&'a ModPath),
    }
    impl<'a> PartialEq for Item<'a> {
        fn eq(&self, other: &Self) -> bool {
            match (&self.kind, &other.kind) {
                (ItemKind::Module(a), ItemKind::Module(b)) => a == b,
                (ItemKind::TypeDef(a), ItemKind::TypeDef(b)) => a.name == b.name,
                (ItemKind::Use(a), ItemKind::Use(b)) => a == b,
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
                ItemKind::Use(m) => {
                    2u8.hash(state);
                    m.hash(state);
                }
            }
        }
    }
    impl<'a> Item<'a> {
        fn synth(self) -> Expr {
            let kind = match self.kind {
                ItemKind::Module(name) => ExprKind::Module {
                    name: name.clone(),
                    value: ModuleKind::Unresolved { from_interface: true },
                },
                ItemKind::TypeDef(td) => ExprKind::TypeDef(td.clone()),
                ItemKind::Use(m) => ExprKind::Use { name: m.clone() },
            };
            let ori = self.ori.cloned().unwrap_or_else(crate::expr::get_origin);
            Expr { id: ExprId::new(), ori, pos: self.pos, kind, dec: None }
        }
    }
    let mut in_sig: LPooled<IndexSet<Item>> = LPooled::take();
    let mut after_bind: LPooled<AHashMap<&ArcStr, Item>> = LPooled::take();
    let mut after_td: LPooled<AHashMap<&ArcStr, Item>> = LPooled::take();
    let mut after_mod: LPooled<AHashMap<&ArcStr, Item>> = LPooled::take();
    let mut after_use: LPooled<AHashMap<&ModPath, Item>> = LPooled::take();
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
                        SigKind::Bind(v) => after_bind.insert(&v.name, name),
                        SigKind::Module(m) => after_mod.insert(m, name),
                        SigKind::TypeDef(td) => after_td.insert(&td.name, name),
                        SigKind::Use(n) => after_use.insert(n, name),
                    };
                }
            }
        }};
    }
    for si in &*sig.items {
        match &si.kind {
            SigKind::Module(name) => push!(Module, name, si),
            SigKind::TypeDef(td) => push!(TypeDef, td, si),
            SigKind::Use(m) => push!(Use, m, si),
            SigKind::Bind(_) => (),
        }
        last = Some(si);
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
        if let ExprKind::Use { name } = &e.kind {
            let probe = Item {
                kind: ItemKind::Use(name),
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
        drop(after_mod);
        drop(after_use);
        return exprs;
    }
    let mut res: LPooled<Vec<Expr>> = LPooled::take();
    if let Some(name) = first.take() {
        if in_sig.shift_remove(&name) {
            res.push(name.synth());
        }
    }
    let mut iter = exprs.iter();
    loop {
        match res.last().map(|e| &e.kind) {
            Some(ExprKind::Bind(v)) => match &v.pattern {
                StructurePattern::Bind(n) => {
                    if let Some(name) = after_bind.remove(n)
                        && in_sig.shift_remove(&name)
                    {
                        res.push(name.synth());
                        continue;
                    }
                }
                _ => (),
            },
            Some(ExprKind::TypeDef(td)) => {
                if let Some(name) = after_td.remove(&td.name)
                    && in_sig.shift_remove(&name)
                {
                    res.push(name.synth());
                    continue;
                }
            }
            Some(ExprKind::Module { name, .. }) => {
                if let Some(name) = after_mod.remove(name)
                    && in_sig.shift_remove(&name)
                {
                    res.push(name.synth());
                    continue;
                }
            }
            Some(ExprKind::Use { name }) => {
                if let Some(name) = after_use.remove(name)
                    && in_sig.shift_remove(&name)
                {
                    res.push(name.synth());
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
        res.push(name.synth());
    }
    Arc::from_iter(res.drain(..))
}

async fn resolve(
    scope: ModPath,
    prepend: Option<ResolverRef>,
    resolvers: Resolvers,
    id: ExprId,
    parent: Arc<Origin>,
    pos: SourcePosition,
    name: ArcStr,
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
    let name = Path::from(name);
    let mut errors: LPooled<Vec<anyhow::Error>> = LPooled::take();
    for r in prepend.iter().map(|r| &**r).chain(resolvers.iter().map(|r| &**r)) {
        let (interface, implementation, impl_packed, intf_packed) =
            check!(r.resolve(&scope, &parent, &name, &mut errors).await);
        // Decode the pre-parsed AST if the VFS entry shipped one, else parse
        // the source. Both run on a blocking thread (decode and parse are CPU
        // work, and `serialize::unpack_module` sets the per-module `Origin` +
        // AbstractId-remap thread-locals on the thread that decodes — mirroring
        // how `parser::parse` sets `set_origin` inside its own closure).
        let exprs = {
            let ori = implementation.clone();
            match impl_packed.filter(|_| !packed_ast_disabled()) {
                Some(bytes) => task::spawn_blocking(move || {
                    serialize::unpack_module(&bytes, Arc::new(ori))
                }),
                None => task::spawn_blocking(move || parser::parse(ori)),
            }
        };
        let sig = match &interface {
            None => None,
            Some(ori) => {
                let ori = ori.clone();
                let sig = match intf_packed.filter(|_| !packed_ast_disabled()) {
                    Some(bytes) => task::spawn_blocking(move || {
                        serialize::unpack_sig(&bytes, Arc::new(ori))
                    }),
                    None => task::spawn_blocking(move || parser::parse_sig(ori)),
                }
                .await?
                .with_context(|| format!("parsing file {interface:?}"))?;
                Some(sig)
            }
        };
        let exprs =
            exprs.await?.with_context(|| format!("parsing file {implementation:?}"))?;
        let exprs = match &sig {
            Some(sig) => add_interface_modules(exprs, &sig),
            None => exprs,
        };
        let value = ModuleKind::Resolved { exprs, sig, from_interface };
        let kind = ExprKind::Module { name: name.clone().into(), value };
        format_with_flags(PrintFlag::NoSource | PrintFlag::NoParents, || {
            info!(
                "load and parse {implementation:?} and {interface:?} {:?}",
                ts.elapsed()
            )
        });
        let _ = implementation; // implementation lives on the inner exprs
        return Ok(Expr { id, ori: parent, pos, kind, dec: None });
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
    pub fn has_unresolved_modules(&self) -> bool {
        self.fold(false, &mut |acc, e| {
            acc || match &e.kind {
                ExprKind::Module { value: ModuleKind::Unresolved { .. }, .. } => true,
                _ => false,
            }
        })
    }

    /// Resolve external modules referenced in the expression using
    /// the resolvers list. Each resolver will be tried in order,
    /// until one succeeds. If no resolver succeeds then an error will
    /// be returned.
    pub async fn resolve_modules<'a>(&'a self, resolvers: &'a Resolvers) -> Result<Expr> {
        self.resolve_modules_in_scope(&ModPath::root(), resolvers).await
    }

    /// Like `resolve_modules` but starts at a non-root scope. Used by
    /// the runtime when typechecking a graphix package crate's source
    /// under that crate's namespace.
    pub async fn resolve_modules_in_scope<'a>(
        &'a self,
        scope: &'a ModPath,
        resolvers: &'a Resolvers,
    ) -> Result<Expr> {
        self.resolve_modules_int(scope, &None, resolvers).await
    }

    async fn resolve_modules_int<'a>(
        &'a self,
        scope: &ModPath,
        prepend: &'a Option<ResolverRef>,
        resolvers: &'a Resolvers,
    ) -> Result<Expr> {
        if self.has_unresolved_modules() {
            self.resolve_modules_inner(scope, prepend, resolvers).await
        } else {
            Ok(self.clone())
        }
    }

    fn resolve_modules_inner<'a>(
        &'a self,
        scope: &'a ModPath,
        prepend: &'a Option<ResolverRef>,
        resolvers: &'a Resolvers,
    ) -> Pin<Box<dyn Future<Output = Result<Expr>> + Send + Sync + 'a>> {
        macro_rules! subexprs {
            ($args:expr) => {{
                try_join_all($args.iter().map(|e| async {
                    e.resolve_modules_int(scope, prepend, resolvers).await
                }))
                .await?
            }};
        }
        macro_rules! subtuples {
            ($args:expr) => {{
                try_join_all($args.iter().map(|(k, e)| async {
                    Ok::<_, anyhow::Error>((
                        k.clone(),
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    ))
                }))
                .await?
            }};
        }
        macro_rules! expr {
            ($kind:expr) => {
                Ok(Expr {
                    id: self.id,
                    ori: self.ori.clone(),
                    pos: self.pos,
                    kind: $kind,
                    // Preserve decorations (comments/attrs) through module
                    // resolution so they survive into the resolved tree.
                    dec: self.dec.clone(),
                })
            };
        }
        macro_rules! only_args {
            ($kind:ident, $args:expr) => {
                Box::pin(async move {
                    let args = Arc::from(subexprs!($args));
                    expr!(ExprKind::$kind { args })
                })
            };
        }
        macro_rules! bin_op {
            ($kind:ident, $lhs:expr, $rhs:expr) => {
                Box::pin(async move {
                    let (lhs, rhs) = try_join!(
                        $lhs.resolve_modules_int(scope, prepend, resolvers),
                        $rhs.resolve_modules_int(scope, prepend, resolvers)
                    )?;
                    expr!(ExprKind::$kind { lhs: Arc::from(lhs), rhs: Arc::from(rhs) })
                })
            };
        }
        if !self.has_unresolved_modules() {
            return Box::pin(async { Ok(self.clone()) });
        }
        match self.kind.clone() {
            ExprKind::Constant(_)
            | ExprKind::NoOp
            | ExprKind::Use { .. }
            | ExprKind::Ref { .. }
            | ExprKind::StructRef { .. }
            | ExprKind::TupleRef { .. }
            | ExprKind::TypeDef { .. } => Box::pin(async move { Ok(self.clone()) }),
            ExprKind::Module {
                value: ModuleKind::Unresolved { from_interface },
                name,
            } => {
                let (id, pos, prepend, resolvers) = (
                    self.id,
                    self.pos,
                    prepend.clone(),
                    std::sync::Arc::clone(resolvers),
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
                    .with_context(|| CouldNotResolve(name.clone()))?;
                    let scope = ModPath(scope.append(&*name));
                    e.resolve_modules_int(&scope, &prepend, &resolvers).await
                })
            }
            ExprKind::Module {
                value: ModuleKind::Resolved { exprs, sig, from_interface },
                name,
            } => Box::pin(async move {
                // The prepend resolver supplies a base for resolving
                // *this module's* sub-modules. The right base is the
                // implementation's sub-module directory, not the
                // parent's directory: for `foo.gx` (pattern 1) that's
                // `<dir>/foo/`; for `foo/mod.gx` (pattern 2) it's just
                // `<dir>/`. We dig the implementation file's path out
                // of the body — the body's exprs carry the impl file
                // as their ori, while `self.ori` only points at the
                // file where `mod foo;` was written.
                let impl_path: Option<&std::path::Path> =
                    exprs.iter().find_map(|e| match &e.ori.source {
                        Source::File(p) => Some(p.as_path()),
                        _ => None,
                    });
                let prepend = match impl_path {
                    Some(p) => {
                        let parent = match p.parent() {
                            Some(par) => par,
                            None => return Ok(self.clone()),
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
                        // Non-file transports (e.g. netidx) are offered to
                        // the resolver list in order — the owning loader
                        // derives a child based at the source's path.
                        source => resolvers.iter().find_map(|m| m.for_source(source)),
                    },
                };
                let exprs = try_join_all(exprs.iter().map(|e| async {
                    e.resolve_modules_int(&scope, &prepend, resolvers).await
                }))
                .await?;
                expr!(ExprKind::Module {
                    value: ModuleKind::Resolved {
                        exprs: Arc::from(exprs),
                        sig,
                        from_interface
                    },
                    name,
                })
            }),
            ExprKind::Module {
                name,
                value: ModuleKind::Dynamic { sandbox, sig, source },
            } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                expr!(ExprKind::Module {
                    name,
                    value: ModuleKind::Dynamic { sandbox, sig, source },
                })
            }),
            ExprKind::ExplicitParens(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::ExplicitParens(Arc::new(e)))
            }),
            ExprKind::Do { exprs } => Box::pin(async move {
                let exprs = Arc::from(subexprs!(exprs));
                expr!(ExprKind::Do { exprs })
            }),
            ExprKind::Bind(b) => Box::pin(async move {
                let BindExpr { rec, pattern, typ, value } = &*b;
                let value = value.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Bind(Arc::new(BindExpr {
                    rec: *rec,
                    pattern: pattern.clone(),
                    typ: typ.clone(),
                    value,
                })))
            }),
            ExprKind::StructWith(StructWithExpr { source, replace }) => {
                Box::pin(async move {
                    expr!(ExprKind::StructWith(StructWithExpr {
                        source: Arc::new(
                            source.resolve_modules_int(scope, prepend, resolvers).await?,
                        ),
                        replace: Arc::from(subtuples!(replace)),
                    }))
                })
            }
            ExprKind::Connect { name, value, deref } => Box::pin(async move {
                let value = value.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Connect { name, value: Arc::new(value), deref })
            }),
            ExprKind::Lambda(l) => Box::pin(async move {
                let LambdaExpr { args, vargs, rtype, constraints, throws, body } = &*l;
                let body = match body {
                    Either::Right(s) => Either::Right(s.clone()),
                    Either::Left(e) => Either::Left(
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    ),
                };
                let l = LambdaExpr {
                    args: args.clone(),
                    vargs: vargs.clone(),
                    rtype: rtype.clone(),
                    throws: throws.clone(),
                    constraints: constraints.clone(),
                    body,
                };
                expr!(ExprKind::Lambda(Arc::new(l)))
            }),
            ExprKind::TypeCast { expr, typ } => Box::pin(async move {
                let expr = expr.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::TypeCast { expr: Arc::new(expr), typ })
            }),
            ExprKind::Apply(ApplyExpr { args, function }) => Box::pin(async move {
                expr!(ExprKind::Apply(ApplyExpr {
                    args: Arc::from(subtuples!(args)),
                    function
                }))
            }),
            ExprKind::Any { args } => only_args!(Any, args),
            ExprKind::Array { args } => only_args!(Array, args),
            ExprKind::Map { args } => Box::pin(async move {
                let args = Arc::from(subtuples!(args));
                expr!(ExprKind::Map { args })
            }),
            ExprKind::MapRef { source, key } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                let key =
                    Arc::new(key.resolve_modules_inner(scope, prepend, resolvers).await?);
                expr!(ExprKind::MapRef { source, key })
            }),
            ExprKind::Tuple { args } => only_args!(Tuple, args),
            ExprKind::StringInterpolate { args } => only_args!(StringInterpolate, args),
            ExprKind::Struct(StructExpr { args }) => Box::pin(async move {
                let args = Arc::from(subtuples!(args));
                expr!(ExprKind::Struct(StructExpr { args }))
            }),
            ExprKind::ArrayRef { source, i } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                let i = Arc::new(i.resolve_modules_int(scope, prepend, resolvers).await?);
                expr!(ExprKind::ArrayRef { source, i })
            }),
            ExprKind::ArraySlice { source, start, end } => Box::pin(async move {
                let source = Arc::new(
                    source.resolve_modules_int(scope, prepend, resolvers).await?,
                );
                let start = match start {
                    None => None,
                    Some(e) => Some(Arc::new(
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    )),
                };
                let end = match end {
                    None => None,
                    Some(e) => Some(Arc::new(
                        e.resolve_modules_int(scope, prepend, resolvers).await?,
                    )),
                };
                expr!(ExprKind::ArraySlice { source, start, end })
            }),
            ExprKind::Variant { tag, args } => Box::pin(async move {
                let args = Arc::from(subexprs!(args));
                expr!(ExprKind::Variant { tag, args })
            }),
            ExprKind::Select(SelectExpr { arg, arms }) => Box::pin(async move {
                let arg =
                    Arc::new(arg.resolve_modules_int(scope, prepend, resolvers).await?);
                let arms = try_join_all(arms.iter().map(|(p, e)| async {
                    let p = match &p.guard {
                        None => p.clone(),
                        Some(e) => {
                            let e =
                                e.resolve_modules_int(scope, prepend, resolvers).await?;
                            Pattern {
                                guard: Some(e),
                                type_predicate: p.type_predicate.clone(),
                                structure_predicate: p.structure_predicate.clone(),
                            }
                        }
                    };
                    let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                    Ok::<_, anyhow::Error>((p, e))
                }))
                .await?;
                expr!(ExprKind::Select(SelectExpr { arg, arms: Arc::from(arms) }))
            }),
            ExprKind::Qop(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Qop(Arc::new(e)))
            }),
            ExprKind::OrNever(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::OrNever(Arc::new(e)))
            }),
            ExprKind::TryCatch(tc) => Box::pin(async move {
                let exprs = try_join_all(tc.exprs.iter().map(|e| async {
                    e.resolve_modules_int(&scope, &prepend, resolvers).await
                }))
                .await?;
                let handler =
                    tc.handler.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::TryCatch(Arc::new(TryCatchExpr {
                    bind: tc.bind.clone(),
                    constraint: tc.constraint.clone(),
                    handler: Arc::new(handler),
                    exprs: Arc::from_iter(exprs),
                })))
            }),
            ExprKind::ByRef(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::ByRef(Arc::new(e)))
            }),
            ExprKind::Deref(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Deref(Arc::new(e)))
            }),
            ExprKind::Not { expr: e } => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Not { expr: Arc::new(e) })
            }),
            ExprKind::Neg(e) => Box::pin(async move {
                let e = e.resolve_modules_int(scope, prepend, resolvers).await?;
                expr!(ExprKind::Neg(Arc::new(e)))
            }),
            ExprKind::Add { lhs, rhs } => bin_op!(Add, lhs, rhs),
            ExprKind::CheckedAdd { lhs, rhs } => bin_op!(CheckedAdd, lhs, rhs),
            ExprKind::Sub { lhs, rhs } => bin_op!(Sub, lhs, rhs),
            ExprKind::CheckedSub { lhs, rhs } => bin_op!(CheckedSub, lhs, rhs),
            ExprKind::Mul { lhs, rhs } => bin_op!(Mul, lhs, rhs),
            ExprKind::CheckedMul { lhs, rhs } => bin_op!(CheckedMul, lhs, rhs),
            ExprKind::Div { lhs, rhs } => bin_op!(Div, lhs, rhs),
            ExprKind::CheckedDiv { lhs, rhs } => bin_op!(CheckedDiv, lhs, rhs),
            ExprKind::Mod { lhs, rhs } => bin_op!(Mod, lhs, rhs),
            ExprKind::CheckedMod { lhs, rhs } => bin_op!(CheckedMod, lhs, rhs),
            ExprKind::And { lhs, rhs } => bin_op!(And, lhs, rhs),
            ExprKind::Or { lhs, rhs } => bin_op!(Or, lhs, rhs),
            ExprKind::Eq { lhs, rhs } => bin_op!(Eq, lhs, rhs),
            ExprKind::Ne { lhs, rhs } => bin_op!(Ne, lhs, rhs),
            ExprKind::Gt { lhs, rhs } => bin_op!(Gt, lhs, rhs),
            ExprKind::Lt { lhs, rhs } => bin_op!(Lt, lhs, rhs),
            ExprKind::Gte { lhs, rhs } => bin_op!(Gte, lhs, rhs),
            ExprKind::Lte { lhs, rhs } => bin_op!(Lte, lhs, rhs),
            ExprKind::Sample { lhs, rhs } => bin_op!(Sample, lhs, rhs),
        }
    }
}
