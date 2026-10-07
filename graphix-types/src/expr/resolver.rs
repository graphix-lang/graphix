use crate::{
    PrintFlag,
    expr::{
        CouldNotResolve, Expr, ExprId, ExprKind, ModPath, ModuleKind, Name, Origin, Sig,
        SigItem, SigKind, Source, UseItem, parser, serialize,
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
use indexmap::IndexMap;
use log::info;
use netidx_core::path::Path;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{fmt::Write as _, path::PathBuf, pin::Pin, str::FromStr, sync::Arc as SArc};
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
pub struct VfsResolver(pub AHashMap<Path, VfsEntry>);

impl VfsResolver {
    pub fn new(vfs: AHashMap<Path, VfsEntry>) -> ResolverRef {
        SArc::new(VfsResolver(vfs))
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

fn resolve_from_vfs(
    scope: &ModPath,
    parent: &Arc<Origin>,
    name: &Path,
    vfs: &AHashMap<Path, VfsEntry>,
) -> Resolution {
    let unit = |e: &VfsEntry| Unit {
        ori: Origin {
            parent: Some(parent.clone()),
            source: Source::Internal(name.clone().into()),
            text: e.source.clone(),
        },
        packed: e.packed.clone(),
    };
    // CR claude for claude: [bug] A script's top-level `mod str;` is looked up here at
    // scope `/`, because resolve_modules_in_scope gives a root file's own statements no
    // prepend and the shell and the LSP put this VFS first. Every package root sits
    // here as `/<pkg>/mod.gx`, so the stdlib str source is compiled as the user's
    // module and the str.gx beside the script is never read. This happens for every
    // registered package name (db, args, list, map, json, http, core, sys, ...): a
    // local db.gx defining `connect` gives "db::connect not defined", a str.gx whose
    // `len` returns 1000 silently runs the stdlib's `len`, a str.gx that does not parse
    // is never reported, and `mod str;` with no file at all still loads. `--check` and
    // the LSP compile the file's statements at the root, where `/str` is the registered
    // package, and refuse the same script with "duplicate module definition str" (a
    // dynamic `mod str` too), while the run accepts it inside its `#do` block. probe:
    // design/review-2026-10-05/repro/t-format-resolver-01.sh (t-format-resolver-01)
    let at = |file: &str| vfs.get(&scope.append(&format_compact!("{name}{file}")));
    // an interface pairs with the implementation beside it, as on disk
    for (imp, intf) in [(".gx", ".gxi"), ("/mod.gx", "/mod.gxi")] {
        if let Some(e) = at(imp) {
            let interface = at(intf).map(unit);
            return Resolution::Resolved { interface, implementation: unit(e) };
        }
    }
    Resolution::TryNextMethod
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
    let mut file = base.clone();
    for part in Path::parts(&name) {
        file.push(part);
    }
    let dir = file.clone();
    file.set_extension("gx");
    let mod_file = dir.join("mod.gx");
    // CR claude for claude: [risk] An interface pairs only with the implementation beside
    // it, and nothing reports an interface left beside the other layout. With foo.gxi
    // next to foo/mod.gx, or foo/mod.gxi next to foo.gx, the interface is dropped
    // without a word and every item it leaves out is public: `mod foo; foo::secret`
    // prints 2, where the paired layout refuses it. When foo.gx and foo/mod.gx both
    // exist, foo.gx wins and foo/mod.gx is dead, since no submodule can be named `mod`.
    // The LSP's scan (graphix-lsp/src/workspace.rs:296) does resolve `mod foo` to the
    // stray foo.gxi, so it leaves foo/mod.gx out of main.gx's project and checks it as
    // a root of its own, with false errors such as "`super` goes above the package
    // root". Refuse these layouts with Resolution::Broken naming both files, here and
    // in resolve_from_vfs (line 240), whose pin at line 919 would then expect the
    // refusal. probe: design/review-2026-10-05/repro/t-format-resolver.r2-10.sh
    // (t-format-resolver.r2-10)
    for imp in [file, mod_file] {
        let intf = imp.with_extension("gxi");
        match read(overrides, &imp).await {
            Ok(None) => continue,
            Err(e) => return Resolution::Broken(e),
            Ok(Some(text)) => {
                let interface = match read(overrides, &intf).await {
                    Ok(i) => i.map(|text| unit(text, intf)),
                    Err(e) => return Resolution::Broken(e),
                };
                return Resolution::Resolved {
                    interface,
                    implementation: unit(text, imp),
                };
            }
        }
    }
    let (file, mod_file) = (dir.with_extension("gx"), dir.join("mod.gx"));
    errors.push(anyhow::anyhow!(
        "{} or {}: no such file",
        file.display(),
        mod_file.display()
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
    // the interface-only declarations in interface order, each keyed
    // after the declaration before it
    let mut pending: LPooled<IndexMap<SpliceKey, &SigItem>> = LPooled::take();
    let mut after: LPooled<AHashMap<Anchor, SpliceKey>> = LPooled::take();
    let mut first: Option<SpliceKey> = None;
    let mut last: Option<&SigItem> = None;
    for si in sig.items.iter() {
        if let Some(key) = SpliceKey::of_sig(&si.kind) {
            // CR claude for claude: [bug] A gxi-only `type` or `trait` is anchored after
            // the .gx statement that binds the `val` listed before it in the .gxi. The
            // body compiles under an env cloned before `bind_sig`, so only statements
            // below that point can name it. If the .gx binds its vals in a different
            // order than the .gxi, a valid module is refused with "undefined type T in
            // m" or "no trait `Show` in scope". With `val f: fn(x: T) -> i64; type T =
            // i64`, no order of the .gx can name T in `let f = |x: T| ..`, and `graphix
            // fmt m.gxi` can move an anchor when it sorts uses. A type never needs a
            // value before it (typedef bodies resolve lazily), so types can be placed
            // first. A `mod` whose body does `use super::x`, or a trait whose default
            // body calls a module value, does need a value before it. probe:
            // design/review-2026-10-05/repro/t-format-resolver.r2-04.sh
            // (t-format-resolver.r2-04)
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
        // CR claude for claude: [bug] Only this reader strips a leading `#!` line.
        // workspace::extract_mod_decls (graphix-lsp/src/workspace.rs:106),
        // symbols::declared (graphix-lsp/src/symbols.rs:53) and format_source (`graphix
        // fmt`, LSP formatting) parse the raw text and fail at the `#`, while running
        // the script and `--check` accept it. So in the LSP a shebang script has no
        // `mod` edges, no document symbols and no formatting. Its modules become roots
        // of their own: they show spurious errors ("`super` goes above the package
        // root"), and their real errors are never reported. Strip the line in one place
        // that every program reader shares, keeping the newline so line numbers stay
        // put. probe: design/review-2026-10-05/repro/lsp-09.py (lsp-09)
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

/// `e` with `kind` in place of its own.
// CR claude for claude: [structure] This is Expr::with_kind
// (graphix-types/src/expr/mod.rs:1364) with the old id kept, and
// graphix-types/src/expr/format.rs:217-219 spells it a third way (Expr::new, then id
// and ori by hand). One Expr method beside with_kind that keeps the id, with with_kind
// as that plus a fresh id, serves both. holds_unresolved's Resolved arm (lines 649-651)
// repeats its `_` arm: for_each_child already visits a resolved module's exprs.
// (t-format-resolver-15)
fn rekind(e: &Expr, kind: ExprKind) -> Expr {
    Expr {
        id: e.id,
        ori: e.ori.clone(),
        pos: e.pos,
        kind,
        dec: e.dec.clone(),
        str_form: e.str_form,
        end: e.end,
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
    // CR claude for claude: [bug] When a nested `mod x;` has no file beside its parent,
    // resolution falls through to the global chain. Those FilesResolvers ignore
    // `scope`, so `x.gx` is loaded by its leaf name from the script's directory, the
    // data dir or GRAPHIX_MODPATH. That contradicts the rule at line 728 and the LSP's
    // model of it (graphix-lsp/src/workspace.rs:188 looks only at <base>/<rel>/x.gx). A
    // missing or misplaced `a/x.gx` is never reported. Instead `a::x` silently becomes
    // a second instance of the top-level `x`, whose abstract types are different
    // nominal types (`type mismatch T does not contain '_N: T`), and two sibling
    // modules that each `mod` the other report an import cycle, which is the only way
    // graphix-shell/tests/import_cycle.rs forms one. A body with a file source should
    // resolve its submodules only through its prepend and report that resolver's error.
    // probe: design/review-2026-10-05/repro/t-format-resolver-05.sh
    // (t-format-resolver-05)
    for r in prepend.iter().chain(resolvers.iter()) {
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
        return Ok(rekind(stmt, kind));
    }
    let mut msg = format_compact!("module {name} could not be found");
    for (i, e) in errors.iter().enumerate() {
        let _ = write!(&mut msg, "{}{e}", if i == 0 { ": " } else { "; " });
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
            ExprKind::Module { value: ModuleKind::Resolved { exprs, .. }, .. } => {
                exprs.iter().any(|e| e.holds_unresolved())
            }
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
    // CR claude for claude: [bug] Module resolution recurses through these boxed futures
    // with no stack guard. Every nested `mod`, and every expression level above one,
    // adds four or five poll frames (resolve_children -> TryJoinAll -> TryMaybeDone ->
    // here) on the tokio worker's 2 MiB stack. Each file is parsed on its own, so the
    // parser's nesting limit does not bound the depth across files. A chain of 500
    // nested module files (550 in an opt-level 3 build), or three files that each nest
    // 330 blocks around their `mod m`, aborts `graphix --check`, a run and `graphix
    // lsp` with a stack overflow. Poll the returned future under
    // crate::stack::ensure_sufficient (or resolve through a worklist), and add a
    // module-file chain to graphix-shell/tests/deep_nesting.rs. probe:
    // design/review-2026-10-05/repro/x-stack-04.sh (`GRAPHIX=<graphix> bash
    // x-stack-04.sh 600` and `... 3 330` exit 134; 200 and `2 330` exit 0).
    // (x-stack-04)
    fn resolve_modules_int<'a>(
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
                // CR claude for claude: [bug] This marks every failure of `resolve` as
                // CouldNotResolve, not just "could not be found": a parse error in the
                // module file and a Broken read get the marker too, and through line
                // 704 a failing nested `mod` carries its own. The REPL
                // (graphix-shell/src/lib.rs:399) treats `e.is::<CouldNotResolve>()` as
                // "there is no init module", and anyhow matches that anywhere in the
                // chain. So an init.gx that does not parse, cannot be read, or loads a
                // broken module is dropped with no message, while a type error in it is
                // reported. The book's own example init.gx (book/src/shell.md:406) does
                // not parse and is dropped this way. Mark only the not-found bail in
                // `resolve`, with the module's path, and have the shell skip only a
                // missing root `init`; --check then stops headlining a submodule's
                // parse error as "could not resolve module bad". probe:
                // design/review-2026-10-05/repro/x-errors-03.py (x-errors-03)
                // CR claude for claude: [bug] An unresolvable module (none found, or a
                // Broken read at 602) and an import cycle (LoadChain::push at 720)
                // leave with no `.at()`. The only context is the positionless
                // CouldNotResolve, though the `mod` statement is `self`. So `graphix
                // --check` prints no line or file, and the LSP's error_location finds
                // no site: the diagnostic lands at (0,0) of the root, over its first
                // word, even when the `mod` is in another file, and that file shows
                // nothing. Parse and type errors in the same submodule are placed
                // correctly. Adding `.at(self)` to this result and to the
                // LoadChain::push error puts each one on its `mod` statement. probe:
                // design/review-2026-10-05/repro/lsp-08.py (lsp-08)
                .with_context(|| CouldNotResolve(name.name.clone()))?;
                let scope = ModPath(scope.append(&**name));
                let r = e.resolve_modules_int(&scope, prepend, chain, resolvers).await?;
                Ok(Some(r.unwrap_or(e)))
            }),
            ExprKind::Module {
                value: ModuleKind::Resolved { exprs, sig, from_interface },
                name,
            } => Box::pin(async move {
                // CR claude for claude: [bug] The body's source is taken from its first
                // expression, but add_interface_modules puts a .gxi's leading use,
                // type, mod or trait that the .gx does not repeat ahead of every
                // implementation statement, with the interface's origin. Then the load
                // chain records a.gxi, a netidx module's submodule base (for_source)
                // becomes the .gxi's path, and compile_module_inner's def_ori
                // (graphix-compiler/src/node/compiler.rs:249) sends go-to-definition on
                // `mod a` to a.gxi, while the same .gxi with its `val` first sends it
                // to a.gx. The import-cycle message also prints the source with Debug,
                // `(File(".../a.gxi"))`. Record the implementation's origin where
                // resolve() and RootFile::into_module build ModuleKind::Resolved, and
                // read it at both sites. probe:
                // design/review-2026-10-05/repro/t-format-resolver-08.py
                // (t-format-resolver-08)
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
                        let Some(parent) = p.parent() else { return Ok(None) };
                        Some(files(match p.file_stem().and_then(|s| s.to_str()) {
                            Some("mod") | None => parent.to_path_buf(),
                            Some(stem) => parent.join(stem),
                        }))
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
                Ok(Some(rekind(self, ExprKind::Module { value, name: name.clone() })))
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
        // CR claude for claude: [bug] Only NotFound reads as absent. For `mod util;`
        // beside a regular file `util` (a script or a binary), opening `util/mod.gx`
        // fails with NotADirectory, resolve_from_files returns Broken, and the `util`
        // module in GRAPHIX_MODPATH or the data dir is never tried, although that path
        // holds no module. NotADirectory also means the path names no file, so it
        // belongs with NotFound. probe:
        // design/review-2026-10-05/repro/t-format-resolver-07.sh (t-format-resolver-07)
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(anyhow::Error::from(e).context(path.display().to_string())),
    };
    let mut buf: LPooled<Vec<u8>> = LPooled::take();
    f.read_to_end(&mut *buf).await.with_context(|| path.display().to_string())?;
    let s = str::from_utf8(&*buf).with_context(|| path.display().to_string())?;
    Ok(Some(ArcStr::from(s)))
}

#[cfg(test)]
mod test {
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

    /// An interface pairs with the implementation beside it only.
    #[tokio::test]
    async fn a_vfs_interface_pairs_beside_its_implementation() {
        let lib = vfs(&[("/m/mod.gx", "let x = 1"), ("/m.gxi", "val y: i64")]);
        let main = Origin { text: literal!("mod m;"), ..Origin::default() };
        let e = resolve_first(main, &[lib]).await.unwrap();
        assert!(body(&e).1.is_none());
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
