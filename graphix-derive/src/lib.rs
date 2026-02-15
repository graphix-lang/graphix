use cargo_toml::Manifest;
use fxhash::FxHashMap;
use proc_macro2::TokenStream;
use quote::quote;
use std::{
    env,
    path::{Component, Path, PathBuf},
    sync::LazyLock,
};
use syn::{
    parse_macro_input,
    punctuated::{Pair, Punctuated},
    token::{self, Comma},
    Ident, Pat, Result, Token,
};
static PROJECT_ROOT: LazyLock<PathBuf> = LazyLock::new(|| {
    env::var("CARGO_MANIFEST_DIR").expect("missing manifest dir").into()
});

static GRAPHIX_SRC: LazyLock<PathBuf> =
    LazyLock::new(|| PROJECT_ROOT.join("src").join("graphix"));

static CARGO_MANIFEST: LazyLock<Manifest> = LazyLock::new(|| {
    Manifest::from_path(PROJECT_ROOT.join("Cargo.toml"))
        .expect("failed to load cargo manifest")
});

static CRATE_NAME: LazyLock<String> =
    LazyLock::new(|| env::var("CARGO_CRATE_NAME").expect("missing crate name"));

static PACKAGE_NAME: LazyLock<String> =
    LazyLock::new(|| match CRATE_NAME.strip_prefix("graphix_package_") {
        Some(name) => name.into(),
        None => CRATE_NAME.clone(),
    });

/* example
defpackage! {
    builtins => [
        Foo,
        submod::Bar,
        Baz as Baz<R, E>,
    ],
    is_custom => |gx, env, e| {
        todo!()
    },
    init_custom => |gx, env, stop, e| {
        todo!()
    }
}
*/

/// A builtin entry: either a simple path (used for both NAME access and
/// registration), or `Path as Type` where Path is used for `::NAME` access
/// and Type is used for `register_builtin::<Type>()`.
struct BuiltinEntry {
    reg_type: syn::Type,
}

impl syn::parse::Parse for BuiltinEntry {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let name_path: syn::Path = input.parse()?;
        if input.peek(Token![as]) {
            let _as: Token![as] = input.parse()?;
            let reg_type: syn::Type = input.parse()?;
            Ok(BuiltinEntry { reg_type })
        } else {
            let reg_type =
                syn::Type::Path(syn::TypePath { qself: None, path: name_path.clone() });
            Ok(BuiltinEntry { reg_type })
        }
    }
}

struct DefPackage {
    builtins: Vec<BuiltinEntry>,
    is_custom: Option<syn::ExprClosure>,
    init_custom: Option<syn::ExprClosure>,
}

impl syn::parse::Parse for DefPackage {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut builtins = Vec::new();
        let mut is_custom = None;
        let mut init_custom = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            let _arrow: Token![=>] = input.parse()?;
            if key == "builtins" {
                let content;
                let _bracket: token::Bracket = syn::bracketed!(content in input);
                builtins = content
                    .parse_terminated(BuiltinEntry::parse, Token![,])?
                    .into_pairs()
                    .filter_map(|p| match p {
                        Pair::Punctuated(v, _) => Some(v),
                        Pair::End(_) => None,
                    })
                    .collect();
            } else if key == "is_custom" {
                is_custom = Some(input.parse::<syn::ExprClosure>()?);
            } else if key == "init_custom" {
                init_custom = Some(input.parse::<syn::ExprClosure>()?);
            } else {
                return Err(input.error("unknown key"));
            }
            if !input.is_empty() {
                let _comma: Option<Token![,]> = input.parse()?;
            }
        }
        Ok(DefPackage { builtins, is_custom, init_custom })
    }
}

fn check_invariants() {
    if !CARGO_MANIFEST.bin.is_empty() {
        panic!("graphix package crates may not have binary targets")
    }
    if !CARGO_MANIFEST.lib.is_some() {
        panic!("graphix package crates must have a lib target")
    }
    let md = std::fs::metadata(&*GRAPHIX_SRC)
        .expect("graphix projects must have a graphix-src directory");
    if !md.is_dir() {
        panic!("graphix projects must have a graphix-src directory")
    }
    // every package must depend on graphix-package-core (except core itself)
    let is_core = *PACKAGE_NAME == "core";
    if !is_core && !CARGO_MANIFEST.dependencies.contains_key("graphix-package-core") {
        panic!("graphix packages must depend on graphix-package-core")
    }
}

/// Collect graphix-package-* dependency names from a Cargo.toml section,
/// preserving document order.
fn collect_package_deps(
    doc: &toml_edit::DocumentMut,
    section: &str,
    seen: &mut std::collections::HashSet<String>,
    result: &mut Vec<String>,
) {
    if let Some(deps) = doc.get(section).and_then(|v| v.as_table()) {
        for (key, _) in deps.iter() {
            if let Some(name) = key.strip_prefix("graphix-package-") {
                if seen.insert(name.to_string()) {
                    result.push(name.to_string());
                }
            }
        }
    }
}

/// Collect graphix-package-* deps from [dependencies] only (used for
/// register() calls that must compile without dev-dependencies).
fn runtime_deps() -> Vec<String> {
    let content = std::fs::read_to_string(PROJECT_ROOT.join("Cargo.toml"))
        .expect("failed to read Cargo.toml");
    let doc: toml_edit::DocumentMut =
        content.parse().expect("failed to parse Cargo.toml");
    let mut seen = std::collections::HashSet::new();
    let mut result = Vec::new();
    collect_package_deps(&doc, "dependencies", &mut seen, &mut result);
    result
}

/// Collect graphix-package-* dependency names from both [dependencies] and
/// [dev-dependencies], preserving the order written in Cargo.toml.
/// Core always comes first. Used for TEST_REGISTER.
fn package_deps() -> Vec<String> {
    let content = std::fs::read_to_string(PROJECT_ROOT.join("Cargo.toml"))
        .expect("failed to read Cargo.toml");
    let doc: toml_edit::DocumentMut =
        content.parse().expect("failed to parse Cargo.toml");
    let mut seen = std::collections::HashSet::new();
    let mut result = Vec::new();
    // core always first
    seen.insert("core".to_string());
    result.push("core".to_string());
    collect_package_deps(&doc, "dependencies", &mut seen, &mut result);
    collect_package_deps(&doc, "dev-dependencies", &mut seen, &mut result);
    // include ourselves if not already present
    if seen.insert(PACKAGE_NAME.clone()) {
        result.push(PACKAGE_NAME.clone());
    }
    result
}

/// Generate the TEST_REGISTER array from Cargo.toml deps.
fn test_harness() -> TokenStream {
    let deps = package_deps();
    let register_fns: Vec<TokenStream> = deps.iter().map(|name| {
        if *name == *PACKAGE_NAME {
            quote! {
                <crate::P as ::graphix_package::Package<::graphix_rt::NoExt>>::register
            }
        } else {
            let crate_ident = syn::Ident::new(
                &format!("graphix_package_{}", name.replace('-', "_")),
                proc_macro2::Span::call_site(),
            );
            quote! {
                <#crate_ident::P as ::graphix_package::Package<::graphix_rt::NoExt>>::register
            }
        }
    }).collect();
    let register_fn_ty = if *PACKAGE_NAME == "core" {
        quote! { crate::testing::RegisterFn }
    } else {
        quote! { ::graphix_package_core::testing::RegisterFn }
    };
    quote! {
        /// Register functions for all package dependencies (for testing).
        #[cfg(test)]
        pub(crate) const TEST_REGISTER: &[#register_fn_ty] = &[
            #(#register_fns),*
        ];
    }
}

// walk the graphix files in src/graphix and build the vfs for this package
fn graphix_files() -> Vec<TokenStream> {
    let mut res = vec![];
    for entry in walkdir::WalkDir::new(&*GRAPHIX_SRC) {
        let entry = entry.expect("could not read");
        if !entry.file_type().is_file() {
            continue;
        }
        let ext = entry.path().extension().and_then(|e| e.to_str());
        if ext != Some("gx") && ext != Some("gxi") {
            continue;
        }
        let path = match entry.path().strip_prefix(&*GRAPHIX_SRC) {
            Ok(p) => p,
            Err(_) => continue,
        };
        let mut vfs_path = format!("/{}", PACKAGE_NAME.clone());
        for c in path.components() {
            match c {
                Component::CurDir
                | Component::ParentDir
                | Component::RootDir
                | Component::Prefix(_) => panic!("invalid path component {c:?}"),
                Component::Normal(p) => match p.to_str() {
                    None => panic!("invalid path component {c:?}"),
                    Some(s) => {
                        vfs_path.push('/');
                        vfs_path.push_str(s)
                    }
                },
            };
        }
        let mut compiler_path = PathBuf::new();
        compiler_path.push("graphix");
        compiler_path.push(path);
        let compiler_path = compiler_path.to_string_lossy().into_owned();
        res.push(quote! {
            let path = ::netidx_core::path::Path::from(#vfs_path);
            if modules.contains_key(&path) {
                ::anyhow::bail!("duplicate graphix module {path}")
            }
            modules.insert(path, ::arcstr::literal!(include_str!(#compiler_path)))
        })
    }
    res
}

fn register_builtins(builtins: &[BuiltinEntry]) -> Vec<TokenStream> {
    let package_name = &*PACKAGE_NAME;
    builtins.iter().map(|entry| {
        let reg_type = &entry.reg_type;
        quote! {
            {
                let name: &str = <#reg_type as ::graphix_compiler::BuiltIn<::graphix_rt::GXRt<X>, X::UserEvent>>::NAME;
                if name.contains(|c: char| c != '_' && !c.is_ascii_alphanumeric()) {
                    ::anyhow::bail!("invalid builtin name {}, must contain only ascii alphanumeric and _", name)
                }
                if !name.starts_with(#package_name) {
                    ::anyhow::bail!("invalid builtin {} name must start with package name {}", name, #package_name)
                }
                ctx.register_builtin::<#reg_type>()?
            }
        }
    }).collect()
}

fn check_args(name: &str, mut req: Vec<&'static str>, args: &Punctuated<Pat, Comma>) {
    fn check_arg(name: &str, req: &mut Vec<&'static str>, pat: &Pat) {
        if req.is_empty() {
            panic!("{name} unexpected argument")
        }
        match pat {
            Pat::Ident(i) => {
                if &i.ident.to_string() == &req[0] {
                    req.remove(0);
                } else {
                    panic!("{name} expected arguments {req:?}")
                }
            }
            _ => panic!("{name} expected arguments {req:?}"),
        }
    }
    for arg in args.pairs() {
        match arg {
            Pair::End(i) => {
                check_arg(name, &mut req, i);
            }
            Pair::Punctuated(i, _) => {
                check_arg(name, &mut req, i);
            }
        }
    }
    if !req.is_empty() {
        panic!("{name} missing required arguments {req:?}")
    }
}

fn is_custom(is_custom: &Option<syn::ExprClosure>) -> TokenStream {
    match is_custom {
        None => quote! { false },
        Some(cl) => {
            check_args("is_custom", vec!["gx", "env", "e"], &cl.inputs);
            let body = &cl.body;
            quote! { #body }
        }
    }
}

fn init_custom(is_custom: &Option<syn::ExprClosure>) -> TokenStream {
    match is_custom {
        None => quote! { unreachable!() },
        Some(cl) => {
            check_args("init_custom", vec!["gx", "env", "stop", "e"], &cl.inputs);
            let body = &cl.body;
            quote! { #body }
        }
    }
}

#[proc_macro]
pub fn defpackage(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    check_invariants();
    let input = parse_macro_input!(input as DefPackage);
    let register_builtins = register_builtins(&input.builtins);
    let is_custom = is_custom(&input.is_custom);
    let init_custom = init_custom(&input.init_custom);
    let graphix_files = graphix_files();
    let test_harness = test_harness();
    let package_name = &*PACKAGE_NAME;

    let dep_registers: Vec<TokenStream> = runtime_deps()
        .iter()
        .filter(|name| **name != *PACKAGE_NAME)
        .map(|name| {
            let crate_ident = syn::Ident::new(
                &format!("graphix_package_{}", name.replace('-', "_")),
                proc_macro2::Span::call_site(),
            );
            quote! {
                <#crate_ident::P as ::graphix_package::Package<X>>::register(ctx, modules, root_mods)?;
            }
        })
        .collect();

    quote! {
        pub struct P;

        impl<X: ::graphix_rt::GXExt> ::graphix_package::Package<X> for P {
            fn register(
                ctx: &mut ::graphix_compiler::ExecCtx<::graphix_rt::GXRt<X>, X::UserEvent>,
                modules: &mut ::fxhash::FxHashMap<::netidx_core::path::Path, ::arcstr::ArcStr>,
                root_mods: &mut ::graphix_package::IndexSet<::arcstr::ArcStr>,
            ) -> ::anyhow::Result<()> {
                if root_mods.contains(#package_name) {
                    return Ok(());
                }
                #(#dep_registers)*
                #(#register_builtins;)*
                #(#graphix_files;)*
                root_mods.insert(::arcstr::literal!(#package_name));
                Ok(())
            }

            #[allow(unused)]
            fn is_custom(
                gx: &::graphix_rt::GXHandle<X>,
                env: &::graphix_compiler::env::Env,
                e: &::graphix_rt::CompExp<X>,
            ) -> bool {
                #is_custom
            }

            #[allow(unused)]
            fn init_custom(
                gx: &::graphix_rt::GXHandle<X>,
                env: &::graphix_compiler::env::Env,
                stop: ::tokio::sync::oneshot::Sender<()>,
                e: ::graphix_rt::CompExp<X>,
            ) -> ::anyhow::Result<Box<dyn ::graphix_package::CustomDisplay<X>>> {
                #init_custom
            }
        }

        #test_harness
    }
    .into()
}

struct TomlCache {
    docs: FxHashMap<PathBuf, toml_edit::DocumentMut>,
}

impl TomlCache {
    fn new() -> Self {
        Self { docs: FxHashMap::default() }
    }

    fn get(&mut self, path: &Path) -> &toml_edit::DocumentMut {
        self.docs.entry(path.to_path_buf()).or_insert_with(|| {
            let content = std::fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
            content
                .parse()
                .unwrap_or_else(|e| panic!("failed to parse {}: {e}", path.display()))
        })
    }
}

/// Resolve the version of a skel dependency.
///
/// For graphix-* crates, reads `[package].version` from the crate's own
/// Cargo.toml. For third-party crates, reads the version requirement from
/// `[workspace.dependencies]` in the workspace root Cargo.toml.
fn resolve_skel_dep_version(
    cache: &mut TomlCache,
    workspace: &Path,
    name: &str,
) -> String {
    if name.starts_with("graphix-") {
        let crate_dir = if name.starts_with("graphix-package") {
            if name == "graphix-package" {
                workspace.join(name)
            } else {
                workspace.join("stdlib").join(name)
            }
        } else {
            workspace.join(name)
        };
        let cargo_toml = crate_dir.join("Cargo.toml");
        let doc = cache.get(&cargo_toml);
        doc.get("package")
            .and_then(|p| p.get("version"))
            .and_then(|v| v.as_str())
            .unwrap_or_else(|| panic!("no [package].version in {}", cargo_toml.display()))
            .to_string()
    } else {
        let ws_toml = workspace.join("Cargo.toml");
        let doc = cache.get(&ws_toml);
        let ws_deps = doc
            .get("workspace")
            .and_then(|w| w.get("dependencies"))
            .and_then(|d| d.as_table())
            .unwrap_or_else(|| {
                panic!("no [workspace.dependencies] in {}", ws_toml.display())
            });
        let dep = ws_deps.get(name).unwrap_or_else(|| {
            panic!("dep {name} not found in [workspace.dependencies]")
        });
        if let Some(s) = dep.as_str() {
            s.to_string()
        } else if let Some(t) = dep.as_inline_table() {
            t.get("version")
                .and_then(|v| v.as_str())
                .unwrap_or_else(|| panic!("dep {name} has no version in workspace deps"))
                .to_string()
        } else if let Some(t) = dep.as_table() {
            t.get("version")
                .and_then(|v| v.as_str())
                .unwrap_or_else(|| panic!("dep {name} has no version in workspace deps"))
                .to_string()
        } else {
            panic!("unexpected dep format for {name} in workspace deps")
        }
    }
}

/// Generate a function returning serde_json::Value with the version of every
/// dependency listed in skel/Cargo.toml. Versions are resolved from the
/// workspace at compile time.
///
/// Invoke from graphix-package/src/lib.rs:
/// ```ignore
/// graphix_derive::skel_dep_versions!();
/// // generates: fn skel_dep_versions() -> serde_json::Value { ... }
/// ```
#[proc_macro]
pub fn skel_dep_versions(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let workspace = PROJECT_ROOT.parent().expect("PROJECT_ROOT has no parent");
    let skel_toml = PROJECT_ROOT.join("skel").join("Cargo.toml");
    let mut cache = TomlCache::new();
    let dep_names: Vec<String> = cache
        .get(&skel_toml)
        .get("dependencies")
        .and_then(|d| d.as_table())
        .unwrap_or_else(|| panic!("no [dependencies] in {}", skel_toml.display()))
        .iter()
        .map(|(name, _)| name.to_string())
        .collect();
    let mut keys = Vec::new();
    let mut values = Vec::new();
    for name in &dep_names {
        let version = resolve_skel_dep_version(&mut cache, workspace, name);
        let key = format!("{}_version", name.replace('-', "_"));
        keys.push(key);
        values.push(version);
    }
    quote! {
        fn skel_dep_versions() -> ::serde_json::Value {
            ::serde_json::json!({
                #( #keys: #values ),*
            })
        }
    }
    .into()
}
