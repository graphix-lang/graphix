#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use cargo_toml::Manifest;
use proc_macro2::TokenStream;
use quote::quote;
use std::{env, path::PathBuf, sync::LazyLock};
use syn::{
    Ident, Pat, Result, Token, parse_macro_input,
    punctuated::{Pair, Punctuated},
    token::{self, Comma},
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
    init_custom => |gx, env, stop, e, run_on_main| {
        todo!()
    },
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
                    .map(|p| p.into_value())
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

/// Read the calling crate's `[dependencies]` for `graphix-package-*` entries,
/// returning `(short_name, optional)` in document order with `core` moved
/// first. Used by the `packages!()`/`package_refs!()` macros (which run in
/// graphix-shell / graphix-tests / embedder crates, not graphix-package crates,
/// so they must NOT call `check_invariants`).
fn graphix_deps_ordered() -> Vec<(String, bool)> {
    let content = std::fs::read_to_string(PROJECT_ROOT.join("Cargo.toml"))
        .expect("failed to read Cargo.toml");
    let doc: toml_edit::DocumentMut =
        content.parse().expect("failed to parse Cargo.toml");
    let mut out: Vec<(String, bool)> = Vec::new();
    if let Some(deps) = doc.get("dependencies").and_then(|v| v.as_table()) {
        for (key, val) in deps.iter() {
            if let Some(short) = key.strip_prefix("graphix-package-") {
                let optional =
                    val.get("optional").and_then(|o| o.as_bool()).unwrap_or(false);
                out.push((short.to_string(), optional));
            }
        }
    }
    if let Some(pos) = out.iter().position(|(n, _)| n == "core") {
        let core = out.remove(pos);
        out.insert(0, core);
    }
    out
}

fn package_crate_ident(short: &str) -> syn::Ident {
    syn::Ident::new(
        &format!("graphix_package_{}", short.replace('-', "_")),
        proc_macro2::Span::call_site(),
    )
}

/// Generate the per-crate TEST_REGISTER (a const slice of `&dyn Package<NoExt>`
/// instances) from Cargo.toml deps + the crate itself.
fn test_harness() -> TokenStream {
    let deps = package_deps();
    let refs: Vec<TokenStream> = deps
        .iter()
        .map(|name| {
            if *name == *PACKAGE_NAME {
                quote! { &crate::P }
            } else {
                let crate_ident = package_crate_ident(name);
                quote! { &#crate_ident::P }
            }
        })
        .collect();
    quote! {
        /// Package instances for all dependencies + this crate (for testing).
        #[cfg(test)]
        pub(crate) const TEST_REGISTER:
            &[&dyn ::graphix_package::Package<::graphix_rt::NoExt>] = &[
            #(#refs),*
        ];
    }
}

// walk the graphix files in src/graphix and build the vfs for this package
fn graphix_files() -> Vec<TokenStream> {
    // The package's `build.rs` (via `graphix-ast-pack`) parses + packs every
    // `src/graphix/*` file into `OUT_DIR/graphix_ast.pack`, a self-contained
    // index of `(vfs_path, source, packed_ast)`. We embed that blob and decode
    // it into the modules map at `register` time, so module resolution decodes
    // a pre-parsed AST instead of re-parsing the source (the startup win). The
    // per-module AST stays packed in `VfsEntry.packed` and is decoded lazily
    // when the module is actually resolved.
    vec![quote! {
        {
            const GRAPHIX_AST_BLOB: &[u8] =
                include_bytes!(concat!(env!("OUT_DIR"), "/graphix_ast.pack"));
            for (path, entry) in
                ::graphix_compiler::expr::serialize::unpack_index(GRAPHIX_AST_BLOB)?
            {
                if modules.contains_key(&path) {
                    ::anyhow::bail!("duplicate graphix module {path}")
                }
                modules.insert(path, entry);
            }
        }
    }]
}

fn main_program_impl() -> TokenStream {
    let main_gx = GRAPHIX_SRC.join("main.gx");
    if main_gx.exists() {
        quote! {
            fn main_program(&self) -> Option<&'static str> {
                if cfg!(feature = "standalone") {
                    Some(include_str!("graphix/main.gx"))
                } else {
                    None
                }
            }
        }
    } else {
        quote! {
            fn main_program(&self) -> Option<&'static str> { None }
        }
    }
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
                let s = i.ident.to_string();
                let s = s.strip_prefix('_').unwrap_or(&s);
                if s == req[0] {
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

fn init_custom(init_custom: &Option<syn::ExprClosure>) -> TokenStream {
    match init_custom {
        None => quote! { unreachable!() },
        Some(cl) => {
            check_args(
                "init_custom",
                vec!["gx", "env", "stop", "e", "run_on_main"],
                &cl.inputs,
            );
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
    let main_program = main_program_impl();
    let test_harness = test_harness();
    let package_name = &*PACKAGE_NAME;

    let dep_registers: Vec<TokenStream> = runtime_deps()
        .iter()
        .filter(|name| **name != *PACKAGE_NAME)
        .map(|name| {
            let crate_ident = package_crate_ident(name);
            quote! {
                ::graphix_package::Package::<X>::register(
                    &#crate_ident::P,
                    ctx,
                    modules,
                    root_mods,
                )?;
            }
        })
        .collect();

    quote! {
        pub struct P;

        impl P {
            // The author's `is_custom`/`init_custom` bodies, kept with their
            // exact original signatures so they compile unchanged; the trait's
            // `maybe_init_custom` orchestrates them.
            #[allow(unused)]
            fn __is_custom<X: ::graphix_rt::GXExt>(
                gx: &::graphix_rt::GXHandle<X>,
                env: &::graphix_compiler::env::Env,
                e: &::graphix_rt::CompExp<X>,
            ) -> bool {
                #is_custom
            }

            #[allow(unused)]
            async fn __init_custom<X: ::graphix_rt::GXExt>(
                gx: &::graphix_rt::GXHandle<X>,
                env: &::graphix_compiler::env::Env,
                stop: ::tokio::sync::oneshot::Sender<()>,
                e: ::graphix_rt::CompExp<X>,
                run_on_main: ::graphix_package::MainThreadHandle,
            ) -> ::anyhow::Result<Box<dyn ::graphix_package::CustomDisplay<X>>> {
                #init_custom
            }
        }

        impl<X: ::graphix_rt::GXExt> ::graphix_package::Package<X> for P {
            fn register(
                &self,
                ctx: &mut ::graphix_compiler::ExecCtx<::graphix_rt::GXRt<X>, X::UserEvent>,
                modules: &mut ::ahash::AHashMap<
                    ::netidx_core::path::Path,
                    ::graphix_compiler::expr::VfsEntry,
                >,
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

            fn maybe_init_custom<'a>(
                &'a self,
                gx: &'a ::graphix_rt::GXHandle<X>,
                env: &'a ::graphix_compiler::env::Env,
                e: ::graphix_rt::CompExp<X>,
                run_on_main: &'a ::graphix_package::MainThreadHandle,
            ) -> ::std::pin::Pin<
                Box<
                    dyn ::std::future::Future<
                        Output = ::anyhow::Result<::graphix_package::CustomResult<X>>,
                    > + 'a,
                >,
            > {
                Box::pin(async move {
                    if !P::__is_custom::<X>(gx, env, &e) {
                        return Ok(::graphix_package::CustomResult::NotCustom(e));
                    }
                    let (tx, rx) = ::tokio::sync::oneshot::channel();
                    let custom =
                        P::__init_custom::<X>(gx, env, tx, e, run_on_main.clone())
                            .await?;
                    Ok(::graphix_package::CustomResult::Custom(
                        ::graphix_package::Cdc { stop: rx, custom },
                    ))
                })
            }

            #main_program
        }

        #test_harness
    }
    .into()
}

/// Build `Vec<Box<dyn Package<_>>>` from the calling crate's `graphix-package-*`
/// dependencies (core first). Optional deps are `#[cfg(feature = "<short>")]`-
/// gated (feature name == short name); non-optional deps are unconditional. Use
/// in a typed position (e.g. `stdlib_packages::<X>()` or `.add_packages(...)`)
/// so `_` resolves to the desired `X`.
#[proc_macro]
pub fn packages(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let pushes: Vec<TokenStream> = graphix_deps_ordered()
        .iter()
        .map(|(short, optional)| {
            let crate_ident = package_crate_ident(short);
            let push = quote! {
                v.push(
                    ::std::boxed::Box::new(#crate_ident::P)
                        as ::std::boxed::Box<dyn ::graphix_package::Package<_>>,
                );
            };
            if *optional {
                quote! { #[cfg(feature = #short)] #push }
            } else {
                push
            }
        })
        .collect();
    quote! {
        {
            // Track Cargo.toml so editing deps re-expands this macro.
            const _: &[u8] =
                include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/Cargo.toml"));
            let mut v: ::std::vec::Vec<
                ::std::boxed::Box<dyn ::graphix_package::Package<_>>,
            > = ::std::vec::Vec::new();
            #(#pushes)*
            v
        }
    }
    .into()
}

/// Build a `const`-compatible `&[&dyn Package<NoExt>]` from the calling crate's
/// `graphix-package-*` dependencies (core first). For crates whose graphix deps
/// are all NON-optional (the test crates) — optional deps can't be `#[cfg]`-
/// gated as array elements. Use as `const X: &[&dyn Package<NoExt>] =
/// graphix_package::package_refs!();`.
#[proc_macro]
pub fn package_refs(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let refs: Vec<TokenStream> = graphix_deps_ordered()
        .iter()
        .map(|(short, _optional)| {
            let crate_ident = package_crate_ident(short);
            quote! { &#crate_ident::P }
        })
        .collect();
    quote! {
        {
            const _: &[u8] =
                include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/Cargo.toml"));
            &[ #(#refs),* ]
        }
    }
    .into()
}
