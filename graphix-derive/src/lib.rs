#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use proc_macro2::TokenStream;
use quote::quote;
use std::{env, path::PathBuf};
use syn::{
    Ident, Pat, Result, Token, parse_macro_input,
    punctuated::{Pair, Punctuated},
    token::{self, Comma},
};

/// What an expansion names the package's dependencies through, so a
/// package need not depend on each at a version that unifies.
fn private() -> TokenStream {
    quote! { ::graphix_package::__private }
}

/// A `graphix-package-*` dependency of the calling crate.
struct Dep {
    /// the short name, `graphix-package-` stripped
    name: String,
    optional: bool,
    /// a dev-dependency only
    dev: bool,
}

/// The calling crate, its manifest read once per expansion.
struct Crate {
    root: PathBuf,
    /// the package's short name, `graphix_package_` stripped
    name: String,
    /// `[dependencies]` then `[dev-dependencies]` in document order,
    /// each once, core first
    deps: Vec<Dep>,
    doc: toml_edit::DocumentMut,
}

impl Crate {
    fn load() -> Self {
        let root: PathBuf =
            env::var("CARGO_MANIFEST_DIR").expect("missing manifest dir").into();
        let crate_name = env::var("CARGO_CRATE_NAME").expect("missing crate name");
        let name =
            crate_name.strip_prefix("graphix_package_").unwrap_or(&crate_name).into();
        let doc: toml_edit::DocumentMut =
            std::fs::read_to_string(root.join("Cargo.toml"))
                .expect("failed to read Cargo.toml")
                .parse()
                .expect("failed to parse Cargo.toml");
        let mut deps: Vec<Dep> = vec![];
        for (section, dev) in [("dependencies", false), ("dev-dependencies", true)] {
            let Some(table) = doc.get(section).and_then(|v| v.as_table()) else {
                continue;
            };
            for (key, val) in table.iter() {
                if let Some(short) = key.strip_prefix("graphix-package-")
                    && !deps.iter().any(|d| d.name == short)
                {
                    let optional =
                        val.get("optional").and_then(|o| o.as_bool()).unwrap_or(false);
                    deps.push(Dep { name: short.into(), optional, dev });
                }
            }
        }
        if let Some(pos) = deps.iter().position(|d| d.name == "core") {
            let core = deps.remove(pos);
            deps.insert(0, core);
        }
        Crate { root, name, deps, doc }
    }

    /// the dependencies `register` reaches: never a dev-dependency
    fn runtime(&self) -> impl Iterator<Item = &Dep> {
        self.deps.iter().filter(|d| !d.dev)
    }

    fn graphix_src(&self) -> PathBuf {
        self.root.join("src").join("graphix")
    }
}

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
    /// `is_custom` and `init_custom`, which make sense only together
    custom: Option<(syn::ExprClosure, syn::ExprClosure)>,
}

impl syn::parse::Parse for DefPackage {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut builtins = None;
        let mut is_custom = None;
        let mut init_custom = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            let _arrow: Token![=>] = input.parse()?;
            let repeated = if key == "builtins" {
                let content;
                let _bracket: token::Bracket = syn::bracketed!(content in input);
                let b = content
                    .parse_terminated(BuiltinEntry::parse, Token![,])?
                    .into_pairs()
                    .map(|p| p.into_value())
                    .collect();
                builtins.replace(b).is_some()
            } else if key == "is_custom" {
                is_custom.replace(input.parse::<syn::ExprClosure>()?).is_some()
            } else if key == "init_custom" {
                init_custom.replace(input.parse::<syn::ExprClosure>()?).is_some()
            } else {
                return Err(syn::Error::new(key.span(), "unknown key"));
            };
            if repeated {
                return Err(syn::Error::new(key.span(), format!("{key} is given twice")));
            }
            if !input.is_empty() {
                let _comma: Option<Token![,]> = input.parse()?;
            }
        }
        let custom = match (is_custom, init_custom) {
            (Some(is), Some(init)) => Some((is, init)),
            (None, None) => None,
            (Some(is), None) => {
                return Err(syn::Error::new_spanned(is, "is_custom needs init_custom"));
            }
            (None, Some(init)) => {
                return Err(syn::Error::new_spanned(init, "init_custom needs is_custom"));
            }
        };
        Ok(DefPackage { builtins: builtins.unwrap_or_default(), custom })
    }
}

fn check_invariants(c: &Crate) {
    let src = c.root.join("src");
    let bins = c
        .doc
        .get("bin")
        .and_then(|b| b.as_array_of_tables())
        .is_some_and(|b| !b.is_empty());
    if bins || src.join("main.rs").exists() || src.join("bin").is_dir() {
        panic!("graphix package crates may not have binary targets")
    }
    if c.doc.get("lib").is_none() && !src.join("lib.rs").exists() {
        panic!("graphix package crates must have a lib target")
    }
    if !c.graphix_src().is_dir() {
        panic!("graphix projects must have a src/graphix directory")
    }
    if c.name != "core" && !c.runtime().any(|d| d.name == "core") {
        panic!("graphix packages must depend on graphix-package-core")
    }
}

fn package_crate_ident(short: &str) -> syn::Ident {
    syn::Ident::new(
        &format!("graphix_package_{}", short.replace('-', "_")),
        proc_macro2::Span::call_site(),
    )
}

/// Generate the per-crate TEST_REGISTER (a const slice of `&dyn Package<NoExt>`
/// instances) from Cargo.toml deps, dev-dependencies included, and the
/// crate itself, core first.
fn test_harness(c: &Crate) -> TokenStream {
    let p = private();
    let mut names: Vec<&str> = vec!["core"];
    names.extend(c.deps.iter().map(|d| d.name.as_str()).filter(|n| *n != "core"));
    if !names.contains(&c.name.as_str()) {
        names.push(&c.name);
    }
    let refs = names.iter().map(|name| {
        if *name == c.name {
            quote! { &crate::P }
        } else {
            let crate_ident = package_crate_ident(name);
            quote! { &#crate_ident::P }
        }
    });
    quote! {
        /// Package instances for all dependencies + this crate (for testing).
        #[cfg(test)]
        pub(crate) const TEST_REGISTER:
            &[&dyn ::graphix_package::Package<#p::graphix_rt::NoExt>] = &[
            #(#refs),*
        ];
    }
}

// the vfs for this package, decoded from the build.rs AST blob
fn graphix_files() -> Vec<TokenStream> {
    let p = private();
    // Each module stays packed in `VfsEntry.packed` and is decoded when
    // it is resolved.
    vec![quote! {
        {
            const GRAPHIX_AST_BLOB: &[u8] =
                include_bytes!(concat!(env!("OUT_DIR"), "/graphix_ast.pack"));
            for (path, entry) in
                #p::graphix_compiler::expr::serialize::unpack_index(GRAPHIX_AST_BLOB)?
            {
                if modules.contains_key(&path) {
                    #p::anyhow::bail!("duplicate graphix module {path}")
                }
                modules.insert(path, entry);
            }
        }
    }]
}

fn main_program_impl(c: &Crate) -> TokenStream {
    if c.graphix_src().join("main.gx").exists() {
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

fn register_builtins(c: &Crate, builtins: &[BuiltinEntry]) -> Vec<TokenStream> {
    let p = private();
    let package_name = &c.name;
    builtins.iter().map(|entry| {
        let reg_type = &entry.reg_type;
        quote! {
            {
                let name: &str = <#reg_type as #p::graphix_compiler::BuiltIn<#p::graphix_rt::GXRt<X>, X::UserEvent>>::NAME;
                if name.contains(|c: char| c != '_' && !c.is_ascii_alphanumeric()) {
                    #p::anyhow::bail!("invalid builtin name {}, must contain only ascii alphanumeric and _", name)
                }
                if !name.starts_with(#package_name) {
                    #p::anyhow::bail!("invalid builtin {} name must start with package name {}", name, #package_name)
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

/// The package's helpers for its custom display, and the body of
/// `maybe_init_custom`: a package without one claims nothing.
fn custom(
    custom: &Option<(syn::ExprClosure, syn::ExprClosure)>,
) -> (TokenStream, TokenStream) {
    let p = private();
    let Some((is, init)) = custom else {
        return (
            quote! {},
            quote! { Box::pin(async move { Ok(::graphix_package::CustomResult::NotCustom(e)) }) },
        );
    };
    check_args("is_custom", vec!["gx", "env", "e"], &is.inputs);
    check_args(
        "init_custom",
        vec!["gx", "env", "stop", "e", "run_on_main"],
        &init.inputs,
    );
    let (is, init) = (&is.body, &init.body);
    let helpers = quote! {
        impl P {
            // The author's bodies keep their exact signatures; the trait's
            // `maybe_init_custom` orchestrates them.
            #[allow(unused)]
            fn __is_custom<X: #p::graphix_rt::GXExt>(
                gx: &#p::graphix_rt::GXHandle<X>,
                env: &#p::graphix_compiler::env::Env,
                e: &#p::graphix_rt::CompExp<X>,
            ) -> bool {
                #is
            }

            #[allow(unused)]
            async fn __init_custom<X: #p::graphix_rt::GXExt>(
                gx: &#p::graphix_rt::GXHandle<X>,
                env: &#p::graphix_compiler::env::Env,
                stop: ::graphix_package::Stop,
                e: #p::graphix_rt::CompExp<X>,
                run_on_main: ::graphix_package::MainThreadHandle,
            ) -> #p::anyhow::Result<Box<dyn ::graphix_package::CustomDisplay<X>>> {
                #init
            }
        }
    };
    let body = quote! {
        Box::pin(async move {
            if !P::__is_custom::<X>(gx, env, &e) {
                return Ok(::graphix_package::CustomResult::NotCustom(e));
            }
            let (tx, rx) = #p::tokio::sync::oneshot::channel();
            let custom =
                P::__init_custom::<X>(gx, env, tx, e, run_on_main.clone())
                    .await?;
            Ok(::graphix_package::CustomResult::Custom(
                ::graphix_package::Cdc { stop: rx, custom },
            ))
        })
    };
    (helpers, body)
}

#[proc_macro]
pub fn defpackage(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let c = Crate::load();
    check_invariants(&c);
    let input = parse_macro_input!(input as DefPackage);
    let p = private();
    let register_builtins = register_builtins(&c, &input.builtins);
    let (custom_helpers, maybe_init_custom) = custom(&input.custom);
    let graphix_files = graphix_files();
    let main_program = main_program_impl(&c);
    let test_harness = test_harness(&c);
    let package_name = &c.name;
    let dep_registers = c.runtime().filter(|d| d.name != c.name).map(|d| {
        let crate_ident = package_crate_ident(&d.name);
        quote! {
            ::graphix_package::Package::<X>::register(
                &#crate_ident::P,
                ctx,
                modules,
                root_mods,
            )?;
        }
    });
    quote! {
        pub struct P;

        #custom_helpers

        impl<X: #p::graphix_rt::GXExt> ::graphix_package::Package<X> for P {
            fn register(
                &self,
                ctx: &mut #p::graphix_compiler::ExecState<#p::graphix_rt::GXRt<X>, X::UserEvent>,
                modules: &mut #p::ahash::AHashMap<
                    #p::netidx_core::path::Path,
                    #p::graphix_compiler::expr::VfsEntry,
                >,
                root_mods: &mut ::graphix_package::IndexSet<#p::arcstr::ArcStr>,
            ) -> #p::anyhow::Result<()> {
                if root_mods.contains(#package_name) {
                    return Ok(());
                }
                #(#dep_registers)*
                #(#register_builtins;)*
                #(#graphix_files;)*
                root_mods.insert(#p::arcstr::literal!(#package_name));
                ctx.env
                    .package_roots
                    .insert(#p::arcstr::literal!(#package_name));
                Ok(())
            }

            fn maybe_init_custom<'a>(
                &'a self,
                gx: &'a #p::graphix_rt::GXHandle<X>,
                env: &'a #p::graphix_compiler::env::Env,
                e: #p::graphix_rt::CompExp<X>,
                run_on_main: &'a ::graphix_package::MainThreadHandle,
            ) -> ::std::pin::Pin<
                Box<
                    dyn ::std::future::Future<
                        Output = #p::anyhow::Result<::graphix_package::CustomResult<X>>,
                    > + 'a,
                >,
            > {
                #maybe_init_custom
            }

            #main_program
        }

        #test_harness
    }
    .into()
}

/// Build `Vec<Box<dyn Package<_>>>` from the calling crate's `graphix-package-*`
/// dependencies (core first). Optional deps are gated on the feature of the
/// same short name. Use in a typed position (`stdlib_packages::<X>()`,
/// `.add_packages(...)`) so `_` resolves.
#[proc_macro]
pub fn packages(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let c = Crate::load();
    let pushes: Vec<TokenStream> = c
        .runtime()
        .map(|Dep { name: short, optional, .. }| {
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
/// `graphix-package-*` dependencies (core first). Only for crates whose graphix
/// deps are all non-optional: array elements cannot be `#[cfg]`-gated.
/// Use as `const X: &[&dyn Package<NoExt>] = graphix_package::package_refs!();`.
#[proc_macro]
pub fn package_refs(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let c = Crate::load();
    let refs: Vec<TokenStream> = c
        .runtime()
        .map(|d| {
            let crate_ident = package_crate_ident(&d.name);
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
