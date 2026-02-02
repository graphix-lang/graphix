use cargo_toml::Manifest;
use graphix_compiler::expr::ModPath;
use proc_macro2::{token_stream, Delimiter, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, ToTokens};
use std::{env, fs, path::PathBuf, str::FromStr, sync::LazyLock};
use syn::{
    parse_macro_input, parse_quote, AttrStyle, Attribute, Data, DeriveInput, Field,
    Fields, GenericParam, Ident, Index, LitStr,
};

#[derive(Clone, Copy)]
enum ProjectType {
    Standalone,
    Package,
}

static PROJECT_ROOT: LazyLock<PathBuf> = LazyLock::new(|| {
    env::var("CARGO_MANIFEST_DIR").expect("missing manifest dir").into()
});

static CARGO_MANIFEST: LazyLock<Manifest> = LazyLock::new(|| {
    Manifest::from_path(&*PROJECT_ROOT).expect("failed to load cargo manifest")
});

static CRATE_NAME: LazyLock<String> =
    LazyLock::new(|| env::var("CARGO_CRATE_NAME").expect("missing crate name"));

static PACKAGE_NAME: LazyLock<String> =
    LazyLock::new(|| match CRATE_NAME.strip_prefix("graphix_package_") {
        Some(name) => name.into(),
        None => CRATE_NAME.clone(),
    });

static PROJECT_TYPE: LazyLock<ProjectType> = LazyLock::new(|| {
    let typ = if CRATE_NAME.starts_with("graphix_package_") {
        if !CARGO_MANIFEST.bin.is_empty() {
            panic!("graphix package crates may not have binary targets")
        }
        if !CARGO_MANIFEST.lib.is_some() {
            panic!("graphix package crates must have a lib target")
        }
        ProjectType::Package
    } else {
        if CARGO_MANIFEST.bin.is_empty() {
            panic!("graphix standalone apps must have a binary target")
        }
        ProjectType::Standalone
    };
    let md = fs::metadata(&PROJECT_ROOT.join("graphix-src"))
        .expect("graphix projects must have a graphix-src directory");
    if !md.is_dir() {
        panic!("graphix projects must have a graphix-src directory")
    }
    typ
});

enum DefBuiltinInput {
    TwoArg { name: syn::LitStr, typ: syn::LitStr },
    ThreeArg { modpath: syn::LitStr, name: syn::LitStr, typ: syn::LitStr },
}

impl syn::parse::Parse for DefBuiltinInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitStr) {
            let first: syn::LitStr = input.parse()?;
            input.parse::<syn::Token![,]>()?;
            let second: syn::LitStr = input.parse()?;

            if input.peek(syn::Token![,]) {
                input.parse::<syn::Token![,]>()?;
                let third: syn::LitStr = input.parse()?;
                Ok(DefBuiltinInput::ThreeArg { modpath: first, name: second, typ: third })
            } else {
                Ok(DefBuiltinInput::TwoArg { name: first, typ: second })
            }
        } else {
            Err(lookahead.error())
        }
    }
}

/// define the required information for a graphix built-in function given the
/// type and module. This macro reads the Cargo.toml to determine what type of
/// project the built-in resides in, a package or a standalone binary, and
/// scopes type refs and the built-in name accordingly. If you are placing your
/// built-in in a sub module of the root package then you must provide the sub
/// module modpath to this function. e.g.
///
/// defbuiltin!("submod::subsubmod", "builtin_name", "fn(i64) -> i64")
///
/// Otherwise you can call defbuiltin! with just the name and the type. The
/// package name and the sub module will be prepended to the name you provide,
/// so there is usually no need to worry about names conflicting with other
/// packages, for example,
///
/// defbuiltin!("submod::subsubmod", "name", "fn(Number) -> Number")
///
/// will generate a NAME of "packagename_submod_subsubmod_name", ref types in
/// the type definition will be scoped to packagename::submod::subsubmod
#[proc_macro]
pub fn defbuiltin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DefBuiltinInput);
    let _ = &*PROJECT_TYPE; // force check project invariants
    let (submod, name, typ) = match input {
        DefBuiltinInput::TwoArg { name, typ } => (None, name, typ),
        DefBuiltinInput::ThreeArg { modpath, name, typ } => (Some(modpath), name, typ),
    };
    let name = match &submod {
        None => LitStr::new(&format!("{}_{}", &*PACKAGE_NAME, name.value()), name.span()),
        Some(sm) => LitStr::new(
            &format!("{}_{}_{}", &*PACKAGE_NAME, sm.value(), name.value()),
            sm.span(),
        ),
    };
    let scope = match &submod {
        None => LitStr::new(&*PACKAGE_NAME, Span::call_site()),
        Some(sm) => {
            LitStr::new(&format!("{}::{}", &*PACKAGE_NAME, sm.value()), Span::call_site())
        }
    };
    // check invariants at compile time
    let _ = ModPath::from_str(&scope.value());
    let _ = graphix_compiler::expr::parser::parse_fn_type(&typ.value())
        .expect("invalid graphix type");
    quote! {
        const NAME: &str = #name;
        const TYP: ::std::sync::LazyLock<graphix_compiler::typ::FnType> =
            ::std::sync::LazyLock::new(|| {
                let scope = graphix_compiler::expr::ModPath::from_str(#scope);
                graphix_compiler::expr::parser::parse_fn_type(#typ)
                    .expect("failed to parse fn type {s}")
                    .scope_refs(&scope)
            });
    }
    .into()
}

/// register this packages' graphix dependencies as defined in Cargo.toml
#[proc_macro]
pub fn register_deps(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    todo!()
}
