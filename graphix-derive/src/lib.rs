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
    let _ = &*PROJECT_TYPE; // force check project invariants
    let deps = CARGO_MANIFEST
        .dependencies
        .keys()
        .filter_map(|name| {
            if name.starts_with("graphix-package-")
                || name.starts_with("graphix_package_")
            {
                Some(name.replace("-", "_"))
            } else {
                None
            }
        })
        .map(|name| {
            quote! {}
        });
    todo!()
}

fn parse_attr<R, F: FnMut(Ident, token_stream::IntoIter) -> R>(
    att: &Attribute,
    mut f: F,
) -> Option<R> {
    match att.style {
        AttrStyle::Inner(_) => None,
        AttrStyle::Outer => match att.path().segments.iter().next() {
            None => None,
            Some(seg) => match seg.ident.to_string().as_str() {
                "builtin" => {
                    let tokens = att.meta.require_list().unwrap().tokens.clone();
                    let mut iter = tokens.into_iter();
                    match iter.next() {
                        Some(TokenTree::Ident(i)) => Some(f(i, iter)),
                        None | Some(_) => None,
                    }
                }
                _ => None,
            },
        },
    }
}

struct BuiltInArgs {
    typ: TokenTree,
    name: TokenTree,
}

impl From<&[Attribute]> for BuiltInArgs {
    fn from(attrs: &[Attribute]) -> Self {
        macro_rules! expect_equal {
            ($iter:expr) => {
                match $iter.next() {
                    Some(TokenTree::Punct(p)) if p.as_char() == '=' => (),
                    _ => panic!("invalid attribute syntax, expected key = value"),
                }
            };
        }
        let mut typ: Option<TokenTree> = None;
        let mut name: Option<TokenTree> = None;
        for a in attrs {
            parse_attr(a, |ident, mut iter| match ident.to_string().as_str() {
                "name" => {
                    if name.is_some() {
                        panic!("name attribute specified multiple times")
                    }
                    expect_equal!(iter);
                    let token =
                        iter.next().expect("invalid name attribute, expected argument");
                    match token {
                        TokenTree::Literal(_) | TokenTree::Ident(_) => {
                            name = Some(token);
                        }
                        _ => panic!("invalid argument to name attribute"),
                    }
                }
                "type" => {
                    if typ.is_some() {
                        panic!("type attribute specified more than once")
                    }
                    expect_equal!(iter);
                    let token =
                        iter.next().expect("invalid type attribute, expected argument");
                    match token {
                        TokenTree::Literal(_) | TokenTree::Ident(_) => {
                            typ = Some(token);
                        }
                        _ => panic!("invalid argument to type attribute"),
                    }
                }
                _ => {}
            });
        }
        if name.is_none() {
            panic!("missing required attribute builtin(name = ?)")
        }
        if typ.is_none() {
            panic!("missing required attribute builtin(type = ?)")
        }
        Self { name: name.unwrap(), typ: typ.unwrap() }
    }
}

#[proc_macro_derive(BuiltIn, attributes(builtin))]
pub fn derive_builtin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let args = BuiltInArgs::from(&*input.attrs);
    match &*PROJECT_TYPE {
        ProjectType::Package => (),
        ProjectType::Standalone => panic!("you must define builtins in a package"),
    }
    let package_name = *PACKAGE_NAME;
    // check invariants at compile time
    let _ = graphix_compiler::expr::parser::parse_fn_type(&args.typ)
        .expect("invalid graphix type");
    // CR estokes: fix generics on BuiltInInitFn
    quote! {
        impl #impl_generics ::graphix_compiler::BuiltIn for #name #ty_generics #where_clause {
            const NAME: &str = ::const_format::concatcp!(#package_name, "_", #args.name);
            const TYP: ::std::sync::LazyLock<graphix_compiler::typ::FnType> =
                ::std::sync::LazyLock::new(|| {
                    ::graphix_compiler::expr::parser::parse_fn_type(#args.typ)
                        .expect("failed to parse fn type {s}")
                });
            const INIT: ::graphix_compiler::BuiltInInitFn<R, E> = Self::init;
        }
    }
    .into()
}
