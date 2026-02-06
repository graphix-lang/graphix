use arcstr::ArcStr;
use cargo_toml::Manifest;
use fxhash::FxHashSet;
use graphix_compiler::expr::ModPath;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use proc_macro2::{token_stream, Delimiter, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, ToTokens};
use std::{
    env,
    fs::{self, FileType},
    path::{Component, PathBuf},
    str::FromStr,
    sync::{atomic::AtomicU64, LazyLock},
};
use syn::{
    parse_macro_input, parse_quote,
    punctuated::{Pair, Punctuated},
    token::{self, Comma},
    AttrStyle, Attribute, Data, DeriveInput, ExprClosure, Field, Fields, GenericParam,
    Ident, Index, LitStr, Pat, PathSegment, Result, Token, TypeParamBound,
};

static PROJECT_ROOT: LazyLock<PathBuf> = LazyLock::new(|| {
    env::var("CARGO_MANIFEST_DIR").expect("missing manifest dir").into()
});

static GRAPHIX_SRC: LazyLock<PathBuf> =
    LazyLock::new(|| PROJECT_ROOT.join("src").join("graphix"));

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

/*
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

/// if the type already has generic parameters constrained to the traits we need
/// for BuiltIn, then extract those names, otherwise None
fn extract_trait_param_names(
    params: &Punctuated<GenericParam, Comma>,
) -> (Option<Ident>, Option<Ident>) {
    let mut r_name: Option<Ident> = None;
    let mut e_name: Option<Ident> = None;
    for g in params {
        match g {
            GenericParam::Type(t) if t.bounds.len() == 1 => match t.bounds.first() {
                Some(TypeParamBound::Trait(tb)) => match tb.path.segments.last() {
                    Some(seg) if seg.ident == Ident::new("Rt", seg.ident.span()) => {
                        r_name = Some(t.ident.clone())
                    }
                    Some(seg)
                        if seg.ident == Ident::new("UserEvent", seg.ident.span()) =>
                    {
                        e_name = Some(t.ident.clone())
                    }
                    None | Some(_) => (),
                },
                None | Some(_) => (),
            },
            _ => (),
        }
    }
    (r_name, e_name)
}

fn unique_ident() -> Ident {
    use std::sync::atomic::Ordering;
    static N: AtomicU64 = AtomicU64::new(0);
    Ident::new(&format!("TY__{}", N.fetch_add(1, Ordering::Relaxed)), Span::call_site())
}

static TO_REGISTER: LazyLock<Mutex<FxHashSet<String>>> =
    LazyLock::new(|| Mutex::new(FxHashSet::default()));

#[proc_macro_derive(BuiltIn, attributes(builtin))]
pub fn derive_builtin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let mut for_impl = input.generics.clone();
    let (impl_generics, ty_generics, where_clause, r_name, e_name) = {
        let (mut r_name, mut e_name) = extract_trait_param_names(&input.generics.params);
        if r_name == None {
            let rn = unique_ident();
            for_impl.params.push(parse_quote!(#rn: ::graphix_compiler::Rt));
            r_name = Some(rn);
        }
        if e_name == None {
            let en = unique_ident();
            for_impl.params.push(parse_quote!(#en: ::graphix_compiler::UserEvent));
            e_name = Some(en);
        }
        let (_, ty_generics, where_clause) = input.generics.split_for_impl();
        let (impl_generics, _, _) = for_impl.split_for_impl();
        (impl_generics, ty_generics, where_clause, r_name.unwrap(), e_name.unwrap())
    };
    let args = BuiltInArgs::from(&*input.attrs);
    match &*PROJECT_TYPE {
        ProjectType::Package => (),
        ProjectType::Standalone => panic!("you must define builtins in a package"),
    }
    let package_name = &*PACKAGE_NAME;
    // check invariants at compile time, if possible
    match &args.typ {
        TokenTree::Literal(l) => {
            graphix_compiler::expr::parser::parse_fn_type(&l.to_string())
                .expect("invalid graphix type");
        }
        _ => (),
    }
    let fn_name = args.name;
    let fn_typ = args.typ;
    let ty_name = input.ident;
    if !TO_REGISTER.lock().insert(ty_name.to_string()) {
        panic!(
            "builtin names must be unique in a package {ty_name} is used more than once"
        )
    }
    quote! {
        impl #impl_generics ::graphix_compiler::BuiltIn<#r_name, #e_name> for #ty_name #ty_generics #where_clause {
            const NAME: &str = ::const_format::concatcp!(#package_name, "_", #fn_name);
            const TYP: ::std::sync::LazyLock<graphix_compiler::typ::FnType> =
                ::std::sync::LazyLock::new(|| {
                    ::graphix_compiler::expr::parser::parse_fn_type(#fn_typ)
                        .expect("failed to parse fn type {s}")
                });
            const INIT: ::graphix_compiler::BuiltInInitFn<R, E> = Self::init;
        }
    }
    .into()
}
*/

/* example
defpackage! {
    builtins => [
        Foo
        submod::Bar,
        Baz<R, E>
    ],
    is_custom => |gx, env, e| {
        todo!()
    },
    init_custom => |gx, env, stop, e| {
        todo!()
    }
}
*/
struct DefPackage {
    builtins: Vec<syn::Path>,
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
                    .parse_terminated(syn::Path::parse, Token![,])?
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
    let md = fs::metadata(&*GRAPHIX_SRC)
        .expect("graphix projects must have a graphix-src directory");
    if !md.is_dir() {
        panic!("graphix projects must have a graphix-src directory")
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
        if ext != Some("gx") || ext != Some("gxi") {
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
            if modules.contains_key(path) {
                bail!("duplicate graphix module {path}")
            }
            modules.insert(path, ::arcstr::literal!(include_str!(#compiler_path)))
        })
    }
    res
}

fn register_builtins(builtins: &[syn::Path]) -> Vec<TokenStream> {
    let package_name = &*PACKAGE_NAME;
    builtins.iter().map(|p| quote! {
        if #p::NAME.contains(|c| c != '_' && !c.is_ascii_alphanumeric()) {
            bail!("invalid builtin name {}, must contain only ascii alphanumeric and _", #p::NAME)
        }
        if !#p::NAME.starts_with(#package_name) {
            bail!("invalid builtin {} name must start with package name {}", #p::NAME, #package_name)
        }
        ctx.register_builtin::<#p>()?
    }).collect()
}

fn check_args(name: &str, mut req: Vec<&'static str>, args: &Punctuated<Pat, Comma>) {
    for arg in args.pairs() {
        match arg {
            Pair::End(_) => {
                if !req.is_empty() {
                    panic!("{name} missing required arguments {req:?}")
                }
            }
            Pair::Punctuated(i, _) => {
                if req.is_empty() {
                    panic!("{name} unexpected argument")
                }
                match i {
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
        }
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
    let input = parse_macro_input!(input as DefPackage);
    let register_builtins = register_builtins(&input.builtins);
    let is_custom = is_custom(&input.is_custom);
    let init_custom = init_custom(&input.init_custom);
    let graphix_files = graphix_files();

    quote! {
        struct P;

        impl<X: ::graphix_rt::GXExt> ::graphix_package::Package<X> for P {
            fn register(
                ctx: ExecCtx<GXRt<X>, X::UserEvent>,
                modules: &mut FxHashMap<netidx_core::path::Path, ArcStr>,
            ) -> Result<()> {
                #(#register_builtins);*
                #(#graphix_files);*
                Ok(())
            }

            #[allow(unused)]
            fn is_custom(gx: &GXHandle<X>, env: &Env, e: &CompExp<X>) -> bool {
                #is_custom
            }

            #[allow(unused)]
            fn init_custom(
                gx: &GXHandle<X>,
                env: &Env,
                stop: oneshot::Sender<()>,
                e: &CompExp<X>,
            ) -> Result<Box<dyn CustomDisplay<X>>> {
                #init_custom
            }
        }
    }
    .into()
}
