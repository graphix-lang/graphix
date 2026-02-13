use cargo_toml::Manifest;
use proc_macro2::TokenStream;
use quote::quote;
use std::{
    env,
    path::{Component, PathBuf},
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
    Manifest::from_path(&*PROJECT_ROOT).expect("failed to load cargo manifest")
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
    let md = std::fs::metadata(&*GRAPHIX_SRC)
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
        if #p::NAME.contains(|c: char| c != '_' && !c.is_ascii_alphanumeric()) {
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
    check_invariants();
    let input = parse_macro_input!(input as DefPackage);
    let register_builtins = register_builtins(&input.builtins);
    let is_custom = is_custom(&input.is_custom);
    let init_custom = init_custom(&input.init_custom);
    let graphix_files = graphix_files();

    quote! {
        struct P;

        impl<X: ::graphix_rt::GXExt> ::graphix_package::Package<X> for P {
            fn register(
                ctx: &mut ExecCtx<GXRt<X>, X::UserEvent>,
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
