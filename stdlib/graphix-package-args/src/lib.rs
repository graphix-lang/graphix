#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use clap::error::ErrorKind;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue, effects::Effect,
    errf, expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_package_core::ProgramArgs;
use immutable_chunkmap::map::Map as CMap;
use netidx::subscriber::Value;
use netidx_core::pack::PackError;
use netidx_derive::{FromValue, IntoValue};
use netidx_value::ValArray;
use poolshark::local::LPooled;

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
enum Kind {
    Positional,
    Option,
    Flag,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct Arg {
    name: ArcStr,
    kind: Kind,
    short: Option<ArcStr>,
    help: Option<ArcStr>,
    default: Option<ArcStr>,
    required: Option<bool>,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct Command {
    name: ArcStr,
    version: Option<ArcStr>,
    about: Option<ArcStr>,
    args: LPooled<Vec<Arg>>,
    subcommands: LPooled<Vec<Command>>,
}

fn build_clap_arg(spec: &Arg) -> clap::Arg {
    let name_owned: String = spec.name.to_string();
    let mut arg = clap::Arg::new(name_owned.clone());

    if let Some(h) = &spec.help {
        arg = arg.help(h.to_string());
    }

    match spec.kind {
        Kind::Positional => {
            if let Some(true) = spec.required {
                arg = arg.required(true);
            }
        }
        Kind::Option => {
            arg = arg.long(name_owned);
            if let Some(s) = &spec.short {
                if let Some(c) = s.chars().next() {
                    arg = arg.short(c);
                }
            }
            if let Some(true) = spec.required {
                arg = arg.required(true);
            }
        }
        Kind::Flag => {
            arg = arg.long(name_owned).action(clap::ArgAction::SetTrue);
            if let Some(s) = &spec.short {
                if let Some(c) = s.chars().next() {
                    arg = arg.short(c);
                }
            }
        }
    }

    if let Some(d) = &spec.default {
        arg = arg.default_value(d.to_string());
    }

    arg
}

/// What clap would refuse with a panic (debug builds) or take wrongly (a
/// later match unreachable, a value misread): a name empty, starting with
/// `-`, or used twice, whatever the kinds, clap's own help/version
/// included; a short of more than one char or claimed twice; a required
/// positional after an optional one; a subcommand named help or twice.
/// A subcommand may not reuse a name an enclosing command uses: every
/// level's values land in one map (`ParseResult.values`).
fn check_spec(spec: &Command) -> Result<(), CompactString> {
    check_level(spec, &mut LPooled::take())
}

/// `spec`, under enclosing commands whose arg names are `enclosing`.
fn check_level<'a>(
    spec: &'a Command,
    enclosing: &mut LPooled<Vec<&'a str>>,
) -> Result<(), CompactString> {
    let mut names: LPooled<Vec<&str>> = LPooled::take();
    let mut shorts: LPooled<Vec<char>> = LPooled::take();
    names.push("help");
    shorts.push('h');
    if spec.version.is_some() {
        names.push("version");
        shorts.push('V');
    }
    let mut optional_positional = false;
    for a in spec.args.iter() {
        let name = &*a.name;
        if name.is_empty() || name.starts_with('-') {
            return Err(format_compact!("{name:?} is not an argument name"));
        }
        if enclosing.contains(&name) {
            return Err(format_compact!("{name} is taken by an enclosing command"));
        }
        if names.contains(&name) {
            return Err(format_compact!("{name} is taken"));
        }
        names.push(name);
        if let Some(s) = &a.short {
            let mut cs = s.chars();
            let c = match (cs.next(), cs.next()) {
                (Some(c), None) if c != '-' => c,
                _ => {
                    return Err(format_compact!(
                        "{name}: #short {s:?} is not one character"
                    ));
                }
            };
            if shorts.contains(&c) {
                return Err(format_compact!("{name}: -{c} is taken"));
            }
            shorts.push(c);
        }
        if let Kind::Positional = a.kind {
            match a.required {
                Some(true) if optional_positional => {
                    return Err(format_compact!(
                        "{name}: a required positional follows an optional one"
                    ));
                }
                Some(true) => (),
                _ => optional_positional = true,
            }
        }
    }
    let depth = enclosing.len();
    enclosing.extend(spec.args.iter().map(|a| &*a.name));
    for (i, sub) in spec.subcommands.iter().enumerate() {
        if sub.name == "help" {
            return Err(format_compact!("help is clap's own subcommand"));
        }
        if spec.subcommands[..i].iter().any(|s| s.name == sub.name) {
            return Err(format_compact!("subcommand {} is named twice", sub.name));
        }
        check_level(sub, enclosing)?;
    }
    enclosing.truncate(depth);
    Ok(())
}

fn build_clap_command(spec: &Command) -> clap::Command {
    let mut cmd = clap::Command::new(spec.name.to_string());

    if let Some(v) = &spec.version {
        cmd = cmd.version(v.to_string());
    }
    if let Some(a) = &spec.about {
        cmd = cmd.about(a.to_string());
    }

    for arg_spec in spec.args.iter() {
        cmd = cmd.arg(build_clap_arg(arg_spec));
    }

    for sub_spec in spec.subcommands.iter() {
        cmd = cmd.subcommand(build_clap_command(sub_spec));
    }

    cmd
}

fn extract_matches(
    matches: &clap::ArgMatches,
    spec: &Command,
    command_chain: &mut Vec<Value>,
    values: &mut CMap<Value, Value, 32>,
) {
    for arg_spec in spec.args.iter() {
        let name = &arg_spec.name;
        let key = Value::String(name.clone());
        let val = match arg_spec.kind {
            Kind::Flag => {
                let set = matches.get_flag(&**name);
                Value::String(ArcStr::from(if set { "true" } else { "false" }))
            }
            Kind::Positional | Kind::Option => match matches.get_one::<String>(&**name) {
                Some(s) => Value::String(ArcStr::from(s.as_str())),
                None => Value::Null,
            },
        };
        *values = values.insert(key, val).0;
    }

    if let Some((sub_name, sub_matches)) = matches.subcommand() {
        command_chain.push(Value::String(ArcStr::from(sub_name)));
        if let Some(sub_spec) = spec.subcommands.iter().find(|s| &*s.name == sub_name) {
            extract_matches(sub_matches, sub_spec, command_chain, values);
        }
    }
}

#[derive(Debug)]
struct Parse {
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for Parse {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "args_parse";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self { out: TagValue::phantom() }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        _buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Self { out: TagValue::phantom() }))
    }
}

impl<R: Rt> Apply<R> for Parse {
    fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let Some(tv) = graphix_package_core::seam_tick(from[0].update(ctx)) else {
            return self.out.ride();
        };
        let spec = tv.value_cloned();
        let spec = match spec.cast_to::<Command>() {
            Ok(spec) => spec,
            Err(e) => {
                let v = errf!("ArgError", "{e}");
                return self.out.set(TagValue::fired(v));
            }
        };
        if let Err(e) = check_spec(&spec) {
            return self.out.set(TagValue::fired(errf!("ArgError", "{e}")));
        }
        let cmd = build_clap_command(&spec);

        let pargs = ctx.libstate.get_or_default::<ProgramArgs>();
        // argv[0] is the script filename — clap consumes it as the binary name
        let raw: Vec<&str> = pargs.0.iter().map(|s| s.as_str()).collect();

        let res = match cmd.try_get_matches_from(raw) {
            Ok(matches) => {
                let mut command_chain = Vec::new();
                let mut values = CMap::new();
                extract_matches(&matches, &spec, &mut command_chain, &mut values);
                #[derive(IntoValue)]
                #[value(shape = "graphix_compiler::rep::GraphixShape")]
                struct Fields {
                    command: ValArray,
                    values: Value,
                }
                let command = ValArray::from_iter_exact(command_chain.drain(..));
                Fields { command, values: Value::Map(values) }.into()
            }
            Err(e) => match e.kind() {
                ErrorKind::DisplayHelp | ErrorKind::DisplayVersion => {
                    errf!("ArgHelp", "{e}")
                }
                _ => errf!("ArgError", "{e}"),
            },
        };
        self.out.set(TagValue::fired(res))
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}
}

graphix_derive::defpackage! {
    builtins => [
        Parse,
    ],
}
