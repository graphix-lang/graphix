#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use graphix_compiler::{
    Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect, errf, expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_package_core::{FireOnce, ProgramArgs};
use immutable_chunkmap::map::Map as CMap;
use netidx::subscriber::Value;
use netidx_core::pack::{Pack, PackError};
use netidx_derive::{FromValue, IntoValue};
use netidx_value::ValArray;
use poolshark::local::LPooled;

#[derive(FromValue)]
enum Kind {
    Positional,
    Option,
    Flag,
}

#[derive(FromValue)]
struct Arg {
    name: ArcStr,
    kind: Kind,
    short: Option<ArcStr>,
    help: Option<ArcStr>,
    default: Option<ArcStr>,
    required: Option<bool>,
}

#[derive(FromValue)]
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
    once: FireOnce,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Parse {
    // Not replayable, so it must not be `Sync`.
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "args_parse";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { once: FireOnce::default(), out: TagValue::phantom() }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(Self { once: FireOnce::decode(buf)?, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Parse {
    fn image_len(&self) -> usize {
        self.once.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.once.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let Some(tv) = graphix_package_core::seam_tick(from[0].update(ctx, event)) else {
            return self.out.ride();
        };
        let spec = tv.value_cloned();
        if !self.once.take() {
            return self.out.ride();
        }

        let spec = match spec.cast_to::<Command>() {
            Ok(spec) => spec,
            Err(e) => {
                let v = errf!("ArgError", "{e}");
                return self.out.set(TagValue::fired(v));
            }
        };
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
                struct Fields {
                    command: ValArray,
                    values: Value,
                }
                let command = ValArray::from_iter_exact(command_chain.drain(..));
                Fields { command, values: Value::Map(values) }.into()
            }
            Err(e) => errf!("ArgError", "{e}"),
        };
        self.out.set(TagValue::fired(res))
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.once.reset();
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

graphix_derive::defpackage! {
    builtins => [
        Parse,
    ],
}
