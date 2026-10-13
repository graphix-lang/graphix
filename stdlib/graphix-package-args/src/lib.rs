#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use clap::error::ErrorKind;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
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

/// What clap would refuse with a panic (debug builds) or take wrongly (a
/// later match unreachable): a short of more than one char, a short or
/// long claimed twice or by clap's own help/version flags, a subcommand
/// named help.
// CR claude for claude: [bug/medium] check_spec misses id clashes that clap panics on.
// It checks longs only among options and flags, so these all get through: a positional
// and a flag or option with one name, two positionals with one name, a positional named
// help (or version when #version is set), a #name that is empty or starts with '-', and
// a subcommand name used twice. clap's debug_asserts panic on each of these inside
// Parse::update. Nothing catches the panic, so the runtime dies ("Error: runtime did
// not respond", exit 1) where the caller should get an `ArgError`. In release the debug
// asserts are off, but the flag-plus-positional case still panics in extract_matches:
// get_flag downcasts the positional's String to bool. Fix: keep one set of ids seeded
// with help (and version when set), add every arg's name whatever its kind, and refuse
// a duplicate. Refuse an empty name or one starting with '-'. Refuse a repeated
// subcommand name. Fix the doc comment above to match. probe:
// design/review-2026-10-12/repro/x-panics-04.gx (x-panics-04)
// CR claude for claude: [bug/medium] check_spec misses shapes that clap's debug_asserts
// refuse, so these specs still panic a tokio worker in a debug build and kill the
// runtime. In a release build clap takes the first match without any check. The missed
// shapes are: a positional and an option with the same name, two positionals with the
// same name, two subcommands with the same name at one level, and a non-required
// positional before a required one. An empty arg name is also accepted. Fix: put every
// arg's name, of every kind, in one set. Return an ArgError for duplicate subcommand
// names, an empty name, and a non-required positional before a required one. Running
// clap's `Command::debug_assert()` on the built command under catch_unwind would also
// keep the check from falling behind clap. Add a pin for each refused shape. probe:
// design/review-2026-10-12/repro/x-fix-stdlib-05.sh (x-fix-stdlib-05)
fn check_spec(spec: &Command) -> Result<(), CompactString> {
    let mut shorts: LPooled<Vec<char>> = LPooled::take();
    let mut longs: LPooled<Vec<&str>> = LPooled::take();
    shorts.push('h');
    longs.push("help");
    if spec.version.is_some() {
        shorts.push('V');
        longs.push("version");
    }
    for a in spec.args.iter() {
        if let Some(s) = &a.short {
            let mut cs = s.chars();
            let c = match (cs.next(), cs.next()) {
                (Some(c), None) if c != '-' => c,
                _ => {
                    return Err(format_compact!(
                        "{}: #short {s:?} is not one character",
                        a.name
                    ));
                }
            };
            if shorts.contains(&c) {
                return Err(format_compact!("{}: -{c} is taken", a.name));
            }
            shorts.push(c);
        }
        // CR claude for claude: [bug/medium] check_spec checks only shorts and the
        // longs of options and flags. Clap also refuses other specs, and they reach
        // build_clap_command unchecked: an arg id used twice across any kinds (two
        // positionals `x`, or a positional and an option both named `x`), a positional
        // named `help`, or `version` when #version is set; two subcommands with one
        // name; and an optional positional before a required one. In a debug build each
        // one panics in clap's debug_asserts inside Parse::update, and the shell
        // reports "runtime did not respond". In release the asserts are compiled out,
        // so clap takes the spec silently. Fix: keep one per-command set of every arg
        // id, pre-seeded with help (and version when set), and refuse duplicates. Also
        // refuse duplicate subcommand names and a required positional after an optional
        // one. Add a graphix-tests pin for each ArgError refusal; there are none today.
        // probe: design/review-2026-10-12/repro/small-pkgs-03.sh (small-pkgs-03)
        if !matches!(a.kind, Kind::Positional) {
            if longs.contains(&&*a.name) {
                return Err(format_compact!("--{} is taken", a.name));
            }
            longs.push(&a.name);
        }
    }
    for sub in spec.subcommands.iter() {
        if sub.name == "help" {
            return Err(format_compact!("help is clap's own subcommand"));
        }
        check_spec(sub)?;
    }
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
        // CR claude for claude: [bug/medium] Every command level's args go into one
        // map: the parent's first, then the matched subcommand's. An absent option is
        // inserted as null and an unset flag as "false". So when a subcommand declares
        // an arg with a parent arg's name, the subcommand's value replaces the parent's
        // even when the subcommand did not receive it. `prog --level 1 sub` returns
        // values{"level"} == null and silently drops the user's --level 1. Fix: have
        // check_spec refuse an arg name that an ancestor command already uses, so the
        // flat map stays unambiguous. Also document in ParseResult (args/mod.gxi and
        // book/src/stdlib/args.md) that values merge every level and that flags come
        // back as the strings "true"/"false". probe:
        // design/review-2026-10-12/repro/gx-stdlib-03.sh (gx-stdlib-03)
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
    const EFFECT: Effect = Effect::Async;
    const NAME: &str = "args_parse";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { once: FireOnce::default(), out: TagValue::phantom() }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(Self { once: FireOnce::decode(buf)?, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Parse {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.once.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let Some(tv) = graphix_package_core::seam_tick(from[0].update(ctx)) else {
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

    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.once.reset();
    }
}

graphix_derive::defpackage! {
    builtins => [
        Parse,
    ],
}
