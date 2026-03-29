#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use graphix_compiler::{
    errf, expr::ExprId, typ::FnType, Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope,
    UserEvent,
};
use graphix_package_core::ProgramArgs;
use immutable_chunkmap::map::Map as CMap;
use netidx::subscriber::Value;
use netidx_value::ValArray;

// ── Value helpers ─────────────────────────────────────────────

fn get_field<'a>(v: &'a Value, name: &str) -> Option<&'a Value> {
    match v {
        Value::Array(a) => {
            for pair in a.iter() {
                if let Value::Array(kv) = pair {
                    if kv.len() == 2 {
                        if let Value::String(k) = &kv[0] {
                            if &**k == name {
                                return Some(&kv[1]);
                            }
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn get_str(v: &Value) -> Option<&ArcStr> {
    match v {
        Value::String(s) => Some(s),
        _ => None,
    }
}

fn get_opt_str(v: &Value) -> Option<Option<&ArcStr>> {
    match v {
        Value::Null => Some(None),
        Value::String(s) => Some(Some(s)),
        _ => None,
    }
}

fn get_opt_bool(v: &Value) -> Option<Option<bool>> {
    match v {
        Value::Null => Some(None),
        Value::Bool(b) => Some(Some(*b)),
        _ => None,
    }
}

fn get_variant_tag(v: &Value) -> Option<&ArcStr> {
    match v {
        Value::String(s) => Some(s),
        Value::Array(a) if !a.is_empty() => {
            if let Value::String(s) = &a[0] {
                Some(s)
            } else {
                None
            }
        }
        _ => None,
    }
}

// ── Build clap from spec ──────────────────────────────────────

fn build_clap_arg(spec: &Value) -> Result<clap::Arg, String> {
    let name = get_field(spec, "name").and_then(get_str).ok_or("arg missing name")?;
    let kind =
        get_field(spec, "kind").and_then(get_variant_tag).ok_or("arg missing kind")?;
    let short =
        get_field(spec, "short").and_then(get_opt_str).ok_or("arg missing short")?;
    let help = get_field(spec, "help").and_then(get_opt_str).ok_or("arg missing help")?;
    let default =
        get_field(spec, "default").and_then(get_opt_str).ok_or("arg missing default")?;
    let required = get_field(spec, "required")
        .and_then(get_opt_bool)
        .ok_or("arg missing required")?;

    let name_owned: String = name.to_string();
    let mut arg = clap::Arg::new(name_owned.clone());

    if let Some(h) = help {
        arg = arg.help(h.to_string());
    }

    match &**kind {
        "Positional" => {
            if let Some(true) = required {
                arg = arg.required(true);
            }
        }
        "Option" => {
            arg = arg.long(name_owned);
            if let Some(s) = short {
                if let Some(c) = s.chars().next() {
                    arg = arg.short(c);
                }
            }
            if let Some(true) = required {
                arg = arg.required(true);
            }
        }
        "Flag" => {
            arg = arg.long(name_owned).action(clap::ArgAction::SetTrue);
            if let Some(s) = short {
                if let Some(c) = s.chars().next() {
                    arg = arg.short(c);
                }
            }
        }
        other => return Err(format!("unknown arg kind: {other}")),
    }

    if let Some(d) = default {
        arg = arg.default_value(d.to_string());
    }

    Ok(arg)
}

fn build_clap_command(spec: &Value) -> Result<clap::Command, String> {
    let name = get_field(spec, "name").and_then(get_str).ok_or("command missing name")?;
    let version = get_field(spec, "version")
        .and_then(get_opt_str)
        .ok_or("command missing version")?;
    let about =
        get_field(spec, "about").and_then(get_opt_str).ok_or("command missing about")?;

    let mut cmd = clap::Command::new(name.to_string());

    if let Some(v) = version {
        cmd = cmd.version(v.to_string());
    }
    if let Some(a) = about {
        cmd = cmd.about(a.to_string());
    }

    if let Some(Value::Array(args)) = get_field(spec, "args") {
        for arg_spec in args.iter() {
            cmd = cmd.arg(build_clap_arg(arg_spec)?);
        }
    }

    if let Some(Value::Array(subs)) = get_field(spec, "subcommands") {
        for sub_spec in subs.iter() {
            cmd = cmd.subcommand(build_clap_command(sub_spec)?);
        }
    }

    Ok(cmd)
}

// ── Extract matches ───────────────────────────────────────────

fn extract_matches(
    matches: &clap::ArgMatches,
    spec: &Value,
    command_chain: &mut Vec<Value>,
    values: &mut CMap<Value, Value, 32>,
) {
    if let Some(Value::Array(args)) = get_field(spec, "args") {
        for arg_spec in args.iter() {
            let Some(name) = get_field(arg_spec, "name").and_then(get_str) else {
                continue;
            };
            let kind = get_field(arg_spec, "kind").and_then(get_variant_tag);
            let key = Value::String(name.clone());
            let val = match kind.map(|s| &**s) {
                Some("Flag") => {
                    let set = matches.get_flag(&**name);
                    Value::String(ArcStr::from(if set { "true" } else { "false" }))
                }
                _ => match matches.get_one::<String>(&**name) {
                    Some(s) => Value::String(ArcStr::from(s.as_str())),
                    None => Value::Null,
                },
            };
            *values = values.insert(key, val).0;
        }
    }

    if let Some((sub_name, sub_matches)) = matches.subcommand() {
        command_chain.push(Value::String(ArcStr::from(sub_name)));
        if let Some(Value::Array(subs)) = get_field(spec, "subcommands") {
            for sub_spec in subs.iter() {
                if let Some(sn) = get_field(sub_spec, "name").and_then(get_str) {
                    if &**sn == sub_name {
                        extract_matches(sub_matches, sub_spec, command_chain, values);
                        break;
                    }
                }
            }
        }
    }
}

// ── Parse builtin ─────────────────────────────────────────────

#[derive(Debug)]
struct Parse {
    fired: bool,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Parse {
    const NAME: &str = "args_parse";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { fired: false }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Parse {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let spec = from[0].update(ctx, event)?;
        if self.fired {
            return None;
        }
        self.fired = true;

        let cmd = match build_clap_command(&spec) {
            Ok(c) => c,
            Err(e) => return Some(errf!("ArgError", "{e}")),
        };

        let pargs = ctx.libstate.get_or_default::<ProgramArgs>();
        // argv[0] is the script filename — clap consumes it as the binary name
        let raw: Vec<&str> = pargs.0.iter().map(|s| s.as_str()).collect();

        match cmd.try_get_matches_from(raw) {
            Ok(matches) => {
                let mut command_chain = Vec::new();
                let mut values = CMap::new();
                extract_matches(&matches, &spec, &mut command_chain, &mut values);
                let command_arr =
                    Value::Array(ValArray::from_iter_exact(command_chain.drain(..)));
                let result: Value = (
                    (ArcStr::from("command"), command_arr),
                    (ArcStr::from("values"), Value::Map(values)),
                )
                    .into();
                Some(result)
            }
            Err(e) => Some(errf!("ArgError", "{e}")),
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.fired = false;
    }
}

graphix_derive::defpackage! {
    builtins => [
        Parse,
    ],
}
