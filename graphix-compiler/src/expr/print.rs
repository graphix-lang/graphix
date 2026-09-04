use super::Sig;
use crate::{
    expr::{
        ApplyExpr, Attr, BindExpr, BindSig, Decorations, Doc, Expr, ExprKind, ImplExpr,
        LambdaExpr, ModuleKind, Sandbox, SelectExpr, SigItem, SigKind, StructExpr,
        StructWithExpr, TraitExpr, TraitMethod, TypeDefBody, TypeDefExpr, UseItem,
        parser,
    },
    typ::Type,
};
use compact_str::format_compact;
use netidx_core::{path::Path, utils::Either};
use netidx_value::{Value, parser::VAL_ESC};
use poolshark::local::LPooled;
use std::fmt::{self, Formatter, Write};

fn pretty_print_exprs_int<'a, A, F: Fn(&'a A) -> &'a Expr>(
    buf: &mut PrettyBuf,
    exprs: &'a [A],
    open: &str,
    close: &str,
    sep: &str,
    f: F,
) -> fmt::Result {
    if exprs.is_empty() {
        return writeln!(buf, "{open}{close}");
    }
    writeln!(buf, "{}", open)?;
    buf.with_indent::<fmt::Result, _>(2, |buf| {
        for i in 0..exprs.len() {
            f(&exprs[i]).fmt_pretty(buf)?;
            if i < exprs.len() - 1 {
                buf.kill_newline();
                writeln!(buf, "{}", sep)?
            }
        }
        Ok(())
    })?;
    writeln!(buf, "{}", close)
}

fn pretty_print_exprs(
    buf: &mut PrettyBuf,
    exprs: &[Expr],
    open: &str,
    close: &str,
    sep: &str,
) -> fmt::Result {
    pretty_print_exprs_int(buf, exprs, open, close, sep, |a| a)
}

/// A body laid out inline after its head (`|x| {`, `=> {`, `catch(e) {`):
/// a block prints as the block, anything else as itself. The body's own
/// decorations are the caller's to place — above the head for a select
/// arm, here for the rest.
fn pretty_body(
    buf: &mut PrettyBuf,
    body: &ExprKind,
    open: &str,
    close: &str,
    sep: &str,
) -> fmt::Result {
    match body {
        ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, open, close, sep),
        body => body.fmt_pretty(buf),
    }
}

/// The lines above a decorated expression — its comments, then its
/// attributes — ahead of whatever the decorations were captured before:
/// the expression itself, or the select-arm pattern or struct field
/// name it follows.
pub(crate) fn write_leading(
    f: &mut impl fmt::Write,
    dec: &Option<Box<Decorations>>,
) -> fmt::Result {
    if let Some(dec) = dec {
        for c in dec.comments.iter() {
            writeln!(f, "//{c}")?;
        }
        for a in dec.attrs.iter() {
            writeln!(f, "{a}")?;
        }
    }
    Ok(())
}

#[derive(Debug)]
pub struct PrettyBuf {
    pub indent: usize,
    pub limit: usize,
    pub buf: LPooled<String>,
}

impl PrettyBuf {
    pub fn new(limit: usize) -> Self {
        Self { indent: 0, limit, buf: LPooled::take() }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn newline(&self) -> bool {
        self.buf.chars().next_back().map(|c| c == '\n').unwrap_or(true)
    }

    pub fn push_indent(&mut self) {
        if self.newline() {
            self.buf.extend((0..self.indent).into_iter().map(|_| ' '));
        }
    }

    pub fn with_indent<R, F: FnOnce(&mut Self) -> R>(&mut self, inc: usize, f: F) -> R {
        self.indent += inc;
        let r = f(self);
        self.indent -= inc;
        r
    }

    pub fn kill_newline(&mut self) {
        if let Some('\n') = self.buf.chars().next_back() {
            self.buf.pop();
        }
    }
}

impl fmt::Write for PrettyBuf {
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.push_indent();
        self.buf.write_char(c)
    }

    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_indent();
        self.buf.write_str(s)
    }

    fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> fmt::Result {
        self.push_indent();
        self.buf.write_fmt(args)
    }
}

pub trait PrettyDisplay: fmt::Display {
    /// Do the actual pretty print. This should not be called directly, it will
    /// be called by fmt_pretty when we know it can't fit on a single line.
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result;

    /// This is the user facing fmt method, it will first try to format the
    /// expression on a single line, and if that is impossible it will call the
    /// pretty printer.
    fn fmt_pretty(&self, buf: &mut PrettyBuf) -> fmt::Result {
        use fmt::Write;
        let start = buf.len();
        let col = start - buf.buf.rfind('\n').map_or(0, |i| i + 1);
        writeln!(buf, "{}", self)?;
        // The fit check is best-effort: col accounts for the line's existing
        // prefix, embedded newlines overcount, and a long token can exceed
        // any limit. Printer policy: perfection isn't possible — fix layouts
        // case-by-case when they obviously look bad.
        if col + buf.len() - start - 1 <= buf.limit {
            return Ok(());
        } else {
            buf.buf.truncate(start);
            self.fmt_pretty_inner(buf)
        }
    }

    /// Pretty print to a pooled string
    fn to_string_pretty(&self, limit: usize) -> LPooled<String> {
        let mut buf = PrettyBuf::new(limit);
        self.fmt_pretty(&mut buf).unwrap();
        buf.buf
    }
}

impl fmt::Display for Doc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(doc) = self.0.as_ref() {
            if doc == "" {
                writeln!(f, "///")?;
            } else {
                for line in doc.lines() {
                    writeln!(f, "///{line}")?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for Attr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.args.is_empty() {
            write!(f, "#[{}]", self.name)
        } else {
            write!(f, "#[{}(", self.name)?;
            for (i, a) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{a}")?;
            }
            write!(f, ")]")
        }
    }
}

impl PrettyDisplay for Doc {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if let Some(doc) = self.0.as_ref() {
            if doc == "" {
                writeln!(buf, "///")?;
            } else {
                for line in doc.lines() {
                    writeln!(buf, "///{line}")?;
                }
            }
        }
        Ok(())
    }
}

impl TypeDefExpr {
    fn write_name_and_params(&self, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "type {}", self.name)?;
        if !self.params.is_empty() {
            write!(f, "<")?;
            for (i, (tv, ct)) in self.params.iter().enumerate() {
                write!(f, "{tv}")?;
                if let Some(ct) = ct {
                    write!(f, ": {ct}")?;
                }
                if i < self.params.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl fmt::Display for TypeDefExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_name_and_params(f)?;
        match &self.body {
            TypeDefBody::Abstract(None) => Ok(()),
            TypeDefBody::Abstract(Some(rep)) => write!(f, " = Abstract<{rep}>"),
            TypeDefBody::Alias(typ) => write!(f, " = {typ}"),
        }
    }
}

impl PrettyDisplay for TypeDefExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.write_name_and_params(buf)?;
        match &self.body {
            TypeDefBody::Abstract(None) => Ok(()),
            TypeDefBody::Abstract(Some(rep)) => write!(buf, " = Abstract<{rep}>"),
            TypeDefBody::Alias(typ) => {
                writeln!(buf, " =")?;
                buf.with_indent(2, |buf| typ.fmt_pretty(buf))
            }
        }
    }
}

impl fmt::Display for TraitMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}val {}: {}", self.doc, self.name, self.typ)?;
        match &self.default {
            None => Ok(()),
            Some(d) => write!(f, " = {d}"),
        }
    }
}

impl PrettyDisplay for TraitMethod {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        write!(buf, "{}val {}: ", self.doc, self.name)?;
        self.typ.fmt_pretty(buf)?;
        match &self.default {
            None => Ok(()),
            Some(d) => {
                buf.kill_newline();
                write!(buf, " = ")?;
                buf.with_indent(2, |buf| d.fmt_pretty(buf))
            }
        }
    }
}

impl fmt::Display for TraitExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "trait {} {{", self.name)?;
        for (i, m) in self.methods.iter().enumerate() {
            write!(f, " {m}")?;
            if i < self.methods.len() - 1 {
                write!(f, ";")?;
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for TraitExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        writeln!(buf, "trait {} {{", self.name)?;
        buf.with_indent(2, |buf| {
            for (i, m) in self.methods.iter().enumerate() {
                m.fmt_pretty(buf)?;
                if i < self.methods.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

impl ImplExpr {
    fn write_head(&self, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "impl")?;
        if !self.params.is_empty() {
            write!(f, "<")?;
            for (i, tv) in self.params.iter().enumerate() {
                write!(f, "{tv}")?;
                let mut first = true;
                for (ctv, c) in self.constraints.iter() {
                    if ctv.name == tv.name {
                        write!(f, "{}{c}", if first { ": " } else { " + " })?;
                        first = false;
                    }
                }
                if i < self.params.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }
        write!(f, " {} for {}", self.trait_name, self.target)
    }
}

impl fmt::Display for ImplExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.write_head(f)?;
        if self.methods.is_empty() {
            return Ok(());
        }
        write!(f, " {{")?;
        for (i, m) in self.methods.iter().enumerate() {
            write!(f, " {m}")?;
            if i < self.methods.len() - 1 {
                write!(f, ";")?;
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for ImplExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.write_head(buf)?;
        if self.methods.is_empty() {
            return writeln!(buf);
        }
        writeln!(buf, " {{")?;
        buf.with_indent(2, |buf| {
            for (i, m) in self.methods.iter().enumerate() {
                m.fmt_pretty(buf)?;
                if i < self.methods.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

impl fmt::Display for Sandbox {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        macro_rules! write_sandbox {
            ($kind:literal, $l:expr) => {{
                write!(f, "sandbox {} [ ", $kind)?;
                for (i, p) in $l.iter().enumerate() {
                    if i < $l.len() - 1 {
                        write!(f, "{}, ", p)?
                    } else {
                        write!(f, "{}", p)?
                    }
                }
                write!(f, " ]")
            }};
        }
        match self {
            Sandbox::Unrestricted => write!(f, "sandbox unrestricted"),
            Sandbox::Blacklist(l) => write_sandbox!("blacklist", l),
            Sandbox::Whitelist(l) => write_sandbox!("whitelist", l),
        }
    }
}

impl PrettyDisplay for Sandbox {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        macro_rules! write_sandbox {
            ($kind:literal, $l:expr) => {{
                writeln!(buf, "sandbox {} [ ", $kind)?;
                buf.with_indent::<fmt::Result, _>(2, |buf| {
                    for (i, p) in $l.iter().enumerate() {
                        if i < $l.len() - 1 {
                            writeln!(buf, "{}, ", p)?
                        } else {
                            writeln!(buf, "{}", p)?
                        }
                    }
                    Ok(())
                })?;
                write!(buf, " ]")
            }};
        }
        match self {
            Sandbox::Blacklist(l) => write_sandbox!("blacklist", l),
            Sandbox::Whitelist(l) => write_sandbox!("whitelist", l),
            Sandbox::Unrestricted => writeln!(buf, "sandbox unrestricted"),
        }
    }
}

impl fmt::Display for BindSig {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "val {}: {}", self.name, self.typ)
    }
}

impl PrettyDisplay for BindSig {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        write!(buf, "val {}: ", self.name)?;
        self.typ.fmt_pretty(buf)
    }
}

impl fmt::Display for SigItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.doc)?;
        match &self.kind {
            SigKind::TypeDef(td) => write!(f, "{td}"),
            SigKind::Trait(t) => write!(f, "{t}"),
            SigKind::Impl(i) => write!(f, "{i}"),
            SigKind::Bind(bind) => write!(f, "{bind}"),
            SigKind::Module(name) => write!(f, "mod {name}"),
            SigKind::Use { reexport, names } => write_use_names(f, *reexport, names),
        }
    }
}

impl PrettyDisplay for SigItem {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        write!(buf, "{}", self.doc)?;
        match &self.kind {
            SigKind::Bind(b) => b.fmt_pretty(buf),
            SigKind::TypeDef(d) => d.fmt_pretty(buf),
            SigKind::Trait(t) => t.fmt_pretty(buf),
            SigKind::Impl(i) => i.fmt_pretty(buf),
            SigKind::Module(name) => writeln!(buf, "mod {name}"),
            SigKind::Use { reexport, names } => {
                write_use_names(buf, *reexport, names)?;
                writeln!(buf)
            }
        }
    }
}

impl fmt::Display for Sig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.toplevel {
            write!(f, "sig {{ ")?;
        }
        for (i, si) in self.iter().enumerate() {
            write!(f, "{si}")?;
            if i < self.len() - 1 {
                write!(f, "; ")?
            }
        }
        if !self.toplevel {
            write!(f, " }}")?
        }
        Ok(())
    }
}

impl PrettyDisplay for Sig {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if !self.toplevel {
            writeln!(buf, "sig {{")?;
        }
        buf.with_indent(2, |buf| {
            for (i, si) in self.iter().enumerate() {
                si.fmt_pretty(buf)?;
                if i < self.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ";")?
                }
            }
            Ok(())
        })?;
        if !self.toplevel {
            writeln!(buf, "}}")?
        }
        Ok(())
    }
}

impl fmt::Display for BindExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let BindExpr { rec, pattern, typ, value } = self;
        let rec = if *rec { " rec" } else { "" };
        match typ {
            None => write!(f, "let{rec} {pattern} = {value}"),
            Some(typ) => write!(f, "let{rec} {pattern}: {typ} = {value}"),
        }
    }
}

impl PrettyDisplay for BindExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let BindExpr { rec, pattern, typ, value } = self;
        let rec = if *rec { " rec" } else { "" };
        match typ {
            None => writeln!(buf, "let{rec} {pattern} = ")?,
            Some(typ) => writeln!(buf, "let{rec} {pattern}: {typ} = ")?,
        }
        buf.with_indent(2, |buf| value.fmt_pretty(buf))
    }
}

impl fmt::Display for StructWithExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { source, replace } = self;
        match &source.kind {
            ExprKind::Ref { .. } => write!(f, "{{ {source} with ")?,
            _ => write!(f, "{{ ({source}) with ")?,
        }
        for (i, (name, e)) in replace.iter().enumerate() {
            write_leading(f, &e.dec)?;
            match &e.kind {
                ExprKind::Ref { name: n }
                    if Path::dirname(&**n).is_none()
                        && Path::basename(&**n) == Some(&**name)
                        && !parser::RESERVED_BINDING.contains(&name.as_str()) =>
                {
                    write!(f, "{name}")?
                }
                e => write!(f, "{name}: {e}")?,
            }
            if i < replace.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for StructWithExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Self { source, replace } = self;
        match &source.kind {
            ExprKind::Ref { .. } => writeln!(buf, "{{ {source} with")?,
            _ => writeln!(buf, "{{ ({source}) with")?,
        }
        buf.with_indent::<fmt::Result, _>(2, |buf| {
            for (i, (name, e)) in replace.iter().enumerate() {
                write_leading(buf, &e.dec)?;
                match &e.kind {
                    ExprKind::Ref { name: n }
                        if Path::dirname(&**n).is_none()
                            && Path::basename(&**n) == Some(&**name)
                            && !parser::RESERVED_BINDING.contains(&name.as_str()) =>
                    {
                        write!(buf, "{name}")?
                    }
                    e => {
                        write!(buf, "{name}: ")?;
                        buf.with_indent(2, |buf| e.fmt_pretty(buf))?
                    }
                }
                if i < replace.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ",")?
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

impl fmt::Display for StructExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { args } = self;
        write!(f, "{{ ")?;
        for (i, (n, e)) in args.iter().enumerate() {
            write_leading(f, &e.dec)?;
            match &e.kind {
                ExprKind::Ref { name }
                    if Path::dirname(&**name).is_none()
                        && Path::basename(&**name) == Some(&**n)
                        && !parser::RESERVED_BINDING.contains(&n.as_str()) =>
                {
                    write!(f, "{n}")?
                }
                e => write!(f, "{n}: {e}")?,
            }
            if i < args.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for StructExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Self { args } = self;
        writeln!(buf, "{{")?;
        buf.with_indent::<fmt::Result, _>(2, |buf| {
            for (i, (n, e)) in args.iter().enumerate() {
                write_leading(buf, &e.dec)?;
                match &e.kind {
                    ExprKind::Ref { name }
                        if Path::dirname(&**name).is_none()
                            && Path::basename(&**name) == Some(&**n)
                            && !parser::RESERVED_BINDING.contains(&n.as_str()) =>
                    {
                        write!(buf, "{n}")?
                    }
                    e => {
                        write!(buf, "{n}: ")?;
                        buf.with_indent(2, |buf| e.fmt_pretty(buf))?;
                    }
                }
                if i < args.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ", ")?
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

/// Whether `e` can be printed as the bare source/function of a postfix
/// operator (`.field`, `.N`, `[i]`, `{k}`, `(args)`) without enclosing parens.
/// True exactly for the identifier and postfix-chain nodes: the parser folds
/// postfix left, so `a.b.c` round-trips to `StructRef(StructRef(a,b),c)`. Any
/// other source (binary op, constant, literal, qop, …) must be parenthesized —
/// e.g. `(a+b).c` would otherwise mis-associate and `(42).0` would lex as a
/// float.
pub(super) fn prints_as_bare_postfix(e: &Expr) -> bool {
    matches!(
        &e.kind,
        ExprKind::Ref { .. }
            | ExprKind::StructRef { .. }
            | ExprKind::TupleRef { .. }
            | ExprKind::ArrayRef { .. }
            | ExprKind::ArraySlice { .. }
            | ExprKind::MapRef { .. }
            | ExprKind::Apply(_)
    )
}

impl fmt::Display for ApplyExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { args, function } = self;
        if prints_as_bare_postfix(function) {
            write!(f, "{function}")?
        } else {
            write!(f, "({function})")?
        }
        write!(f, "(")?;
        for i in 0..args.len() {
            match &args[i].0 {
                None => write!(f, "{}", &args[i].1)?,
                Some(name) => match &args[i].1.kind {
                    ExprKind::Ref { name: n }
                        if Path::dirname(&n.0).is_none()
                            && Path::basename(&n.0) == Some(name.as_str()) =>
                    {
                        write!(f, "#{name}")?
                    }
                    _ => write!(f, "#{name}: {}", &args[i].1)?,
                },
            }
            if i < args.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, ")")
    }
}

impl PrettyDisplay for ApplyExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Self { args, function } = self;
        if prints_as_bare_postfix(function) {
            function.fmt_pretty(buf)?
        } else {
            write!(buf, "(")?;
            function.fmt_pretty(buf)?;
            buf.kill_newline();
            write!(buf, ")")?;
        }
        buf.kill_newline();
        writeln!(buf, "(")?;
        buf.with_indent::<fmt::Result, _>(2, |buf| {
            for i in 0..args.len() {
                match &args[i].0 {
                    None => args[i].1.fmt_pretty(buf)?,
                    Some(name) => match &args[i].1.kind {
                        ExprKind::Ref { name: n }
                            if Path::dirname(&n.0).is_none()
                                && Path::basename(&n.0) == Some(name.as_str()) =>
                        {
                            writeln!(buf, "#{name}")?
                        }
                        _ => {
                            write!(buf, "#{name}: ")?;
                            buf.with_indent(2, |buf| args[i].1.fmt_pretty(buf))?
                        }
                    },
                }
                if i < args.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ",")?
                }
            }
            Ok(())
        })?;
        writeln!(buf, ")")
    }
}

impl fmt::Display for LambdaExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let LambdaExpr { args, vargs, rtype, constraints, throws, body } = self;
        for (i, (tvar, typ)) in constraints.iter().enumerate() {
            write!(f, "{tvar}: {typ}")?;
            if i < constraints.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "|")?;
        for (i, a) in args.iter().enumerate() {
            match &a.labeled {
                None => {
                    write!(f, "{}", a.pattern)?;
                    if let Some(t) = &a.constraint {
                        write!(f, ": {t}")?
                    }
                }
                Some(def) => {
                    write!(f, "#{}", a.pattern)?;
                    if let Some(t) = &a.constraint {
                        write!(f, ": {t}")?
                    }
                    if let Some(def) = def {
                        write!(f, " = {def}")?;
                    }
                }
            }
            if vargs.is_some() || i < args.len() - 1 {
                write!(f, ", ")?
            }
        }
        if let Some(typ) = vargs {
            match typ {
                None => write!(f, "@args")?,
                Some(typ) => write!(f, "@args: {typ}")?,
            }
        }
        write!(f, "| ")?;
        if let Some(t) = rtype {
            match t {
                Type::Fn(ft) => write!(f, "-> ({ft}) ")?,
                Type::ByRef(t) => match &**t {
                    Type::Fn(ft) => write!(f, "-> &({ft}) ")?,
                    t => write!(f, "-> &{t} ")?,
                },
                t => write!(f, "-> {t} ")?,
            }
        }
        if let Some(t) = throws {
            write!(f, "throws {t} ")?
        }
        match body {
            Either::Right(builtin) => write!(f, "'{builtin}"),
            Either::Left(body) => write!(f, "{body}"),
        }
    }
}

impl PrettyDisplay for LambdaExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let LambdaExpr { args, vargs, rtype, constraints, throws, body } = self;
        for (i, (tvar, typ)) in constraints.iter().enumerate() {
            write!(buf, "{tvar}: {typ}")?;
            if i < constraints.len() - 1 {
                write!(buf, ", ")?;
            }
        }
        write!(buf, "|")?;
        for (i, a) in args.iter().enumerate() {
            match &a.labeled {
                None => {
                    write!(buf, "{}", a.pattern)?;
                    if let Some(typ) = &a.constraint {
                        write!(buf, ": {typ}")?;
                    }
                }
                Some(def) => {
                    write!(buf, "#{}", a.pattern)?;
                    if let Some(t) = &a.constraint {
                        write!(buf, ": {t}")?
                    }
                    if let Some(def) = def {
                        write!(buf, " = {def}")?;
                    }
                }
            }
            if vargs.is_some() || i < args.len() - 1 {
                write!(buf, ", ")?
            }
        }
        if let Some(typ) = vargs {
            write!(buf, "@args")?;
            if let Some(t) = typ {
                write!(buf, ": {t}")?
            }
        }
        write!(buf, "| ")?;
        if let Some(t) = rtype {
            match t {
                Type::Fn(ft) => write!(buf, "-> ({ft}) ")?,
                Type::ByRef(t) => match &**t {
                    Type::Fn(ft) => write!(buf, "-> &({ft}) ")?,
                    t => write!(buf, "-> &{t} ")?,
                },
                t => write!(buf, "-> {t} ")?,
            }
        }
        if let Some(t) = throws {
            write!(buf, "throws {t} ")?
        }
        match body {
            Either::Right(builtin) => {
                writeln!(buf, "'{builtin}")
            }
            Either::Left(body) => {
                write_leading(buf, &body.dec)?;
                pretty_body(buf, &body.kind, "{", "}", ";")
            }
        }
    }
}

impl fmt::Display for SelectExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let SelectExpr { arg, arms } = self;
        write!(f, "select {arg} {{")?;
        for (i, (pat, rhs)) in arms.iter().enumerate() {
            write_leading(f, &rhs.dec)?;
            if let Some(tp) = &pat.type_predicate {
                write!(f, "{tp} as ")?;
            }
            write!(f, "{} ", pat.structure_predicate)?;
            if let Some(guard) = &pat.guard {
                write!(f, "if {guard} ")?;
            }
            write!(f, "=> {}", rhs.kind)?;
            if i < arms.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, "}}")
    }
}

impl PrettyDisplay for SelectExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let SelectExpr { arg, arms } = self;
        write!(buf, "select ")?;
        arg.fmt_pretty(buf)?;
        buf.kill_newline();
        writeln!(buf, " {{")?;
        buf.with_indent(2, |buf| {
            for (i, (pat, expr)) in arms.iter().enumerate() {
                write_leading(buf, &expr.dec)?;
                if let Some(tp) = &pat.type_predicate {
                    write!(buf, "{tp} as ")?;
                }
                write!(buf, "{} ", pat.structure_predicate)?;
                if let Some(guard) = &pat.guard {
                    write!(buf, "if ")?;
                    buf.with_indent(2, |buf| guard.fmt_pretty(buf))?;
                    buf.kill_newline();
                    write!(buf, " ")?;
                }
                write!(buf, "=> ")?;
                let last = i == arms.len() - 1;
                let term = if last { "}" } else { "}," };
                buf.with_indent(2, |buf| pretty_body(buf, &expr.kind, "{", term, ";"))?;
                if !last && !matches!(expr.kind, ExprKind::Do { .. }) {
                    buf.kill_newline();
                    writeln!(buf, ",")?
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

impl PrettyDisplay for ExprKind {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        macro_rules! binop {
            ($sep:literal, $lhs:expr, $rhs:expr) => {{
                writeln!(buf, "{} {}", $lhs, $sep)?;
                $rhs.fmt_pretty(buf)
            }};
        }
        match self {
            ExprKind::Constant(_)
            | ExprKind::NoOp
            | ExprKind::Use { .. }
            | ExprKind::Ref { .. }
            | ExprKind::StructRef { .. }
            | ExprKind::TupleRef { .. }
            | ExprKind::TypeDef { .. }
            | ExprKind::ArrayRef { .. }
            | ExprKind::MapRef { .. }
            | ExprKind::ArraySlice { .. }
            | ExprKind::StringInterpolate { .. }
            | ExprKind::Module {
                name: _,
                value: ModuleKind::Unresolved { .. } | ModuleKind::Resolved { .. },
            } => {
                writeln!(buf, "{self}")
            }
            ExprKind::ExplicitParens(e) => {
                writeln!(buf, "(")?;
                buf.with_indent(2, |buf| e.fmt_pretty(buf))?;
                writeln!(buf, ")")
            }
            ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, "{", "}", ";"),
            ExprKind::Seq { trigger, body } => {
                write!(buf, "seq ")?;
                if let Some(t) = trigger {
                    t.fmt_pretty(buf)?;
                    buf.kill_newline();
                    write!(buf, " ")?;
                }
                pretty_print_exprs(buf, body, "{", "}", ";")
            }
            ExprKind::Until(e) => {
                write!(buf, "until ")?;
                e.fmt_pretty(buf)
            }
            ExprKind::SeqDo { body } => pretty_print_exprs(buf, body, "do {", "}", ";"),
            ExprKind::Array { args } => pretty_print_exprs(buf, args, "[", "]", ","),
            ExprKind::List { args } => pretty_print_exprs(buf, args, "[<", ">]", ","),
            ExprKind::Tuple { args } => pretty_print_exprs(buf, args, "(", ")", ","),
            ExprKind::Bind(b) => b.fmt_pretty(buf),
            ExprKind::Trait(t) => t.fmt_pretty(buf),
            ExprKind::Impl(i) => i.fmt_pretty(buf),
            ExprKind::StructWith(sw) => sw.fmt_pretty(buf),
            ExprKind::Module {
                name,
                value: ModuleKind::Dynamic { sandbox, sig, source },
            } => {
                writeln!(buf, "mod {name} dynamic {{")?;
                buf.with_indent(2, |buf| {
                    sandbox.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                    sig.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                    write!(buf, "source ")?;
                    buf.with_indent(2, |buf| source.fmt_pretty(buf))?;
                    buf.kill_newline();
                    writeln!(buf, ";")
                })?;
                writeln!(buf, "}}")
            }
            ExprKind::Connect { name, value, deref } => {
                let deref = if *deref { "*" } else { "" };
                writeln!(buf, "{deref}{name} <- ")?;
                buf.with_indent(2, |buf| value.fmt_pretty(buf))
            }
            ExprKind::TypeCast { expr, typ } => {
                writeln!(buf, "cast<{typ}>(")?;
                buf.with_indent(2, |buf| expr.fmt_pretty(buf))?;
                writeln!(buf, ")")
            }
            ExprKind::Map { args } => {
                writeln!(buf, "{{")?;
                buf.with_indent::<fmt::Result, _>(2, |buf| {
                    for (i, (k, v)) in args.iter().enumerate() {
                        writeln!(buf, "{k} => {v}")?;
                        if i < args.len() - 1 {
                            buf.kill_newline();
                            writeln!(buf, ",")?
                        }
                    }
                    Ok(())
                })?;
                writeln!(buf, "}}")
            }
            ExprKind::Any { args } => {
                write!(buf, "any")?;
                pretty_print_exprs(buf, args, "(", ")", ",")
            }
            ExprKind::Never { typ, args } => {
                match typ {
                    Some(t) => write!(buf, "never<{t}>")?,
                    None => write!(buf, "never")?,
                }
                pretty_print_exprs(buf, args, "(", ")", ",")
            }
            ExprKind::Variant { tag: _, args } if args.len() == 0 => {
                write!(buf, "{self}")
            }
            ExprKind::Variant { tag, args } => {
                write!(buf, "`{tag}")?;
                pretty_print_exprs(buf, args, "(", ")", ",")
            }
            ExprKind::Construct { name, arg } => {
                write!(buf, "{name}")?;
                pretty_print_exprs(buf, std::slice::from_ref(&**arg), "(", ")", ",")
            }
            ExprKind::Struct(st) => st.fmt_pretty(buf),
            ExprKind::Qop(e) => {
                e.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, "?")
            }
            ExprKind::OrNever(e) => {
                e.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, "$")
            }
            ExprKind::Catch(c) => {
                match &c.constraint {
                    None => write!(buf, "catch({}) ", c.bind)?,
                    Some(t) => write!(buf, "catch({}: {t}) ", c.bind)?,
                }
                write_leading(buf, &c.handler.dec)?;
                pretty_body(buf, &c.handler.kind, "{", "}", "; ")
            }
            ExprKind::Apply(ae) => ae.fmt_pretty(buf),
            ExprKind::Lambda(l) => l.fmt_pretty(buf),
            ExprKind::Eq { lhs, rhs } => binop!("==", lhs, rhs),
            ExprKind::Ne { lhs, rhs } => binop!("!=", lhs, rhs),
            ExprKind::Lt { lhs, rhs } => binop!("<", lhs, rhs),
            ExprKind::Gt { lhs, rhs } => binop!(">", lhs, rhs),
            ExprKind::Lte { lhs, rhs } => binop!("<=", lhs, rhs),
            ExprKind::Gte { lhs, rhs } => binop!(">=", lhs, rhs),
            ExprKind::And { lhs, rhs } => binop!("&&", lhs, rhs),
            ExprKind::Or { lhs, rhs } => binop!("||", lhs, rhs),
            ExprKind::Add { lhs, rhs } => binop!("+", lhs, rhs),
            ExprKind::CheckedAdd { lhs, rhs } => binop!("+?", lhs, rhs),
            ExprKind::Sub { lhs, rhs } => binop!("-", lhs, rhs),
            ExprKind::CheckedSub { lhs, rhs } => binop!("-?", lhs, rhs),
            ExprKind::Mul { lhs, rhs } => binop!("*", lhs, rhs),
            ExprKind::CheckedMul { lhs, rhs } => binop!("*?", lhs, rhs),
            ExprKind::Div { lhs, rhs } => binop!("/", lhs, rhs),
            ExprKind::CheckedDiv { lhs, rhs } => binop!("/?", lhs, rhs),
            ExprKind::Mod { lhs, rhs } => binop!("%", lhs, rhs),
            ExprKind::CheckedMod { lhs, rhs } => binop!("%?", lhs, rhs),
            ExprKind::Sample { lhs, rhs } => binop!("~", lhs, rhs),
            ExprKind::StrictSample { lhs, rhs } => binop!("~!", lhs, rhs),
            ExprKind::Not { expr } => match &expr.kind {
                ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, "!{", "}", ";"),
                _ => {
                    write!(buf, "!")?;
                    expr.fmt_pretty(buf)
                }
            },
            ExprKind::ByRef(e) => {
                write!(buf, "&")?;
                e.fmt_pretty(buf)
            }
            ExprKind::Deref(e) => {
                write!(buf, "*")?;
                buf.with_indent(2, |buf| e.fmt_pretty(buf))
            }
            ExprKind::Neg(e) => {
                write!(buf, "-")?;
                e.fmt_pretty(buf)
            }
            ExprKind::Select(se) => se.fmt_pretty(buf),
        }
    }
}

/// Print a use statement's names in GROUPED form: a single path plain
/// (`use a::b`), several grouped under their longest common prefix
/// (`use a::{b, c::d}`, a name equal to the prefix rendering as
/// `self`), and prefixless groups bare (`use {a, b}` — the degenerate
/// hand-built case; the parser accepts it). The parser accepts both
/// grouped and ungrouped input; printing always regroups.
fn write_use_names<W: fmt::Write>(
    f: &mut W,
    reexport: bool,
    names: &[UseItem],
) -> fmt::Result {
    use netidx_core::path::Path;
    if reexport {
        write!(f, "pub ")?;
    }
    write!(f, "use ")?;
    if names.len() == 1 {
        return write!(f, "{}", names[0]);
    }
    let segs: Vec<Vec<&str>> =
        names.iter().map(|n| Path::parts(&n.path.0).collect()).collect();
    let mut lcp = 0;
    'lcp: loop {
        let Some(first) = segs.first().and_then(|s| s.get(lcp)) else {
            break;
        };
        // never absorb a glob's `*` into the prefix: an empty suffix
        // prints as `self`, which is not a glob
        if *first == "*" {
            break;
        }
        for s in segs[1..].iter() {
            if s.get(lcp) != Some(first) {
                break 'lcp;
            }
        }
        lcp += 1;
    }
    for (i, part) in segs[0][..lcp].iter().enumerate() {
        if i > 0 {
            write!(f, "::")?;
        }
        write!(f, "{part}")?;
    }
    if lcp > 0 {
        write!(f, "::")?;
    }
    write!(f, "{{")?;
    for (i, (sg, item)) in segs.iter().zip(names.iter()).enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        let suffix = &sg[lcp..];
        if suffix.is_empty() {
            write!(f, "self")?;
        } else {
            for (j, part) in suffix.iter().enumerate() {
                if j > 0 {
                    write!(f, "::")?;
                }
                write!(f, "{part}")?;
            }
        }
        if let Some(n) = &item.rename {
            write!(f, " as {n}")?;
        }
    }
    write!(f, "}}")
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        crate::stack::ensure_sufficient(|| self.fmt_inner(f))
    }
}

impl ExprKind {
    fn fmt_inner(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print_exprs(
            f: &mut fmt::Formatter,
            exprs: &[Expr],
            open: &str,
            close: &str,
            sep: &str,
        ) -> fmt::Result {
            write!(f, "{open}")?;
            for i in 0..exprs.len() {
                write!(f, "{}", &exprs[i])?;
                if i < exprs.len() - 1 {
                    write!(f, "{sep}")?
                }
            }
            write!(f, "{close}")
        }
        // Multiline strings print in the form a human would write them:
        // a constant with newlines as a raw string (`r#"…"#`, verbatim
        // — hash count one past the longest `#` run following any `"`
        // in the content), an interpolation with newlines as a
        // triple-quoted template. Content with control characters
        // other than \n/\t (raw) or \n/\t/\r/\0 (triple) keeps the
        // escaped single-line form — escapes are the only readable
        // spelling for those anyway. Both forms reparse to the
        // identical AST (adjacent literal parts merge in the parser).
        fn raw_printable(s: &str) -> bool {
            s.contains('\n')
                && !s.chars().any(|c| c.is_control() && c != '\n' && c != '\t')
        }
        fn raw_hashes(s: &str) -> usize {
            let mut n = 0;
            let mut run: Option<usize> = None;
            for c in s.chars() {
                match (c, &mut run) {
                    ('"', _) => run = Some(0),
                    ('#', Some(r)) => {
                        *r += 1;
                        n = n.max(*r);
                    }
                    _ => {
                        if let Some(r) = run.take() {
                            n = n.max(r);
                        }
                    }
                }
            }
            if let Some(r) = run {
                n = n.max(r);
            }
            if s.contains('"') { n + 1 } else { 0 }
        }
        fn write_raw(f: &mut fmt::Formatter<'_>, s: &str) -> fmt::Result {
            let n = raw_hashes(s);
            write!(f, "r")?;
            for _ in 0..n {
                write!(f, "#")?;
            }
            write!(f, "\"{s}\"")?;
            for _ in 0..n {
                write!(f, "#")?;
            }
            Ok(())
        }
        fn triple_printable(args: &[Expr]) -> bool {
            let mut any_nl = false;
            for a in args {
                if let ExprKind::Constant(Value::String(s)) = &a.kind {
                    if s.contains('\n') {
                        any_nl = true;
                    }
                    if s.chars().any(|c| {
                        c.is_control() && c != '\n' && c != '\t' && c != '\r' && c != '\0'
                    }) {
                        return false;
                    }
                }
            }
            any_nl
        }
        // One literal part of a triple template. Brackets are PLAIN
        // CONTENT in templates (splices are the marked thing, `\[e]`),
        // so they print bare. A `"` prints bare
        // unless it would touch another quote (within the part, the
        // next part's first char, or the closing delimiter) — those
        // print `\"` so no unescaped `"""` can form. The very first
        // content char must not be a real newline (the parser strips
        // one there), so it prints as the `\n` escape.
        fn write_triple_lit(
            f: &mut fmt::Formatter<'_>,
            s: &str,
            first_content: bool,
            next_starts_quote: bool,
            is_final: bool,
        ) -> fmt::Result {
            let chars: Vec<char> = s.chars().collect();
            for (i, c) in chars.iter().enumerate() {
                let last = i + 1 == chars.len();
                match c {
                    '\\' => write!(f, "\\\\")?,
                    '[' => write!(f, "\\[")?,
                    ']' => write!(f, "\\]")?,
                    '\t' => write!(f, "\\t")?,
                    '\r' => write!(f, "\\r")?,
                    '\0' => write!(f, "\\0")?,
                    '\n' if i == 0 && first_content => write!(f, "\\n")?,
                    '\n' => writeln!(f)?,
                    '"' => {
                        let touches = chars.get(i + 1) == Some(&'"')
                            || (last && next_starts_quote)
                            || (last && is_final);
                        if touches { write!(f, "\\\"")? } else { write!(f, "\"")? }
                    }
                    c => write!(f, "{c}")?,
                }
            }
            Ok(())
        }
        match self {
            ExprKind::Constant(v @ Value::String(s)) => {
                if raw_printable(s) {
                    write_raw(f, s)
                } else {
                    v.fmt_ext(f, &parser::GRAPHIX_ESC, true)
                }
            }
            ExprKind::NoOp => Ok(()),
            ExprKind::ExplicitParens(e) => write!(f, "({e})"),
            ExprKind::Constant(v) => v.fmt_ext(f, &VAL_ESC, true),
            ExprKind::Bind(b) => write!(f, "{b}"),
            ExprKind::StructWith(sw) => write!(f, "{sw}"),
            ExprKind::Connect { name, value, deref } => {
                let deref = if *deref { "*" } else { "" };
                write!(f, "{deref}{name} <- {value}")
            }
            ExprKind::Use { reexport, names } => write_use_names(f, *reexport, names),
            ExprKind::Ref { name } => {
                write!(f, "{name}")
            }
            ExprKind::StructRef { source, field } => {
                if prints_as_bare_postfix(source) {
                    write!(f, "{source}.{field}")
                } else {
                    write!(f, "({source}).{field}")
                }
            }
            ExprKind::TupleRef { source, field } => {
                if prints_as_bare_postfix(source) {
                    write!(f, "{source}.{field}")
                } else {
                    write!(f, "({source}).{field}")
                }
            }
            ExprKind::Module {
                value:
                    ModuleKind::Resolved { from_interface: true, .. }
                    | ModuleKind::Unresolved { from_interface: true },
                ..
            } => Ok(()),
            ExprKind::Module { name, value } => {
                write!(f, "mod {name}")?;
                match value {
                    ModuleKind::Resolved { .. } | ModuleKind::Unresolved { .. } => Ok(()),
                    ModuleKind::Dynamic { sandbox, sig, source } => {
                        write!(f, " dynamic {{ {sandbox};")?;
                        write!(f, " {sig};")?;
                        write!(f, " source {source} }}")
                    }
                }
            }
            ExprKind::TypeCast { expr, typ } => write!(f, "cast<{typ}>({expr})"),
            ExprKind::TypeDef(td) => write!(f, "{td}"),
            ExprKind::Trait(t) => write!(f, "{t}"),
            ExprKind::Impl(i) => write!(f, "{i}"),
            ExprKind::Do { exprs } => print_exprs(f, &**exprs, "{", "}", "; "),
            ExprKind::Seq { trigger, body } => {
                write!(f, "seq ")?;
                if let Some(t) = trigger {
                    write!(f, "{t} ")?;
                }
                print_exprs(f, body, "{", "}", "; ")
            }
            ExprKind::Until(e) => write!(f, "until {e}"),
            ExprKind::SeqDo { body } => print_exprs(f, body, "do {", "}", "; "),
            ExprKind::Lambda(l) => write!(f, "{l}"),
            ExprKind::Array { args } => print_exprs(f, args, "[", "]", ", "),
            ExprKind::List { args } => print_exprs(f, args, "[<", ">]", ", "),
            ExprKind::Map { args } => {
                write!(f, "{{")?;
                for (i, (k, v)) in args.iter().enumerate() {
                    write!(f, "{k} => {v}")?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            ExprKind::MapRef { source, key } => {
                if prints_as_bare_postfix(source) {
                    write!(f, "{source}{{{key}}}")
                } else {
                    write!(f, "({source}){{{key}}}")
                }
            }
            ExprKind::Any { args } => {
                write!(f, "any")?;
                print_exprs(f, args, "(", ")", ", ")
            }
            ExprKind::Never { typ, args } => {
                match typ {
                    Some(t) => write!(f, "never<{t}>")?,
                    None => write!(f, "never")?,
                }
                print_exprs(f, args, "(", ")", ", ")
            }
            ExprKind::Tuple { args } => print_exprs(f, args, "(", ")", ", "),
            ExprKind::Variant { tag, args } if args.len() == 0 => {
                write!(f, "`{tag}")
            }
            ExprKind::Variant { tag, args } => {
                write!(f, "`{tag}")?;
                print_exprs(f, args, "(", ")", ", ")
            }
            ExprKind::Construct { name, arg } => write!(f, "{name}({arg})"),
            ExprKind::Struct(st) => write!(f, "{st}"),
            ExprKind::Qop(e) => write!(f, "{}?", e),
            ExprKind::OrNever(e) => write!(f, "{}$", e),
            ExprKind::Catch(c) => match &c.constraint {
                None => write!(f, "catch({}) {}", c.bind, c.handler),
                Some(t) => write!(f, "catch({}: {t}) {}", c.bind, c.handler),
            },
            ExprKind::StringInterpolate { args } => {
                if triple_printable(args) {
                    write!(f, "\"\"\"")?;
                    for (idx, a) in args.iter().enumerate() {
                        match &a.kind {
                            ExprKind::Constant(Value::String(s)) if s.len() > 0 => {
                                let next_starts_quote =
                                    args.get(idx + 1).is_some_and(|n| {
                                        matches!(
                                            &n.kind,
                                            ExprKind::Constant(Value::String(t))
                                                if t.starts_with('"')
                                        )
                                    });
                                write_triple_lit(
                                    f,
                                    s,
                                    idx == 0,
                                    next_starts_quote,
                                    idx + 1 == args.len(),
                                )?;
                            }
                            other => write!(f, "[{other}]")?,
                        }
                    }
                    write!(f, "\"\"\"")
                } else {
                    write!(f, "\"")?;
                    for s in args.iter() {
                        match &s.kind {
                            ExprKind::Constant(Value::String(s)) if s.len() > 0 => {
                                let es = parser::GRAPHIX_ESC.escape(&*s);
                                write!(f, "{es}",)?;
                            }
                            s => {
                                write!(f, "[{s}]")?;
                            }
                        }
                    }
                    write!(f, "\"")
                }
            }
            ExprKind::ArrayRef { source, i } => {
                if prints_as_bare_postfix(source) {
                    write!(f, "{}[{}]", source, i)
                } else {
                    write!(f, "({})[{}]", &source, &i)
                }
            }
            ExprKind::ArraySlice { source, start, end } => {
                let s = match start.as_ref() {
                    None => "",
                    Some(e) => &format_compact!("{e}"),
                };
                let e = match &end.as_ref() {
                    None => "",
                    Some(e) => &format_compact!("{e}"),
                };
                if prints_as_bare_postfix(source) {
                    write!(f, "{}[{}..{}]", source, s, e)
                } else {
                    write!(f, "({})[{}..{}]", source, s, e)
                }
            }
            ExprKind::Apply(ap) => write!(f, "{ap}"),
            ExprKind::Select(se) => write!(f, "{se}"),
            ExprKind::Eq { lhs, rhs } => write!(f, "{lhs} == {rhs}"),
            ExprKind::Ne { lhs, rhs } => write!(f, "{lhs} != {rhs}"),
            ExprKind::Gt { lhs, rhs } => write!(f, "{lhs} > {rhs}"),
            ExprKind::Lt { lhs, rhs } => write!(f, "{lhs} < {rhs}"),
            ExprKind::Gte { lhs, rhs } => write!(f, "{lhs} >= {rhs}"),
            ExprKind::Lte { lhs, rhs } => write!(f, "{lhs} <= {rhs}"),
            ExprKind::And { lhs, rhs } => write!(f, "{lhs} && {rhs}"),
            ExprKind::Or { lhs, rhs } => write!(f, "{lhs} || {rhs}"),
            ExprKind::Add { lhs, rhs } => write!(f, "{lhs} + {rhs}"),
            ExprKind::CheckedAdd { lhs, rhs } => write!(f, "{lhs} +? {rhs}"),
            ExprKind::Sub { lhs, rhs } => write!(f, "{lhs} - {rhs}"),
            ExprKind::CheckedSub { lhs, rhs } => write!(f, "{lhs} -? {rhs}"),
            ExprKind::Mul { lhs, rhs } => write!(f, "{lhs} * {rhs}"),
            ExprKind::CheckedMul { lhs, rhs } => write!(f, "{lhs} *? {rhs}"),
            ExprKind::Div { lhs, rhs } => write!(f, "{lhs} / {rhs}"),
            ExprKind::CheckedDiv { lhs, rhs } => write!(f, "{lhs} /? {rhs}"),
            ExprKind::Mod { lhs, rhs } => write!(f, "{lhs} % {rhs}"),
            ExprKind::CheckedMod { lhs, rhs } => write!(f, "{lhs} %? {rhs}"),
            ExprKind::Sample { lhs, rhs } => write!(f, "{lhs} ~ {rhs}"),
            ExprKind::StrictSample { lhs, rhs } => write!(f, "{lhs} ~! {rhs}"),
            ExprKind::ByRef(e) => write!(f, "&{e}"),
            ExprKind::Deref(e) => write!(f, "*{e}"),
            ExprKind::Neg(e) => write!(f, "-{e}"),
            ExprKind::Not { expr } => write!(f, "!{expr}"),
        }
    }
}
