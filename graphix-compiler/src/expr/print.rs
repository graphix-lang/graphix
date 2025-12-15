use super::Sig;
use crate::{
    expr::{
        parser, ApplyExpr, Bind, BindSig, Doc, Expr, ExprKind, Lambda, ModSig,
        ModuleKind, Sandbox, SelectExpr, SigItem, Struct, StructWith, TypeDef,
    },
    typ::Type,
};
use compact_str::{format_compact, CompactString};
use netidx::{path::Path, utils::Either};
use netidx_value::{parser::VAL_ESC, Value};
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
    writeln!(buf, "{}", open)?;
    buf.with_indent::<fmt::Result, _>(2, |buf| {
        for i in 0..exprs.len() {
            f(&exprs[i]).kind.fmt_pretty(buf)?;
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
        writeln!(buf, "{}", self)?;
        if buf.len() - start <= buf.limit {
            return Ok(());
        } else {
            buf.buf.truncate(start);
            self.fmt_pretty_inner(buf)
        }
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

impl TypeDef {
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

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.doc);
        self.write_name_and_params(f)?;
        write!(f, " = {}", self.typ)
    }
}

impl PrettyDisplay for TypeDef {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.doc.fmt_pretty(buf)?;
        self.write_name_and_params(buf)?;
        writeln!(buf, " =");
        buf.with_indent(2, |buf| self.typ.fmt_pretty(buf))
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
                });
                write!(buf, " ]")
            }};
        }
        match self {
            Sandbox::Unrestricted => writeln!(buf, "sandbox unrestricted"),
            Sandbox::Blacklist(l) => write_sandbox!("blacklist", l),
            Sandbox::Whitelist(l) => write_sandbox!("whitelist", l),
        }
    }
}

impl fmt::Display for BindSig {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.doc);
        write!(f, "val {}: {}", self.name, self.typ)
    }
}

impl PrettyDisplay for BindSig {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.doc.fmt_pretty(buf)?;
        write!(buf, "val {}: ", self.name)?;
        self.typ.fmt_pretty(buf)
    }
}

impl fmt::Display for ModSig {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.doc);
        write!(f, "mod {}: {}", self.name, self.sig)
    }
}

impl PrettyDisplay for ModSig {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.doc.fmt_pretty(buf)?;
        writeln!(buf, "mod {}:", self.name)?;
        self.sig.fmt_pretty(buf)
    }
}

impl fmt::Display for SigItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SigItem::TypeDef(td) => write!(f, "{td}"),
            SigItem::Bind(bind) => write!(f, "{bind}"),
            SigItem::Module(sig) => write!(f, "{sig}"),
        }
    }
}

impl PrettyDisplay for SigItem {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        match self {
            SigItem::Bind(b) => b.fmt_pretty(buf),
            SigItem::TypeDef(d) => d.fmt_pretty(buf),
            SigItem::Module(m) => m.fmt_pretty(buf),
        }
    }
}

impl fmt::Display for Sig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.toplevel {
            write!(f, "sig {{ ")?;
        }
        for (i, si) in self.iter().enumerate() {
            write!(f, "{si}")?;
            if i < self.len() - 1 {
                write!(f, "; ")?
            }
        }
        if self.toplevel {
            write!(f, " }}")?
        }
        Ok(())
    }
}

impl PrettyDisplay for Sig {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if self.toplevel {
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
        if self.toplevel {
            writeln!(buf, "}}")?
        }
        Ok(())
    }
}

impl fmt::Display for Bind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Bind { rec, pattern, typ, value } = self;
        let rec = if *rec { " rec" } else { "" };
        match typ {
            None => write!(f, "let{} {pattern} = {value}", rec),
            Some(typ) => write!(f, "let{} {pattern}: {typ} = {value}", rec),
        }
    }
}

impl PrettyDisplay for Bind {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Bind { rec, pattern, typ, value } = self;
        let rec = if *rec { " rec" } else { "" };
        match typ {
            None => writeln!(buf, "let{} {pattern} = ", rec)?,
            Some(typ) => writeln!(buf, "let{} {pattern}: {typ} = ", rec)?,
        }
        buf.with_indent(2, |buf| value.fmt_pretty(buf))
    }
}

impl fmt::Display for StructWith {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { source, replace } = self;
        match &source.kind {
            ExprKind::Ref { .. } => write!(f, "{{ {source} with ")?,
            _ => write!(f, "{{ ({source}) with ")?,
        }
        for (i, (name, e)) in replace.iter().enumerate() {
            match &e.kind {
                ExprKind::Ref { name: n }
                    if Path::dirname(&**n).is_none()
                        && Path::basename(&**n) == Some(&**name) =>
                {
                    write!(f, "{name}")?
                }
                _ => write!(f, "{name}: {e}")?,
            }
            if i < replace.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for StructWith {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Self { source, replace } = self;
        match &source.kind {
            ExprKind::Ref { .. } => writeln!(buf, "{{ {source} with")?,
            _ => writeln!(buf, "{{ ({source}) with")?,
        }
        buf.with_indent::<fmt::Result, _>(2, |buf| {
            for (i, (name, e)) in replace.iter().enumerate() {
                match &e.kind {
                    ExprKind::Ref { name: n }
                        if Path::dirname(&**n).is_none()
                            && Path::basename(&**n) == Some(&**name) =>
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

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { args } = self;
        write!(f, "{{ ")?;
        for (i, (n, e)) in args.iter().enumerate() {
            match &e.kind {
                ExprKind::Ref { name }
                    if Path::dirname(&**name).is_none()
                        && Path::basename(&**name) == Some(&**n) =>
                {
                    write!(f, "{n}")?
                }
                _ => write!(f, "{n}: {e}")?,
            }
            if i < args.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for Struct {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Self { args } = self;
        writeln!(buf, "{{")?;
        buf.with_indent::<fmt::Result, _>(2, |buf| {
            for (i, (n, e)) in args.iter().enumerate() {
                match &e.kind {
                    ExprKind::Ref { name }
                        if Path::dirname(&**name).is_none()
                            && Path::basename(&**name) == Some(&**n) =>
                    {
                        write!(buf, "{n}")?
                    }
                    _ => {
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

impl fmt::Display for ApplyExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { args, function } = self;
        match &function.kind {
            ExprKind::Ref { name: _ } => write!(f, "{function}")?,
            function => write!(f, "({function})")?,
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
        match &function.kind {
            ExprKind::Ref { .. } => function.fmt_pretty(buf)?,
            e => {
                write!(buf, "(")?;
                e.fmt_pretty(buf)?;
                buf.kill_newline();
                write!(buf, ")")?;
            }
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
        })?;
        writeln!(buf, ")")
    }
}

impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Lambda { args, vargs, rtype, constraints, throws, body } = self;
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
                None => write!(f, "@args"),
                Some(typ) => write!(f, "@args: {typ}"),
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

impl PrettyDisplay for Lambda {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Lambda { args, vargs, rtype, constraints, throws, body } = self;
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
                    if let Some(t) = a.constraint {
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
            Either::Left(body) => match &body.kind {
                ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, "{", "}", ";"),
                _ => body.fmt_pretty(buf),
            },
        }
    }
}

impl fmt::Display for SelectExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let SelectExpr { arg, arms } = self;
        write!(f, "select {arg} {{")?;
        for (i, (pat, rhs)) in arms.iter().enumerate() {
            if let Some(tp) = &pat.type_predicate {
                write!(f, "{tp} as ")?;
            }
            write!(f, "{} ", pat.structure_predicate)?;
            if let Some(guard) = &pat.guard {
                write!(f, "if {guard} ")?;
            }
            write!(f, "=> {rhs}")?;
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
                if let ExprKind::Do { exprs } = &expr.kind {
                    let term = if i < arms.len() - 1 { "}," } else { "}" };
                    buf.with_indent(2, |buf| {
                        pretty_print_exprs(buf, exprs, "{", term, ";")
                    })?;
                } else if i < arms.len() - 1 {
                    buf.with_indent(2, |buf| expr.fmt_pretty(buf))?;
                    buf.kill_newline();
                    writeln!(buf, ",")?
                } else {
                    buf.with_indent(2, |buf| expr.fmt_pretty(buf))?;
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
                write!(buf, "(")?;
                writeln!(buf, "{} {}", $lhs, $sep)?;
                $rhs.fmt_pretty(buf)?;
                write!(buf, ")")
            }};
        }
        match self {
            ExprKind::Constant(_)
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
                value: ModuleKind::Unresolved | ModuleKind::Resolved(_),
            } => {
                writeln!(buf, "{self}")
            }
            ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, "{", "}", ";"),
            ExprKind::Array { args } => pretty_print_exprs(buf, args, "[", "]", ","),
            ExprKind::Tuple { args } => pretty_print_exprs(buf, args, "(", ")", ","),
            ExprKind::Bind(b) => b.fmt_pretty(buf),
            ExprKind::StructWith(sw) => sw.fmt_pretty(buf),
            ExprKind::Module { name, value: ModuleKind::Inline(exprs) } => {
                write!(buf, "mod {name} ")?;
                pretty_print_exprs(buf, exprs, "{", "}", ";")
            }
            ExprKind::Module {
                name,
                value: ModuleKind::Dynamic { sandbox, sig, source },
            } => {
                writeln!(buf, "mod {name} dynamic {{")?;
                buf.with_indent(2, |buf| {
                    sandbox.fmt_pretty(buf)?;
                    sig.fmt_pretty(buf)?;
                    write!(buf, "source ")?;
                    buf.with_indent(2, |buf| source.fmt_pretty(buf))
                })
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
            ExprKind::Variant { tag: _, args } if args.len() == 0 => {
                write!(buf, "{self}")
            }
            ExprKind::Variant { tag, args } => {
                write!(buf, "`{tag}")?;
                pretty_print_exprs(buf, args, "(", ")", ",")
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
            ExprKind::TryCatch(tc) => {
                writeln!(buf, "try")?;
                pretty_print_exprs(buf, &tc.exprs, "", "", "; ")?;
                match &tc.constraint {
                    None => write!(buf, "catch({}) => ", tc.bind)?,
                    Some(t) => write!(buf, "catch({}: {t}) => ", tc.bind)?,
                }
                match &tc.handler.kind {
                    ExprKind::Do { exprs } => {
                        pretty_print_exprs(buf, exprs, "{", "}", "; ")
                    }
                    _ => {
                        writeln!(buf, "")?;
                        buf.with_indent(2, |buf| tc.handler.fmt_pretty(buf))
                    }
                }
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
            ExprKind::Sub { lhs, rhs } => binop!("-", lhs, rhs),
            ExprKind::Mul { lhs, rhs } => binop!("*", lhs, rhs),
            ExprKind::Div { lhs, rhs } => binop!("/", lhs, rhs),
            ExprKind::Mod { lhs, rhs } => binop!("%", lhs, rhs),
            ExprKind::Sample { lhs, rhs } => binop!("~", lhs, rhs),
            ExprKind::Not { expr } => match &expr.kind {
                ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, "!{", "}", ";"),
                _ => {
                    writeln!(buf, "!(")?;
                    expr.fmt_pretty(buf)?;
                    writeln!(buf, ")")
                }
            },
            ExprKind::ByRef(e) => {
                write!(buf, "&")?;
                e.fmt_pretty(buf)
            }
            ExprKind::Deref(e) => match &e.kind {
                ExprKind::Connect { .. } | ExprKind::Qop(_) => {
                    writeln!(buf, "*(")?;
                    buf.with_indent(2, |buf| e.fmt_pretty(buf))?;
                    writeln!(buf, ")")
                }
                _ => {
                    write!(buf, "*")?;
                    buf.with_indent(2, |buf| e.fmt_pretty(buf))
                }
            },
            ExprKind::Select(se) => se.fmt_pretty(buf),
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn write_binop(
            f: &mut fmt::Formatter,
            op: &str,
            lhs: &Expr,
            rhs: &Expr,
        ) -> fmt::Result {
            write!(f, "(")?;
            write!(f, "{lhs} {op} {rhs}")?;
            write!(f, ")")
        }
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
        let mut tbuf = CompactString::new("");
        let exp = |export| if export { "pub " } else { "" };
        match self {
            ExprKind::Constant(v @ Value::String(_)) => {
                v.fmt_ext(f, &parser::GRAPHIX_ESC, true)
            }
            ExprKind::Constant(v) => v.fmt_ext(f, &VAL_ESC, true),
            ExprKind::Bind(b) => write!(f, "{b}"),
            ExprKind::StructWith(sw) => write!(f, "{sw}"),
            ExprKind::Connect { name, value, deref } => {
                let deref = if *deref { "*" } else { "" };
                write!(f, "{deref}{name} <- {value}")
            }
            ExprKind::Use { name } => {
                write!(f, "use {name}")
            }
            ExprKind::Ref { name } => {
                write!(f, "{name}")
            }
            ExprKind::StructRef { source, field } => match &source.kind {
                ExprKind::Ref { .. } => {
                    write!(f, "{source}.{field}")
                }
                source => write!(f, "({source}).{field}"),
            },
            ExprKind::TupleRef { source, field } => match &source.kind {
                ExprKind::Ref { .. } => {
                    write!(f, "{source}.{field}")
                }
                source => write!(f, "({source}).{field}"),
            },
            ExprKind::Module { name, value } => {
                write!(f, "mod {name}")?;
                match value {
                    ModuleKind::Resolved(_) | ModuleKind::Unresolved => Ok(()),
                    ModuleKind::Inline(exprs) => print_exprs(f, &**exprs, "{", "}", "; "),
                    ModuleKind::Dynamic { sandbox, sig, source } => {
                        write!(f, " dynamic {{ {sandbox};")?;
                        write!(f, " {sig};")?;
                        write!(f, " source {source} }}")
                    }
                }
            }
            ExprKind::TypeCast { expr, typ } => write!(f, "cast<{typ}>({expr})"),
            ExprKind::TypeDef(td) => write!(f, "{td}"),
            ExprKind::Do { exprs } => print_exprs(f, &**exprs, "{", "}", "; "),
            ExprKind::Lambda(l) => write!(f, "{l}"),
            ExprKind::Array { args } => print_exprs(f, args, "[", "]", ", "),
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
            ExprKind::MapRef { source, key } => match &source.kind {
                ExprKind::Ref { name } => write!(f, "{name}{{{key}}}"),
                _ => write!(f, "({source}){{{key}}}"),
            },
            ExprKind::Any { args } => {
                write!(f, "any")?;
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
            ExprKind::Struct(st) => write!(f, "{st}"),
            ExprKind::Qop(e) => write!(f, "{}?", e),
            ExprKind::OrNever(e) => write!(f, "{}$", e),
            ExprKind::TryCatch(tc) => {
                write!(f, "try ")?;
                print_exprs(f, &tc.exprs, "", "", "; ")?;
                match &tc.constraint {
                    None => write!(f, " catch({}) => {}", tc.bind, tc.handler),
                    Some(t) => write!(f, " catch({}: {t}) => {}", tc.bind, tc.handler),
                }
            }
            ExprKind::StringInterpolate { args } => {
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
            ExprKind::ArrayRef { source, i } => match &source.kind {
                ExprKind::Ref { .. } => {
                    write!(f, "{}[{}]", source, i)
                }
                _ => write!(f, "({})[{}]", &source, &i),
            },
            ExprKind::ArraySlice { source, start, end } => {
                let s = match start.as_ref() {
                    None => "",
                    Some(e) => &format_compact!("{e}"),
                };
                let e = match &end.as_ref() {
                    None => "",
                    Some(e) => &format_compact!("{e}"),
                };
                match &source.kind {
                    ExprKind::Ref { .. } => {
                        write!(f, "{}[{}..{}]", source, s, e)
                    }
                    _ => write!(f, "({})[{}..{}]", source, s, e),
                }
            }
            ExprKind::Apply(ap) => write!(f, "{ap}"),
            ExprKind::Select(se) => write!(f, "{se}"),
            ExprKind::Eq { lhs, rhs } => write_binop(f, "==", lhs, rhs),
            ExprKind::Ne { lhs, rhs } => write_binop(f, "!=", lhs, rhs),
            ExprKind::Gt { lhs, rhs } => write_binop(f, ">", lhs, rhs),
            ExprKind::Lt { lhs, rhs } => write_binop(f, "<", lhs, rhs),
            ExprKind::Gte { lhs, rhs } => write_binop(f, ">=", lhs, rhs),
            ExprKind::Lte { lhs, rhs } => write_binop(f, "<=", lhs, rhs),
            ExprKind::And { lhs, rhs } => write_binop(f, "&&", lhs, rhs),
            ExprKind::Or { lhs, rhs } => write_binop(f, "||", lhs, rhs),
            ExprKind::Add { lhs, rhs } => write_binop(f, "+", lhs, rhs),
            ExprKind::Sub { lhs, rhs } => write_binop(f, "-", lhs, rhs),
            ExprKind::Mul { lhs, rhs } => write_binop(f, "*", lhs, rhs),
            ExprKind::Div { lhs, rhs } => write_binop(f, "/", lhs, rhs),
            ExprKind::Mod { lhs, rhs } => write_binop(f, "%", lhs, rhs),
            ExprKind::Sample { lhs, rhs } => write_binop(f, "~", lhs, rhs),
            ExprKind::ByRef(e) => write!(f, "&{e}"),
            ExprKind::Deref(e) => match &e.kind {
                ExprKind::Qop(e) => write!(f, "*({e}?)"),
                ExprKind::Connect { .. } => write!(f, "*({e})"),
                _ => write!(f, "*{e}"),
            },
            ExprKind::Not { expr } => {
                write!(f, "(!{expr})")
            }
        }
    }
}
