use crate::expr::{Expr, ExprKind};
use triomphe::Arc;

/// A binary operator: the token it is written with, how tightly it binds,
/// and the expression it builds. Every binary operator is left-associative.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Add,
    CheckedAdd,
    Sub,
    CheckedSub,
    Mul,
    CheckedMul,
    Div,
    CheckedDiv,
    Mod,
    CheckedMod,
    Sample,
    StrictSample,
}

impl BinOp {
    /// Every operator, a token before any token it is a prefix of.
    pub const ALL: [BinOp; 20] = [
        BinOp::Eq,
        BinOp::Ne,
        BinOp::Gte,
        BinOp::Lte,
        BinOp::And,
        BinOp::Or,
        BinOp::Gt,
        BinOp::Lt,
        BinOp::CheckedAdd,
        BinOp::Add,
        BinOp::CheckedSub,
        BinOp::Sub,
        BinOp::CheckedMul,
        BinOp::Mul,
        BinOp::CheckedDiv,
        BinOp::Div,
        BinOp::CheckedMod,
        BinOp::Mod,
        BinOp::StrictSample,
        BinOp::Sample,
    ];

    pub fn token(self) -> &'static str {
        match self {
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::Lte => "<=",
            BinOp::Gte => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::Add => "+",
            BinOp::CheckedAdd => "+?",
            BinOp::Sub => "-",
            BinOp::CheckedSub => "-?",
            BinOp::Mul => "*",
            BinOp::CheckedMul => "*?",
            BinOp::Div => "/",
            BinOp::CheckedDiv => "/?",
            BinOp::Mod => "%",
            BinOp::CheckedMod => "%?",
            BinOp::Sample => "~",
            BinOp::StrictSample => "~!",
        }
    }

    /// The character that, following the token, makes it something else:
    /// `>]` closes a list literal and `<-` is a connect.
    pub fn not_before(self) -> Option<char> {
        match self {
            BinOp::Gt => Some(']'),
            BinOp::Lt => Some('-'),
            _ => None,
        }
    }

    /// Higher binds tighter.
    pub fn precedence(self) -> u8 {
        match self {
            BinOp::Sample | BinOp::StrictSample => 0,
            BinOp::Or => 1,
            BinOp::And => 2,
            BinOp::Eq | BinOp::Ne => 3,
            BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => 4,
            BinOp::Add | BinOp::CheckedAdd | BinOp::Sub | BinOp::CheckedSub => 5,
            BinOp::Mul
            | BinOp::CheckedMul
            | BinOp::Div
            | BinOp::CheckedDiv
            | BinOp::Mod
            | BinOp::CheckedMod => 6,
        }
    }

    pub fn build(self, lhs: Arc<Expr>, rhs: Arc<Expr>) -> ExprKind {
        match self {
            BinOp::Eq => ExprKind::Eq { lhs, rhs },
            BinOp::Ne => ExprKind::Ne { lhs, rhs },
            BinOp::Lt => ExprKind::Lt { lhs, rhs },
            BinOp::Gt => ExprKind::Gt { lhs, rhs },
            BinOp::Lte => ExprKind::Lte { lhs, rhs },
            BinOp::Gte => ExprKind::Gte { lhs, rhs },
            BinOp::And => ExprKind::And { lhs, rhs },
            BinOp::Or => ExprKind::Or { lhs, rhs },
            BinOp::Add => ExprKind::Add { lhs, rhs },
            BinOp::CheckedAdd => ExprKind::CheckedAdd { lhs, rhs },
            BinOp::Sub => ExprKind::Sub { lhs, rhs },
            BinOp::CheckedSub => ExprKind::CheckedSub { lhs, rhs },
            BinOp::Mul => ExprKind::Mul { lhs, rhs },
            BinOp::CheckedMul => ExprKind::CheckedMul { lhs, rhs },
            BinOp::Div => ExprKind::Div { lhs, rhs },
            BinOp::CheckedDiv => ExprKind::CheckedDiv { lhs, rhs },
            BinOp::Mod => ExprKind::Mod { lhs, rhs },
            BinOp::CheckedMod => ExprKind::CheckedMod { lhs, rhs },
            BinOp::Sample => ExprKind::Sample { lhs, rhs },
            BinOp::StrictSample => ExprKind::StrictSample { lhs, rhs },
        }
    }

    /// The operator and operands of a binary-operator expression.
    pub fn of(e: &ExprKind) -> Option<(BinOp, &Arc<Expr>, &Arc<Expr>)> {
        let (op, lhs, rhs) = match e {
            ExprKind::Eq { lhs, rhs } => (BinOp::Eq, lhs, rhs),
            ExprKind::Ne { lhs, rhs } => (BinOp::Ne, lhs, rhs),
            ExprKind::Lt { lhs, rhs } => (BinOp::Lt, lhs, rhs),
            ExprKind::Gt { lhs, rhs } => (BinOp::Gt, lhs, rhs),
            ExprKind::Lte { lhs, rhs } => (BinOp::Lte, lhs, rhs),
            ExprKind::Gte { lhs, rhs } => (BinOp::Gte, lhs, rhs),
            ExprKind::And { lhs, rhs } => (BinOp::And, lhs, rhs),
            ExprKind::Or { lhs, rhs } => (BinOp::Or, lhs, rhs),
            ExprKind::Add { lhs, rhs } => (BinOp::Add, lhs, rhs),
            ExprKind::CheckedAdd { lhs, rhs } => (BinOp::CheckedAdd, lhs, rhs),
            ExprKind::Sub { lhs, rhs } => (BinOp::Sub, lhs, rhs),
            ExprKind::CheckedSub { lhs, rhs } => (BinOp::CheckedSub, lhs, rhs),
            ExprKind::Mul { lhs, rhs } => (BinOp::Mul, lhs, rhs),
            ExprKind::CheckedMul { lhs, rhs } => (BinOp::CheckedMul, lhs, rhs),
            ExprKind::Div { lhs, rhs } => (BinOp::Div, lhs, rhs),
            ExprKind::CheckedDiv { lhs, rhs } => (BinOp::CheckedDiv, lhs, rhs),
            ExprKind::Mod { lhs, rhs } => (BinOp::Mod, lhs, rhs),
            ExprKind::CheckedMod { lhs, rhs } => (BinOp::CheckedMod, lhs, rhs),
            ExprKind::Sample { lhs, rhs } => (BinOp::Sample, lhs, rhs),
            ExprKind::StrictSample { lhs, rhs } => (BinOp::StrictSample, lhs, rhs),
            _ => return None,
        };
        Some((op, lhs, rhs))
    }
}
