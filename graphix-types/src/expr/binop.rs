use netidx_derive::Pack;

/// A binary operator: the token it is written with, how tightly it binds,
/// and the expression it builds. Every binary operator is left-associative.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Pack)]
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
    /// `~` paying every trigger that found its argument absent, where `~`
    /// banks one: built only by a lowering (the `seqq` request queue),
    /// never parsed, printed as `~`.
    BankedSample,
}

impl BinOp {
    /// A checked arithmetic operator (`+?` and the like).
    pub fn is_checked(self) -> bool {
        matches!(
            self,
            Self::CheckedAdd
                | Self::CheckedSub
                | Self::CheckedMul
                | Self::CheckedDiv
                | Self::CheckedMod
        )
    }

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
            BinOp::Sample | BinOp::BankedSample => "~",
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
            BinOp::Sample | BinOp::StrictSample | BinOp::BankedSample => 0,
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
}
