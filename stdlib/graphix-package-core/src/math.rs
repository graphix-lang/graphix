use graphix_compiler::{effects::EffectKind, ExecCtx, Rt, UserEvent};
use netidx_value::Value;

use crate::{CachedArgs, CachedVals, EvalCached};

macro_rules! unary_f64 {
    ($ev:ident, $ty:ident, $name:literal, $op:ident) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;
        impl<R: Rt, E: UserEvent> EvalCached<R, E> for $ev {
            const EFFECT: EffectKind = EffectKind::Sync;
            const NAME: &str = $name;

            fn eval(
                &mut self,
                _ctx: &mut ExecCtx<R, E>,
                from: &CachedVals,
            ) -> Option<Value> {
                let x = from.get::<f64>(0)?;
                Some(Value::F64(x.$op()))
            }
        }
        pub(crate) type $ty = CachedArgs<$ev>;
    };
}

macro_rules! binary_f64 {
    ($ev:ident, $ty:ident, $name:literal, $op:ident) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;
        impl<R: Rt, E: UserEvent> EvalCached<R, E> for $ev {
            const EFFECT: EffectKind = EffectKind::Sync;
            const NAME: &str = $name;

            fn eval(
                &mut self,
                _ctx: &mut ExecCtx<R, E>,
                from: &CachedVals,
            ) -> Option<Value> {
                let x = from.get::<f64>(0)?;
                let y = from.get::<f64>(1)?;
                Some(Value::F64(x.$op(y)))
            }
        }
        pub(crate) type $ty = CachedArgs<$ev>;
    };
}

macro_rules! unary_f64_pred {
    ($ev:ident, $ty:ident, $name:literal, $op:ident) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;
        impl<R: Rt, E: UserEvent> EvalCached<R, E> for $ev {
            const EFFECT: EffectKind = EffectKind::Sync;
            const NAME: &str = $name;

            fn eval(
                &mut self,
                _ctx: &mut ExecCtx<R, E>,
                from: &CachedVals,
            ) -> Option<Value> {
                let x = from.get::<f64>(0)?;
                Some(Value::Bool(x.$op()))
            }
        }
        pub(crate) type $ty = CachedArgs<$ev>;
    };
}

// ── Trigonometric ──────────────────────────────────────────────────
unary_f64!(MathSinEv, MathSin, "core_math_sin", sin);
unary_f64!(MathCosEv, MathCos, "core_math_cos", cos);
unary_f64!(MathTanEv, MathTan, "core_math_tan", tan);
unary_f64!(MathAsinEv, MathAsin, "core_math_asin", asin);
unary_f64!(MathAcosEv, MathAcos, "core_math_acos", acos);
unary_f64!(MathAtanEv, MathAtan, "core_math_atan", atan);
binary_f64!(MathAtan2Ev, MathAtan2, "core_math_atan2", atan2);

// ── Hyperbolic ─────────────────────────────────────────────────────
unary_f64!(MathSinhEv, MathSinh, "core_math_sinh", sinh);
unary_f64!(MathCoshEv, MathCosh, "core_math_cosh", cosh);
unary_f64!(MathTanhEv, MathTanh, "core_math_tanh", tanh);
unary_f64!(MathAsinhEv, MathAsinh, "core_math_asinh", asinh);
unary_f64!(MathAcoshEv, MathAcosh, "core_math_acosh", acosh);
unary_f64!(MathAtanhEv, MathAtanh, "core_math_atanh", atanh);

// ── Exponential / logarithmic ──────────────────────────────────────
unary_f64!(MathExpEv, MathExp, "core_math_exp", exp);
unary_f64!(MathExp2Ev, MathExp2, "core_math_exp2", exp2);
unary_f64!(MathExpM1Ev, MathExpM1, "core_math_exp_m1", exp_m1);
unary_f64!(MathLnEv, MathLn, "core_math_ln", ln);
unary_f64!(MathLn1pEv, MathLn1p, "core_math_ln_1p", ln_1p);
unary_f64!(MathLog2Ev, MathLog2, "core_math_log2", log2);
unary_f64!(MathLog10Ev, MathLog10, "core_math_log10", log10);
binary_f64!(MathLogEv, MathLog, "core_math_log", log);

// ── Power / root ───────────────────────────────────────────────────
binary_f64!(MathPowEv, MathPow, "core_math_pow", powf);
unary_f64!(MathSqrtEv, MathSqrt, "core_math_sqrt", sqrt);
unary_f64!(MathCbrtEv, MathCbrt, "core_math_cbrt", cbrt);
binary_f64!(MathHypotEv, MathHypot, "core_math_hypot", hypot);

// ── Rounding / sign ────────────────────────────────────────────────
unary_f64!(MathFloorEv, MathFloor, "core_math_floor", floor);
unary_f64!(MathCeilEv, MathCeil, "core_math_ceil", ceil);
unary_f64!(MathRoundEv, MathRound, "core_math_round", round);
unary_f64!(MathTruncEv, MathTrunc, "core_math_trunc", trunc);
unary_f64!(MathFractEv, MathFract, "core_math_fract", fract);
unary_f64!(MathAbsEv, MathAbs, "core_math_abs", abs);
unary_f64!(MathSignumEv, MathSignum, "core_math_signum", signum);
binary_f64!(MathCopysignEv, MathCopysign, "core_math_copysign", copysign);

// ── Comparison / clamp ─────────────────────────────────────────────
// Binary f64 min/max with IEEE-754 NaN semantics (return the non-NaN
// operand). Polymorphic n-ary min/max already live in core::mod.
binary_f64!(MathMinEv, MathMin, "core_math_min", min);
binary_f64!(MathMaxEv, MathMax, "core_math_max", max);

#[derive(Debug, Default)]
pub(crate) struct MathClampEv;
impl<R: Rt, E: UserEvent> EvalCached<R, E> for MathClampEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_math_clamp";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let x = from.get::<f64>(0)?;
        let lo = from.get::<f64>(1)?;
        let hi = from.get::<f64>(2)?;
        Some(Value::F64(x.clamp(lo, hi)))
    }
}
pub(crate) type MathClamp = CachedArgs<MathClampEv>;

// ── Predicates ─────────────────────────────────────────────────────
unary_f64_pred!(MathIsNanEv, MathIsNan, "core_math_is_nan", is_nan);
unary_f64_pred!(MathIsFiniteEv, MathIsFinite, "core_math_is_finite", is_finite);
unary_f64_pred!(MathIsInfiniteEv, MathIsInfinite, "core_math_is_infinite", is_infinite);

// ── Conversion ─────────────────────────────────────────────────────
unary_f64!(MathToDegreesEv, MathToDegrees, "core_math_to_degrees", to_degrees);
unary_f64!(MathToRadiansEv, MathToRadians, "core_math_to_radians", to_radians);
