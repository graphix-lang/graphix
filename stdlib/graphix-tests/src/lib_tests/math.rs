use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const SIN_ZERO: &str = "math::sin(f64:0.0)";
run!(math_sin_zero, SIN_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 0.0)
});

const SIN_HALF_PI: &str =
    "math::abs(math::sin(math::pi / f64:2.0) - f64:1.0) < f64:1e-12";
run!(math_sin_half_pi, SIN_HALF_PI, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

const COS_ZERO: &str = "math::cos(f64:0.0)";
run!(math_cos_zero, COS_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 1.0)
});

// sin(pi) is near zero, not exactly.
const SIN_PI: &str = "math::abs(math::sin(math::pi)) < f64:1e-12";
run!(math_sin_pi, SIN_PI, |v: Result<&Value>| { matches!(v, Ok(Value::Bool(true))) });

const SQRT_FOUR: &str = "math::sqrt(f64:4.0)";
run!(math_sqrt_four, SQRT_FOUR, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 2.0)
});

const POW_2_10: &str = "math::pow(f64:2.0, f64:10.0)";
run!(math_pow_2_10, POW_2_10, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 1024.0)
});

const HYPOT_3_4: &str = "math::hypot(f64:3.0, f64:4.0)";
run!(math_hypot_3_4, HYPOT_3_4, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 5.0)
});

const ROUNDING: &str = "math::floor(f64:1.7) == f64:1.0 \
      && math::ceil(f64:1.2) == f64:2.0 \
      && math::round(f64:0.5) == f64:1.0 \
      && math::trunc(f64:-1.7) == f64:-1.0";
run!(math_rounding, ROUNDING, |v: Result<&Value>| { matches!(v, Ok(Value::Bool(true))) });

const ABS: &str = "math::abs(f64:-3.5) == f64:3.5 && math::abs(f64:2.0) == f64:2.0";
run!(math_abs, ABS, |v: Result<&Value>| { matches!(v, Ok(Value::Bool(true))) });

const LN_E: &str = "math::abs(math::ln(math::e) - f64:1.0) < f64:1e-12";
run!(math_ln_e, LN_E, |v: Result<&Value>| { matches!(v, Ok(Value::Bool(true))) });

const LOGS: &str = "math::log2(f64:8.0) == f64:3.0 \
      && math::abs(math::log(f64:100.0, f64:10.0) - f64:2.0) < f64:1e-12";
run!(math_logs, LOGS, |v: Result<&Value>| { matches!(v, Ok(Value::Bool(true))) });

const ATAN2: &str =
    "math::abs(math::atan2(f64:1.0, f64:1.0) - math::pi / f64:4.0) < f64:1e-12";
run!(math_atan2, ATAN2, |v: Result<&Value>| { matches!(v, Ok(Value::Bool(true))) });

// clamp on an invalid range (lo > hi) returns a catchable `ClampError`.
const CLAMP_INVALID: &str = "is_err(math::clamp(f64:42.0, f64:0.0, f64:-1.0))";
run!(math_clamp_invalid_errors, CLAMP_INVALID, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const CONSTANT_SANITY: &str = "math::tau > math::pi \
      && math::pi > f64:3.14 \
      && math::pi < f64:3.15 \
      && math::e > f64:2.71 \
      && math::e < f64:2.72 \
      && math::sqrt_2 > f64:1.41 \
      && math::sqrt_2 < f64:1.42";
run!(math_constant_sanity, CONSTANT_SANITY, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

const PREDICATES: &str = "math::is_nan(math::nan) \
      && !math::is_nan(math::pi) \
      && math::is_infinite(math::infinity) \
      && !math::is_infinite(math::pi) \
      && math::is_finite(math::pi) \
      && !math::is_finite(math::nan) \
      && !math::is_finite(math::infinity)";
run!(math_predicates, PREDICATES, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// clamp on a valid range; `$` unwraps the `[f64, Error]` union.
const CLAMP: &str = "math::clamp(f64:5.0, f64:0.0, f64:10.0)$ == f64:5.0 \
      && math::clamp(f64:-3.0, f64:0.0, f64:10.0)$ == f64:0.0 \
      && math::clamp(f64:42.0, f64:0.0, f64:10.0)$ == f64:10.0";
run!(math_clamp, CLAMP, |v: Result<&Value>| { matches!(v, Ok(Value::Bool(true))) });

const DEG_RAD: &str =
    "math::abs(math::to_degrees(math::to_radians(f64:45.0)) - f64:45.0) < f64:1e-12";
run!(math_deg_rad, DEG_RAD, |v: Result<&Value>| { matches!(v, Ok(Value::Bool(true))) });
