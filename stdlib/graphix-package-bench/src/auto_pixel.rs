// Hand-integrated output of `graphix compile` on
// `bench/mandelbrot_bench_annotated.gx`, copied here to measure the
// end-to-end runtime with BOTH the iterate kernel AND the per-pixel
// callback fused. The only manual edits from the emitter output are
// struct and NAME renames to satisfy the bench- prefix requirement.
//
// See `auto_iterate.rs` for the iterate-only version.

use crate::auto_iterate::fused_iterate_body;

#[allow(unused_parens, unreachable_code, unused_mut, clippy::too_many_arguments)]
#[inline]
pub fn fused_pixel_body(mut idx: i64) -> i64 {
    let mut px = idx % 100i64;
    let mut py = idx / 100i64;
    let mut cr = -2.5f64 + (px as f64) * ((1f64 - -2.5f64) / (100i64 as f64));
    let mut ci = -1.3125f64 + (py as f64) * ((1.3125f64 - -1.3125f64) / (75i64 as f64));
    fused_iterate_body(0f64, 0f64, cr, ci, 64i64)
}

#[derive(Debug)]
pub struct FusedPixelAuto {
    pub args: ::graphix_package_core::CachedVals,
}

impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>
    ::graphix_compiler::BuiltIn<R, E> for FusedPixelAuto
{
    const NAME: &'static str = "bench_pixel_auto";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ::graphix_compiler::ExecCtx<R, E>,
        _typ: &'a ::graphix_compiler::typ::FnType,
        _resolved: ::std::option::Option<&'d ::graphix_compiler::typ::FnType>,
        _scope: &'b ::graphix_compiler::Scope,
        from: &'c [::graphix_compiler::Node<R, E>],
        _top_id: ::graphix_compiler::expr::ExprId,
    ) -> ::anyhow::Result<::std::boxed::Box<dyn ::graphix_compiler::Apply<R, E>>> {
        ::std::result::Result::Ok(::std::boxed::Box::new(FusedPixelAuto {
            args: ::graphix_package_core::CachedVals::new(from),
        }))
    }
}

impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>
    ::graphix_compiler::Apply<R, E> for FusedPixelAuto
{
    fn update(
        &mut self,
        ctx: &mut ::graphix_compiler::ExecCtx<R, E>,
        from: &mut [::graphix_compiler::Node<R, E>],
        event: &mut ::graphix_compiler::Event<E>,
    ) -> ::std::option::Option<::netidx::subscriber::Value> {
        if !self.args.update(ctx, from, event) {
            return ::std::option::Option::None;
        }
        match &self.args.0[..] {
            [::std::option::Option::Some(::netidx::subscriber::Value::I64(__a0))] => {
                let __r = fused_pixel_body(*__a0);
                ::std::option::Option::Some(::netidx::subscriber::Value::I64(__r))
            }
            _ => ::std::option::Option::None,
        }
    }

    fn sleep(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<R, E>) {
        self.args.clear()
    }
}
