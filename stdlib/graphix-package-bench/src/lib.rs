#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use graphix_compiler::{
    expr::ExprId, typ::FnType, Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope, UserEvent,
};
use graphix_package_core::CachedVals;
use netidx::subscriber::Value;

#[derive(Debug)]
struct MandelbrotIterate {
    args: CachedVals,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for MandelbrotIterate {
    const NAME: &str = "bench_mandelbrot_iterate";
    const NEEDS_CALLSITE: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(MandelbrotIterate { args: CachedVals::new(from) }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for MandelbrotIterate {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if !self.args.update(ctx, from, event) {
            return None;
        }
        match &self.args.0[..] {
            [
                Some(Value::F64(zr0)),
                Some(Value::F64(zi0)),
                Some(Value::F64(cr)),
                Some(Value::F64(ci)),
                Some(Value::I64(max_iter)),
            ] => {
                let cr = *cr;
                let ci = *ci;
                let mut zr = *zr0;
                let mut zi = *zi0;
                let mut i = *max_iter;
                let n: i64 = loop {
                    if i == 0 {
                        break 0;
                    }
                    if zr * zr + zi * zi > 4.0 {
                        break i;
                    }
                    let nzr = zr * zr - zi * zi + cr;
                    let nzi = 2.0 * zr * zi + ci;
                    zr = nzr;
                    zi = nzi;
                    i -= 1;
                };
                Some(Value::I64(n))
            }
            _ => None,
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.args.clear()
    }
}

pub mod auto_iterate;
pub mod auto_pixel;
pub use auto_iterate::FusedIterateAuto;
pub use auto_pixel::FusedPixelAuto;

graphix_derive::defpackage! {
    builtins => [MandelbrotIterate, FusedIterateAuto, FusedPixelAuto],
}
