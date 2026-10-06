#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, ExecCtx, Node, Rt, Scope, TagValue, UserEvent,
    effects::Effect, expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_package_core::CachedVals;
use netidx::subscriber::Value;
use netidx_core::pack::PackError;

#[derive(Debug)]
struct MandelbrotIterate {
    args: CachedVals,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for MandelbrotIterate {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "bench_mandelbrot_iterate";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(MandelbrotIterate {
            args: CachedVals::new(from),
            out: TagValue::phantom(),
        }))
    }

    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let args = CachedVals::image_decode(buf)?;
        Ok(Box::new(MandelbrotIterate { args, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for MandelbrotIterate {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.args.image_encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if !self.args.update(ctx, from) {
            return self.out.ride();
        }
        let res = match &self.args.0[..] {
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
        };
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.args.clear()
    }
}

pub mod auto_iterate;
pub mod auto_pixel;
pub use auto_iterate::FusedIterateAuto;
pub use auto_pixel::FusedPixelAuto;

// CR claude for claude: [dead] Nothing in this workspace or ../netidx calls
// bench::mandelbrot_iterate, iterate_auto or pixel_auto. Even so, the shell's default
// `all` feature compiles this package and registers it in every session and image, and
// every change to the BuiltIn, Apply or image traits has to edit it. Its docs name
// things that do not exist: mod.gxi and auto_*.rs credit
// `graphix_compiler::fusion::emit_function_kernel` and
// `bench/mandelbrot_bench_annotated.gx`, and design/strict_fusion.md:98 calls
// mandelbrot_iterate the bench's un-fused comparison point, though no bench calls it.
// FusedIterateAuto and FusedPixelAuto keep the default Effect::Async for pure
// functions, and mandelbrot_iterate with a negative max_iter on a point that never
// escapes loops about 2^63 times inside one update, which polls no interrupt. Deleting
// the package also means dropping INTERNAL_PACKAGES (graphix-package/src/lib.rs:231)
// and the assertion at graphix-package/src/test.rs:777. probe:
// design/review-2026-10-05/repro/small-pkgs-18.gx (small-pkgs-18)
graphix_derive::defpackage! {
    builtins => [MandelbrotIterate, FusedIterateAuto, FusedPixelAuto],
}
