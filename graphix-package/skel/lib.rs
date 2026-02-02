use anyhow::Result;
use graphix_compiler::{expr::ModuleResolver, ExecCtx};
use graphix_package::Package;
use graphix_rt::{GXExt, GXRt};

#[derive(Package)]
pub struct T;

#[graphix_package]
impl<X: GXExt> Package<X> for T {
    type CustomDisplay = ();

    fn register(
        &mut self,
        ctx: ExecCtx<GXRt<X>, <X as GXExt>::UserEvent>,
    ) -> Result<ModuleResolver> {
        todo!()
    }
}
