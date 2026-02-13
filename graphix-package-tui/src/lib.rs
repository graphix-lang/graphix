use anyhow::Result;
use async_trait::async_trait;
use fxhash::FxHashMap;
use graphix_compiler::{
    env::Env,
    expr::{ExprId, ModPath},
    typ::Type,
};
use graphix_package::{CustomDisplay, Package};
use graphix_rt::{CompExp, GXExt, GXHandle, GXRt};
use netidx::publisher::Value;
use std::sync::LazyLock;
use triomphe::Arc;

pub(crate) mod tui;

use tui::Tui;

static TUITYP: LazyLock<Type> = LazyLock::new(|| Type::Ref {
    scope: ModPath::root(),
    name: ModPath::from(["tui", "Tui"]),
    params: Arc::from_iter([]),
});

#[async_trait]
impl<X: GXExt> CustomDisplay<X> for Tui<X> {
    async fn clear(&mut self) {
        self.stop().await;
    }

    async fn process_update(&mut self, _env: &Env, id: ExprId, v: Value) {
        self.update(id, v).await;
    }
}

graphix_derive::defpackage! {
    builtins => [],
    is_custom => |gx, env, e| {
        if let Some(typ) = e.typ.with_deref(|t| t.cloned())
            && typ != Type::Bottom
            && typ != Type::Any
        {
            TUITYP.contains(env, &typ).unwrap_or(false)
        } else {
            false
        }
    },
    init_custom => |gx, env, stop, e| {
        Ok(Box::new(Tui::<X>::start(gx, env.clone(), e)))
    },
}
