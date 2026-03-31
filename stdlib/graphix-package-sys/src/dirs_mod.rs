use graphix_compiler::{
    expr::ExprId, typ::FnType, Apply, BuiltIn, Event, ExecCtx, Node, Rt, Scope, UserEvent,
};
use netidx::subscriber::Value;

macro_rules! dirs_builtin {
    ($name:ident, $builtin:literal, $fn:path) => {
        #[derive(Debug)]
        pub(crate) struct $name {
            fired: bool,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $name {
            const NAME: &str = $builtin;
            const NEEDS_CALLSITE: bool = false;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut ExecCtx<R, E>,
                _typ: &'a FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                _from: &'c [Node<R, E>],
                _top_id: ExprId,
            ) -> anyhow::Result<Box<dyn Apply<R, E>>> {
                Ok(Box::new(Self { fired: false }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $name {
            fn update(
                &mut self,
                _ctx: &mut ExecCtx<R, E>,
                _from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> Option<Value> {
                if event.init && !self.fired {
                    self.fired = true;
                    match $fn() {
                        Some(p) => Some(Value::String(crate::convert_path(&p))),
                        None => Some(Value::Null),
                    }
                } else {
                    None
                }
            }

            fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

            fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
                self.fired = false;
            }
        }
    };
}

dirs_builtin!(HomeDir, "sys_dirs_home_dir", dirs::home_dir);
dirs_builtin!(CacheDir, "sys_dirs_cache_dir", dirs::cache_dir);
dirs_builtin!(ConfigDir, "sys_dirs_config_dir", dirs::config_dir);
dirs_builtin!(ConfigLocalDir, "sys_dirs_config_local_dir", dirs::config_local_dir);
dirs_builtin!(DataDir, "sys_dirs_data_dir", dirs::data_dir);
dirs_builtin!(DataLocalDir, "sys_dirs_data_local_dir", dirs::data_local_dir);
dirs_builtin!(ExecutableDir, "sys_dirs_executable_dir", dirs::executable_dir);
dirs_builtin!(PreferenceDir, "sys_dirs_preference_dir", dirs::preference_dir);
dirs_builtin!(RuntimeDir, "sys_dirs_runtime_dir", dirs::runtime_dir);
dirs_builtin!(StateDir, "sys_dirs_state_dir", dirs::state_dir);
dirs_builtin!(AudioDir, "sys_dirs_audio_dir", dirs::audio_dir);
dirs_builtin!(DesktopDir, "sys_dirs_desktop_dir", dirs::desktop_dir);
dirs_builtin!(DocumentDir, "sys_dirs_document_dir", dirs::document_dir);
dirs_builtin!(DownloadDir, "sys_dirs_download_dir", dirs::download_dir);
dirs_builtin!(FontDir, "sys_dirs_font_dir", dirs::font_dir);
dirs_builtin!(PictureDir, "sys_dirs_picture_dir", dirs::picture_dir);
dirs_builtin!(PublicDir, "sys_dirs_public_dir", dirs::public_dir);
dirs_builtin!(TemplateDir, "sys_dirs_template_dir", dirs::template_dir);
dirs_builtin!(VideoDir, "sys_dirs_video_dir", dirs::video_dir);
