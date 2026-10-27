use netidx::subscriber::Value;

/// A builtin that produces `$value` once, where it is born (again at a
/// rebirth), and nothing after: `$ctx` names the `ExecCtx` for `$value`.
macro_rules! born_builtin {
    ($name:ident, $builtin:literal, |$ctx:ident| $value:expr) => {
        #[derive(Debug)]
        pub(crate) struct $name {
            once: ::graphix_package_core::FireOnce,
            out: ::graphix_compiler::TagValue,
        }

        impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent> ::graphix_compiler::BuiltIn<R, E> for $name {
            // XCR claude for claude: [bug/medium] A born builtin answers in the update
            // where it is born and never later, which is Sync in effects.rs's terms
            // (same cycle, a once-flag of state), not Async. The Async label makes
            // `#[sync]` refuse any function that reads sys::dirs::config_dir() or
            // sys::args(). It also makes select.rs::acts_on_the_world treat home_dir()
            // in an arm as an effect, which draws a spurious warn_birth_only warning.
            // args::Parse has the same mislabel. Declare Effect::Sync here and in
            // args::Parse. probe:
            // design/review-2026-10-26/repro/x-builtin-effects-born-builtin-async.gx
            // (x-builtin-effects-03)
            // 2026-10-27 claude: born_builtin (sys::args, sys::dirs::*) and
            // args::Parse declare Effect::Sync: each answers in the update its input
            // or birth fires and keeps only its latch or resident. Pin:
            // lib_tests::sys::born_builtins_and_parse_are_sync (#[sync] over
            // config_dir, sys::args and args::parse; each refused alone with its Async
            // restored). The arm warning half was sys-io-05.
            const EFFECT: ::graphix_compiler::effects::Effect =
                ::graphix_compiler::effects::Effect::Sync;
            const NAME: &str = $builtin;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut ::graphix_compiler::CompileCtx<R, E>,
                _typ: &'a ::graphix_compiler::typ::FnType,
                _resolved: Option<&'d ::graphix_compiler::typ::FnType>,
                _scope: &'b ::graphix_compiler::Scope,
                _from: &'c [::graphix_compiler::Node<R, E>],
                _top_id: ::graphix_compiler::expr::ExprId,
            ) -> anyhow::Result<Box<dyn ::graphix_compiler::Apply<R, E>>> {
                Ok(Box::new(Self { once: ::graphix_package_core::FireOnce::default(), out: ::graphix_compiler::TagValue::phantom() }))
            }

            fn image_decode(
                _ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>,
                _from: &[::graphix_compiler::Node<R, E>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn ::graphix_compiler::Apply<R, E>>, ::netidx_core::pack::PackError> {
                let once = <::graphix_package_core::FireOnce as ::netidx_core::pack::Pack>::decode(buf)?;
                Ok(Box::new(Self { once, out: ::graphix_compiler::TagValue::phantom() }))
            }
        }

        impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent> ::graphix_compiler::Apply<R, E> for $name {
            fn image_encode(&self, buf: &mut ::graphix_compiler::image::ImageBuf,
            ) -> Result<(), ::netidx_core::pack::PackError> {
                ::netidx_core::pack::Pack::encode(&self.once, buf)
            }

            fn update(
                &mut self,
                ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>,
                _from: &mut [::graphix_compiler::Node<R, E>],
            ) -> &::graphix_compiler::TagValue {
                if ctx.event.born() && self.once.take() {
                    let $ctx = &mut *ctx;
                    let v = $value;
                    self.out.set(::graphix_compiler::TagValue::fired(v))
                } else {
                    self.out.ride()
                }
            }

            fn delete(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>) {}

            fn sleep(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>) {}

            fn reborn(&mut self, _ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>) {
                self.once.reset();
            }
        }
    };
}

pub(crate) use born_builtin;

macro_rules! dirs_builtin {
    ($name:ident, $builtin:literal, $fn:path) => {
        born_builtin!($name, $builtin, |_ctx| match $fn() {
            Some(p) => Value::String(crate::convert_path(&p)),
            None => Value::Null,
        });
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
