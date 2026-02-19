use zed_extension_api::{self as zed, LanguageServerId, Result};

struct GraphixExtension;

impl zed::Extension for GraphixExtension {
    fn new() -> Self {
        GraphixExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let path = worktree
            .which("graphix")
            .ok_or_else(|| "graphix not found in PATH".to_string())?;
        Ok(zed::Command {
            command: path,
            args: vec!["lsp".into()],
            env: Default::default(),
        })
    }
}

zed::register_extension!(GraphixExtension);
