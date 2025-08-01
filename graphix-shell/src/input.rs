use super::{completion::BComplete, Env, Output};
use anyhow::{bail, Error, Result};
use futures::{channel::mpsc, StreamExt};
use graphix_rt::GXExt;
use reedline::{
    default_emacs_keybindings, DefaultPrompt, DefaultPromptSegment, Emacs, IdeMenu,
    KeyCode, KeyModifiers, MenuBuilder, Reedline, ReedlineEvent, ReedlineMenu, Signal,
};
use tokio::{sync::oneshot, task};

pub(super) struct InputReader<X: GXExt> {
    go: Option<oneshot::Sender<Option<Env<X>>>>,
    recv: mpsc::UnboundedReceiver<(oneshot::Sender<Option<Env<X>>>, Result<Signal>)>,
}

impl<X: GXExt> InputReader<X> {
    pub(super) fn run(
        mut c_rx: oneshot::Receiver<Option<Env<X>>>,
    ) -> mpsc::UnboundedReceiver<(oneshot::Sender<Option<Env<X>>>, Result<Signal>)> {
        let (tx, rx) = mpsc::unbounded();
        task::spawn(async move {
            let mut keybinds = default_emacs_keybindings();
            keybinds.add_binding(
                KeyModifiers::NONE,
                KeyCode::Tab,
                ReedlineEvent::UntilFound(vec![
                    ReedlineEvent::Menu("completion".into()),
                    ReedlineEvent::MenuNext,
                ]),
            );
            let menu = IdeMenu::default().with_name("completion");
            let mut line_editor = Reedline::create()
                .with_menu(ReedlineMenu::EngineCompleter(Box::new(menu)))
                .with_edit_mode(Box::new(Emacs::new(keybinds)));
            let prompt = DefaultPrompt {
                left_prompt: DefaultPromptSegment::Basic("".into()),
                right_prompt: DefaultPromptSegment::Empty,
            };
            loop {
                match c_rx.await {
                    Err(_) => break, // shutting down
                    Ok(None) => (),
                    Ok(Some(env)) => {
                        line_editor =
                            line_editor.with_completer(Box::new(BComplete(env)));
                    }
                }
                let r = task::block_in_place(|| {
                    line_editor.read_line(&prompt).map_err(Error::from)
                });
                let (o_tx, o_rx) = oneshot::channel();
                c_rx = o_rx;
                if let Err(_) = tx.unbounded_send((o_tx, r)) {
                    break;
                }
            }
        });
        rx
    }

    pub(super) fn new() -> Self {
        let (tx_go, rx_go) = oneshot::channel();
        let recv = Self::run(rx_go);
        Self { go: Some(tx_go), recv }
    }

    pub(super) async fn read_line(
        &mut self,
        output: &mut Output<X>,
        env: &mut Option<Env<X>>,
    ) -> Result<Signal> {
        match output {
            Output::Tui(tui) => Ok(tui.wait_signal().await),
            Output::Text(_) => {
                tokio::signal::ctrl_c().await?;
                Ok(Signal::CtrlC)
            }
            Output::None => {
                if let Some(tx) = self.go.take() {
                    let _ = tx.send(env.take());
                }
                match self.recv.next().await {
                    None => bail!("input stream ended"),
                    Some((go, sig)) => {
                        self.go = Some(go);
                        sig
                    }
                }
            }
        }
    }
}
