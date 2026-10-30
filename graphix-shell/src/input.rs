use super::{Env, Output, completion::BComplete};
use anyhow::{Error, Result, bail};
use futures::{StreamExt, channel::mpsc};
use graphix_package::Ended;
use graphix_rt::GXExt;
use reedline::{
    DefaultPrompt, DefaultPromptSegment, Emacs, IdeMenu, KeyCode, KeyModifiers,
    MenuBuilder, Reedline, ReedlineEvent, ReedlineMenu, Signal,
    default_emacs_keybindings,
};
use std::io::IsTerminal;
use tokio::{sync::oneshot, task};

/// What the shell reads next: a line editor signal, or the display that
/// showed the output ended because the program asked it to.
pub(super) enum Read {
    Signal(Signal),
    Ended,
}

pub(super) struct InputReader {
    go: Option<oneshot::Sender<Option<Env>>>,
    recv: mpsc::UnboundedReceiver<(oneshot::Sender<Option<Env>>, Result<Signal>)>,
}

impl InputReader {
    pub(super) fn run(
        mut c_rx: oneshot::Receiver<Option<Env>>,
    ) -> mpsc::UnboundedReceiver<(oneshot::Sender<Option<Env>>, Result<Signal>)> {
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
            // a stdin that is no terminal is read line by line
            let mut line_editor = std::io::stdin().is_terminal().then(|| {
                Reedline::create()
                    .with_menu(ReedlineMenu::EngineCompleter(Box::new(menu)))
                    .with_edit_mode(Box::new(Emacs::new(keybinds)))
            });

            let prompt = DefaultPrompt {
                left_prompt: DefaultPromptSegment::Basic("".into()),
                right_prompt: DefaultPromptSegment::Empty,
            };
            loop {
                match c_rx.await {
                    Err(_) => break, // shutting down
                    Ok(None) => (),
                    Ok(Some(env)) => {
                        line_editor = line_editor
                            .map(|ed| ed.with_completer(Box::new(BComplete(env))));
                    }
                }
                let r = task::block_in_place(|| match &mut line_editor {
                    Some(ed) => ed.read_line(&prompt).map_err(Error::from),
                    None => {
                        let mut line = String::new();
                        match std::io::stdin().read_line(&mut line) {
                            Ok(0) => Ok(Signal::CtrlD),
                            Ok(_) => Ok(Signal::Success(line)),
                            Err(e) => Err(e.into()),
                        }
                    }
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

    pub(super) async fn read_line<X: GXExt>(
        &mut self,
        output: &mut Output<X>,
        env: &mut Option<Env>,
        script: bool,
    ) -> Result<Read> {
        match output {
            Output::Custom(cdc) => match (&mut cdc.stop).await {
                Ok(Ok(Ended::Interrupted)) => Ok(Read::Signal(Signal::CtrlC)),
                Ok(Ok(Ended::Requested)) => Ok(Read::Ended),
                Ok(Err(e)) => Err(e.context("the display failed")),
                Err(_) => bail!("the display died"),
            },
            // a REPL's piped lines go on being read while a value shows, a
            // new one replacing it
            Output::Text(_) if !script && !std::io::stdin().is_terminal() => {
                self.next_line(env).await.map(Read::Signal)
            }
            // left by Ctrl-C, which the shell hears as SIGINT
            // CR claude for claude: [design/medium] At a terminal a value display
            // gives the prompt back only at Ctrl-C, and Ctrl-C now resets the
            // session (D7), so a REPL keeps no binding across anything it shows:
            // `let x = 41`, `x + 1`, Ctrl-C, then `x` is undefined; before D7 the
            // same Ctrl-C left the display and kept `x`. This follows the ruling
            // ("ctrl-c to clear the rt, ctrl-d to exit"), which may not have
            // counted Ctrl-C's second job. If it did not: keep Ctrl-C uniform and
            // let another key (Enter or Esc) leave a text display without
            // stopping anything; a piped REPL is unaffected. probe:
            // ~/tmp/cr29/scratch/review2/RV6/repl_tty_value.py (repl-value-reset-01)
            // 2026-10-30 claude: Eric ruled (2026-10-30, cr-discuss round 4 quick
            // picks): Ctrl-C gives the prompt back and keeps the session, as before
            // round 3 D7; the interrupt (stop the runtime and replace it with a
            // fresh one) moves to another key, Ctrl-Alt-I.
            Output::EmptyScript | Output::Text(_) => std::future::pending().await,
            Output::None => self.next_line(env).await.map(Read::Signal),
        }
    }

    /// The next line the reader takes, started over `env` if none is
    /// being read.
    async fn next_line(&mut self, env: &mut Option<Env>) -> Result<Signal> {
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
