use super::{Env, Output, completion::BComplete};
use anyhow::{Error, Result, bail};
use futures::{StreamExt, channel::mpsc};
use graphix_rt::GXExt;
use reedline::{
    DefaultPrompt, DefaultPromptSegment, Emacs, IdeMenu, KeyCode, KeyModifiers,
    MenuBuilder, Reedline, ReedlineEvent, ReedlineMenu, Signal,
    default_emacs_keybindings,
};
use std::io::IsTerminal;
use tokio::{sync::oneshot, task};

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
    ) -> Result<Signal> {
        match output {
            Output::Custom(cdc) => match (&mut cdc.stop).await {
                Ok(Ok(())) => Ok(Signal::CtrlC),
                Ok(Err(e)) => Err(e.context("the display failed")),
                Err(_) => bail!("the display died"),
            },
            // CR claude for eric: [bug/medium] With a stdin that is not a terminal, a
            // REPL line that produces a value leaves the output as `Output::Text`. From
            // then on, this arm waits only for Ctrl-C and never polls the line reader
            // again. Later piped lines are never compiled, and EOF (the reader's
            // `Signal::CtrlD`) is never seen. The shell hangs until it is signalled:
            // `printf '1 + 1\nsys::exit(0)\n' | graphix -n` prints 2 and never exits
            // (probe: design/review-2026-10-12/repro/x-fix-tools-03.sh). Piped input
            // needs a rule, and the choice decides what a piped session prints. Option
            // one: for a non-terminal stdin, keep reading lines while a text output is
            // showing, so a new line replaces the output the way Enter does after
            // Ctrl-C, and treat EOF as Ctrl-D. The cost is that a value still arriving
            // when the next line or EOF lands is lost. Option two: on EOF, keep
            // streaming the last output until `sys::exit` or a signal, but still read
            // the lines before it. Also reported as (x-rerun-1-03). (x-fix-tools-03)
            Output::EmptyScript | Output::Text(_) => {
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
