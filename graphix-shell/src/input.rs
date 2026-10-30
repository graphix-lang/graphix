use super::{Env, Key, Output, completion::BComplete};
use anyhow::{Error, Result, bail};
use crossterm::event::Event;
use futures::{StreamExt, channel::mpsc};
use graphix_package::Ended;
use graphix_rt::GXExt;
use reedline::{
    DefaultPrompt, DefaultPromptSegment, EditMode, Emacs, IdeMenu, KeyCode, KeyModifiers,
    MenuBuilder, PromptEditMode, Reedline, ReedlineEvent, ReedlineMenu, ReedlineRawEvent,
    Signal, default_emacs_keybindings,
};
use std::{
    io::IsTerminal,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};
use tokio::{sync::oneshot, task};

/// What the shell reads next: a line editor signal, a key that leaves
/// the display or interrupts the runtime, or the display that showed the
/// output ended because the program asked it to.
pub(super) enum Read {
    Signal(Signal),
    Key(Key),
    Ended,
}

/// Emacs editing, with the interrupt key, Ctrl-\ (Ctrl-4 as a terminal
/// reports its control code), abandoning the line as Ctrl-C does and
/// raising `interrupt`, which tells the two apart.
struct EditKeys {
    emacs: Emacs,
    interrupt: Arc<AtomicBool>,
}

impl EditMode for EditKeys {
    fn parse_event(&mut self, event: ReedlineRawEvent) -> ReedlineEvent {
        let event = Event::from(event);
        if let Event::Key(k) = &event
            && k.modifiers == KeyModifiers::CONTROL
            && let KeyCode::Char('\\' | '4') = k.code
        {
            self.interrupt.store(true, Ordering::Relaxed);
            return ReedlineEvent::CtrlC;
        }
        match ReedlineRawEvent::try_from(event) {
            Ok(event) => self.emacs.parse_event(event),
            Err(()) => ReedlineEvent::None,
        }
    }

    fn edit_mode(&self) -> PromptEditMode {
        self.emacs.edit_mode()
    }
}

type Lines = mpsc::UnboundedReceiver<(oneshot::Sender<Option<Env>>, Result<Read>)>;

pub(super) struct InputReader {
    go: Option<oneshot::Sender<Option<Env>>>,
    recv: Lines,
}

impl InputReader {
    pub(super) fn run(mut c_rx: oneshot::Receiver<Option<Env>>) -> Lines {
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
            let interrupt = Arc::new(AtomicBool::new(false));
            // a stdin that is no terminal is read line by line
            let mut line_editor = std::io::stdin().is_terminal().then(|| {
                let keys = EditKeys {
                    emacs: Emacs::new(keybinds),
                    interrupt: interrupt.clone(),
                };
                Reedline::create()
                    .with_menu(ReedlineMenu::EngineCompleter(Box::new(menu)))
                    .with_edit_mode(Box::new(keys))
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
                    Some(ed) => {
                        // takes the wake a TUI display's crossterm event
                        // stream can leave, which fails the line editor's
                        // first read (UPSTREAM.md)
                        let _ = crossterm::event::poll(Duration::ZERO);
                        ed.read_line(&prompt).map_err(Error::from)
                    }
                    None => {
                        let mut line = String::new();
                        match std::io::stdin().read_line(&mut line) {
                            Ok(0) => Ok(Signal::CtrlD),
                            Ok(_) => Ok(Signal::Success(line)),
                            Err(e) => Err(e.into()),
                        }
                    }
                });
                let r = r.map(|sig| match sig {
                    Signal::CtrlC if interrupt.swap(false, Ordering::Relaxed) => {
                        Read::Key(Key::Interrupt)
                    }
                    sig => Read::Signal(sig),
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
                Ok(Ok(Ended::Left)) => Ok(Read::Key(Key::Leave)),
                Ok(Ok(Ended::Interrupted)) => Ok(Read::Key(Key::Interrupt)),
                Ok(Ok(Ended::Requested)) => Ok(Read::Ended),
                Ok(Err(e)) => Err(e.context("the display failed")),
                Err(_) => bail!("the display died"),
            },
            // a REPL's piped lines go on being read while a value shows, a
            // new one replacing it
            Output::Text(_) if !script && !std::io::stdin().is_terminal() => {
                self.next_line(env).await
            }
            // left by Ctrl-C, which the terminal sends the shell as SIGINT
            // XCR claude for claude: [design/medium] At a terminal a value display
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
            // 2026-10-30 claude: Eric ruled (2026-10-30, follow-up): the interrupt
            // key is Ctrl-\ (SIGQUIT, delivered by the terminal like Ctrl-C), not
            // Ctrl-Alt-I: the shell does not read the keyboard during a display or
            // an evaluation.
            // 2026-10-30 claude: Built per the rulings. Ctrl-C never stops the
            // runtime: at a value display, where the terminal sends it as SIGINT, it
            // leaves the display with every binding kept; at the prompt it is
            // reedline's own Ctrl-C (abandons the line); while an input waits on a
            // busy runtime it stops nothing and prints a hint naming Ctrl-\ (lib.rs
            // busy). Ctrl-\ is the interrupt, uniform: SIGQUIT at a display or while
            // busy (the session's key task stops the runtime), a key at the prompt
            // (input.rs EditKeys: reedline's Ctrl-C plus a flag), Ended::Interrupted
            // from a TUI, whose Ctrl-C is the new Ended::Left; Ctrl-Break on Windows
            // (uncompiled there). A script ends with 130 at either key. Pins, all
            // failing on 5f4b4677f: graphix-shell/tests/repl_interrupt.rs
            // ctrl_c_at_a_terminal_display_keeps_the_session,
            // ctrl_c_at_a_terminal_prompt_keeps_the_session,
            // ctrl_c_at_an_idle_prompt_keeps_the_session,
            // ctrl_c_while_busy_names_the_interrupt,
            // ctrl_backslash_at_a_terminal_prompt_resets_the_session,
            // ctrl_backslash_at_a_terminal_display_resets_the_session,
            // interrupt_at_an_idle_prompt_resets_the_session,
            // interrupt_frees_a_wedged_repl, tui_display_keys; interrupt_wedge.rs
            // quit_frees_first_cycle_wedge_jit and quit_frees_later_cycle_wedge_jit;
            // tui render_test the_program_sees_size_and_events and terminal_test
            // ctrl_backslash_is_read_while_work_waits (fail with the Ctrl-\ key
            // unmatched).
            Output::EmptyScript | Output::Text(_) => std::future::pending().await,
            Output::None => self.next_line(env).await,
        }
    }

    /// The next line the reader takes, started over `env` if none is
    /// being read.
    async fn next_line(&mut self, env: &mut Option<Env>) -> Result<Read> {
        if let Some(tx) = self.go.take() {
            let _ = tx.send(env.take());
        }
        match self.recv.next().await {
            None => bail!("input stream ended"),
            Some((go, read)) => {
                self.go = Some(go);
                read
            }
        }
    }
}
