//! The server's stdio transport. The reader queues every message as it
//! arrives, so the server sees a burst as one: lsp-server's own channel
//! has no capacity, so its queue always looks empty.

use crossbeam_channel::unbounded;
use lsp_server::{Connection, Message};
use std::{
    io::{self, stdin, stdout},
    thread::{self, JoinHandle},
};

pub struct IoThreads {
    reader: JoinHandle<io::Result<()>>,
    writer: JoinHandle<io::Result<()>>,
}

impl IoThreads {
    /// Wait for the reader to see `exit` or the end of input, and the
    /// writer to flush what the server sent.
    pub fn join(self) -> io::Result<()> {
        for t in [self.reader, self.writer] {
            match t.join() {
                Ok(r) => r?,
                Err(e) => std::panic::resume_unwind(e),
            }
        }
        Ok(())
    }
}

/// A connection over stdin and stdout.
pub fn stdio() -> io::Result<(Connection, IoThreads)> {
    let (to_server, receiver) = unbounded();
    let (sender, from_server) = unbounded::<Message>();
    let reader = thread::Builder::new().name("lsp reader".into()).spawn(move || {
        let mut stdin = stdin().lock();
        while let Some(msg) = Message::read(&mut stdin)? {
            let exit = matches!(&msg, Message::Notification(n) if n.method == "exit");
            if to_server.send(msg).is_err() || exit {
                break;
            }
        }
        Ok(())
    })?;
    let writer = thread::Builder::new().name("lsp writer".into()).spawn(move || {
        let mut stdout = stdout().lock();
        from_server.into_iter().try_for_each(|msg| msg.write(&mut stdout))
    })?;
    Ok((Connection { sender, receiver }, IoThreads { reader, writer }))
}
