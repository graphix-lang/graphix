//! The server's stdio transport. The reader queues every message as it
//! arrives, so the server sees a burst as one: lsp-server's own channel
//! has no capacity, so its queue always looks empty.

use crossbeam_channel::unbounded;
use lsp_server::{Connection, Message};
use std::{
    io::{self, BufRead, ErrorKind, stdin, stdout},
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

/// Read the next frame's body into `body`: false at the end of input,
/// an error for one that is not framed (a header without a
/// `Content-Length`, the input ending inside a frame).
fn read_frame(
    r: &mut impl BufRead,
    header: &mut String,
    body: &mut Vec<u8>,
) -> io::Result<bool> {
    let invalid =
        |e: &dyn std::fmt::Display| io::Error::new(ErrorKind::InvalidData, e.to_string());
    let mut len = None;
    loop {
        header.clear();
        if r.read_line(header)? == 0 {
            return match len {
                None => Ok(false),
                Some(_) => Err(invalid(&"the input ended inside a frame's header")),
            };
        }
        let line = header.trim_end_matches(['\r', '\n']);
        if line.is_empty() {
            break;
        }
        if let Some((name, value)) = line.split_once(':')
            && name.eq_ignore_ascii_case("content-length")
        {
            len = Some(value.trim().parse::<usize>().map_err(|e| invalid(&e))?);
        }
    }
    let len = len.ok_or_else(|| invalid(&"a frame without a Content-Length"))?;
    body.resize(len, 0);
    r.read_exact(body)?;
    Ok(true)
}

/// A connection over stdin and stdout.
pub fn stdio() -> io::Result<(Connection, IoThreads)> {
    let (to_server, receiver) = unbounded();
    let (sender, from_server) = unbounded::<Message>();
    let reader = thread::Builder::new().name("lsp reader".into()).spawn(move || {
        let mut stdin = stdin().lock();
        let (mut header, mut body) = (String::new(), Vec::new());
        while read_frame(&mut stdin, &mut header, &mut body)? {
            let msg = match serde_json::from_slice::<Message>(&body) {
                Ok(msg) => msg,
                Err(e) => {
                    log::warn!("ignored a frame that is no JSON-RPC message: {e}");
                    continue;
                }
            };
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
