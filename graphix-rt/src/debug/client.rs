//! A debugger client over any byte stream: what `graphix debug` and a
//! test speak the protocol with.

use super::proto::{
    Command, Event, Hello, Outcome, Program, Reply, Request, Response, VERSION,
};
use anyhow::{Result, anyhow, bail};
use serde_derive::Deserialize;
use std::collections::VecDeque;
use tokio::io::{
    AsyncBufReadExt, AsyncRead, AsyncWrite, AsyncWriteExt, BufReader, Lines, ReadHalf,
    WriteHalf,
};

#[derive(Deserialize)]
#[serde(untagged)]
enum Line {
    Response(Response),
    Event(Event),
}

pub struct Client<S> {
    lines: Lines<BufReader<ReadHalf<S>>>,
    out: WriteHalf<S>,
    sent: u64,
    /// The events that arrived while a request waited for its answer.
    events: VecDeque<Event>,
    /// The answers that arrived while another was waited for.
    answers: Vec<(u64, Outcome)>,
    program: Option<Program>,
}

impl<S: AsyncRead + AsyncWrite + Unpin> Client<S> {
    /// Open a session over `stream`: the two hellos.
    pub async fn connect(stream: S) -> Result<Self> {
        let (rd, mut out) = tokio::io::split(stream);
        let mut lines = BufReader::new(rd).lines();
        let ours = Hello { graphix_debug: VERSION, program: None };
        let mut hello = serde_json::to_vec(&ours)?;
        hello.push(b'\n');
        out.write_all(&hello).await?;
        out.flush().await?;
        let Some(line) = lines.next_line().await? else {
            bail!("the debugger closed the connection")
        };
        let program = match serde_json::from_str::<Hello>(&line) {
            Ok(h) if h.graphix_debug == VERSION => h.program,
            Ok(h) => bail!("the debugger speaks version {}", h.graphix_debug),
            Err(e) => bail!("this is no graphix debugger: {e}"),
        };
        Ok(Self {
            lines,
            out,
            sent: 0,
            events: VecDeque::new(),
            answers: Vec::new(),
            program,
        })
    }

    /// What the debugger said of the program it debugs.
    pub fn program(&self) -> Option<&Program> {
        self.program.as_ref()
    }

    async fn line(&mut self) -> Result<Option<Line>> {
        match self.lines.next_line().await? {
            None => Ok(None),
            Some(line) => Ok(Some(serde_json::from_str(&line)?)),
        }
    }

    /// Send `cmd` and return its number, to take its answer by later
    /// ([`Self::answered`]): a `wait` is answered when the program is
    /// held, and the requests sent after it are answered meanwhile.
    pub async fn send(&mut self, cmd: Command) -> Result<u64> {
        self.sent += 1;
        let mut line = serde_json::to_vec(&Request { id: self.sent, cmd })?;
        line.push(b'\n');
        self.out.write_all(&line).await?;
        self.out.flush().await?;
        Ok(self.sent)
    }

    /// The answer to the request `id`, or its refusal; an error is the
    /// connection's. Events and other requests' answers that arrive
    /// meanwhile are kept for [`Self::event`] and their own takers.
    pub async fn answered(&mut self, id: u64) -> Result<Outcome> {
        loop {
            if let Some(i) = self.answers.iter().position(|(of, _)| *of == id) {
                break Ok(self.answers.swap_remove(i).1);
            }
            match self.line().await? {
                None => bail!("the debugger closed the connection"),
                Some(Line::Event(e)) => self.events.push_back(e),
                Some(Line::Response(r)) => self.answers.push((r.id, r.result)),
            }
        }
    }

    /// Ask `cmd` and wait for its answer or its refusal.
    pub async fn ask(&mut self, cmd: Command) -> Result<Outcome> {
        let id = self.send(cmd).await?;
        self.answered(id).await
    }

    /// [`Self::ask`], a refusal an error too.
    pub async fn request(&mut self, cmd: Command) -> Result<Reply> {
        match self.ask(cmd).await? {
            Outcome::Ok(reply) => Ok(reply),
            Outcome::Err(e) => Err(anyhow!("{e}")),
        }
    }

    /// An event that arrived before the last answer, if one is left.
    pub fn queued(&mut self) -> Option<Event> {
        self.events.pop_front()
    }

    /// Whether an event that arrived before the last answer is left.
    pub fn has_queued(&self) -> bool {
        !self.events.is_empty()
    }

    /// The next event; `None` once the debugger has closed the session.
    /// Cancel safe.
    pub async fn event(&mut self) -> Result<Option<Event>> {
        if let Some(e) = self.events.pop_front() {
            return Ok(Some(e));
        }
        loop {
            match self.line().await? {
                None => break Ok(None),
                Some(Line::Event(e)) => break Ok(Some(e)),
                Some(Line::Response(r)) => self.answers.push((r.id, r.result)),
            }
        }
    }
}
