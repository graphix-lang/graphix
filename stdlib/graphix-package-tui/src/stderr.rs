use anyhow::{Context, Result, bail};
use parking_lot::Mutex;
use poolshark::global::{GPooled, Pool};
use std::{
    fs::File,
    io::{self, PipeReader, PipeWriter, Read, Write},
    os::fd::{AsRawFd, BorrowedFd, OwnedFd},
    sync::{LazyLock, mpsc},
    thread,
};

const KEEP: usize = 32 * 1024;
static BUFFERS: LazyLock<Pool<Vec<u8>>> = LazyLock::new(|| Pool::new(4, 2 * KEEP));

struct HeldOutput {
    bytes: GPooled<Vec<u8>>,
    head: usize,
    tail: usize,
    next: usize,
    total: u64,
}

impl HeldOutput {
    fn new() -> Self {
        let mut bytes = BUFFERS.take();
        bytes.resize(2 * KEEP, 0);
        Self { bytes, head: 0, tail: 0, next: 0, total: 0 }
    }

    fn push(&mut self, input: &[u8]) {
        self.total = self.total.saturating_add(input.len() as u64);
        for &byte in input {
            if self.head < KEEP {
                self.bytes[self.head] = byte;
                self.head += 1;
            } else {
                self.bytes[KEEP + self.next] = byte;
                self.next = (self.next + 1) % KEEP;
                self.tail = (self.tail + 1).min(KEEP);
            }
        }
    }

    fn replay(&self, out: &mut impl Write) -> io::Result<()> {
        out.write_all(&self.bytes[..self.head])?;
        let dropped = self.total.saturating_sub((self.head + self.tail) as u64);
        if dropped > 0 {
            writeln!(out, "\n... {dropped} bytes dropped ...")?;
        }
        let start = if self.tail == KEEP { self.next } else { 0 };
        out.write_all(&self.bytes[KEEP + start..KEEP + self.tail])?;
        out.write_all(&self.bytes[KEEP..KEEP + start])
    }
}

/// Instructions to the reader, one byte each on its control pipe.
const HOLD: u8 = b'h';
const RELEASE: u8 = b'r';

/// Serve the capture pipe for the life of the process: while held, keep
/// what arrives; between holds, forward it to the real stderr. The read
/// end is never closed, so a child that inherited the pipe while stderr
/// was held can always write to it.
fn serve(
    mut input: PipeReader,
    mut control: PipeReader,
    mut out: File,
    acks: mpsc::Sender<()>,
) -> Result<()> {
    let mut held: Option<HeldOutput> = None;
    let mut chunk = [0; 4096];
    loop {
        let mut fds = [
            libc::pollfd { fd: control.as_raw_fd(), events: libc::POLLIN, revents: 0 },
            libc::pollfd { fd: input.as_raw_fd(), events: libc::POLLIN, revents: 0 },
        ];
        // Both descriptors remain owned here for the whole poll.
        if unsafe { libc::poll(fds.as_mut_ptr(), fds.len() as _, -1) } < 0 {
            let error = io::Error::last_os_error();
            if error.kind() == io::ErrorKind::Interrupted {
                continue;
            }
            return Err(error.into());
        }
        if fds[0].revents != 0 {
            let mut op = [0];
            control.read_exact(&mut op)?;
            match op[0] {
                HOLD => held = Some(HeldOutput::new()),
                _ => {
                    if let Some(mut output) = held.take() {
                        drain(&mut input, &mut output, &mut chunk)?;
                        let _ = output.replay(&mut out);
                    }
                }
            }
            let _ = acks.send(());
            continue;
        }
        match input.read(&mut chunk) {
            Ok(n) => match &mut held {
                Some(output) => output.push(&chunk[..n]),
                None => {
                    let _ = out.write_all(&chunk[..n]);
                }
            },
            Err(e) if e.kind() == io::ErrorKind::Interrupted => (),
            Err(e) => return Err(e.into()),
        }
    }
}

/// Keep what is in the pipe now: what was written while it was held.
fn drain(
    input: &mut PipeReader,
    output: &mut HeldOutput,
    chunk: &mut [u8],
) -> Result<()> {
    let mut available: libc::c_int = 0;
    // FIONREAD writes one c_int; input still owns the pipe descriptor.
    if unsafe { libc::ioctl(input.as_raw_fd(), libc::FIONREAD, &mut available) } < 0 {
        return Err(io::Error::last_os_error().into());
    }
    let mut remaining = available.max(0) as usize;
    while remaining > 0 {
        let limit = remaining.min(chunk.len());
        match input.read(&mut chunk[..limit]) {
            Ok(n) => {
                output.push(&chunk[..n]);
                remaining -= n;
            }
            Err(e) if e.kind() == io::ErrorKind::Interrupted => (),
            Err(e) => return Err(e.into()),
        }
    }
    Ok(())
}

/// The process's one capture pipe and its reader, made at the first hold.
struct Capture {
    /// The pipe's write end, put on fd 2 while held.
    writer: PipeWriter,
    /// The stderr that was there before the first hold.
    saved: OwnedFd,
    control: PipeWriter,
    acks: mpsc::Receiver<()>,
    reader: thread::ThreadId,
    held: bool,
}

impl Capture {
    fn new() -> Result<Self> {
        // The process owns its standard error descriptor.
        let saved = unsafe { BorrowedFd::borrow_raw(2) }.try_clone_to_owned()?;
        let out = File::from(saved.try_clone()?);
        let (input, writer) = io::pipe()?;
        let (control_rx, control) = io::pipe()?;
        let (ack_tx, acks) = mpsc::channel();
        let reader = thread::Builder::new()
            .name("tui-stderr".into())
            .spawn(move || serve(input, control_rx, out, ack_tx))
            .context("capture stderr")?
            .thread()
            .id();
        Ok(Self { writer, saved, control, acks, reader, held: false })
    }

    /// Tell the reader `op` and wait until it has done it, unless this
    /// is the reader itself (a panic there).
    fn tell(&mut self, op: u8) {
        if self.control.write_all(&[op]).is_ok() && thread::current().id() != self.reader
        {
            let _ = self.acks.recv();
        }
    }

    fn hold(&mut self) -> Result<()> {
        if self.held {
            bail!("stderr is already held");
        }
        self.tell(HOLD);
        // writer owns the source descriptor, and fd 2 is the process's stderr.
        if unsafe { libc::dup2(self.writer.as_raw_fd(), 2) } < 0 {
            self.tell(RELEASE);
            return Err(io::Error::last_os_error().into());
        }
        self.held = true;
        Ok(())
    }

    fn release(&mut self) {
        if std::mem::take(&mut self.held) {
            // saved owns the descriptor that the first hold cloned from stderr.
            unsafe { libc::dup2(self.saved.as_raw_fd(), 2) };
            self.tell(RELEASE);
        }
    }
}

static CAPTURE: Mutex<Option<Capture>> = Mutex::new(None);

/// Stderr held while a display has the terminal: what is written to it
/// is kept, and written out when this drops.
pub(super) struct StderrHeld;

pub(super) fn restore_for_panic() {
    if let Some(mut capture) = CAPTURE.try_lock()
        && let Some(capture) = &mut *capture
    {
        capture.release()
    }
}

impl StderrHeld {
    pub(super) fn hold() -> Result<Self> {
        let mut capture = CAPTURE.lock();
        let capture = match &mut *capture {
            Some(capture) => capture,
            None => capture.insert(Capture::new()?),
        };
        capture.hold()?;
        Ok(Self)
    }
}

impl Drop for StderrHeld {
    fn drop(&mut self) {
        if let Some(capture) = &mut *CAPTURE.lock() {
            capture.release()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use poolshark::local::LPooled;

    #[test]
    fn short_output_is_replayed_exactly() -> Result<()> {
        for length in [0, 1, KEEP - 1, KEEP, KEEP + 1, 2 * KEEP] {
            let input: LPooled<Vec<u8>> = (0..length).map(|i| (i % 251) as u8).collect();
            let mut held = HeldOutput::new();
            for chunk in input.chunks(173) {
                held.push(chunk);
            }
            let mut output: LPooled<Vec<u8>> = LPooled::take();
            held.replay(&mut *output)?;
            assert_eq!(*input, *output);
        }
        Ok(())
    }
}
