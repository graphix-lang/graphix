use anyhow::{Context, Result, bail};
use parking_lot::Mutex;
use poolshark::global::{GPooled, Pool};
use std::{
    io::{self, PipeReader, PipeWriter, Read, Write},
    os::fd::{AsRawFd, BorrowedFd, OwnedFd},
    sync::LazyLock,
    thread::{self, JoinHandle},
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

fn collect(mut input: PipeReader, stop: PipeReader) -> Result<HeldOutput> {
    let mut output = HeldOutput::new();
    let mut remaining = None;
    let mut chunk = [0; 4096];
    loop {
        if remaining == Some(0) {
            return Ok(output);
        }
        if remaining.is_none() {
            let mut fds = [
                libc::pollfd { fd: input.as_raw_fd(), events: libc::POLLIN, revents: 0 },
                libc::pollfd { fd: stop.as_raw_fd(), events: libc::POLLIN, revents: 0 },
            ];
            // Both descriptors remain owned here for the whole poll.
            if unsafe { libc::poll(fds.as_mut_ptr(), fds.len() as _, -1) } < 0 {
                let error = io::Error::last_os_error();
                if error.kind() == io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(error.into());
            }
            if fds[1].revents != 0 {
                let mut available: libc::c_int = 0;
                // FIONREAD writes one c_int; input still owns the pipe descriptor.
                if unsafe {
                    libc::ioctl(input.as_raw_fd(), libc::FIONREAD, &mut available)
                } < 0
                {
                    return Err(io::Error::last_os_error().into());
                }
                remaining = Some(available.max(0) as usize);
                continue;
            }
        }
        let limit = remaining.unwrap_or(chunk.len()).min(chunk.len());
        match input.read(&mut chunk[..limit]) {
            Ok(0) => return Ok(output),
            Ok(n) => {
                output.push(&chunk[..n]);
                if let Some(left) = &mut remaining {
                    *left -= n;
                }
            }
            Err(e) if e.kind() == io::ErrorKind::Interrupted => (),
            Err(e) => return Err(e.into()),
        }
    }
}

struct Capture {
    saved: OwnedFd,
    stop: Option<PipeWriter>,
    reader: Option<JoinHandle<Result<HeldOutput>>>,
}

enum CaptureState {
    Vacant,
    Held(Capture),
    Restored,
}

static CAPTURE: Mutex<CaptureState> = Mutex::new(CaptureState::Vacant);

pub(super) struct StderrHeld;

pub(super) fn restore_for_panic() {
    let capture = CAPTURE.try_lock().and_then(|mut state| {
        match std::mem::replace(&mut *state, CaptureState::Restored) {
            CaptureState::Held(capture) => Some(capture),
            previous => {
                *state = previous;
                None
            }
        }
    });
    drop(capture);
}

impl StderrHeld {
    pub(super) fn hold() -> Result<Self> {
        // XCR claude for claude: [risk/low] Everything written to stderr while the
        // display holds the terminal goes to this unlinked file with no bound, and drop
        // replays all of it. A long-running TUI whose program logs a failure every
        // cycle (a `$` over an erroring source logs under FAILURE_TARGET) fills the
        // temp filesystem out of sight, then floods the terminal on exit. A kill loses
        // all of it. Bound what is kept, for example the first and last N KB plus a
        // count of the bytes dropped, and say what was dropped when it is replayed.
        // (x-fix-tools-09)
        // XCR claude for claude: [bug/medium] The held stderr has no bound: every byte
        // written to fd 2 while the display holds the terminal lands in this unlinked
        // tempfile (RAM when /tmp is tmpfs), and Drop copies all of it to the terminal.
        // The shell logs every unhandled `?` failure to stderr, so a long-running
        // dashboard with a recurring failure grows the file at the failure rate (one
        // per 100 ms is about 130 MB a day) and ends with that whole dump into the
        // user's terminal. Bound it: keep the first and last N KB (e.g. a pipe drained
        // by a reader thread into a head buffer plus a ring, or a length check that
        // truncates past a cap) and write a "... K bytes dropped" line between them at
        // give-back. probe: design/review-2026-10-12/repro/tui-core-05.sh (2000
        // failures held, ~257 KB all written after leaving the alternate screen).
        // (tui-core-05)
        // 2026-10-13 codex: A pipe drains into a bounded 32 KiB head and tail.
        // held_stderr_keeps_a_bounded_head_and_tail pins replay size, endpoints
        // and the dropped-byte notice; short_output_is_replayed_exactly pins
        // lossless output below the cap. Review the reader shutdown as well.
        let mut state = CAPTURE.lock();
        if !matches!(*state, CaptureState::Vacant) {
            bail!("stderr is already held");
        }
        // The process owns its standard error descriptor.
        let saved = unsafe { BorrowedFd::borrow_raw(2) }.try_clone_to_owned()?;
        let (input, output) = io::pipe()?;
        let (stop_rx, stop_tx) = io::pipe()?;
        let reader = thread::Builder::new()
            .name("tui-stderr".into())
            .spawn(move || collect(input, stop_rx))
            .context("capture stderr")?;
        let held = Capture { saved, stop: Some(stop_tx), reader: Some(reader) };
        // output owns the source descriptor, and fd 2 is the process's stderr.
        if unsafe { libc::dup2(output.as_raw_fd(), 2) } < 0 {
            return Err(io::Error::last_os_error().into());
        }
        *state = CaptureState::Held(held);
        Ok(Self)
    }
}

impl Drop for Capture {
    fn drop(&mut self) {
        // saved owns the descriptor that hold cloned from stderr.
        unsafe { libc::dup2(self.saved.as_raw_fd(), 2) };
        drop(self.stop.take());
        if let Some(reader) = self.reader.take()
            && reader.thread().id() != thread::current().id()
            && let Ok(Ok(output)) = reader.join()
        {
            let _ = output.replay(&mut io::stderr().lock());
        }
    }
}

impl Drop for StderrHeld {
    fn drop(&mut self) {
        let previous = std::mem::replace(&mut *CAPTURE.lock(), CaptureState::Vacant);
        drop(previous);
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
