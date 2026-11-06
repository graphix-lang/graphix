# Stdlib notes

Status: current principle.
Pins: each package's tests; `book/src/stdlib/`.

Notes on single packages that span more than one file.

- `xls`: calamine opens every BIFF xls sheet densely, including for
  `sheets`; `MAX_CELLS` is a post-open limit for xls, not an opening
  memory bound. The ods preflight does not cover xls. This accepted
  limitation is documented in `book/src/stdlib/xls.md` and `UPSTREAM.md`.

- `sys::process`: a child lives in a task of its own (`own_child`),
  which selects on its exit against kill requests over `ctl` and reaps
  it; dropping the last `Proc` closes `ctl`, which kills a
  `kill_on_drop` child; redirects are `Pipe`/`Inherit`/`Null`. Shell tests are Unix-gated; the stdout and
  wait-status ones have `cmd.exe` twins.

- `tui`: a display holds no descriptor: what is written to stdout or
  stderr while it runs goes where the stream points, over the display on
  a terminal. It reads the terminal's events on a thread of its own whose
  every crossterm poll is bounded, and gives the terminal back once that
  thread has stopped, or after a bounded wait for a terminal that went
  away (`Events`); never crossterm's `EventStream`
  (UPSTREAM.md), whose feature the workspace leaves off.

- GUI (iced): uses the iced sub-crates directly; `GuiTestHarness::dt()`
  downcasts; updates walk every window’s widget tree, including newly
  compiled children (`design/gui_updates.md`); tests fire callbacks via `gx.call(callable_id, args)`;
  test contexts default to `NetConfig::Internal`; publisher coalescing
  collapses rapid updates — space them with timers. A control's send
  (`widgets::Echoes`) shows until echoed, and is rejected only when its
  call was answered and it is not echoed once the runtime is idle: a
  widget left waiting asks `GXHandle::notify_idle`, and the
  `GXEvent::Idle` that answers comes after every update before it, to
  the display's `CustomDisplay::idle`.

- Package manager: `packages.toml` v2 (`[stdlib]` tracks the shell
  version, `[packages]` for externals); `update` presents a maskable
  change set and builds before writing the manifest; hard error on
  non-TTY without `--yes`. Rebuilds refuse an executable beneath Cargo
  build markers (`.rustc_info.json` or a profile's `.cargo-lock`), before
  unpacking and again before replacement; binaries installed elsewhere
  may rebuild.
