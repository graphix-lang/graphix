// gui-widgets-b-01: text_editor rebuild keeps the old cursor: typing after a
// shorter program write panics the GUI.
// TextEditorW::handle_update (src/widgets/text_editor.rs) rebuilds its Content
// from a program-written text and moves the old cursor into it. A shorter text
// leaves the cursor past its end; this is that sequence.
// Run: cp this file to stdlib/graphix-package-gui/tests/review_gui_widgets_b_01.rs,
//   cargo test -p graphix-package-gui --test review_gui_widgets_b_01 -- --exact <name>
// Expected: each edit after the rebuild succeeds (cursor clamped into the text).
// Observed (each case alone): cursor stays at line 0 col 5 / col 6 / line 2 col 5;
//   Insert panics "cosmic-text-0.15.0/src/buffer_line.rs:182:30: assertion failed:
//   self.is_char_boundary(at)"; Backspace panics at cosmic-text edit/editor.rs:644:41.
//   In one process the later cases panic "Write font system: PoisonError": the first
//   panic poisons iced's global font system lock.
use iced_widget::text_editor::{Action, Content, Edit, Motion};
use std::panic::{AssertUnwindSafe, catch_unwind};

fn rebuild(editor: &mut Content, text: &str) {
    let cursor = editor.cursor();
    *editor = Content::with_text(text);
    editor.move_to(cursor);
}

fn survives(start: &str, written: &str, action: Action) -> bool {
    let mut ed = Content::with_text(start);
    ed.perform(Action::Move(Motion::DocumentEnd));
    rebuild(&mut ed, written);
    eprintln!("cursor after rebuild: {:?}", ed.cursor());
    catch_unwind(AssertUnwindSafe(|| ed.perform(action))).is_ok()
}

#[test]
fn type_after_the_program_clears_the_text() {
    assert!(
        survives("hello", "", Action::Edit(Edit::Insert('x'))),
        "typing after the program cleared the editor panicked"
    );
}

#[test]
fn type_after_the_program_truncates_the_text() {
    assert!(
        survives("hello!", "hello", Action::Edit(Edit::Insert('x'))),
        "typing after the program dropped the last char panicked"
    );
}

#[test]
fn backspace_after_the_program_clears_a_multiline_text() {
    assert!(
        survives("one\ntwo\nthree", "", Action::Edit(Edit::Backspace)),
        "backspace after the program cleared the editor panicked"
    );
}
