use poolshark::local::LPooled;

#[test]
fn toggling_mouse_does_not_toggle_focus() {
    let mut output: LPooled<Vec<u8>> = LPooled::take();
    crate::set_mouse_to(&mut *output, true);
    crate::set_mouse_to(&mut *output, false);
    let output = std::str::from_utf8(&output).unwrap();
    assert!(!output.contains("1004"), "{output:?}");
}

#[test]
fn display_modes_enable_focus() -> anyhow::Result<()> {
    let mut output: LPooled<Vec<u8>> = LPooled::take();
    crate::take_terminal_modes(&mut *output)?;
    let output = std::str::from_utf8(&output)?;
    assert!(output.contains("\x1b[?1004h"));
    assert!(!output.contains("1000"));
    Ok(())
}
