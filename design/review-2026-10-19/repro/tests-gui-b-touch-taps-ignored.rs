//! A tap on a button presses it, as a click does.
//! cargo test -p graphix-package-gui --test review_tests_gui_b_1 -- --nocapture
//! Expected: both the click and the tap produce one Call.
//! Observed: the click produces a Call; the tap produces none, because
//! window::track_cursor ignores touch events, so the frame's cursor is
//! Unavailable when the FingerPressed arrives and iced's button tests
//! cursor.is_over(bounds).
use graphix_package_core::testing::Fixture;
use graphix_package_gui::{
    frame::frame,
    theme::GraphixTheme,
    widgets::{self, Message},
    window::track_cursor,
};
use graphix_rt::NoExt;
use iced_core::{Event, Point, Size, clipboard, mouse, touch};
use iced_runtime::user_interface::Cache;
use iced_wgpu::{graphics::Shell, wgpu};

const REGISTER: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

async fn renderer() -> widgets::Renderer {
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends: wgpu::Backends::from_env().unwrap_or(wgpu::Backends::PRIMARY),
        ..Default::default()
    });
    let adapter = match instance
        .request_adapter(&wgpu::RequestAdapterOptions::default())
        .await
    {
        Ok(a) => a,
        Err(_) => instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                force_fallback_adapter: true,
                ..Default::default()
            })
            .await
            .expect("adapter"),
    };
    let (device, queue) =
        adapter.request_device(&wgpu::DeviceDescriptor::default()).await.unwrap();
    let engine = iced_wgpu::Engine::new(
        &adapter,
        device,
        queue,
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

async fn calls_from(events: &[Event]) -> anyhow::Result<usize> {
    let code = r#"
        use gui::{button::button, text::text};
        let calls = 0;
        let result = button(#on_press: |t| calls <- t ~ calls + 1, &text(&"Tap"))
    "#;
    let (f, v) = Fixture::new(code, REGISTER, |_| ()).await?;
    let mut w = widgets::compile(f.gx.clone(), v).await?;
    let mut r = renderer().await;
    let mut cache = Cache::default();
    let mut cursor = mouse::Cursor::Unavailable;
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let mut calls = 0;
    for ev in events {
        track_cursor(&mut cursor, ev);
        let mut msgs = Vec::new();
        frame(
            &mut w,
            Size::new(300.0, 50.0),
            &mut cache,
            &mut r,
            std::slice::from_ref(ev),
            cursor,
            &mut clipboard::Null,
            &mut msgs,
            &theme,
        );
        calls += msgs.iter().filter(|m| matches!(m, Message::Call(..))).count();
    }
    Ok(calls)
}

#[tokio::test(flavor = "current_thread")]
async fn a_tap_presses_a_button() -> anyhow::Result<()> {
    let at = Point::new(10.0, 10.0);
    let click = calls_from(&[
        Event::Mouse(mouse::Event::CursorMoved { position: at }),
        Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
    ])
    .await?;
    let id = touch::Finger(1);
    let tap = calls_from(&[
        Event::Touch(touch::Event::FingerPressed { id, position: at }),
        Event::Touch(touch::Event::FingerLifted { id, position: at }),
    ])
    .await?;
    println!("click calls: {click}, tap calls: {tap}");
    assert_eq!(click, 1, "the click control");
    assert_eq!(tap, 1, "a tap is a press");
    Ok(())
}
