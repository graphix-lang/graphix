//! wgpu + iced renderer setup: one Surface and Renderer per window.
//! Use `iced_wgpu::wgpu`, never a direct wgpu dependency.

use anyhow::{Context, Result};
use iced_wgpu::{
    graphics::{Shell, Viewport, shell::Notifier},
    wgpu,
};
use std::sync::Arc;
use winit::window::Window;

pub struct GpuState {
    pub instance: wgpu::Instance,
    pub adapter: wgpu::Adapter,
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
    pub format: wgpu::TextureFormat,
}

impl GpuState {
    pub async fn new(window: Arc<Window>) -> Result<Self> {
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });
        let surface =
            instance.create_surface(window).context("failed to create init surface")?;
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
            })
            .await
            .context("no suitable GPU adapter found")?;
        let (device, queue) = adapter
            .request_device(&wgpu::DeviceDescriptor::default())
            .await
            .context("failed to create GPU device")?;
        // iced packs colors as linear values and needs an sRGB target
        let formats = surface.get_capabilities(&adapter).formats;
        let format = formats
            .iter()
            .find(|f| f.is_srgb())
            .or(formats.first())
            .copied()
            .context("surface has no supported formats")?;
        drop(surface);
        Ok(Self { instance, adapter, device, queue, format })
    }

    /// Create an iced Engine + Renderer pair for a window.
    pub fn create_renderer(&self) -> iced_wgpu::Renderer {
        let engine = iced_wgpu::Engine::new(
            &self.adapter,
            self.device.clone(),
            self.queue.clone(),
            self.format,
            None,
            Shell::new(Wake),
        );
        iced_wgpu::Renderer::new(
            engine,
            iced_core::Font::DEFAULT,
            iced_core::Pixels(16.0),
        )
    }
}

/// The engine's notifier: an image decoded off the main thread asks the
/// event loop for the frame that shows it.
struct Wake;

impl Notifier for Wake {
    fn request_redraw(&self) {
        if let Some(w) = crate::REDRAW_WAKER.get() {
            w.wake()
        }
    }

    fn invalidate_layout(&self) {
        self.request_redraw()
    }
}

pub struct WindowSurface {
    pub surface: wgpu::Surface<'static>,
    pub config: wgpu::SurfaceConfiguration,
    pub renderer: iced_wgpu::Renderer,
    pub viewport: Viewport,
}

impl WindowSurface {
    pub fn new(gpu: &GpuState, window: Arc<Window>) -> Result<Self> {
        let size = window.inner_size();
        let surface = gpu
            .instance
            .create_surface(window.clone())
            .context("failed to create wgpu surface")?;
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: gpu.format,
            width: size.width.max(1),
            height: size.height.max(1),
            present_mode: wgpu::PresentMode::AutoVsync,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&gpu.device, &config);
        let renderer = gpu.create_renderer();
        let scale = window.scale_factor() as f32;
        let viewport = Viewport::with_physical_size(
            iced_core::Size::new(size.width, size.height),
            scale,
        );
        Ok(Self { surface, config, renderer, viewport })
    }

    pub fn resize(&mut self, gpu: &GpuState, width: u32, height: u32, scale: f64) {
        self.config.width = width.max(1);
        self.config.height = height.max(1);
        self.surface.configure(&gpu.device, &self.config);
        self.viewport = Viewport::with_physical_size(
            iced_core::Size::new(width, height),
            scale as f32,
        );
    }

    pub fn logical_size(&self) -> iced_core::Size {
        self.viewport.logical_size()
    }
}
