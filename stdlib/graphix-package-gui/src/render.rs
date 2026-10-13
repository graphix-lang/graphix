//! wgpu + iced renderer setup: one Surface and Renderer per window.
//! Use `iced_wgpu::wgpu`, never a direct wgpu dependency.

use anyhow::{Context, Result};
use iced_core::Size;
use iced_wgpu::{
    graphics::{Shell, Viewport, shell::Notifier},
    wgpu,
};
use std::sync::Arc;
use winit::window::Window;

fn surface_size(width: u32, height: u32, limit: u32) -> Size<u32> {
    Size::new(width.clamp(1, limit), height.clamp(1, limit))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn surface_dimensions_fit_the_device() {
        for limit in [2048, 8192, 16384] {
            assert_eq!(surface_size(0, 0, limit), Size::new(1, 1));
            assert_eq!(surface_size(800, 600, limit), Size::new(800, 600));
            assert_eq!(
                surface_size(u32::MAX, 9000, limit),
                Size::new(limit, 9000.min(limit))
            );
            assert_eq!(
                surface_size(9000, u32::MAX, limit),
                Size::new(9000.min(limit), limit)
            );
        }
    }
}

pub struct GpuState {
    pub instance: wgpu::Instance,
    pub adapter: wgpu::Adapter,
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
    pub format: wgpu::TextureFormat,
    max_texture_dimension_2d: u32,
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
            .request_device(&wgpu::DeviceDescriptor {
                required_limits: wgpu::Limits::downlevel_defaults()
                    .using_resolution(adapter.limits()),
                ..Default::default()
            })
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
        let max_texture_dimension_2d = device.limits().max_texture_dimension_2d;
        Ok(Self { instance, adapter, device, queue, format, max_texture_dimension_2d })
    }

    /// Create an iced Engine + Renderer pair for a window.
    pub fn create_renderer(&self) -> iced_wgpu::Renderer {
        // CR claude for claude: [perf/low] Every window builds a new iced_wgpu::Engine,
        // so each one compiles its own quad, text, triangle and image pipelines and
        // keeps its own glyph atlas. Opening a window pays pipeline creation again, and
        // a multi-window program holds one copy per window. Engine is Clone
        // (Arc-shared), and iced's own compositor builds one and clones it per window.
        // Fix: build the Engine once in GpuState::new, store it, and have
        // create_renderer pass engine.clone() to Renderer::new. Image caches stay per
        // Renderer either way. (gui-core-07)
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
        let size = surface_size(size.width, size.height, gpu.max_texture_dimension_2d);
        let surface = gpu
            .instance
            .create_surface(window.clone())
            .context("failed to create wgpu surface")?;
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: gpu.format,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::AutoVsync,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&gpu.device, &config);
        let renderer = gpu.create_renderer();
        let scale = window.scale_factor() as f32;
        let viewport =
            Viewport::with_physical_size(Size::new(size.width, size.height), scale);
        Ok(Self { surface, config, renderer, viewport })
    }

    pub fn resize(&mut self, gpu: &GpuState, width: u32, height: u32, scale: f64) {
        let size = surface_size(width, height, gpu.max_texture_dimension_2d);
        self.config.width = size.width;
        self.config.height = size.height;
        self.surface.configure(&gpu.device, &self.config);
        self.viewport = Viewport::with_physical_size(size, scale as f32);
    }

    pub fn logical_size(&self) -> Size {
        self.viewport.logical_size()
    }
}
