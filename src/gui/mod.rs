mod dialog;
mod file;
mod gates;

use dialog::Dialog;
use file::OpenDialog;

use eframe::{egui, epi};

pub struct App {
	dialog: Option<Box<dyn Dialog>>,
}

impl App {
	pub fn new() -> Self {
		Self { dialog: None }
	}
}

impl epi::App for App {
	fn name(&self) -> &str {
		"Logimu"
	}

	/// Called each time the UI needs repainting, which may be many times per second.
	/// Put your widgets into a `SidePanel`, `TopPanel`, `CentralPanel`, `Window` or `Area`.
	fn update(&mut self, ctx: &egui::CtxRef, frame: &mut epi::Frame<'_>) {

		use egui::*;

		TopBottomPanel::top("top_panel").show(ctx, |ui| {
			menu::bar(ui, |ui| {
				menu::menu(ui, "File", |ui| {
					if ui.button("New").clicked() {
					}
					if ui.button("Open").clicked() {
						self.dialog = Some(Box::new(OpenDialog {}));
					}
					if ui.button("Close").clicked() {
						frame.quit()
					}
					if ui.button("Save").clicked() {
					}
					if ui.button("Save as").clicked() {
					}
				});
			});
		});

		if let Some(dialog) = self.dialog.as_mut() {
			let r = Window::new(dialog.name())
				.anchor(Align2([Align::Center; 2]), Vec2::ZERO)
				.show(ctx, |ui| dialog.show(ui));
			r.map(|r| r.inner.map(|r| r.then(|| self.dialog = None)));
		}

		egui::SidePanel::left("side_panel").show(ctx, |ui| {
			ui.heading("Gates");

			for name in ["Add", "Or", "Xor", "Not"] {
				if ui.button(name).clicked() {

				}
			}
		});

		CentralPanel::default().show(ctx, |ui| {
			use epaint::*;
			let rect = ui.max_rect();
			let paint = ui.painter_at(rect);
			for y in (0..rect.height() as u16 + 100).step_by(16) {
				for x in (0..rect.width() as u16 + 100).step_by(16) {
					let pos = Pos2::new(f32::from(x), f32::from(y));
					paint.circle(pos, 1.0, Color32::GRAY, Stroke::none());
				}
			}
			let e = ui.interact(ui.max_rect(), ui.id(), Sense::drag());
			let color = e.dragged().then(|| Color32::RED).unwrap_or(Color32::GREEN);
			e.hover_pos().map(|pos| {
				let pos = ((pos.to_vec2() / 16.0).round() * 16.0).to_pos2();
				gates::draw_or(&paint, pos, gates::Direction::Up);
				paint.circle_stroke(pos, 3.0, Stroke::new(2.0, color));
			});
		});
	}
}
