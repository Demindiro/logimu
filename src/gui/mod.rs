mod component;
mod dialog;
mod file;
mod gates;

use dialog::Dialog;
use file::OpenDialog;
use component::*;

use crate::circuit;

use eframe::{egui, epi};

pub struct App {
	dialog: Option<Box<dyn Dialog>>,
	component: Option<usize>,
	components: [Box<dyn ComponentPlacer>; 2],
	component_direction: circuit::Direction,
	wire_start: Option<circuit::Point>,
	circuit: Box<circuit::Circuit>,
}

impl App {
	pub fn new() -> Self {
		let components: [Box<dyn ComponentPlacer>; 2] = [
			Box::new(gates::AndGate),
			Box::new(gates::OrGate),
		];
		Self {
			dialog: None,
			component: None,
			components,
			component_direction: circuit::Direction::Up,
			wire_start: None,
			circuit: Default::default(),
		}
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

		if ctx.input().key_pressed(Key::R) {
			self.component_direction = self.component_direction.rotate_clockwise();
		}

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
			ui.heading("Components");

			if ui.button("wire").clicked() {
				self.component = None;
			} else {
				for (i, c) in self.components.iter().enumerate() {
					if ui.button(c.name()).clicked() {
						self.component = Some(i);
						break;
					}
				}
			}
		});

		CentralPanel::default().show(ctx, |ui| {
			use epaint::*;
			let rect = ui.max_rect();
			let paint = ui.painter_at(rect);
			for y in (rect.min.y as u16..rect.max.y as u16).step_by(16) {
				for x in (rect.min.x as u16..rect.max.x as u16).step_by(16) {
					let pos = Pos2::new(f32::from(x), f32::from(y));
					paint.circle(pos, 1.0, Color32::GRAY, Stroke::none());
				}
			}
			let e = ui.interact(ui.max_rect(), ui.id(), Sense::drag());
			let color = e.dragged().then(|| Color32::RED).unwrap_or(Color32::GREEN);

			let pos2point = |pos: Pos2| {
				let (x, y) = ((pos.x - rect.min.x) / 16.0, (pos.y - rect.min.y) / 16.0);
				circuit::Point { x: x.round() as u16, y: y.round() as u16 }
			};
			let point2pos = |point: circuit::Point| {
				let (x, y) = (f32::from(point.x), f32::from(point.y));
				Pos2::new(rect.min.x + x * 16.0, rect.min.y + y * 16.0)
			};

			let wire_stroke = Stroke::new(3.0, Color32::WHITE);

			let aabb = circuit::Aabb::new(pos2point(rect.min), pos2point(rect.max));

			// Draw existing components
			for (c, p, d) in self.circuit.components(aabb) {
				self.components[0].draw(&paint, point2pos(p), d);
			}

			// Draw existing wires
			for w in self.circuit.wires(aabb) {
				let stroke = match e.hover_pos().map(|p| w.intersect_point(pos2point(p))) {
					Some(true) => Stroke::new(3.0, Color32::YELLOW),
					_ => wire_stroke,
				};
				paint.line_segment([point2pos(w.from), point2pos(w.to)], stroke);
			}

			// Draw interaction objects (pointer, component, wire ...)
			if let Some(pos) = e.hover_pos() {
				let point = pos2point(pos);
				let pos = point2pos(point);

				if let Some(c) = self.component.map(|c| &self.components[c]) {
					c.draw(&paint, pos, self.component_direction);

					if e.clicked() {
						self.circuit.add_component(c.instance(), point, self.component_direction);
					}
				} else {
					paint.circle_stroke(pos, 3.0, Stroke::new(2.0, color));

					if let Some(start) = self.wire_start {
						paint.line_segment([point2pos(start), pos], wire_stroke);

						if e.drag_released() {
							self.circuit.add_wire(circuit::Wire { from: start, to: point });
							self.wire_start = None;
						}
					} else {
						if e.drag_started() {
							self.wire_start = Some(point);
						}
					}
				}
			}
		});
	}
}
