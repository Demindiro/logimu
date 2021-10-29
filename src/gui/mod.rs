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
	component: Option<Box<dyn ComponentPlacer>>,
	components: [(&'static str, fn() -> Box<dyn ComponentPlacer>); 2],
	component_direction: circuit::Direction,
	wire_start: Option<circuit::Point>,
	circuit: Box<circuit::Circuit<Box<dyn ComponentPlacer>>>,
}

impl App {
	pub fn new() -> Self {
		let components: [(&'static str, fn() -> Box<dyn ComponentPlacer>); 2] = [
			("and", || todo!()),
			("or", || todo!()),
		];
		let mut s = Self {
			dialog: None,
			component: None,
			components,
			component_direction: circuit::Direction::Up,
			wire_start: None,
			circuit: Default::default(),
		};

		let circuit = &mut s.circuit;

		use circuit::*;

		//let (i0, i1, l0, l1, r0, lr, o0, cp, o1) = (In, In, And, Not, Or, And, Out, Xor, Out);
		let (l0, r0, lr, cp) = (
			s.components[0].clone(),
			s.components[1].clone(),
			s.components[0].clone(),
			s.components[1].clone(),
		);

		// Inputs
		//let i0 = circuit.add_component(&i0, Point::new(0, 0), Direction::Right);
		//let i1 = circuit.add_component(&i1, Point::new(0, 4), Direction::Right);

		// Connect inputs to AND
		circuit.add_wire(Wire::new(Point::new(1, 0), Point::new(4, 0)));
		circuit.add_wire(Wire::new(Point::new(1, 4), Point::new(4, 1)));

		// Place AND and NOT
		//let l0 = circuit.add_component(l0, Point::new(4, 0), Direction::Right);
		//let l1 = circuit.add_component(l1, Point::new(8, 0), Direction::Right);
		// Connect AND to NOT
		circuit.add_wire(Wire::new(Point::new(5, 0), Point::new(8, 0)));

		// Place OR
		//let r0 = circuit.add_component(r0, Point::new(4, 4), Direction::Right);
		// Connect inputs to OR
		circuit.add_wire(Wire::new(Point::new(1, 0), Point::new(4, 4)));
		circuit.add_wire(Wire::new(Point::new(1, 4), Point::new(4, 5)));
		
		// Connect AND & OR to AND and connect AND to output
		circuit.add_wire(Wire::new(Point::new(9, 0), Point::new(12, 0)));
		circuit.add_wire(Wire::new(Point::new(5, 4), Point::new(12, 1)));
		circuit.add_wire(Wire::new(Point::new(13, 0), Point::new(16, 0)));
		// Place AND and output
		//let lr = circuit.add_component(lr, Point::new(12, 0), Direction::Right);
		//let o0 = circuit.add_component(o0, Point::new(16, 0), Direction::Right);

		// Place XOR and output
		//let cp = circuit.add_component(cp, Point::new(4, 8), Direction::Right);
		//let o1 = circuit.add_component(&o1, Point::new(16, 8), Direction::Right);
		// Connect inputs to XOR and XOR to output
		circuit.add_wire(Wire::new(Point::new(0, 0), Point::new(4, 8)));
		circuit.add_wire(Wire::new(Point::new(0, 4), Point::new(4, 9)));
		circuit.add_wire(Wire::new(Point::new(5, 8), Point::new(16, 8)));

		s
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
					if ui.button(c.0).clicked() {
						self.component = Some(c.1());
						break;
					}
				}
			}
		});

		CentralPanel::default().show(ctx, |ui| {
			use epaint::*;
			let rect = ui.max_rect();
			let paint = ui.painter_at(rect);
			let paint = ui.painter();
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
				c.draw(&paint, point2pos(p), d);
				for &po in c.inputs().into_iter().chain(c.outputs()) {
					(d * po).map(|po| (p + po).map(|p| paint.circle_filled(point2pos(p), 2.0, Color32::GREEN)));
				}
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

				if let Some(c) = self.component.take() {
					c.draw(&paint, pos, self.component_direction);

					if e.clicked() {
						self.circuit.add_component(c, point, self.component_direction);
					} else {
						self.component = Some(c);
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
