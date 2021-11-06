mod component;
mod dialog;
mod file;
mod ic;
mod gates;

use dialog::Dialog;
use file::OpenDialog;
use component::*;

use crate::circuit;
use crate::simulator;
use crate::simulator::ir::IrOp;
use crate::simulator::GraphNodeHandle;
use crate::circuit::{Circuit, CircuitComponent, Ic, WireHandle};

use core::any::TypeId;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;
use std::rc::Rc;
use eframe::{egui, epi};

const COMPONENTS: &[(&'static str, fn() -> Box<dyn ComponentPlacer>)] = {
	fn a() -> core::num::NonZeroU8 { core::num::NonZeroU8::new(1).unwrap() }
	fn b() -> simulator::NonZeroOneU8 { simulator::NonZeroOneU8::new(2).unwrap() }
	use simulator::*;
	&[
		("and", || Box::new(AndGate::new(b(), a()))),
		("or", || Box::new(OrGate::new(b(), a()))),
		("not", || Box::new(NotGate::new(a()))),
		("xor", || Box::new(XorGate::new(b(), a()))),
	]
};

#[derive(Debug)]
pub enum LoadCircuitError {
	Io(io::Error),
	Serde(ron::Error),
}

#[derive(Debug)]
pub enum SaveCircuitError {
	Io(io::Error),
	Serde(ron::Error),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ComponentOrWire {
	Component(GraphNodeHandle),
	Wire(WireHandle),
}

pub struct App {
	dialog: Option<Box<dyn Dialog>>,
	component: Option<Box<dyn ComponentPlacer>>,
	component_direction: circuit::Direction,
	wire_start: Option<circuit::Point>,
	circuit: Box<circuit::Circuit<Box<dyn ComponentPlacer>>>,
	ic_components: HashMap<Box<str>, Ic>,

	selected: Option<ComponentOrWire>,
	selected_wires: Vec<WireHandle>,
	
	inputs: Vec<usize>,
	outputs: Vec<usize>,
	memory: Box<[usize]>,
	// TODO we shouldn't delay updates by a frame.
	needs_update: bool,

	file_path: String,
}

impl App {
	pub fn new() -> Self {
		use crate::simulator::*;
		let mut s = Self {
			dialog: None,
			component: None,
			component_direction: circuit::Direction::Up,
			wire_start: None,
			circuit: Default::default(),
			ic_components: Default::default(),

			selected: None,
			selected_wires: Default::default(),

			inputs: Vec::new(),
			outputs: Vec::new(),
			memory: Box::default(),
			needs_update: false,

			file_path: String::new(),
		};
		let f = std::env::args().skip(1).next();
		let f = f.as_deref().unwrap_or("/tmp/ok.logimu");
		let _ = dbg!(s.load_from_file(f));
		s.file_path = f.into();

		for f in std::fs::read_dir(Path::new(f).parent().unwrap()).unwrap() {
			let f = f.unwrap();
			let f = f.file_name();
			let f = f.to_str().unwrap();
			if f.ends_with(".logimu") {
				dbg!(f);
				let _ = dbg!(s.load_ic(&("/tmp/".to_owned() + f)));
			}
		}

		s
	}

	pub fn load_from_file(&mut self, path: impl Into<String>) -> Result<(), LoadCircuitError> {
		let path = path.into();
		let f = fs::File::open(&path).map_err(LoadCircuitError::Io)?;
		self.circuit = ron::de::from_reader(f).map_err(LoadCircuitError::Serde)?;
		self.inputs.clear();
		self.outputs.clear();

		for (c, ..) in self.circuit.components(circuit::Aabb::ALL) {
			c.external_input().map(|i| self.inputs.resize((i + 1).max(self.inputs.len()), 0));
			c.external_output().map(|i| self.outputs.resize((i + 1).max(self.outputs.len()), 0));
		}

		self.file_path = path;
		Ok(())
	}

	pub fn save_to_file(&mut self, path: Option<&str>) -> Result<(), SaveCircuitError> {
		let path = path.unwrap_or(&self.file_path);
		dbg!(path);
		let f = fs::File::create(&path).map_err(SaveCircuitError::Io)?;
		ron::ser::to_writer(f, &self.circuit).map_err(SaveCircuitError::Serde)?;
		Ok(())
	}

	pub fn load_ic(&mut self, path: &str) -> Result<(), LoadCircuitError> {
		let f = fs::File::open(path).map_err(LoadCircuitError::Io)?;
		let c: Circuit<Box<dyn ComponentPlacer>> = ron::de::from_reader(f)
			.map_err(LoadCircuitError::Serde)?;
		self.ic_components.insert(path.to_string().into(), Ic::from_circuit(c, path));
		Ok(())
	}
}

impl epi::App for App {
	fn name(&self) -> &str {
		"Logimu"
	}

	/// Called each time the UI needs repainting, which may be many times per second.
	/// Put your widgets into a `SidePanel`, `TopPanel`, `CentralPanel`, `Window` or `Area`.
	fn update(&mut self, ctx: &egui::CtxRef, frame: &mut epi::Frame<'_>) {

		// TODO don't run circuit every frame
		let (ir, mem_size) = self.circuit.generate_ir();
		if mem_size != self.memory.len() {
			self.memory = core::iter::repeat(0).take(mem_size).collect::<Box<_>>();
			dbg!(&ir);
		}
		simulator::ir::interpreter::run(&ir, &mut self.memory, &self.inputs, &mut self.outputs);

		use egui::*;

		if ctx.input().key_pressed(Key::R) {
			self.component_direction = self.component_direction.rotate_clockwise();
		}

		// Check if we should remove any selected components and/or wires
		if ctx.input().key_pressed(Key::Backspace) || ctx.input().key_pressed(Key::Delete) {
			match self.selected.take() {
				Some(ComponentOrWire::Component(h)) => {
					self.circuit.remove_component(h);
				}
				Some(ComponentOrWire::Wire(h)) => todo!(),
				None => (),
			}
			for w in self.selected_wires.drain(..) {
				self.circuit.remove_wire(w).unwrap();
			}
		}

		let mut save = ctx.input().key_pressed(Key::S) && ctx.input().modifiers.ctrl;

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
					save |= ui.button("Save").clicked();
					if ui.button("Save as").clicked() {
					}
				});
			});
		});

		if save {
			let e = self.save_to_file(None);
			dbg!(e);
		}

		if let Some(dialog) = self.dialog.as_mut() {
			let r = Window::new(dialog.name())
				.anchor(Align2([Align::Center; 2]), Vec2::ZERO)
				.show(ctx, |ui| dialog.show(ui));
			r.map(|r| r.inner.map(|r| r.then(|| self.dialog = None)));
		}

		egui::SidePanel::left("side_panel").show(ctx, |ui| {
			ui.heading("Components");

			ui.label(format!("Inputs: {}", self.inputs.len()));
			ui.label(format!("Outputs: {}", self.outputs.len()));

			// Built in components
			let bits = core::num::NonZeroU8::new(1).unwrap();
			if ui.button("wire").clicked() {
				self.component = None;
			}
			if ui.button("in").clicked() {
				self.component = Some(Box::new(simulator::In::new(bits, self.inputs.len())));
			}
			if ui.button("out").clicked() {
				self.component = Some(Box::new(simulator::Out::new(bits, self.outputs.len())));
			}
			for c in COMPONENTS.iter() {
				if ui.button(c.0).clicked() {
					self.component = Some(c.1());
				}
			}

			ui.separator();

			// Custom components (ICs)
			for (name, ic) in self.ic_components.iter() {
				if ui.button(name).clicked() {
					self.component = Some(Box::new(ic.clone()));
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
			let color = e.dragged_by(PointerButton::Primary).then(|| Color32::RED).unwrap_or(Color32::GREEN);

			let pos2point = |pos: Pos2| {
				let (x, y) = ((pos.x - rect.min.x) / 16.0, (pos.y - rect.min.y) / 16.0);
				circuit::Point { x: x.round() as u16, y: y.round() as u16 }
			};
			let point2pos = |point: circuit::Point| {
				let (x, y) = (f32::from(point.x), f32::from(point.y));
				Pos2::new(rect.min.x + x * 16.0, rect.min.y + y * 16.0)
			};
			let draw_aabb = |point: circuit::Point, aabb: circuit::RelativeAabb, stroke: Stroke| {
				let delta = Vec2::new(8.0, 8.0);
				let (min, max) = ((point + aabb.min).unwrap(), (point + aabb.max).unwrap());
				let (min, max) = (point2pos(min) - delta, point2pos(max) + delta);
				let rect = Rect { min, max };
				paint.rect_stroke(rect, 8.0, stroke);
			};

			let wire_stroke = Stroke::new(3.0, Color32::WHITE);
			let selected_color = Color32::LIGHT_BLUE.linear_multiply(0.5);

			let aabb = circuit::Aabb::new(pos2point(rect.min), pos2point(rect.max));

			// Clear current selected if no modifiers are pressed during select
			if e.clicked_by(PointerButton::Secondary) && !ui.input().modifiers.shift {
				self.selected_wires.clear();
			}

			// Draw outlines for selected wires
			for w in self.selected_wires.iter() {
				let (w, ..) = self.circuit.wire(*w).unwrap();
				let (from, to) = (point2pos(w.from), point2pos(w.to));
				paint.line_segment([from, to], Stroke::new(6.0, selected_color));
				paint.circle_filled(from, 3.0, selected_color);
				paint.circle_filled(to, 3.0, selected_color);
			}

			// Draw existing components
			let mut hover_box = None;
			for (c, p, d, h) in self.circuit.components(aabb) {
				c.draw(&paint, point2pos(p), d, &self.inputs, &self.outputs);
				let aabb = d * c.aabb();
				let delta = Vec2::new(8.0, 8.0);
				let (min, max) = ((p + aabb.min).unwrap(), (p + aabb.max).unwrap());
				let (min, max) = (point2pos(min) - delta, point2pos(max) + delta);
				let rect = Rect { min, max };
				if e.hover_pos().map_or(false, |p| rect.contains(p)) {
					// Draw a box around the component
					hover_box = Some(rect);
					if e.clicked_by(PointerButton::Secondary) {
						// Mark the component as selected.
						self.selected = Some(ComponentOrWire::Component(h));
					}
					if e.clicked_by(PointerButton::Middle) {
						// Toggle input if it is one
						c.external_input().map(|i| self.inputs[i] = !self.inputs[i]);
					}
				}
				for &po in c.inputs().into_iter().chain(c.outputs()) {
					(p + d * po).map(|p| paint.circle_filled(point2pos(p), 2.0, Color32::GREEN));
				}
			}

			// Draw existing wires
			for (w, wh, h) in self.circuit.wires(aabb) {
				let intersects = e.hover_pos().map_or(false, |p| w.intersect_point(pos2point(p)));
				let stroke = match intersects {
					true => Stroke::new(3.0, Color32::YELLOW),
					_ => Stroke::new(3.0, [Color32::DARK_GREEN, Color32::GREEN][*self.memory.get(h.index()).unwrap_or(&0) & 1]),
				};
				if intersects && e.clicked_by(PointerButton::Secondary) {
					self.selected_wires.push(wh);
				}
				let (from, to) = (point2pos(w.from), point2pos(w.to));
				paint.line_segment([from, to], stroke);
			}

			// Draw interaction objects (pointer, component, wire ...)
			if let Some(pos) = e.hover_pos() {
				let point = pos2point(pos);
				let pos = point2pos(point);

				if let Some(c) = self.component.take() {
					c.draw(&paint, pos, self.component_direction, &self.inputs, &self.outputs);

					if e.clicked_by(PointerButton::Primary) {
						(c.type_id() == TypeId::of::<simulator::In>()).then(|| self.inputs.push(0));
						(c.type_id() == TypeId::of::<simulator::Out>()).then(|| self.outputs.push(0));
						self.circuit.add_component(c, point, self.component_direction);
						self.needs_update = true;
					} else {
						self.component = Some(c);
					}
				} else {
					paint.circle_stroke(pos, 3.0, Stroke::new(2.0, color));

					if let Some(start) = self.wire_start {
						paint.line_segment([point2pos(start), pos], wire_stroke);

						// FIXME egui for some reason thinks that the primary button is not
						// pressed if the secondary was pressed and then released at the same
						// time.
						if e.drag_released() && !e.dragged_by(PointerButton::Primary) {
							self.circuit.add_wire(circuit::Wire { from: start, to: point });
							self.wire_start = None;
							self.needs_update = true;
						}
					} else {
						if e.drag_started() && e.dragged_by(PointerButton::Primary) {
							self.wire_start = Some(point);
						}
					}
				}
			}

			// Draw a box around the selected component
			let stroke = Stroke::new(2.0, selected_color);
			if let Some(ComponentOrWire::Component(h)) = self.selected {
				if let Some((c, p, d)) = self.circuit.component(h) {
					draw_aabb(p, d * c.aabb(), stroke);
				} else {
					self.selected = None;
				}
			}

			// Draw a box around the hovered component
			hover_box.map(|rect| paint.rect_stroke(rect, 8.0, Stroke::new(2.0, Color32::YELLOW)));
		});
	}
}
