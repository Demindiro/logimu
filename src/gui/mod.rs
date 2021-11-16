mod component;
mod components_info;
mod dialog;
mod file;
mod gates;
mod ic;
mod inputs_outputs;
mod log;
mod script;

use component::*;
use components_info::*;
use dialog::Dialog;
use file::OpenDialog;
use inputs_outputs::*;
use log::*;
use script::*;

use crate::circuit;
use crate::circuit::{CircuitComponent, Ic, WireHandle};
use crate::simulator;

use crate::simulator::{GraphNodeHandle, PropertyValue, SetProperty};

use core::any::TypeId;
use eframe::{egui, epi};
use std::collections::BTreeMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

const COMPONENTS: &[(&'static str, fn() -> Box<dyn ComponentPlacer>)] = {
	fn a() -> core::num::NonZeroU8 {
		core::num::NonZeroU8::new(1).unwrap()
	}
	fn b() -> simulator::NonZeroOneU8 {
		simulator::NonZeroOneU8::new(2).unwrap()
	}
	use simulator::*;
	&[
		("and", || Box::new(AndGate::new(b()))),
		("or", || Box::new(OrGate::new(b()))),
		("not", || Box::new(NotGate::new())),
		("xor", || Box::new(XorGate::new(b()))),
		("splitter", || Box::new(Splitter::new())),
		("merger", || Box::new(Merger::new(a()))),
		("constant", || Box::new(Constant::new(a(), 0))),
		("rom", || Box::new(ReadOnlyMemory::default())),
	]
};

#[derive(Debug)]
pub enum SaveCircuitError {
	Io(io::Error),
	Serde(ron::Error),
}

#[derive(Debug)]
pub enum LoadCircuitError {
	Io(io::Error),
	Serde(ron::Error),
}

pub struct App {
	dialog: Option<Box<dyn Dialog>>,
	component: Option<Box<dyn ComponentPlacer>>,
	component_direction: circuit::Direction,
	wire_start: Option<circuit::Point>,
	circuit: Box<circuit::Circuit<Box<dyn ComponentPlacer>>>,
	ic_components: BTreeMap<Box<Path>, Ic>,

	selected_components: Vec<GraphNodeHandle>,
	selected_wires: Vec<WireHandle>,

	inputs: Vec<usize>,
	outputs: Vec<usize>,
	memory: Box<[usize]>,
	// TODO we shouldn't delay updates by a frame.
	needs_update: bool,

	file_path: Box<Path>,

	script_editor: ScriptEditor,
	log: Log,
	components_info: ComponentsInfo,
	io_editor: InputsOutputs,

	logged_parse_error: bool,
}

impl App {
	pub fn new() -> Self {
		let mut s = Self {
			dialog: None,
			component: None,
			component_direction: circuit::Direction::Up,
			wire_start: None,
			circuit: Default::default(),
			ic_components: Default::default(),

			selected_components: Default::default(),
			selected_wires: Default::default(),

			inputs: Vec::new(),
			outputs: Vec::new(),
			memory: Box::default(),
			needs_update: false,

			file_path: PathBuf::new().into(),

			script_editor: Default::default(),
			log: Default::default(),
			components_info: Default::default(),
			io_editor: Default::default(),

			logged_parse_error: false,
		};
		let f = std::env::args().skip(1).next();
		let f = PathBuf::from(f.as_deref().unwrap_or("/tmp/ok.logimu"));
		match s.load_from_file(f.clone().into()) {
			Ok(()) => s.log.debug(format!("Loaded {:?}", f.clone())),
			Err(e) => {
				s.log.error(format!("Failed to load {:?}: {:?}", f, e));
				std::env::set_current_dir(f.parent().unwrap()).unwrap();
			}
		}

		for f in std::fs::read_dir(".").unwrap() {
			let f = f.unwrap().path();
			if f.extension().and_then(|p| p.to_str()) == Some("logimu") {
				match s.load_ic(f.clone().into()) {
					Ok(()) => s.log.debug(format!("Loaded {:?}", f)),
					Err(e) => s.log.error(format!("Failed to load {:?}: {:?}", f, e)),
				}
			}
		}

		s.file_path = PathBuf::from(f.components().last().unwrap().as_os_str()).into();

		s
	}

	pub fn load_from_file(&mut self, path: Box<Path>) -> Result<(), LoadCircuitError> {
		let f = fs::File::open(&path).map_err(LoadCircuitError::Io)?;
		std::env::set_current_dir(path.parent().unwrap()).unwrap();
		self.circuit = ron::de::from_reader(f).map_err(LoadCircuitError::Serde)?;
		self.inputs.clear();
		self.outputs.clear();

		for (c, ..) in self.circuit.components(circuit::Aabb::ALL) {
			c.external_input()
				.map(|i| self.inputs.resize((i + 1).max(self.inputs.len()), 0));
			c.external_output()
				.map(|i| self.outputs.resize((i + 1).max(self.outputs.len()), 0));
		}

		self.file_path = path;
		Ok(())
	}

	pub fn save_to_file(&mut self, path: Option<&Path>) -> Result<(), SaveCircuitError> {
		let path = path.unwrap_or(&self.file_path);
		let f = fs::File::create(&path).map_err(SaveCircuitError::Io)?;
		ron::ser::to_writer(f, &self.circuit).map_err(SaveCircuitError::Serde)?;
		Ok(())
	}

	pub fn load_ic(&mut self, path: Box<Path>) -> Result<(), circuit::LoadError> {
		self.ic_components.insert(path.clone(), Ic::get_ic(path)?);
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
		}
		simulator::ir::interpreter::run(&ir, &mut self.memory, &self.inputs, &mut self.outputs);

		use egui::*;

		if ctx.input().key_pressed(Key::R) {
			self.component_direction = self.component_direction.rotate_clockwise();
		}

		// Check if we should remove any selected components and/or wires
		let del = [Key::Backspace, Key::Delete]
			.iter()
			.any(|e| ctx.input().key_pressed(*e));
		if del && ctx.memory().focus().is_none() {
			for c in self.selected_components.drain(..) {
				self.circuit.remove_component(c).unwrap();
			}
			for w in self.selected_wires.drain(..) {
				self.circuit.remove_wire(w).unwrap();
			}
		}

		let mut save = ctx.input().key_pressed(Key::S) && ctx.input().modifiers.ctrl;

		TopBottomPanel::top("top_panel").show(ctx, |ui| {
			menu::bar(ui, |ui| {
				menu::menu(ui, "File", |ui| {
					if ui.button("New").clicked() {}
					if ui.button("Open").clicked() {
						self.dialog = Some(Box::new(OpenDialog {}));
					}
					if ui.button("Close").clicked() {
						frame.quit()
					}
					save |= ui.button("Save").clicked();
					if ui.button("Save as").clicked() {}
				});
				self.script_editor.open |= ui.button("Script").clicked();
				menu::menu(ui, "Test", |ui| {
					let run_all = ui.button("All").clicked();
					ui.separator();
					match self.circuit.tests() {
						Ok(t) => {
							for t in t {
								let mut debug = String::default();
								if ui.button(t.name()).clicked() || run_all {
									let res = t.run(
										&mut self.memory,
										&mut self.inputs,
										&mut self.outputs,
										&mut debug,
									);
									(!debug.is_empty()).then(|| self.log.debug(debug));
									match res {
										Ok(()) => {
											self.log.success(format!("Test '{}' passed!", t.name()))
										}
										Err(e) => self.log.error(e.to_string()),
									}
									self.log.open = true;
								}
								self.logged_parse_error = false;
							}
						}
						Err(e) => {
							// Prevent spamming the log with parse errors.
							if !self.logged_parse_error {
								self.log.open = true;
								self.log.error(e.to_string());
								self.logged_parse_error = true;
							}
						}
					}
				});
				self.log.open |= ui.button("Log").clicked();
			});
		});

		if save {
			match self.save_to_file(None) {
				Ok(_) => self
					.log
					.debug(format!("Saved circuit to {:?}", &self.file_path)),
				Err(e) => self.log.error(format!(
					"Failed to save circuit to {:?}: {:?}",
					&self.file_path, e
				)),
			}
		}

		if let Some(dialog) = self.dialog.as_mut() {
			let r = Window::new(dialog.name())
				.anchor(Align2([Align::Center; 2]), Vec2::ZERO)
				.show(ctx, |ui| dialog.show(ui));
			r.map(|r| r.inner.map(|r| r.then(|| self.dialog = None)));
		}

		self.script_editor.show(ctx, &mut self.circuit);
		self.log.show(ctx);

		// If one of the selected components has an external input, allow modifying it.
		let (mut ei, mut eo) = (Vec::new(), Vec::new());
		for (c, ..) in self.circuit.components(circuit::Aabb::ALL) {
			if let Some(i) = c.external_input() {
				ei.push((c.label().unwrap_or_default(), i));
			}
			if let Some(o) = c.external_output() {
				eo.push((c.label().unwrap_or_default(), o));
			}
		}
		self.io_editor
			.show(ctx, &ei, &eo, &mut self.inputs, &self.outputs);

		let show_components = |ui: &mut Ui| {
			ui.heading("Components");

			ui.label(format!("Inputs: {}", self.inputs.len()));
			ui.label(format!("Outputs: {}", self.outputs.len()));

			ui.separator();

			// Built in components
			let bits = core::num::NonZeroU8::new(1).unwrap();
			if ui.button("wire").clicked() {
				self.component = None;
			}
			if ui.button("in").clicked() {
				self.component = Some(Box::new(simulator::In::new("", bits, self.inputs.len())));
			}
			if ui.button("out").clicked() {
				self.component = Some(Box::new(simulator::Out::new("", bits, self.outputs.len())));
			}
			for c in COMPONENTS.iter() {
				if ui.button(c.0).clicked() {
					self.component = Some(c.1());
				}
			}

			ui.separator();

			// Custom components (ICs)
			for (name, ic) in self.ic_components.iter() {
				if ui.button(name.display()).clicked() {
					self.component = Some(Box::new(ic.clone()));
				}
			}
		};

		egui::SidePanel::left("components_list").show(ctx, |ui| {
			egui::ScrollArea::vertical().show(ui, show_components)
		});

		// Show component properties
		if let Some(c) = self.component.as_mut() {
			let changed = self.components_info.show(ctx, &[c], &mut self.log);
			for (name, value) in changed {
				if let Err(e) = c.set_property(&name, value) {
					self.log.error(e.to_string());
				}
			}
		} else {
			let c = self
				.selected_components
				.iter()
				.map(|&h| self.circuit.component(h).unwrap().0)
				.collect::<Vec<_>>();
			let changed = self.components_info.show(ctx, &c, &mut self.log);
			for (name, value) in changed {
				for &h in self.selected_components.iter() {
					let c = self.circuit.component_mut(h).unwrap().0;
					if let Err(e) = c.set_property(&name, value.clone()) {
						self.log.error(e.to_string());
					}
				}
			}
		}

		CentralPanel::default().show(ctx, |ui| {
			use epaint::*;
			let rect = ui.max_rect();
			let _paint = ui.painter_at(rect);
			let paint = ui.painter();
			for y in (rect.min.y as u16..rect.max.y as u16).step_by(16) {
				for x in (rect.min.x as u16..rect.max.x as u16).step_by(16) {
					let pos = Pos2::new(f32::from(x), f32::from(y));
					paint.circle(pos, 1.0, Color32::GRAY, Stroke::none());
				}
			}
			let e = ui.interact(Rect::EVERYTHING, ui.id(), Sense::drag());
			let hover_pos = ctx
				.input()
				.pointer
				.hover_pos()
				// Shrink as I haven't figured out how to get rid of the padding.
				.and_then(|p| ui.max_rect().shrink(-8.0).contains(p).then(|| p));

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
				self.selected_components.clear();
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
			let mut allow_place_wire = true;
			for (c, p, d, h) in self.circuit.components(aabb) {
				c.draw(&paint, point2pos(p), d, &self.inputs, &self.outputs);
				let aabb = c.aabb(d);
				let delta = Vec2::new(8.0, 8.0);
				let (min, max) = (p.saturating_add(aabb.min), p.saturating_add(aabb.max));
				let (min, max) = (point2pos(min) - delta, point2pos(max) + delta);
				let rect = Rect { min, max };
				let hover_on_component = hover_pos.map_or(false, |p| rect.contains(p));
				if hover_on_component {
					// Draw a box around the component
					hover_box = Some(rect);
					if e.clicked_by(PointerButton::Secondary) {
						// Mark the component as selected, or unselect if already selected.
						if let Some(i) = self.selected_components.iter().position(|e| e == &h) {
							self.selected_components.remove(i);
						} else {
							self.selected_components.push(h);
						}
					}
					if e.clicked_by(PointerButton::Middle) {
						// Toggle input if it is one
						c.external_input()
							.map(|i| self.inputs[i] = self.inputs[i].wrapping_add(1));
					}
				}
				let mut hover_on_port = false;
				for ((i, &po), is_in) in c
					.input_points()
					.iter()
					.enumerate()
					.map(|po| (po, true))
					.chain(c.output_points().iter().enumerate().map(|po| (po, false)))
				{
					if let Some(p) = p + d * po {
						paint.circle_filled(point2pos(p), 2.0, Color32::GREEN);
						if hover_pos.map_or(false, |h| pos2point(h) == p) {
							hover_on_port = true;
							let name = is_in
								.then(|| c.input_name(i))
								.unwrap_or_else(|| c.output_name(i));
							egui::containers::popup::show_tooltip_at(
								ctx,
								ui.id(),
								Some(point2pos(p) + Vec2::new(8.0, 8.0)),
								|ui| ui.label(name),
							);
						}
					}
				}
				allow_place_wire &= !hover_on_component | hover_on_port;
			}

			// Draw existing wires
			for (w, wh, h) in self.circuit.wires(aabb) {
				let intersects = e
					.hover_pos()
					.map_or(false, |p| w.intersect_point(pos2point(p)));
				let stroke = match intersects {
					true => Stroke::new(3.0, Color32::YELLOW),
					_ => Stroke::new(
						3.0,
						[Color32::DARK_GREEN, Color32::GREEN]
							[*self.memory.get(h.index()).unwrap_or(&0) & 1],
					),
				};
				if intersects && e.clicked_by(PointerButton::Secondary) {
					// Mark the wire as selected, or unselect if already selected.
					if let Some(i) = self.selected_wires.iter().position(|e| e == &wh) {
						// Swap remove won't affect any ordering in the GUI.
						self.selected_wires.swap_remove(i);
					} else {
						self.selected_wires.push(wh);
					}
				}
				let (from, to) = (point2pos(w.from), point2pos(w.to));
				paint.line_segment([from, to], stroke);
			}

			// Draw interaction objects (pointer, component, wire ...)
			if let Some(pos) = hover_pos {
				let point = pos2point(pos);
				let pos = point2pos(point);

				if let Some(c) = self.component.take() {
					c.draw(
						&paint,
						pos,
						self.component_direction,
						&self.inputs,
						&self.outputs,
					);

					if e.clicked_by(PointerButton::Primary) {
						(c.type_id() == TypeId::of::<simulator::In>()).then(|| self.inputs.push(0));
						(c.type_id() == TypeId::of::<simulator::Out>())
							.then(|| self.outputs.push(0));
						self.circuit
							.add_component(c, point, self.component_direction);
						self.needs_update = true;
					} else {
						self.component = Some(c);
					}
				} else if let Some(start) = self.wire_start {
					paint.circle_stroke(pos, 3.0, Stroke::new(2.0, Color32::RED));

					paint.line_segment([point2pos(start), pos], wire_stroke);

					// FIXME egui for some reason thinks that the primary button is not
					// pressed if the secondary was pressed and then released at the same
					// time.
					if e.drag_released() && !e.dragged_by(PointerButton::Primary) {
						self.circuit.add_wire(circuit::Wire::new(start, point));
						self.wire_start = None;
						self.needs_update = true;
					}
				} else if allow_place_wire {
					paint.circle_stroke(pos, 3.0, Stroke::new(2.0, Color32::GREEN));
					if e.drag_started() && e.dragged_by(PointerButton::Primary) {
						self.wire_start = Some(point);
					}
				}
			}

			// Draw boxes around the selected components
			let stroke = Stroke::new(2.0, selected_color);
			for h in self.selected_components.iter() {
				let (c, p, d) = self.circuit.component(*h).unwrap();
				draw_aabb(p, c.aabb(d), stroke);
			}

			// Draw a box around the hovered component
			hover_box.map(|rect| paint.rect_stroke(rect, 8.0, Stroke::new(2.0, Color32::YELLOW)));
		});
	}
}
