mod component;
mod components_info;
mod copy;
mod gates;
mod ic;
mod inputs_outputs;
mod log;
mod script;

use component::*;
use components_info::*;
use copy::*;
use inputs_outputs::*;
use log::*;
use script::*;

use crate::circuit;
use crate::circuit::{Aabb, CircuitComponent, Direction, Ic, PointOffset, WireHandle};
use crate::simulator;

use crate::simulator::{ir, GraphNodeHandle, PropertyValue, SetProperty};

use core::any::TypeId;
use core::{fmt, mem};
use eframe::{egui, epi};
use rfd::FileDialog;
use std::collections::{BTreeMap, HashSet};
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
		("controlled buffer", || Box::new(ControlledBuffer::new())),
		("rom", || Box::new(ReadOnlyMemory::default())),
		("probe", || Box::new(Probe::default())),
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

impl fmt::Display for LoadCircuitError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Io(e) => e.fmt(f),
			Self::Serde(e) => e.fmt(f),
		}
	}
}

pub struct App {
	component: Option<Box<dyn ComponentPlacer>>,
	component_direction: circuit::Direction,
	wire_start: Option<circuit::Point>,
	circuit: Box<circuit::Circuit<Box<dyn ComponentPlacer>>>,
	ic_components: BTreeMap<Box<Path>, Ic>,

	selected_components: Vec<GraphNodeHandle>,
	selected_wires: Vec<WireHandle>,

	inputs: Vec<ir::Value>,
	outputs: Vec<ir::Value>,
	program_state: simulator::State,
	// TODO we shouldn't delay updates by a frame.
	needs_update: bool,

	file_path: Option<Box<Path>>,

	script_editor: ScriptEditor,
	log: Log,
	components_info: ComponentsInfo,
	io_editor: InputsOutputs,

	logged_parse_error: bool,

	drag_component: Option<(GraphNodeHandle, PointOffset, Direction)>,
	copied_properties: CopiedProperties,
	circuit_offset: egui::Vec2,

	enable_simulation: bool,
	debug_simulation: bool,
}

impl App {
	pub fn new() -> Self {
		let mut s = Self {
			component: None,
			component_direction: circuit::Direction::Up,
			wire_start: None,
			circuit: Default::default(),
			ic_components: Default::default(),

			selected_components: Default::default(),
			selected_wires: Default::default(),

			inputs: Vec::new(),
			outputs: Vec::new(),
			program_state: Default::default(),
			needs_update: false,

			file_path: None,

			script_editor: Default::default(),
			log: Default::default(),
			components_info: Default::default(),
			io_editor: Default::default(),

			logged_parse_error: false,

			drag_component: None,
			copied_properties: Default::default(),
			circuit_offset: Default::default(),

			enable_simulation: true,
			debug_simulation: false,
		};
		if let Some(f) = std::env::args().skip(1).next() {
			let f = PathBuf::from(f);
			match s.load_from_file(f.clone().into()) {
				Ok(()) => (),
				// Switch to directory regardless
				Err(_) => s.switch_to_file_dir(&f),
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

			s.file_path = Some(PathBuf::from(f.components().last().unwrap().as_os_str()).into());
		}
		s
	}

	pub fn load_from_file(&mut self, path: Box<Path>) -> Result<(), LoadCircuitError> {
		let f = fs::File::open(&path).map_err(|e| {
			self.log.error(format!("Failed to open {:?}: {}", path, e));
			LoadCircuitError::Io(e)
		})?;
		self.switch_to_file_dir(&path);
		self.circuit = ron::de::from_reader(f).map_err(|e| {
			self.log
				.error(format!("Failed to deserialize {:?}: {}", path, e));
			LoadCircuitError::Serde(e)
		})?;
		self.inputs.clear();
		self.outputs.clear();

		for (c, ..) in self.circuit.components(circuit::Aabb::ALL) {
			c.external_input().map(|i| {
				self.inputs
					.resize((i + 1).max(self.inputs.len()), ir::Value::Set(0)) // FIXME use Floating
			});
			c.external_output().map(|i| {
				self.outputs
					.resize((i + 1).max(self.outputs.len()), ir::Value::Floating)
			});
		}

		self.log.debug(format!("Loaded {:?}", &path));
		self.file_path = Some(path);
		self.needs_update = true;
		Ok(())
	}

	pub fn save_to_file(&mut self, path: &Path) -> Result<(), SaveCircuitError> {
		let f = fs::File::create(&path).map_err(SaveCircuitError::Io)?;
		ron::ser::to_writer(f, &self.circuit).map_err(SaveCircuitError::Serde)?;
		Ok(())
	}

	pub fn load_ic(&mut self, path: Box<Path>) -> Result<(), circuit::LoadError> {
		self.ic_components.insert(path.clone(), Ic::get_ic(path)?);
		Ok(())
	}

	fn file_dialog(&self) -> FileDialog {
		let fd = FileDialog::new().add_filter("logimu", &["logimu"]);
		if let Some(p) = self.file_path.as_ref() {
			fd.set_file_name(p.to_str().unwrap())
		} else {
			fd
		}
	}

	/// Save the circuit. May be cancelled by the user.
	fn save(&mut self, ask_path: bool) {
		if ask_path || self.file_path.is_none() {
			if let Some(path) = self.file_dialog().save_file().map(Into::into) {
				self.file_path = Some(path);
			} else {
				return; // Cancelled by user.
			}
		}
		let path = self.file_path.as_ref().unwrap().clone();
		match self.save_to_file(&path) {
			Ok(_) => self.log.debug(format!("Saved circuit to {:?}", path)),
			Err(e) => self
				.log
				.error(format!("Failed to save circuit to {:?}: {:?}", path, e)),
		}
	}

	/// Create a new empty circuit.
	fn clear_circuit(&mut self) {
		self.file_path = None;
		self.circuit = Default::default();
		self.selected_components = Default::default();
		self.selected_wires = Default::default();
		self.inputs = Default::default();
		self.outputs = Default::default();
		self.program_state = Default::default();
	}

	/// Set working directory to the parent directory of a file. Ignores `""`.
	fn switch_to_file_dir(&self, path: &Path) {
		let parent = path.parent().unwrap();
		if parent != Path::new("") {
			std::env::set_current_dir(path.parent().unwrap()).unwrap();
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

		// Cancel any active actions.
		if ctx.input().key_pressed(Key::Escape) {
			self.wire_start = None;
			self.component = None;
			self.drag_component = None;
		}

		if ctx.input().key_pressed(Key::R) {
			self.component_direction = self.component_direction.rotate_clockwise();
			self.drag_component
				.as_mut()
				.map(|(.., d)| *d = d.rotate_clockwise());
		}

		// Check if we should remove any selected components and/or wires
		let del = [Key::Backspace, Key::Delete]
			.iter()
			.any(|e| ctx.input().key_pressed(*e));
		if del && ctx.memory().focus().is_none() {
			for c in self.selected_components.drain(..) {
				self.circuit.remove_component(c).unwrap();
			}
			let _ = self.circuit.remove_wires(&self.selected_wires);
			self.selected_wires.clear();
		}

		if ctx.input().key_pressed(Key::S) && ctx.input().modifiers.ctrl {
			self.save(false);
		}
		let mut step_simulation = ctx.input().key_pressed(Key::I);

		TopBottomPanel::top("top_panel").show(ctx, |ui| {
			menu::bar(ui, |ui| {
				menu::menu(ui, "File", |ui| {
					if ui.button("New").clicked() {
						self.clear_circuit();
					}
					if ui.button("Open").clicked() {
						if let Some(file) = FileDialog::new().pick_file() {
							// The status is already logged in the call.
							let _ = self.load_from_file(file.into());
						}
					}
					if ui.button("Close").clicked() {
						frame.quit()
					}
					if ui.button("Save").clicked() {
						self.save(false);
					}
					if ui.button("Save as").clicked() {
						self.save(true);
					}
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
										&mut self.program_state,
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
				menu::menu(ui, "Simulation", |ui| {
					ui.checkbox(&mut self.enable_simulation, "Enabled");
					step_simulation |= ui.button("Step").clicked();
					ui.checkbox(&mut self.debug_simulation, "Debug simulation");
				});
			});
		});

		// Run circuit
		if self.needs_update {
			let program = std::sync::Arc::new(self.circuit.generate_ir());
			self.program_state = mem::take(&mut self.program_state).adapt(program.clone());
			self.needs_update = false;
		}
		self.program_state.write_inputs(&self.inputs);
		if self.enable_simulation {
			self.program_state.run(1024);
		} else if step_simulation {
			self.log.debug("stepping simulation");
			self.program_state.step();
		}
		self.program_state.read_outputs(&mut self.outputs);

		self.script_editor.show(ctx, &mut self.circuit);
		self.log.show(ctx);

		// If one of the selected components has an external input, allow modifying it.
		let (mut ei, mut eo) = (Vec::new(), Vec::new());
		for (c, ..) in self.circuit.components(circuit::Aabb::ALL) {
			if let Some(i) = c.external_input() {
				let bits = c.outputs()[0].bits.get();
				let range = 0..=((1 << bits) - 1);
				ei.push((c.label().unwrap_or_default(), range, i));
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
			// Scroll the window
			let mut d = ctx.input().scroll_delta / 50.0 * 16.0;
			ctx.input().modifiers.shift.then(|| (d.x, d.y) = (d.y, d.x));
			self.circuit_offset += d;

			use epaint::*;
			let rect = ui.max_rect().shrink2(-ui.spacing().window_padding);
			let paint = ui.painter_at(rect);
			//let paint = ui.painter();

			// Draw a dot at each point
			for y in (rect.min.y as u16..rect.max.y as u16).step_by(16) {
				for x in (rect.min.x as u16..rect.max.x as u16).step_by(16) {
					let pos = Pos2::new(f32::from(x), f32::from(y));
					let d = pos - rect.min - self.circuit_offset;
					if d.x >= 0.0 && d.y >= 0.0 {
						paint.circle(pos, 1.0, Color32::GRAY, Stroke::none());
					}
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
				let pos = pos - self.circuit_offset;
				let (x, y) = ((pos.x - rect.min.x) / 16.0, (pos.y - rect.min.y) / 16.0);
				circuit::Point { x: x.round() as u16, y: y.round() as u16 }
			};
			let point2pos = |point: circuit::Point| {
				let (x, y) = (f32::from(point.x), f32::from(point.y));
				Pos2::new(rect.min.x + x * 16.0, rect.min.y + y * 16.0) + self.circuit_offset
			};
			let draw_aabb =
				|point: circuit::Point, aabb: circuit::RelativeAabb, stroke: Stroke| -> Rect {
					let delta = Vec2::new(8.0, 8.0);
					let (min, max) = ((point + aabb.min).unwrap(), (point + aabb.max).unwrap());
					let (min, max) = (point2pos(min) - delta, point2pos(max) + delta);
					let rect = Rect { min, max };
					paint.rect_stroke(rect, 8.0, stroke);
					rect
				};

			let wire_stroke = Stroke::new(3.0, Color32::WHITE);
			let selected_color = Color32::LIGHT_BLUE.linear_multiply(0.5);
			let move_alpha = 0.5;

			let aabb = circuit::Aabb::new(pos2point(rect.min), pos2point(rect.max));

			// Clear current selected if no modifiers are pressed during select
			if e.clicked_by(PointerButton::Secondary) && !ui.input().modifiers.shift {
				self.selected_components.clear();
				self.selected_wires.clear();
			}

			// Draw outlines for selected wires
			let mut endpoints = HashSet::new();
			for w in self.selected_wires.iter() {
				let radius = 1.5;
				let offt = 1.0;
				let (w, ..) = self.circuit.wire(*w).unwrap();
				let (min, max) = w.into();
				let stroke = Stroke::new((radius + offt) * 2.0, selected_color);
				paint.line_segment([point2pos(min), point2pos(max)], stroke);

				// Draw a circle to avoid disjoint-looking wires.
				// Use a bigger circle to indicate intersections.
				for p in [min, max] {
					if endpoints.insert(p) {
						let r = self.circuit.wire_endpoints(p).count() > 2;
						let r = radius * f32::from(1 + u8::from(r) * 2) + offt;
						paint.circle_filled(point2pos(p), r, selected_color);
					}
				}
			}

			// Draw existing components
			let mut hover_box = None;
			let mut allow_place_wire = true;
			let mut hover_component = None;
			let shift = ctx.input().modifiers.shift_only();
			for (c, p, d, h) in self.circuit.components(aabb) {
				// Don't draw components that are being moved
				if !shift && self.drag_component.map_or(false, |(c, ..)| c == h) {
					continue;
				}

				let draw = Draw {
					painter: &paint,
					alpha: 1.0,
					position: point2pos(p),
					direction: d,
					inputs: &self.inputs,
					outputs: &self.outputs,
					program_state: &self.program_state,
					point: p,
					nexus_at: &|p| self.circuit.wire_endpoints(p).next().map(|(.., n)| n),
				};
				c.draw(draw);
				let aabb = c.aabb(d);
				let delta = Vec2::new(8.0, 8.0);
				let (min, max) = (p.saturating_add(aabb.min), p.saturating_add(aabb.max));
				let (min, max) = (point2pos(min) - delta, point2pos(max) + delta);
				let rect = Rect { min, max };
				let hover_on_component = hover_pos.map_or(false, |p| rect.contains(p));
				if hover_on_component {
					// Draw a box around the component
					(hover_box, hover_component) = (Some(rect), Some(h));
					if !shift && e.clicked_by(PointerButton::Secondary) {
						// Mark the component as selected, or unselect if already selected.
						if let Some(i) = self.selected_components.iter().position(|e| e == &h) {
							self.selected_components.remove(i);
						} else {
							self.selected_components.push(h);
						}
					}
					if e.clicked_by(PointerButton::Middle) {
						// Toggle input if it is one
						if let Some(i) = c.external_input() {
							self.inputs[i] = match self.inputs[i] {
								ir::Value::Set(i) => {
									let m = 1 << c.outputs()[0].bits.get();
									ir::Value::Set(i.wrapping_add(1).rem_euclid(m))
								}
								ir::Value::Short | ir::Value::Floating => ir::Value::Set(0),
							};
						}
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

			// Copy or paste component properties
			hover_component
				.and_then(|h| self.circuit.component_mut(h))
				.map(|(c, ..)| {
					if shift && e.clicked_by(PointerButton::Primary) {
						self.copied_properties.apply(c)
					} else if shift && e.clicked_by(PointerButton::Secondary) {
						self.copied_properties = c.into();
					}
				});

			// Check if we're hovering over a wire
			let mut wires = Vec::new();
			if let Some(p) = hover_pos.map(pos2point) {
				for (.., h) in self.circuit.wires(Aabb::new(p, p)) {
					(!wires.contains(&h)).then(|| wires.push(h));
				}
			}

			// Draw existing wires
			let mut endpoints = HashSet::new();
			for (w, wh, h) in self.circuit.wires(aabb) {
				let radius = 1.5;
				let intersects = wires.contains(&h);
				let color = if intersects {
					Color32::YELLOW
				} else {
					pub use crate::simulator::ir::Value;
					match self.program_state.read_nexus(h) {
						Value::Set(v) => [Color32::DARK_GREEN, Color32::GREEN][v & 1],
						Value::Floating => Color32::BLUE,
						Value::Short => Color32::RED,
					}
				};
				let stroke = Stroke::new(radius * 2.0, color);
				let intersects = hover_pos.map_or(false, |p| w.intersect_point(pos2point(p)));
				if intersects && e.clicked_by(PointerButton::Secondary) {
					// Mark the wire as selected, or unselect if already selected.
					if let Some(i) = self.selected_wires.iter().position(|e| e == &wh) {
						// Swap remove won't affect any ordering in the GUI.
						self.selected_wires.swap_remove(i);
					} else {
						self.selected_wires.push(wh);
					}
				}
				let (min, max) = w.into();
				paint.line_segment([point2pos(min), point2pos(max)], stroke);

				// Draw a circle to avoid disjoint-looking wires.
				// Use a bigger circle to indicate intersections.
				for p in [min, max] {
					if endpoints.insert(p) {
						let r = self.circuit.wire_endpoints(p).count() > 2;
						let r = radius * f32::from(1 + u8::from(r) * 2);
						paint.circle_filled(point2pos(p), r, color);
					}
				}
			}

			// Draw interaction objects (pointer, component, wire ...)
			if let Some(pos) = hover_pos {
				let point = pos2point(pos);
				let pos = point2pos(point);

				if let Some(c) = self.component.take() {
					let draw = Draw {
						painter: &paint,
						alpha: move_alpha,
						position: pos,
						direction: self.component_direction,
						inputs: &self.inputs,
						outputs: &self.outputs,
						program_state: &self.program_state,
						point,
						nexus_at: &|p| self.circuit.wire_endpoints(p).next().map(|(.., n)| n),
					};
					c.draw(draw);

					if e.clicked_by(PointerButton::Primary) {
						(c.type_id() == TypeId::of::<simulator::In>())
							.then(|| self.inputs.push(ir::Value::Set(0))); // FIXME use Floating
						(c.type_id() == TypeId::of::<simulator::Out>())
							.then(|| self.outputs.push(ir::Value::Floating));
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
				} else if let Some((h, p, d)) = self.drag_component {
					let p = point.saturating_add(p);
					let (c, ..) = self.circuit.component(h).unwrap();
					let draw = Draw {
						painter: &paint,
						alpha: move_alpha,
						position: point2pos(p),
						direction: d,
						inputs: &self.inputs,
						outputs: &self.outputs,
						program_state: &self.program_state,
						point: p,
						nexus_at: &|p| self.circuit.wire_endpoints(p).next().map(|(.., n)| n),
					};
					c.draw(draw);
					if e.drag_released() && !e.dragged_by(PointerButton::Primary) {
						self.drag_component = None;
						self.circuit.move_component(h, p, d).unwrap();
					}
				} else if allow_place_wire {
					paint.circle_stroke(pos, 3.0, Stroke::new(2.0, Color32::GREEN));
					if e.drag_started() && e.dragged_by(PointerButton::Primary) {
						self.wire_start = Some(point);
					}
				} else if let Some(c) = hover_component {
					if e.drag_started() && e.dragged_by(PointerButton::Primary) {
						let (_, p, d) = self.circuit.component(c).unwrap();
						self.drag_component = Some((c, (p - point).unwrap(), d));
					}
				}
			}

			// Mark any components that will be updated in the next step
			if self.debug_simulation {
				let list = self.program_state.dirty_components().collect::<Vec<_>>();
				for (_, p, _, h) in self.circuit.components(aabb) {
					if list.contains(&h) {
						paint.circle_filled(point2pos(p), 4.0, Color32::RED);
					}
				}
			}

			// Draw boxes around the selected components
			// Also add a rotate button at the top-left corner
			let stroke = Stroke::new(2.0, selected_color);
			for h in self.selected_components.iter() {
				let (c, p, d) = self.circuit.component(*h).unwrap();
				let mut rect = draw_aabb(p, c.aabb(d), stroke);
				rect.max.x += 8.0;
				rect.min.y -= 14.0;
				rect.min.x = rect.max.x;
				rect.max.y = rect.min.y;
				let button = Label::new("⟳")
					.text_color(Color32::WHITE)
					.sense(Sense::click());
				if ui.put(rect, button).clicked() {
					let (_, pos, dir) = self
						.circuit
						.component(*h)
						.expect("invalid selected component");
					self.circuit
						.move_component(*h, pos, dir.rotate_clockwise())
						.unwrap();
				}
			}

			// Draw a box around the hovered component
			hover_box.map(|rect| paint.rect_stroke(rect, 8.0, Stroke::new(2.0, Color32::YELLOW)));
		});
	}
}
