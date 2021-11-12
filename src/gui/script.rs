use crate::circuit::{Circuit, CircuitComponent};

use eframe::egui;

pub struct ScriptEditor {
	pub open: bool,
}

impl ScriptEditor {
	pub fn show<C>(&mut self, ctx: &egui::CtxRef, circuit: &mut Circuit<C>)
	where
		C: CircuitComponent,
	{
		if !self.open {
			return;
		}
		egui::Window::new("Script")
			.open(&mut self.open)
			.show(ctx, |ui| {
				ui.add(
					egui::TextEdit::multiline(&mut circuit.script_source)
						.code_editor()
						.desired_width(500.0),
				);
			});
	}
}

impl Default for ScriptEditor {
	fn default() -> Self {
		Self { open: false }
	}
}
