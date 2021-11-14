use core::fmt;
use eframe::egui;

/// List of circuit inputs, each with a numeric input to change it.
#[derive(Default)]
pub struct Inputs {}

impl Inputs {
	pub fn show(
		&mut self,
		ctx: &egui::CtxRef,
		inputs: &[(impl fmt::Display, usize)],
		values: &mut [usize],
	) {
		if values.is_empty() {
			return;
		}

		egui::Window::new("Inputs").show(ctx, |ui| {
			egui::ScrollArea::vertical()
				.max_width(f32::INFINITY)
				.show(ui, |ui| {
					for (l, i) in inputs.iter() {
						ui.horizontal(|ui| {
							ui.add(egui::DragValue::new(&mut values[*i]));
							ui.label(l);
						});
					}
				});
		});
	}
}
