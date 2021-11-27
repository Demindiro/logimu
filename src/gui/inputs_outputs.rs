use crate::simulator::ir::Value;
use core::fmt;
use core::ops::RangeInclusive;
use eframe::egui;

/// List of circuit inputs & outputs, each with a numeric input to change it.
#[derive(Default)]
pub struct InputsOutputs {}

impl InputsOutputs {
	pub fn show(
		&mut self,
		ctx: &egui::CtxRef,
		inputs: &[(impl fmt::Display, RangeInclusive<i64>, usize)],
		outputs: &[(impl fmt::Display, usize)],
		input_values: &mut [Value],
		output_values: &[Value],
	) {
		if inputs.is_empty() && outputs.is_empty() {
			return;
		}

		egui::Window::new("Inputs & outputs").show(ctx, |ui| {
			egui::ScrollArea::vertical()
				.max_width(f32::INFINITY)
				.show(ui, |ui| {
					ui.horizontal(|ui| {
						if !inputs.is_empty() {
							ui.vertical(|ui| {
								for (l, r, i) in inputs.iter() {
									ui.horizontal(|ui| {
										let mut v = match input_values[*i] {
											Value::Set(v) => v,
											_ => todo!(),
										};
										ui.add(egui::DragValue::new(&mut v).clamp_range(r.clone()));
										input_values[*i] = Value::Set(v);
										ui.label(l);
									});
								}
							});
						}
						// FIXME separator doesn't go entirely to the bottom
						(!inputs.is_empty() && !outputs.is_empty()).then(|| ui.separator());
						if !outputs.is_empty() {
							ui.vertical(|ui| {
								for (l, i) in outputs.iter() {
									let v = match output_values[*i] {
										Value::Set(i) => i.to_string(),
										Value::Floating => "x".to_string(),
										Value::Short => "E".to_string(),
									};
									ui.horizontal(|ui| {
										ui.add(egui::Label::new(v).monospace());
										ui.label(l);
									});
								}
							});
						}
					})
				});
		});
	}
}
