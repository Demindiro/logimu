use crate::circuit::{Circuit, CircuitComponent};
use core::mem;
use eframe::egui::{
	self,
	text::{LayoutJob, LayoutSection, TextFormat},
	Align, Color32, TextStyle,
};

#[derive(Default)]
pub struct ScriptEditor {
	pub open: bool,
	id: Option<egui::Id>,
}

impl ScriptEditor {
	pub fn show<C>(&mut self, ctx: &egui::CtxRef, circuit: &mut Circuit<C>)
	where
		C: CircuitComponent,
	{
		if !self.open {
			return;
		}

		let id = self.id;
		let mut layouter = |ui: &egui::Ui, string: &str, wrap_width| {
			let cursor = id
				.and_then(|id| egui::TextEdit::cursor(ui, id))
				.map_or(usize::MAX, |c| c.primary.ccursor.index);
			let fmt_default = TextFormat { style: TextStyle::Monospace, ..Default::default() };
			let fmt_red = TextFormat {
				style: TextStyle::Monospace,
				color: Color32::RED,
				..Default::default()
			};
			let fmt_green = TextFormat {
				style: TextStyle::Monospace,
				color: Color32::GREEN,
				..Default::default()
			};

			// Figure out which braces to mark
			let mut redundant_open = Vec::new();
			let mut redundant_close = Vec::new();
			let mut closest_open = usize::MAX;
			let mut closest_open_level = usize::MAX;
			let mut closest_close = usize::MAX;
			for (i, c) in string.bytes().enumerate() {
				match c {
					b'(' => {
						if i < cursor {
							closest_open = i;
							closest_open_level = redundant_open.len();
						}
						redundant_open.push(i);
					}
					b')' => {
						redundant_open
							.pop()
							.is_none()
							.then(|| redundant_close.push(i));
						if i < cursor {
							closest_open = *redundant_open.last().unwrap_or(&usize::MAX);
							closest_open_level = redundant_open.len().wrapping_sub(1);
						}
						if i >= cursor
							&& closest_open_level == redundant_open.len()
							&& closest_close == usize::MAX
						{
							closest_close = i;
						}
					}
					_ => (),
				}
			}
			(closest_open_level < redundant_open.len()).then(|| closest_open = usize::MAX);

			// Create sections to mark braces
			let (mut close_i, mut open_i) = (0, 0);
			let mut pop = || {
				let c = *redundant_close.get(close_i).unwrap_or(&usize::MAX);
				let o = *redundant_open.get(open_i).unwrap_or(&usize::MAX);
				let mut tmp = usize::MAX;
				let (n, i) = if c < o {
					close_i += 1;
					(c, &mut close_i)
				} else if o < c {
					open_i += 1;
					(o, &mut open_i)
				} else {
					(usize::MAX, &mut tmp)
				};
				let (n, red) = if closest_open < n {
					*i -= 1;
					(mem::replace(&mut closest_open, usize::MAX), false)
				} else if closest_close < n {
					*i -= 1;
					(mem::replace(&mut closest_close, usize::MAX), false)
				} else {
					dbg!("wtf");
					(n, true)
				};
				(n != usize::MAX).then(|| (n, red))
			};

			let mut sections = Vec::new();
			let mut start = 0;
			while let Some((i, red)) = pop() {
				if i > start {
					sections.push(LayoutSection {
						leading_space: 0.0,
						byte_range: start..i,
						format: fmt_default,
					});
				}
				sections.push(LayoutSection {
					leading_space: 0.0,
					byte_range: i..i + 1,
					format: red.then(|| fmt_red).unwrap_or(fmt_green),
				});
				start = i + 1;
			}

			if start != string.len() {
				sections.push(LayoutSection {
					leading_space: 0.0,
					byte_range: start..string.len(),
					format: fmt_default,
				});
			}

			let job = LayoutJob {
				text: string.into(),
				sections,
				wrap_width,
				first_row_min_height: 0.0,
				break_on_newline: true,
				halign: Align::LEFT,
				justify: false,
			};
			ui.fonts().layout_job(job)
		};
		egui::Window::new("Script")
			.open(&mut self.open)
			.show(ctx, |ui| {
				egui::ScrollArea::vertical().show(ui, |ui| {
					self.id = Some(
						ui.add(
							egui::TextEdit::multiline(&mut circuit.script_source)
								.code_editor()
								.desired_width(f32::INFINITY)
								.desired_rows(16)
								.layouter(&mut layouter),
						)
						.id,
					);
				});
			});
	}
}
