use super::{Log, PropertyValue, SetProperty};
use crate::circuit::CircuitComponent;
use eframe::egui;

/// Show properties of one or more components.
#[derive(Default)]
pub struct ComponentsInfo {
	property_value_buffer: Option<(Box<str>, String)>,
}

impl ComponentsInfo {
	/// # Returns
	///
	/// A list of changed properties and the new value.
	pub fn show<C>(
		&mut self,
		ctx: &egui::CtxRef,
		components: &[&C],
		log: &mut Log,
	) -> Vec<(Box<str>, SetProperty)>
	where
		C: CircuitComponent,
	{
		if components.is_empty() {
			return [].into();
		}

		let mut changed = Vec::new();

		// Only show common properties
		let mut it = components.iter().map(|c| c.properties());
		let mut props: Vec<_> = it.next().unwrap().into();
		for c in it {
			let mut common = Vec::new();
			for p in props {
				c.iter().find(|e| e.name == p.name).map(|_| common.push(p));
			}
			props = common;
		}

		let show = |ui: &mut egui::Ui| {
			let mut prop_buf = self.property_value_buffer.take();
			for prop in props {
				// Capitalize the name
				let name = prop
					.name
					.chars()
					.enumerate()
					.map(|(i, c)| (i == 0).then(|| c.to_ascii_uppercase()).unwrap_or(c))
					.collect::<String>();
				match prop.value {
					PropertyValue::Int { value, range } => {
						let mut v = value;
						ui.add(egui::Slider::new(&mut v, range.clone()).text(name));
						if v != value {
							changed.push((prop.name.clone(), SetProperty::Int(v)));
						}
					}
					PropertyValue::Str { value } => {
						let mut value = value.to_string();
						if ui
							.add(egui::TextEdit::singleline(&mut value).hint_text(name))
							.changed()
						{
							changed.push((prop.name, SetProperty::Str(value.into())));
						}
					}
					PropertyValue::Mask { value } => {
						let (mut text, modify) = match prop_buf.take() {
							Some((name, buf)) if name == prop.name => (buf, true),
							pb => {
								prop_buf = pb;
								(mask_to_string(value), false)
							}
						};
						let te = egui::TextEdit::singleline(&mut text).hint_text(name);
						if ui.add(te).has_focus() {
							self.property_value_buffer = Some((prop.name, text));
						} else if modify {
							match string_to_mask(&text) {
								Ok(mask) => changed.push((prop.name, SetProperty::Mask(mask))),
								Err(e) => log.error(e),
							}
						}
					}
				}
			}
		};

		egui::Window::new("Component").show(ctx, |ui| {
			egui::ScrollArea::vertical()
				.max_width(f32::INFINITY)
				.show(ui, show)
		});

		changed
	}
}

/// Convert a mask to a human-readable string.
pub fn mask_to_string(mut mask: usize) -> String {
	let mut s = "".to_string();
	let (mut offt, mut comma) = (0, false);
	while mask > 0 {
		if mask & 1 > 0 {
			comma.then(|| s.push(','));
			s.extend(offt.to_string().chars());
			let o = offt;
			while mask & 2 > 0 {
				mask >>= 1;
				offt += 1;
			}
			if offt != o {
				s.push('-');
				s.extend(offt.to_string().chars());
			}
			comma = true;
		}
		mask >>= 1;
		offt += 1;
	}
	s
}

/// Convert a human-readable mask string to an actual mask.
fn string_to_mask(s: &str) -> Result<usize, String> {
	let mut mask = 0;
	for r in s.split(',').map(str::trim) {
		if let Some((min, max)) = r.split_once('-') {
			let (min, max) = (min.trim_end(), max.trim_start());
			let min = min.parse::<u8>().map_err(|e| e.to_string())?;
			let max = max.parse::<u8>().map_err(|e| e.to_string())?;
			for i in min..=max {
				mask |= 1 << i;
			}
		} else {
			mask |= 1 << r.parse::<u8>().map_err(|e| e.to_string())?;
		}
	}
	Ok(mask)
}
