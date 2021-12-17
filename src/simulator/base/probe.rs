use super::*;
use serde::{Deserialize, Serialize};

/// A component that displays the value of a nexus but does **not** act as an output.
#[derive(Default, Serialize, Deserialize)]
pub struct Probe {
	#[serde(default)]
	pub display_mode: DisplayMode,
}

#[derive(Serialize, Deserialize)]
pub enum DisplayMode {
	Hex,
	Binary,
	UnsignedDecimal,
	SignedDecimal,
}

impl Default for DisplayMode {
	fn default() -> Self {
		Self::Hex
	}
}

impl Component for Probe {
	fn inputs(&self) -> Box<[InputType]> {
		[InputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[].into()
	}

	fn generate_ir(&self, _: GenerateIr) -> usize {
		0
	}

	fn properties(&self) -> Box<[Property]> {
		[Property {
			name: "Display mode".into(),
			read_only: false,
			value: PropertyValue::Enum {
				value: match self.display_mode {
					DisplayMode::Hex => "hex",
					DisplayMode::Binary => "bin",
					DisplayMode::UnsignedDecimal => "dec",
					DisplayMode::SignedDecimal => "udec",
				}
				.into(),
				options: ["hex".into(), "bin".into(), "dec".into(), "udec".into()].into(),
			},
		}]
		.into()
	}

	fn set_property(&mut self, name: &str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		match name {
			"Display mode" => {
				self.display_mode = match &*value.into_enum().ok_or("expected enum")? {
					"hex" => DisplayMode::Hex,
					"bin" => DisplayMode::Binary,
					"dec" => DisplayMode::UnsignedDecimal,
					"udec" => DisplayMode::SignedDecimal,
					_ => Err("invalid enum value")?,
				};
			}
			_ => Err("unknown property")?,
		}
		Ok(())
	}
}
