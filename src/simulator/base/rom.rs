use super::*;
use core::cell::Cell;
use std::sync::Arc;
use thin_dst::ThinArc;

/// A component representing read-only memory.
#[derive(Default, Serialize, Deserialize)]
pub struct ReadOnlyMemory {
	contents: Vec<usize>,
	#[serde(skip)]
	cached: Cell<Option<ThinArc<(), usize>>>,
}

impl ReadOnlyMemory {
	pub fn get(&self, index: usize) -> Option<usize> {
		self.contents.get(index).copied()
	}
}

impl Component for ReadOnlyMemory {
	fn inputs(&self) -> Box<[InputType]> {
		[InputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[OutputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn generate_ir(&self, gen: GenerateIr) -> usize {
		let (address, out) = (gen.inputs[0], gen.outputs[0]);
		if address != usize::MAX && out != usize::MAX {
			let mut cached = self.cached.take();
			let memory = cached
				.get_or_insert_with(|| ThinArc::new((), self.contents.clone()))
				.clone();
			(gen.out)([IrOp::Read { memory }].into());
			self.cached.set(cached);
		}
		0
	}

	fn properties(&self) -> Box<[Property]> {
		// TODO some 'memory' / 'dialog' property should be used for editing a large amount of
		// data
		let range = i32::MIN.into()..=u32::MAX.into();
		self.contents
			.iter()
			.chain(Some(&0))
			.enumerate()
			.map(|(i, e)| {
				Property::new(
					format!("0x{:03x}", i),
					PropertyValue::Int { value: *e as i64, range: range.clone() },
				)
			})
			.collect()
	}

	fn set_property(&mut self, name: &str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		if !name.starts_with("0x") {
			Err("invalid property")?;
		}
		match (
			usize::from_str_radix(name.split_at(2).1, 16),
			value.as_int(),
		) {
			(Ok(i), Some(v)) if i < self.contents.len() => self.contents[i] = v as usize,
			(Ok(i), Some(v)) if i == self.contents.len() => self.contents.push(v as usize),
			(Ok(_), Some(_)) => Err("address out of range")?,
			(Err(_), ..) => Err("invalid property")?,
			(.., None) => Err("expected integer")?,
		}
		self.cached.set(None);
		Ok(())
	}
}
