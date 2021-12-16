use crate::simulator::{Component, Property, PropertyValue, SetProperty};

/// Copied properties of a component
#[derive(Default)]
pub struct CopiedProperties {
	properties: Box<[Property]>,
}

impl CopiedProperties {
	pub fn apply(&self, to: &mut dyn Component) {
		for p in &*self.properties {
			let v = match &p.value {
				PropertyValue::Int { value, .. } => SetProperty::Int(value.clone()),
				PropertyValue::Mask { value, .. } => SetProperty::Mask(value.clone()),
				PropertyValue::Str { value } => SetProperty::Str(value.clone()),
				PropertyValue::Bool { value } => SetProperty::Bool(value.clone()),
				PropertyValue::Enum { value, .. } => SetProperty::Enum(value.clone()),
			};
			let _ = to.set_property(&p.name, v);
		}
	}
}

impl<C> From<&C> for CopiedProperties
where
	C: Component,
{
	fn from(c: &C) -> Self {
		Self { properties: c.properties() }
	}
}

impl<C> From<&mut C> for CopiedProperties
where
	C: Component,
{
	fn from(c: &mut C) -> Self {
		Self::from(&*c)
	}
}
