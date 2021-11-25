use core::fmt;
use core::iter::FromIterator;

/// A form of collection optimized for storing unique integers within a limited range and fast
/// iteration of thise numbers.
#[derive(Default)]
pub struct IntegerSet {
	/// A list of values added to the set.
	values: Vec<usize>,
	/// A bitmap indicating which values have been added, where each value corresponds to an
	/// index.
	bitmap: Vec<u8>,
}

impl IntegerSet {
	/// Insert a number in this set.
	///
	/// If the set did not have this number, `true` is returned. Otherwise, `false` is returned.
	pub fn insert(&mut self, num: usize) -> bool {
		let (i, m) = (num / 8, 1 << num % 8);
		if self.bitmap.get(i).map_or(true, |&e| e & m == 0) {
			self.bitmap.resize(self.bitmap.len().max(i + 1), 0);
			self.bitmap[i] |= m;
			self.values.push(num);
			true
		} else {
			false
		}
	}

	/// Iterate over all numbers in this set.
	pub fn iter(&self) -> core::slice::Iter<usize> {
		self.values.iter()
	}

	/// Clear the set, returning all values in the set.
	pub fn drain(&mut self) -> std::vec::Drain<usize> {
		self.bitmap.clear();
		self.values.drain(..)
	}

	pub fn is_empty(&self) -> bool {
		self.values.is_empty()
	}

	pub fn len(&self) -> usize {
		self.values.len()
	}
}

impl FromIterator<usize> for IntegerSet {
	fn from_iter<I>(iter: I) -> Self
	where
		I: IntoIterator<Item = usize>,
	{
		let mut s = Self::default();
		for v in iter {
			s.insert(v);
		}
		s
	}
}

impl fmt::Debug for IntegerSet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut f = f.debug_set();
		f.entries(&self.values);
		f.finish()
	}
}
