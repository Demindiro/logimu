use core::iter::FromIterator;
use core::ops::BitOrAssign;
use core::{fmt, mem};
use serde::{Deserialize, Serialize};

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
	#[allow(dead_code)]
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

	#[allow(dead_code)]
	pub fn contains(&self, num: usize) -> bool {
		let (i, m) = (num / 8, 1 << num % 8);
		self.bitmap.get(i).map_or(false, |&e| e & m > 0)
	}

	/// Merge a set into this set.
	pub fn union(&mut self, with: Self) {
		// Update values
		let (mut values, oh, bm) = if self.values.len() >= with.values.len() {
			(mem::take(&mut self.values), with.values, &self.bitmap)
		} else {
			(with.values, mem::take(&mut self.values), &with.bitmap)
		};
		for v in oh {
			let (i, m) = (v / 8, 1 << v % 8);
			if bm.get(i).map_or(true, |&e| e & m == 0) {
				values.push(v);
			}
		}

		// Update bitmap
		let (mut bitmap, oh) = if self.bitmap.len() >= with.bitmap.len() {
			(mem::take(&mut self.bitmap), with.bitmap)
		} else {
			(with.bitmap, mem::take(&mut self.bitmap))
		};
		for (w, r) in bitmap.iter_mut().zip(oh.iter()) {
			*w |= *r;
		}

		*self = Self { values, bitmap };
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

impl BitOrAssign<Self> for IntegerSet {
	fn bitor_assign(&mut self, rhs: Self) {
		self.union(rhs)
	}
}

/// An integer set optimized for `u8`
#[derive(Default, Serialize, Deserialize)]
pub struct IntegerSetU8([u128; 2]);

impl IntegerSetU8 {
	/// Insert a number in this set.
	///
	/// If the set did not have this number, `true` is returned. Otherwise, `false` is returned.
	pub fn insert(&mut self, num: u8) -> bool {
		let (i, m) = self.index_mask(num);
		let had = self.contains(num);
		self.0[i] |= m;
		!had
	}

	/// Remove a number from this set.
	///
	/// If the set did have this number, `true` is returned. Otherwise, `false` is returned.
	pub fn remove(&mut self, num: u8) -> bool {
		let (i, m) = self.index_mask(num);
		let had = self.contains(num);
		self.0[i] &= !m;
		had
	}

	/// Check if this set contains the given number.
	pub fn contains(&self, num: u8) -> bool {
		let (i, m) = self.index_mask(num);
		self.0[i] & m > 0
	}

	/// Iterate over all numbers in this set.
	#[allow(dead_code)]
	pub fn iter(&self) -> IntegerSetU8Iter {
		IntegerSetU8Iter { set: &self, index: Some(0) }
	}

	fn index_mask(&self, n: u8) -> (usize, u128) {
		(usize::from(n / 128), 1 << n % 128)
	}
}

pub struct IntegerSetU8Iter<'a> {
	set: &'a IntegerSetU8,
	index: Option<u8>,
}

impl Iterator for IntegerSetU8Iter<'_> {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		while let Some(i) = self.index {
			self.index = i.checked_add(1);
			if self.set.0[usize::from(i / 128)] & 1 << (i % 128) > 0 {
				return Some(i);
			}
		}
		None
	}
}
