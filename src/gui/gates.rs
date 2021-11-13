use super::ComponentPlacer;
use crate::circuit::{CircuitComponent, Direction, PointOffset, RelativeAabb};

use crate::simulator::*;
use core::num::NonZeroU8;
use eframe::egui::paint::{CircleShape, Mesh, RectShape};
use eframe::egui::{Align2, Color32, Painter, Pos2, Rect, Shape, Stroke, TextStyle, Vec2};

const IN_NOT: &[PointOffset] = &[PointOffset::new(-1, 0)];
const OUT: &[PointOffset] = &[PointOffset::new(1, 0)];
const CENTER: &[PointOffset] = &[PointOffset::new(0, 0)];

macro_rules! impl_cc {
	($name:ident, $in:expr, $out:expr, (($min_x:literal, $min_y:literal), ($max_x:literal, $max_y:literal))) => {
		#[typetag::serde]
		impl CircuitComponent for $name {
			fn input_points(&self) -> Box<[PointOffset]> {
				let i = i16::from(self.inputs.get());
				(-i / 2..0)
					.chain((i % 2 != 0).then(|| 0))
					.chain(1..=i / 2)
					.map(|y| PointOffset::new(-1, y.try_into().unwrap()))
					.collect()
			}

			fn output_points(&self) -> Box<[PointOffset]> {
				$out.into()
			}

			fn aabb(&self, dir: Direction) -> RelativeAabb {
				dir * RelativeAabb::new(
					PointOffset::new($min_x, $min_y),
					PointOffset::new($max_x, $max_y),
				)
			}
		}
	};
}

impl_cc!(AndGate, IN, OUT, ((-1, -1), (1, 1)));

#[typetag::serde]
impl ComponentPlacer for AndGate {
	fn name(&self) -> Box<str> {
		"and".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], _: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);
		let radius = 16.0;

		painter.add(CircleShape { center: pos, radius, fill: Color32::WHITE, stroke });
		let top_left = pos + dir.rotate_vec2(Vec2::new(-radius, -radius));
		let top_right = pos + dir.rotate_vec2(Vec2::new(0.0, -radius));
		let bottom_left = pos + dir.rotate_vec2(Vec2::new(-radius, radius));
		painter.rect_filled(
			Rect::from_two_pos(top_right, bottom_left),
			0.0,
			Color32::WHITE,
		);
		let offt = dir.rotate_vec2(Vec2::new(radius, 0.0));
		painter.line_segment([top_left, bottom_left], stroke);
		painter.line_segment([top_left, top_left + offt], stroke);
		painter.line_segment([bottom_left, bottom_left + offt], stroke);
	}
}

impl_cc!(OrGate, IN, OUT, ((-1, -1), (1, 1)));

#[typetag::serde]
impl ComponentPlacer for OrGate {
	fn name(&self) -> Box<str> {
		"or".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], _: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);

		let mut v = Vec::new();

		let verts_a = [
			(0.5, 0.0),
			(0.4, 0.0258),
			(0.3, 0.0456),
			(0.2, 0.0596),
			(0.1, 0.0680),
		];
		let verts_b = [
			(0.495, 0.1),
			(0.4798, 0.2),
			(0.4539, 0.3),
			(0.4165, 0.4),
			(0.366, 0.5),
			(0.3, 0.6),
			(0.2141, 0.7),
			(0.1, 0.8),
		];
		v.push(Pos2::new(0.866, 0.0));
		v.extend(verts_b.into_iter().rev().map(|(y, x)| Pos2::new(x, y)));
		v.extend(verts_a.into_iter().map(|(y, x)| Pos2::new(x, y)));
		v.push(Pos2::new(0.0707, 0.0));
		v.extend(verts_a.into_iter().rev().map(|(y, x)| Pos2::new(x, -y)));
		v.extend(verts_b.into_iter().map(|(y, x)| Pos2::new(x, -y)));

		v.iter_mut().for_each(|e| {
			*e = pos
				+ dir.rotate_vec2(Vec2::new(e.x * 32.0, e.y * 32.0) / 0.866 - Vec2::new(16.0, 0.0))
		});

		use eframe::egui::paint::{TextureId, Vertex, WHITE_UV};
		let mesh = Mesh {
			indices: (1..v.len() - 1)
				.flat_map(|i| [i, i + 1, 0])
				.map(|v| v.try_into().unwrap())
				.collect(),
			vertices: v
				.iter()
				.map(|&pos| Vertex { pos, uv: WHITE_UV, color: Color32::WHITE })
				.collect(),
			texture_id: TextureId::Egui,
		};

		v.push(v[0]);
		painter.add(Shape::Mesh(mesh));
		painter.add(Shape::line(v, stroke));
	}
}

impl_cc!(XorGate, IN, OUT, ((-1, -1), (1, 1)));

#[typetag::serde]
impl ComponentPlacer for XorGate {
	fn name(&self) -> Box<str> {
		"xor".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], _: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);

		let mut v = Vec::new();

		let verts_a = [
			(0.5, 0.0),
			(0.4, 0.0258),
			(0.3, 0.0456),
			(0.2, 0.0596),
			(0.1, 0.0680),
		];
		let verts_b = [
			(0.495, 0.1),
			(0.4798, 0.2),
			(0.4539, 0.3),
			(0.4165, 0.4),
			(0.366, 0.5),
			(0.3, 0.6),
			(0.2141, 0.7),
			(0.1, 0.8),
		];
		v.push(Pos2::new(0.866, 0.0));
		v.extend(verts_b.into_iter().rev().map(|(y, x)| Pos2::new(x, y)));
		v.extend(verts_a.into_iter().map(|(y, x)| Pos2::new(x, y)));
		v.push(Pos2::new(0.0707, 0.0));
		v.extend(verts_a.into_iter().rev().map(|(y, x)| Pos2::new(x, -y)));
		v.extend(verts_b.into_iter().map(|(y, x)| Pos2::new(x, -y)));

		v.iter_mut().for_each(|e| {
			*e = pos
				+ dir.rotate_vec2(Vec2::new(e.x * 32.0, e.y * 32.0) / 0.866 - Vec2::new(16.0, 0.0))
		});

		use eframe::egui::paint::{TextureId, Vertex, WHITE_UV};
		let mesh = Mesh {
			indices: (1..v.len() - 1)
				.flat_map(|i| [i, i + 1, 0])
				.map(|v| v.try_into().unwrap())
				.collect(),
			vertices: v
				.iter()
				.map(|&pos| Vertex { pos, uv: WHITE_UV, color: Color32::WHITE })
				.collect(),
			texture_id: TextureId::Egui,
		};

		v.push(v[0]);
		painter.add(Shape::Mesh(mesh));
		painter.add(Shape::line(v, stroke));
		let v = verts_a
			.into_iter()
			.map(|(y, x)| (-y, x))
			.chain([(0.0, 0.0707)])
			.chain(verts_a.into_iter().rev())
			.map(|(y, x)| {
				pos + dir.rotate_vec2(
					Vec2::new(x * 32.0 - 6.0 * 0.866, y * 32.0) / 0.866 - Vec2::new(16.0, 0.0),
				)
			})
			.collect();
		painter.add(Shape::line(v, stroke));
	}
}

#[typetag::serde]
impl CircuitComponent for NotGate {
	fn input_points(&self) -> Box<[PointOffset]> {
		IN_NOT.into()
	}

	fn output_points(&self) -> Box<[PointOffset]> {
		OUT.into()
	}

	fn aabb(&self, dir: Direction) -> RelativeAabb {
		dir * RelativeAabb::new(PointOffset::new(-1, 0), PointOffset::new(1, 0))
	}
}

#[typetag::serde]
impl ComponentPlacer for NotGate {
	fn name(&self) -> Box<str> {
		"not".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], _: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);

		let mut v = Vec::new();

		let verts = [(-16.0, 7.0), (-16.0, -7.0), (10.0, 0.0)];
		v.extend(verts.into_iter().rev().map(|(x, y)| Pos2::new(x, y)));

		v.iter_mut()
			.for_each(|e| *e = pos + dir.rotate_vec2(e.to_vec2()));
		painter.add(Shape::convex_polygon(v, Color32::WHITE, stroke));
		painter.add(Shape::Circle(CircleShape {
			center: pos + dir.rotate_vec2(Vec2::new(11.0, 0.0)),
			radius: 4.0,
			fill: Color32::WHITE,
			stroke,
		}));
	}
}

#[typetag::serde]
impl CircuitComponent for In {
	fn input_points(&self) -> Box<[PointOffset]> {
		[].into()
	}

	fn output_points(&self) -> Box<[PointOffset]> {
		CENTER.into()
	}

	fn external_input(&self) -> Option<usize> {
		Some(self.index)
	}

	fn aabb(&self, dir: Direction) -> RelativeAabb {
		aabb_in_out(self.bits, dir)
	}
}

#[typetag::serde]
impl ComponentPlacer for In {
	fn name(&self) -> Box<str> {
		"in".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, inputs: &[usize], _: &[usize]) {
		draw_in_out(
			painter,
			pos,
			dir,
			inputs.get(self.index).copied(),
			self.bits.get(),
			0.0,
		)
	}
}

#[typetag::serde]
impl CircuitComponent for Out {
	fn input_points(&self) -> Box<[PointOffset]> {
		CENTER.into()
	}

	fn output_points(&self) -> Box<[PointOffset]> {
		[].into()
	}

	fn external_output(&self) -> Option<usize> {
		Some(self.index)
	}

	fn aabb(&self, dir: Direction) -> RelativeAabb {
		aabb_in_out(self.bits, dir)
	}
}

#[typetag::serde]
impl ComponentPlacer for Out {
	fn name(&self) -> Box<str> {
		"out".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], outputs: &[usize]) {
		draw_in_out(
			painter,
			pos,
			dir,
			outputs.get(self.index).copied(),
			self.bits.get(),
			8.0,
		)
	}
}

fn aabb_in_out(bits: NonZeroU8, dir: Direction) -> RelativeAabb {
	let w = (bits.get() > 1).then(|| 4).unwrap_or(1);
	let h = match dir {
		Direction::Left | Direction::Right => [0, 1, 1, 2][usize::from(bits.get() - 1) / 8],
		Direction::Up | Direction::Down => [1, 2, 3, 4][usize::from(bits.get() - 1) / 8],
	};
	let ((ax, ay), (bx, by)) = match dir {
		Direction::Right => ((0, -h), (w, h)),
		Direction::Left => ((-w, -h), (0, h)),
		Direction::Up => ((-w / 2, -h), (w / 2, 0)),
		Direction::Down => ((-w / 2, 0), (w / 2, h)),
	};
	RelativeAabb::new(PointOffset::new(ax, ay), PointOffset::new(bx, by))
}

fn draw_in_out(
	painter: &Painter,
	pos: Pos2,
	dir: Direction,
	value: Option<usize>,
	bits: u8,
	corner_radius: f32,
) {
	let stroke = Stroke::new(3.0, Color32::BLACK);
	if bits == 1 {
		let rect = Rect::from_center_size(pos, Vec2::new(16.0, 16.0))
			.translate(dir.rotate_vec2(Vec2::new(8.0, 0.0)));
		let fill = value
			.map(|i| [Color32::DARK_GREEN, Color32::GREEN][i & 1])
			.unwrap_or(Color32::BLUE);
		painter.add(Shape::Rect(RectShape { corner_radius, fill, rect, stroke }));
	} else {
		let mut s = String::new();
		for i in 0..bits {
			(i != 0 && i % 8 == 0).then(|| s.push('\n'));
			s.push(value.map(|n| ['0', '1'][(n >> i) & 1]).unwrap_or('x'));
		}
		(bits > 8).then(|| s.extend((0..(64 - bits) % 8).map(|_| ' ')));
		let x = 16.0 * 4.0;
		let y = 16.0 * [1.0, 2.0, 3.0, 4.0][usize::from(bits - 1) >> 3];
		let mut offt = dir.rotate_vec2(Vec2::new(8.0 * 4.0, 0.0));
		let mut rect = Rect::from_center_size(pos, Vec2::new(x, y));
		match dir {
			Direction::Left | Direction::Right => (),
			Direction::Up => offt.y = -y / 2.0,
			Direction::Down => offt.y = y / 2.0,
		};
		rect = rect.translate(offt);
		painter.add(Shape::Rect(RectShape {
			corner_radius,
			fill: Color32::DARK_GRAY,
			rect,
			stroke,
		}));
		painter.text(
			pos + offt,
			Align2::CENTER_CENTER,
			s.chars().rev().collect::<String>(),
			TextStyle::Monospace,
			Color32::WHITE,
		);
	}
}

#[typetag::serde]
impl CircuitComponent for Splitter {
	fn input_points(&self) -> Box<[PointOffset]> {
		IN_NOT.into()
	}

	fn output_points(&self) -> Box<[PointOffset]> {
		(0..self.outputs().len().try_into().unwrap())
			.map(|y| PointOffset::new(1, y))
			.collect()
	}

	fn aabb(&self, dir: Direction) -> RelativeAabb {
		aabb_merger_splitter(&*self.input_points(), &*self.output_points(), dir)
	}
}

#[typetag::serde]
impl ComponentPlacer for Splitter {
	fn name(&self) -> Box<str> {
		"splitter".into()
	}

	fn draw(
		&self,
		painter: &Painter,
		pos: Pos2,
		dir: Direction,
		inputs: &[usize],
		outputs: &[usize],
	) {
		draw_merger_splitter(
			painter,
			pos,
			dir,
			inputs,
			outputs,
			&*self.input_points(),
			&*self.output_points(),
		)
	}
}

#[typetag::serde]
impl CircuitComponent for Merger {
	fn input_points(&self) -> Box<[PointOffset]> {
		(0..self.inputs().len().try_into().unwrap())
			.map(|y| PointOffset::new(-1, y))
			.collect()
	}

	fn output_points(&self) -> Box<[PointOffset]> {
		OUT.into()
	}

	fn aabb(&self, dir: Direction) -> RelativeAabb {
		aabb_merger_splitter(&*self.input_points(), &*self.output_points(), dir)
	}
}

#[typetag::serde]
impl ComponentPlacer for Merger {
	fn name(&self) -> Box<str> {
		"merger".into()
	}

	fn draw(
		&self,
		painter: &Painter,
		pos: Pos2,
		dir: Direction,
		inputs: &[usize],
		outputs: &[usize],
	) {
		draw_merger_splitter(
			painter,
			pos,
			dir,
			inputs,
			outputs,
			&*self.input_points(),
			&*self.output_points(),
		)
	}
}

fn aabb_merger_splitter(
	inputs: &[PointOffset],
	outputs: &[PointOffset],
	dir: Direction,
) -> RelativeAabb {
	let mut aabb = RelativeAabb::new(inputs[0], outputs[0]);
	inputs.iter().for_each(|&o| aabb = aabb.expand(o));
	outputs.iter().for_each(|&o| aabb = aabb.expand(o));
	dir * aabb
}

fn draw_merger_splitter(
	painter: &Painter,
	pos: Pos2,
	dir: Direction,
	_inputs: &[usize],
	_outputs: &[usize],
	in_pos: &[PointOffset],
	out_pos: &[PointOffset],
) {
	let stroke = Stroke::new(3.0, Color32::WHITE);

	let aabb = aabb_merger_splitter(in_pos, out_pos, Direction::Right);

	let (top, btm) = (
		dir * PointOffset::new(0, aabb.max.y),
		dir * PointOffset::new(0, aabb.min.y),
	);
	let top = Vec2::new(f32::from(top.x) * 16.0, f32::from(top.y) * 16.0);
	let btm = Vec2::new(f32::from(btm.x) * 16.0, f32::from(btm.y) * 16.0);
	painter.line_segment([pos + top, pos + btm], stroke);

	for (dx, p) in out_pos
		.iter()
		.map(|p| (0.0, p))
		.chain(in_pos.iter().map(|p| (1.0, p)))
	{
		let y = f32::from(p.y);
		let (a, b) = (Vec2::new(0.0 - dx, y) * 16.0, Vec2::new(1.0 - dx, y) * 16.0);
		painter.line_segment([pos + dir * a, pos + dir * b], stroke);
	}
}

#[typetag::serde]
impl CircuitComponent for Constant {
	fn input_points(&self) -> Box<[PointOffset]> {
		[].into()
	}

	fn output_points(&self) -> Box<[PointOffset]> {
		CENTER.into()
	}

	fn aabb(&self, dir: Direction) -> RelativeAabb {
		dir * RelativeAabb::new(PointOffset::new(0, 0), PointOffset::new(0, 0))
	}
}

#[typetag::serde]
impl ComponentPlacer for Constant {
	fn name(&self) -> Box<str> {
		"constant".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, _: Direction, _: &[usize], _: &[usize]) {
		let pad = (self.bits.get() + 3) / 4;
		painter.text(
			pos,
			Align2::CENTER_CENTER,
			format!("{:01$x}", self.value, pad.into()),
			TextStyle::Monospace,
			Color32::LIGHT_GRAY,
		);
	}
}

#[typetag::serde]
impl CircuitComponent for ReadOnlyMemory {
	fn input_points(&self) -> Box<[PointOffset]> {
		[PointOffset::new(-4, 0)].into()
	}

	fn output_points(&self) -> Box<[PointOffset]> {
		[PointOffset::new(4, 0)].into()
	}

	fn aabb(&self, dir: Direction) -> RelativeAabb {
		dir * RelativeAabb::new(PointOffset::new(-4, -3), PointOffset::new(4, 3))
	}
}

#[typetag::serde]
impl ComponentPlacer for ReadOnlyMemory {
	fn name(&self) -> Box<str> {
		"rom".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, inputs: &[usize], _: &[usize]) {
		let RelativeAabb { min, max } = self.aabb(dir);
		let min = Vec2::new(f32::from(min.x) * 16.0, f32::from(min.y) * 16.0);
		let max = Vec2::new(f32::from(max.x) * 16.0, f32::from(max.y) * 16.0);
		let (min, max) = (pos + min, pos + max);

		let rect = Rect::from_min_max(min, max);
		let stroke = Stroke::new(3.0, Color32::BLACK);
		painter.add(RectShape { rect, corner_radius: 0.0, fill: Color32::WHITE, stroke });
		let rect = rect.shrink(16.0);
		let stroke = Stroke::new(0.0, Color32::BLACK);
		painter.add(RectShape { rect, corner_radius: 0.0, fill: Color32::DARK_GRAY, stroke });

		for d in -2..=2i8 {
			let v = inputs
				.get(0)
				.and_then(|i| {
					if d > 0 {
						i.checked_add(d as usize)
					} else {
						i.checked_sub(-d as usize)
					}
				})
				.map(|i| format!("{:03x} {:08x}", i, self.get(i).unwrap_or(0)))
				.unwrap_or_else(|| "xxx xxxxxxxx".into());
			painter.text(
				pos + Vec2::new(0.0, f32::from(d) * 12.0),
				Align2::CENTER_CENTER,
				v,
				TextStyle::Monospace,
				Color32::WHITE,
			);
		}
	}
}
