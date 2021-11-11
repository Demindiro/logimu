use super::ComponentPlacer;
use crate::circuit::{CircuitComponent, Direction, PointOffset, RelativeAabb};
use crate::simulator;
use crate::simulator::*;
use core::num::NonZeroU8;
use eframe::egui::paint::{CircleShape, Mesh, RectShape, Vertex};
use eframe::egui::{Align2, Color32, Painter, Pos2, Rect, Shape, Stroke, TextStyle, Vec2};

const IN: &[PointOffset] = &[PointOffset::new(-1, -1), PointOffset::new(-1, 1)];
const IN_NOT: &[PointOffset] = &[PointOffset::new(-1, 0)];
const OUT: &[PointOffset] = &[PointOffset::new(1, 0)];
const CENTER: &[PointOffset] = &[PointOffset::new(0, 0)];

macro_rules! impl_cc {
	($name:ident, $in:expr, $out:expr, (($min_x:literal, $min_y:literal), ($max_x:literal, $max_y:literal))) => {
		#[typetag::serde]
		impl CircuitComponent for $name {
			fn inputs(&self) -> Box<[PointOffset]> {
				let i = i16::from(self.inputs.get());
				(-i / 2..0)
					.chain((i % 2 != 0).then(|| 0))
					.chain(1..=i / 2)
					.map(|y| PointOffset::new(-1, y.try_into().unwrap()))
					.collect()
			}

			fn outputs(&self) -> Box<[PointOffset]> {
				$out.into()
			}

			fn aabb(&self) -> RelativeAabb {
				RelativeAabb::new(
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
	fn inputs(&self) -> Box<[PointOffset]> {
		IN_NOT.into()
	}

	fn outputs(&self) -> Box<[PointOffset]> {
		OUT.into()
	}

	fn aabb(&self) -> RelativeAabb {
		RelativeAabb::new(PointOffset::new(-1, 0), PointOffset::new(1, 0))
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
	fn inputs(&self) -> Box<[PointOffset]> {
		[].into()
	}

	fn outputs(&self) -> Box<[PointOffset]> {
		CENTER.into()
	}

	fn external_input(&self) -> Option<usize> {
		Some(self.index)
	}

	fn aabb(&self) -> RelativeAabb {
		RelativeAabb::new(PointOffset::new(0, 0), PointOffset::new(1, 0))
	}
}

#[typetag::serde]
impl ComponentPlacer for In {
	fn name(&self) -> Box<str> {
		"in".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, inputs: &[usize], _: &[usize]) {
		let bits = self.bits.get();
		if bits == 1 {
			let stroke = Stroke::new(3.0, Color32::BLACK);
			let rect = Rect::from_center_size(pos, Vec2::new(16.0, 16.0))
				.translate(dir.rotate_vec2(Vec2::new(8.0, 0.0)));
			let fill = inputs
				.get(self.index)
				.map(|i| [Color32::DARK_GREEN, Color32::GREEN][*i & 1])
				.unwrap_or(Color32::BLUE);
			painter.add(Shape::Rect(RectShape {
				corner_radius: 0.0,
				fill,
				rect,
				stroke,
			}));
		} else {
			let mask = 1usize.wrapping_shl(bits.into()).wrapping_sub(1);
			let text = inputs
				.get(self.index)
				.map(|i| format!("{:01$b}", *i & mask, bits.into()))
				.unwrap_or_else(|| "x".repeat(bits.into()));
			painter.text(
				pos,
				Align2::RIGHT_CENTER,
				text,
				TextStyle::Monospace,
				Color32::WHITE,
			);
		}
	}
}

#[typetag::serde]
impl CircuitComponent for Out {
	fn inputs(&self) -> Box<[PointOffset]> {
		CENTER.into()
	}

	fn outputs(&self) -> Box<[PointOffset]> {
		[].into()
	}

	fn external_output(&self) -> Option<usize> {
		Some(self.index)
	}

	fn aabb(&self) -> RelativeAabb {
		RelativeAabb::new(PointOffset::new(0, 0), PointOffset::new(1, 0))
	}
}

#[typetag::serde]
impl ComponentPlacer for Out {
	fn name(&self) -> Box<str> {
		"out".into()
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], outputs: &[usize]) {
		let bits = self.bits.get();
		if bits == 1 {
			let stroke = Stroke::new(3.0, Color32::BLACK);
			let center = pos + dir.rotate_vec2(Vec2::new(8.0, 0.0));
			let fill = outputs
				.get(self.index)
				.map(|i| [Color32::DARK_GREEN, Color32::GREEN][*i & 1])
				.unwrap_or(Color32::BLUE);
			painter.add(Shape::Circle(CircleShape {
				center,
				radius: 8.0,
				fill,
				stroke,
			}));
		} else {
			let mask = 1usize.wrapping_shl(bits.into()).wrapping_sub(1);
			let text = outputs
				.get(self.index)
				.map(|i| format!("{:01$b}", *i & mask, bits.into()))
				.unwrap_or_else(|| "x".repeat(bits.into()));
			painter.text(
				pos,
				Align2::LEFT_CENTER,
				text,
				TextStyle::Monospace,
				Color32::WHITE,
			);
		}
	}
}

#[typetag::serde]
impl CircuitComponent for Splitter {
	fn inputs(&self) -> Box<[PointOffset]> {
		IN_NOT.into()
	}

	fn outputs(&self) -> Box<[PointOffset]> {
		(0..self.output_count().try_into().unwrap())
			.map(|y| PointOffset::new(1, y))
			.collect()
	}

	fn aabb(&self) -> RelativeAabb {
		aabb_merger_splitter(&*self.inputs(), &*self.outputs())
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
			&*self.inputs(),
			&*self.outputs(),
		)
	}
}

#[typetag::serde]
impl CircuitComponent for Merger {
	fn inputs(&self) -> Box<[PointOffset]> {
		(0..self.input_count().try_into().unwrap())
			.map(|y| PointOffset::new(-1, y))
			.collect()
	}

	fn outputs(&self) -> Box<[PointOffset]> {
		OUT.into()
	}

	fn aabb(&self) -> RelativeAabb {
		aabb_merger_splitter(&*self.inputs(), &*self.outputs())
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
			&*self.inputs(),
			&*self.outputs(),
		)
	}
}

fn aabb_merger_splitter(inputs: &[PointOffset], outputs: &[PointOffset]) -> RelativeAabb {
	let mut aabb = RelativeAabb::new(inputs[0], outputs[0]);
	inputs.iter().for_each(|&o| aabb = aabb.expand(o));
	outputs.iter().for_each(|&o| aabb = aabb.expand(o));
	aabb
}

fn draw_merger_splitter(
	painter: &Painter,
	pos: Pos2,
	dir: Direction,
	inputs: &[usize],
	outputs: &[usize],
	in_pos: &[PointOffset],
	out_pos: &[PointOffset],
) {
	let stroke = Stroke::new(3.0, Color32::WHITE);

	let aabb = aabb_merger_splitter(in_pos, out_pos);

	let (top, btm) = (
		PointOffset::new(0, aabb.max.y),
		PointOffset::new(0, aabb.min.y),
	);
	let (top, btm) = (dir * top, dir * btm);
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
		let (a, b) = (dir * a, dir * b);
		painter.line_segment([pos + a, pos + b], stroke);
	}
}

#[typetag::serde]
impl CircuitComponent for Constant {
	fn inputs(&self) -> Box<[PointOffset]> {
		[].into()
	}

	fn outputs(&self) -> Box<[PointOffset]> {
		CENTER.into()
	}

	fn aabb(&self) -> RelativeAabb {
		RelativeAabb::new(PointOffset::new(0, 0), PointOffset::new(0, 0))
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
