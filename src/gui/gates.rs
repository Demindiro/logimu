use super::ComponentPlacer;
use crate::simulator;
use crate::simulator::{AndGate, OrGate, XorGate, NotGate, In, Out};
use crate::circuit::{Direction, CircuitComponent, PointOffset};
use core::num::NonZeroU8;
use eframe::egui::{Painter, Pos2, Stroke, Color32, Rect, Vec2, Shape};
use eframe::egui::paint::{CircleShape, RectShape, Mesh, Vertex};

const IN: &[PointOffset] = &[PointOffset::new(-1, -1), PointOffset::new(-1, 1)];
const IN_NOT: &[PointOffset] = &[PointOffset::new(-1, 0)];
const OUT: &[PointOffset] = &[PointOffset::new(1, 0)];
const CENTER: &[PointOffset] = &[PointOffset::new(0, 0)];

macro_rules! impl_cc {
	($name:ident, $in:expr, $out:expr) => {
		impl CircuitComponent for $name {
			fn inputs(&self) -> &[PointOffset] {
				$in
			}

			fn outputs(&self) -> &[PointOffset] {
				$out
			}
		}
	};
}

impl_cc!(AndGate, IN, OUT);

#[typetag::serde]
impl ComponentPlacer for AndGate {
	fn name(&self) -> &str {
		"and"
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], _: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);
		let radius = 16.0;

		painter.add(CircleShape {
			center: pos,
			radius,
			fill: Color32::WHITE,
			stroke,
		});
		let top_left    = pos + dir.rotate_vec2(Vec2::new(-radius, -radius));
		let top_right   = pos + dir.rotate_vec2(Vec2::new(    0.0, -radius));
		let bottom_left = pos + dir.rotate_vec2(Vec2::new(-radius,  radius));
		painter.rect_filled(
			Rect::from_two_pos(top_right, bottom_left),
			0.0,
			Color32::WHITE,
		);
		let offt = dir.rotate_vec2(Vec2::new(radius, 0.0));
		painter.line_segment([top_left, bottom_left], stroke);
		painter.line_segment([top_left   , top_left    + offt], stroke);
		painter.line_segment([bottom_left, bottom_left + offt], stroke);
	}
}

impl_cc!(OrGate, IN, OUT);

#[typetag::serde]
impl ComponentPlacer for OrGate {
	fn name(&self) -> &str {
		"or"
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
			(0.495 , 0.1),
			(0.4798, 0.2),
			(0.4539, 0.3),
			(0.4165, 0.4),
			(0.366 , 0.5),
			(0.3   , 0.6),
			(0.2141, 0.7),
			(0.1   , 0.8),
		];
		v.push(Pos2::new(0.866, 0.0));
		v.extend(verts_b.into_iter().rev().map(|(y, x)| Pos2::new(x,  y)));
		v.extend(verts_a.into_iter().map(|(y, x)| Pos2::new(x,  y)));
		v.push(Pos2::new(0.0707, 0.0));
		v.extend(verts_a.into_iter().rev().map(|(y, x)| Pos2::new(x, -y)));
		v.extend(verts_b.into_iter().map(|(y, x)| Pos2::new(x, -y)));

		v.iter_mut().for_each(|e| *e = pos + dir.rotate_vec2(Vec2::new(e.x * 32.0, e.y * 32.0) / 0.866 - Vec2::new(16.0, 0.0)));

		use eframe::egui::paint::{Vertex, WHITE_UV, TextureId};
		let mesh = Mesh {
			indices: (1..v.len() - 1).flat_map(|i| [i, i + 1, 0]).map(|v| v.try_into().unwrap()).collect(),
			vertices: v.iter().map(|&pos| Vertex { pos, uv: WHITE_UV, color: Color32::WHITE }).collect(),
			texture_id: TextureId::Egui,
		};

		v.push(v[0]);
		painter.add(Shape::Mesh(mesh));
		painter.add(Shape::line(v, stroke));
	}
}

impl_cc!(XorGate, IN, OUT);

#[typetag::serde]
impl ComponentPlacer for XorGate {
	fn name(&self) -> &str {
		"xor"
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
			(0.495 , 0.1),
			(0.4798, 0.2),
			(0.4539, 0.3),
			(0.4165, 0.4),
			(0.366 , 0.5),
			(0.3   , 0.6),
			(0.2141, 0.7),
			(0.1   , 0.8),
		];
		v.push(Pos2::new(0.866, 0.0));
		v.extend(verts_b.into_iter().rev().map(|(y, x)| Pos2::new(x,  y)));
		v.extend(verts_a.into_iter().map(|(y, x)| Pos2::new(x,  y)));
		v.push(Pos2::new(0.0707, 0.0));
		v.extend(verts_a.into_iter().rev().map(|(y, x)| Pos2::new(x, -y)));
		v.extend(verts_b.into_iter().map(|(y, x)| Pos2::new(x, -y)));

		v.iter_mut().for_each(|e| *e = pos + dir.rotate_vec2(Vec2::new(e.x * 32.0, e.y * 32.0) / 0.866 - Vec2::new(16.0, 0.0)));

		use eframe::egui::paint::{Vertex, WHITE_UV, TextureId};
		let mesh = Mesh {
			indices: (1..v.len() - 1).flat_map(|i| [i, i + 1, 0]).map(|v| v.try_into().unwrap()).collect(),
			vertices: v.iter().map(|&pos| Vertex { pos, uv: WHITE_UV, color: Color32::WHITE }).collect(),
			texture_id: TextureId::Egui,
		};

		v.push(v[0]);
		painter.add(Shape::Mesh(mesh));
		painter.add(Shape::line(v, stroke));
		let v = verts_a
			.into_iter()
			.map(|(y, x)| (-y, x)).chain([(0.0, 0.0707)])
			.chain(verts_a.into_iter().rev())
			.map(|(y, x)| pos + dir.rotate_vec2(Vec2::new(x * 32.0 - 6.0 * 0.866, y * 32.0) / 0.866 - Vec2::new(16.0, 0.0)))
			.collect();
		painter.add(Shape::line(v, stroke));
	}
}

impl_cc!(NotGate, IN_NOT, OUT);

#[typetag::serde]
impl ComponentPlacer for NotGate {
	fn name(&self) -> &str {
		"not"
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], _: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);

		let mut v = Vec::new();

		let verts = [
			(-16.0, 7.0),
			(-16.0, -7.0),
			(10.0, 0.0),
		];
		v.extend(verts.into_iter().rev().map(|(x, y)| Pos2::new(x, y)));

		v.iter_mut().for_each(|e| *e = pos + dir.rotate_vec2(e.to_vec2()));
		painter.add(Shape::convex_polygon(v, Color32::WHITE, stroke));
		painter.add(Shape::Circle(CircleShape { center: pos + dir.rotate_vec2(Vec2::new(11.0, 0.0)), radius: 4.0, fill: Color32::WHITE, stroke }));
	}
}

impl CircuitComponent for In {
	fn inputs(&self) -> &[PointOffset] {
		&[]
	}

	fn outputs(&self) -> &[PointOffset] {
		CENTER
	}

	fn external_input(&self) -> Option<usize> {
		Some(self.index)
	}
}

#[typetag::serde]
impl ComponentPlacer for In {
	fn name(&self) -> &str {
		"in"
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, inputs: &[usize], _: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);
		let rect = Rect::from_center_size(pos, Vec2::new(16.0, 16.0))
			.translate(dir.rotate_vec2(Vec2::new(8.0, 0.0)));
		let fill = inputs.get(self.index).map(|i| [Color32::DARK_GREEN, Color32::GREEN][*i & 1])
			.unwrap_or(Color32::BLUE);
		painter.add(Shape::Rect(RectShape { corner_radius: 0.0, fill, rect, stroke }));
	}
}

impl CircuitComponent for Out {
	fn inputs(&self) -> &[PointOffset] {
		CENTER
	}

	fn outputs(&self) -> &[PointOffset] {
		&[]
	}

	fn external_output(&self) -> Option<usize> {
		Some(self.index)
	}
}

#[typetag::serde]
impl ComponentPlacer for Out {
	fn name(&self) -> &str {
		"out"
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction, _: &[usize], outputs: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);
		let center = pos + dir.rotate_vec2(Vec2::new(8.0, 0.0));
		let fill = outputs.get(self.index).map(|i| [Color32::DARK_GREEN, Color32::GREEN][*i & 1])
			.unwrap_or(Color32::BLUE);
		painter.add(Shape::Circle(CircleShape { center, radius: 8.0, fill, stroke }));
	}
}
