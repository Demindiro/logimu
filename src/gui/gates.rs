use super::ComponentPlacer;
use crate::simulator;
use crate::circuit::{Direction, CircuitComponent, PointOffset};
use core::num::NonZeroU8;
use eframe::egui::{Painter, Pos2, Stroke, Color32, Rect, Vec2, Shape};
use eframe::egui::paint::{CircleShape, Mesh, Vertex};

pub struct AndGate;

impl CircuitComponent for AndGate {
	fn id(&self) -> usize {
		0
	}

	fn inputs(&self) -> &[PointOffset] {
		const I: [PointOffset; 2] = [PointOffset::new(-1, -1), PointOffset::new(-1, 1)];
		&I
	}

	fn outputs(&self) -> &[PointOffset] {
		const O: [PointOffset; 1] = [PointOffset::new(1, 0)];
		&O
	}
}

impl ComponentPlacer for AndGate {
	fn name(&self) -> &str {
		"and"
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction) {
		let stroke = Stroke::new(3.0, Color32::BLACK);
		let radius = 24.0;

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

	fn instance(&self) -> Box<dyn simulator::Component> {
		Box::new(simulator::AndGate::new(simulator::NonZeroOneU8::new(2).unwrap(), NonZeroU8::new(1).unwrap()))
	}
}

pub struct OrGate;

impl CircuitComponent for OrGate {
	fn id(&self) -> usize {
		1
	}

	fn inputs(&self) -> &[PointOffset] {
		const I: [PointOffset; 2] = [PointOffset::new(0, -1), PointOffset::new(0, 1)];
		&I
	}

	fn outputs(&self) -> &[PointOffset] {
		const O: [PointOffset; 1] = [PointOffset::new(2, 0)];
		&O
	}
}

impl ComponentPlacer for OrGate {
	fn name(&self) -> &str {
		"or"
	}

	fn draw(&self, painter: &Painter, pos: Pos2, dir: Direction) {
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

		v.iter_mut().for_each(|e| *e = pos + dir.rotate_vec2(Vec2::new(e.x * 32.0, e.y * 32.0) / 0.866));

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

	fn instance(&self) -> Box<dyn simulator::Component> {
		Box::new(simulator::OrGate::new(simulator::NonZeroOneU8::new(2).unwrap(), NonZeroU8::new(1).unwrap()))
	}
}
