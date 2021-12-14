use core::fmt;
use std::collections::VecDeque;

use eframe::egui::{self, Color32};

#[derive(Default)]
pub struct Log {
	pub open: bool,
	entries: VecDeque<(Tag, Box<str>)>,
}

impl Log {
	const MAX_ENTRIES: usize = 1024;

	pub fn show(&mut self, ctx: &egui::CtxRef) {
		if !self.open {
			return;
		}
		let mut open = self.open;
		egui::Window::new("Log").open(&mut open).show(ctx, |ui| {
			egui::ScrollArea::vertical()
				.stick_to_bottom()
				.max_width(f32::INFINITY)
				.show(ui, |ui| {
					for (t, m) in self.entries.iter() {
						ui.add(egui::Label::new(m).monospace().text_color(t.color()));
					}
				});
		});
		self.open = open;
	}

	pub fn push(&mut self, tag: Tag, entry: impl Into<Box<str>>) {
		(self.entries.len() >= Self::MAX_ENTRIES).then(|| self.entries.pop_front());
		let entry = entry.into();
		println!("{}{}", tag.prefix(), &entry);
		self.entries.push_back((tag, entry));
	}

	pub fn debug(&mut self, message: impl Into<Box<str>>) {
		self.push(Tag::Debug, message);
	}

	pub fn error(&mut self, message: impl Into<Box<str>>) {
		self.push(Tag::Error, message);
		self.open = true;
	}

	pub fn success(&mut self, message: impl Into<Box<str>>) {
		self.push(Tag::Success, message);
		self.open = true;
	}
}

impl fmt::Display for Log {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for (t, m) in self.entries.iter() {
			f.write_str(t.prefix())?;
			f.write_str(m)?;
			f.write_str("\n")?;
		}
		Ok(())
	}
}

pub enum Tag {
	Success,
	Error,
	Debug,
}

impl Tag {
	fn color(&self) -> Color32 {
		match self {
			Self::Success => Color32::LIGHT_GREEN,
			Self::Error => Color32::RED,
			Self::Debug => Color32::GRAY,
		}
	}

	fn prefix(&self) -> &str {
		match self {
			Self::Success => "[success] ",
			Self::Error => "[error]   ",
			Self::Debug => "[debug]   ",
		}
	}
}
