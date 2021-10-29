#![feature(trait_upcasting)]

mod circuit;
mod gui;
mod simulator;

fn main() {
	eframe::run_native(Box::new(gui::App::new()), eframe::NativeOptions {
		always_on_top: false,
		decorated: true,
		drag_and_drop_support: false,
		icon_data: None,
		initial_window_size: None,
		maximized: false,
		resizable: true,
		transparent: false,
	})
}
