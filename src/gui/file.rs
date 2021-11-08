use std::fs;

pub struct OpenDialog {}

impl super::Dialog for OpenDialog {
	fn name(&self) -> &str {
		"Open file"
	}

	fn show(&mut self, ui: &mut eframe::egui::Ui) -> bool {
		let dir = fs::read_dir(".").unwrap();
		for e in dir {
			let e = e.unwrap();
			if ui.button(e.file_name().to_string_lossy()).clicked() {
				println!("Pressed {}", e.file_name().to_string_lossy());
				return true;
			}
		}
		false
	}
}
