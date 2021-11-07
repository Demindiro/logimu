pub trait Dialog {
    fn name(&self) -> &str;

    #[must_use = "Return value indicates whether the dialog should be closed"]
    fn show(&mut self, ui: &mut eframe::egui::Ui) -> bool;
}
