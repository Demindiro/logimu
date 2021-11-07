#![feature(new_uninit)]

mod arena;
mod circuit;
mod gui;
mod simulator;

#[macro_export]
macro_rules! impl_dyn {
	{
		$trait:ident for $ty:ty {
			$($fn:ident($($args:ident: $aty:ty),*$(,)?) -> $ret:ty;)*
		}
	} => {
		impl $trait for $ty {
			$(
				fn $fn(&self, $($args: $aty,)*) -> $ret {
					(**self).$fn($($args,)*)
				}
			)*
		}
	};
}

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
