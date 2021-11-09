#![feature(binary_heap_into_iter_sorted)]
#![feature(new_uninit)]
#![feature(map_try_insert)]

mod arena;
mod circuit;
mod gui;
mod simulator;

#[macro_export]
macro_rules! impl_dyn {
	{
		$(#[$outer:meta])*
		$trait:ident for $ty:ty {
			$(ref $fn:ident($($args:ident: $aty:ty),*$(,)?) -> $ret:ty;)*
			$(mut $fn_mut:ident($($args_mut:ident: $aty_mut:ty),*$(,)?) -> $ret_mut:ty;)*
		}
	} => {
		$(#[$outer])*
		impl $trait for $ty {
			$(
				fn $fn(&self $(, $args: $aty)*) -> $ret {
					(**self).$fn($($args,)*)
				}
			)*
			$(
				fn $fn_mut(&mut self $(, $args_mut: $aty_mut)*) -> $ret_mut {
					(**self).$fn_mut($($args_mut,)*)
				}
			)*
		}
	};
}

fn main() {
	eframe::run_native(
		Box::new(gui::App::new()),
		eframe::NativeOptions {
			always_on_top: false,
			decorated: true,
			drag_and_drop_support: false,
			icon_data: None,
			initial_window_size: None,
			maximized: false,
			resizable: true,
			transparent: false,
		},
	)
}
