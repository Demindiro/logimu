build: linux windows macos

linux:
	cargo b --release --target=x86_64-unknown-linux-gnu

windows:
	cargo b --release --target=x86_64-pc-windows-gnu

macos:
	cargo b --release --target=x86_64-apple-darwin
