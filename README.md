# Logimu: Circuit & logic gate simulator

The main goal of Logimu is to simulate large circuits at a high speed.

## Features

- Live simulation
- Embed other circuits in the form of ICs
- Embedded tests in a LISP dialect

## Building

To build Logimu, you will need a **nightly** Rust compiler. Follow the
instructions [here](https://www.rust-lang.org/learn/get-started). Then,
run this:

```
rustup toolchain add nightly
```

Then go the the directory and build with

```
cargo +nightly build --release
```

You can then run the project with `cargo +nightly run --release` or by
running the binary in `target/` directly.

## Screenshot

![GUI example](https://static.salt-inc.org/logimu/gui_2.png)
