# Rust Channel for Guix

Features in addition to those provided by the base GNU Guix rust distribution:

- Rust 1.64
- Wasm targets (Don't work as expected at present as they use the wrong linker, will have to re-write the config.toml when I have the time)
    - wasm32-unknown-unknown
- Profiler
- Sanitizers
- Clippy
- Miri
- Rust-demangler
- Llvm-tools bundled with the source code, as a package output
- rust-lld
- Nightly tracking the latest stable release, to use the -Z flags
- Built using the latest llvm-14 and gcc-12

## Manifest
    (specifications->manifest
      (list
            "rust-nightly"
            "rust-nightly:cargo"
            "rust-nightly:rustfmt"
            "rust-nightly:clippy"
            "rust-nightly:miri"
            "rust-nightly:demangler"
            "rust-nightly:llvm-tools"
            "rust-nightly-src"))

## Channel configuration
    (channel
        (name 'rust)
        (url "https://github.com/paulalesius/guix-rust")
        (branch "main")
        (introduction
            (make-channel-introduction
                "1fc51a49b05763f9097ed3a8c77ef4156d89e843"
            (openpgp-fingerprint
                "4FBD 7445 5FDD 4E6E 75B1 D91B FAB8 710F BCD2 0E1C"))))
