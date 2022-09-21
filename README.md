# Rust Channel for Guix

Features in addition to those provided by the base GNU Guix rust distribution:

- Rust 1.63
- Profiler
- Sanitizers
- Clippy
- Miri
- Rust-demangler
- Nightly enabled for the stable release, to use the -Z flags
- Built using the latest llvm-14 and gcc-12

These improvements are only applied to Rust version 1.63 and later.

## Manifest
    (specifications->manifest
      (list
            "rust-nightly"
            "rust-nightly:cargo"
            "rust-nightly:rustfmt"
            "rust-nightly:clippy"
            "rust-nightly:miri"
            "rust-nightly:demangler"
            "rust-nightly-src"))

## Channel configuration
    (channel
        (name 'rust)
        (url "https://github.com/paulalesius/guix-rust")
        (branch "main")
        (introduction
            (make-channel-introduction
                "7e35a1889ea8dc093b316a0ed4ff2caae9a767c7"
            (openpgp-fingerprint
                "4FBD 7445 5FDD 4E6E 75B1 D91B FAB8 710F BCD2 0E1C"))))
