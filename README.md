# Rust Channel for Guix

Contains various build improvements including:

- Rust 1.63
- Profilers
- Sanitizers
- Clippy as a new package output
- Nightly enabled for the stable release, to use the -Z flag
- Built using the latest llvm and gcc

These improvements are only applied to Rust version 1.63 and onward.

## Channel configuration
<code>
(channel
    (name 'rust)
    (url "https://github.com/paulalesius/guix-rust")
    (branch "main")
    (introduction
        (make-channel-introduction
            "7e35a1889ea8dc093b316a0ed4ff2caae9a767c7"
        (openpgp-fingerprint
            "4FBD 7445 5FDD 4E6E 75B1 D91B FAB8 710F BCD2 0E1C"))))
</code>
