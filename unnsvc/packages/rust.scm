(define-module (unnsvc packages rust)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ccache)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-26))

(define* (nix-system->gnu-triplet-for-rust
          #:optional (system (%current-system)))
  (match system
    ("x86_64-linux"   "x86_64-unknown-linux-gnu")
    ("i686-linux"     "i686-unknown-linux-gnu")
    ("armhf-linux"    "armv7-unknown-linux-gnueabihf")
    ("aarch64-linux"  "aarch64-unknown-linux-gnu")
    ("mips64el-linux" "mips64el-unknown-linux-gnuabi64")
    (_                (nix-system->gnu-triplet system))))

(define* (rust-uri version #:key (dist "static"))
  (string-append "https://" dist ".rust-lang.org/dist/"
                 "rustc-" version "-src.tar.gz"))

(define* (rust-bootstrapped-package base-rust version checksum)
  "Bootstrap rust VERSION with source checksum CHECKSUM using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
     (origin
       (inherit (package-source base-rust))
       (uri (rust-uri version))
       (sha256 (base32 checksum))))
    (native-inputs
     (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust))))))

(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(define rust-1.58
  (rust-bootstrapped-package
   rust "1.58.1" "1iq7kj16qfpkx8gvw50d8rf7glbm6s0pj2y1qkrz7mi56vfsyfd8"))

(define rust-1.59
  (rust-bootstrapped-package
   rust-1.58 "1.59.0" "1yc5bwcbmbwyvpfq7zvra78l0r8y3lbv60kbr62fzz2vx2pfxj57"))

(define rust-1.60
  (rust-bootstrapped-package
   rust-1.59 "1.60.0" "1drqr0a26x1rb2w3kj0i6abhgbs3jx5qqkrcwbwdlx7n3inq5ji0"))

(define rust-1.61
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.60 "1.61.0"
          "1vfs05hkf9ilk19b2vahqn8l6k17pl9nc1ky9kgspaascx8l62xd")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:tests? _ #f)
          #f))))))

;; Latest needs to be public so it can be included in a profile, while just "rust" is
;; intended to build system artifacts.
(define-public rust-1.62
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.61 "1.62.1"
          "0gqkg34ic77dcvsz69qbdng6g3zfhl6hnhx7ha1mjkyrzipvxb3j")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:tests? _ #f)
          #f))))))

(define-public rust-1.63
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.62 "1.63.0"
          "1l4rrbzhxv88pnfq94nbyb9m6lfnjwixma3mwjkmvvs2aqlq158z")))
    (package
      (inherit base-rust)
      (name "rust-nightly")
      (outputs (cons*
                "clippy"
                "demangler"
                "miri"
                "llvm-tools"
                (package-outputs base-rust)))
      (source
       (origin
         (inherit (package-source base-rust))
         ;; Don't remove llvm-project as it's required to build profiler and sanitizers
         (snippet '(lambda _ 'nil))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:tests? _ #f) #f)
         ((#:validate-runpath? _ #f) #f)
         ((#:phases phases)
          `(modify-phases ,phases
            (delete 'check)
            (add-after 'configure 'set-nightly-config
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((gcc (assoc-ref inputs "gcc"))
                       (gnu-triplet ,(or (%current-target-system)
                                      (nix-system->gnu-triplet-for-rust))))
                  (substitute* "config.toml"
                    (("^submodules = .*" all)
                     (string-append all
                                 "extended = true\n"
                                 "tools = [\"cargo\", \"clippy\", \"rustfmt\", \"analysis\", \"src\", \"rust-demangler\"]\n"
                                 "profiler = true\n"
                                 "sanitizers = true\n"
                                 "verbose = 0\n"))
                    (("channel = \"stable\"")
                     "channel = \"nightly\"")
                    (("\\[llvm\\]")
                      (string-append
                                "[llvm]\n"
                                 "cxxflags = \"-I" gcc "/include/c++/" gnu-triplet "/\"\n"))))))
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (let ((job-spec (string-append
                                 "-j" (if parallel-build?
                                          (number->string (parallel-job-count))
                                          "1"))))
                  (invoke "./x.py" job-spec "build"))))
            (replace 'install
               ;; Phase overridden to also install clippy.
               (lambda* (#:key outputs #:allow-other-keys)
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "rustfmt"))))
                 (invoke "./x.py" "install" "rustfmt")
                 (substitute* "config.toml"
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "clippy"))))
                 (invoke "./x.py" "install" "clippy")
                 (substitute* "config.toml"
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "demangler"))))
                 (invoke "./x.py" "install" "rust-demangler")
                 (substitute* "config.toml"
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "miri"))))
                 (invoke "./x.py" "install" "miri")
                 ;; Just copy llvm-tools to install path
                 (let* ((llvm-tools (assoc-ref outputs "llvm-tools"))
                        (gnu-triplet ,(or (%current-target-system)
                                      (nix-system->gnu-triplet-for-rust))))
                   (copy-recursively
                    (string-append
                     "build/"
                     gnu-triplet
                     "/stage2/lib/rustlib/"
                     gnu-triplet)
                    llvm-tools))))
            (add-after 'wrap-rustc 'wrap-clippy-and-miri
              ;; Clippy also needs to be wrapped to find librustc_driver
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (clippy (assoc-ref outputs "clippy"))
                      (miri (assoc-ref outputs "miri")))
                  (wrap-program (string-append clippy "/bin/clippy-driver")
                    `("LD_LIBRARY_PATH" ":"
                      suffix (,(string-append out "/lib"))))
                  (wrap-program (string-append miri "/bin/miri")
                    `("LD_LIBRARY_PATH" ":"
                      suffix (,(string-append out "/lib")))))))))))
      (inputs (alist-replace "llvm" (list llvm-14)
                             (package-inputs base-rust)))
      (native-inputs (cons* `("gcc" ,gcc-12)
                            `("ninja" ,ninja)
                            (package-native-inputs base-rust))))))

(define-public rust-1.64
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.63 "1.64.0"
          "018j720b2n12slp4xk64jc6shkncd46d621qdyzh2a8s3r49zkdk")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:tests? _ #f)
          #f)
         ((#:phases phases)
          `(modify-phases ,phases
            ;; Lockfile checksums are now verified for the bootstrap, fix them to the
            ;; reference hash used for vendored dependencies in Guix.
            (add-after 'patch-generated-file-shebangs 'set-bootstrap-no-locked-deps
              (lambda* _
                (substitute* "src/bootstrap/Cargo.lock"
                  (("(checksum = )\".*\"" all name)
                   (string-append name "\"" ,%cargo-reference-hash "\"")))
                (substitute* "src/tools/rust-analyzer/Cargo.lock"
                  (("(checksum = )\".*\"" all name)
                   (string-append name "\"" ,%cargo-reference-hash "\""))))))))))))

(define-public rust-nightly-src
   (package
     (inherit rust-1.64)
     (name "rust-nightly-src")
     (build-system copy-build-system)
     (native-inputs '())
     (inputs '())
     (native-search-paths '())
     (outputs '("out"))
     (arguments
      `(#:validate-runpath? #f
        #:install-plan
        '(("library" "lib/rustlib/src/rust/library")
          ("src" "lib/rustlib/src/rust/src"))))
     (synopsis "Source code for the Rust standard library")
     (description "This package provide source code for the Rust standard
library, only use by rust-analyzer, make rust-analyzer out of the box.")))
