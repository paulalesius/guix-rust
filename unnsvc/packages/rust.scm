(define-module (unnsvc packages rust)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages llvm))

(define rust-1.58
  (rust-bootstrapped-package
   rust-1.57 "1.58.1" "1iq7kj16qfpkx8gvw50d8rf7glbm6s0pj2y1qkrz7mi56vfsyfd8"))

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
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:tests? _ #f)
         #f)
         ((#:phases phases)
          `(modify-phases ,phases
            (delete 'check)))))
      (inputs (alist-replace "llvm" (list llvm-14)
                             (package-inputs base-rust))))))
