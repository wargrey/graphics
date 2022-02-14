#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-target-path : (-> Path-String Symbol Bytes Path)
  (lambda [src.rkt symid .ext]
    (build-path (or (path-only src.rkt) (current-directory))
                (graphics-subpath src.rkt symid .ext))))

(define graphics-target-path/compiled : (-> Path-String Symbol Bytes Path)
  (lambda [src.rkt symid .ext]
    (build-path (or (path-only src.rkt) (current-directory))
                (car (use-compiled-file-paths)) "graphics"
                (graphics-subpath src.rkt symid .ext))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-subpath : (-> Path-String Symbol Bytes Path)
  (lambda [src.rkt symid .ext]
    (path-add-extension (build-path (assert (file-name-from-path src.rkt) path?)
                                    (symbol->immutable-string symid))
                        .ext)))
