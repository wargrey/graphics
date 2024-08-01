#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-target-path : (-> Path-String Symbol String Path)
  (lambda [src.rkt symid .ext]
    (build-path (or (path-only src.rkt) (current-directory))
                (graphics-subpath #false symid .ext))))

(define graphics-target-path/compiled : (-> Path-String Symbol String Path)
  (lambda [src.rkt symid .ext]
    (build-path (or (path-only src.rkt) (current-directory))
                (car (use-compiled-file-paths)) "graphics"
                (graphics-subpath (path-replace-extension (assert (file-name-from-path src.rkt) path?) #"")
                                  symid .ext))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-subpath : (-> (Option Path-String) Symbol String Path)
  (lambda [subroot symid .ext]
    (define self : String (symbol->immutable-string symid))
    
    (path-add-extension (cond [(not subroot) self]
                              [else (build-path subroot self)])
                        .ext)))
