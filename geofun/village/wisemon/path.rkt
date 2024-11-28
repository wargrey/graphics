#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-target-path : (-> Path-String Symbol String (Option Integer) Path)
  (lambda [src.rkt symid .ext subidx]
    (build-path (or (path-only src.rkt) (current-directory))
                (graphics-subpath #false symid .ext subidx))))

(define graphics-target-path/compiled : (-> Path-String Symbol String (Option Integer) Path)
  (lambda [src.rkt symid .ext subidx]
    (build-path (or (path-only src.rkt) (current-directory))
                (car (use-compiled-file-paths)) "graphics"
                (graphics-subpath (path-replace-extension (assert (file-name-from-path src.rkt) path?) #"")
                                  symid .ext subidx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-subpath : (-> (Option Path-String) Symbol String (Option Integer) Path)
  (lambda [subroot symid .ext subidx]
    (define self : Path-String
      (if (not subidx)
          (symbol->immutable-string symid)
          (build-path (symbol->immutable-string symid)
                      (number->string subidx))))
    
    (path-add-extension (cond [(not subroot) self]
                              [else (build-path subroot self)])
                        .ext)))
