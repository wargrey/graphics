#lang racket/base

(require psd/bitmap)

(require racket/list)
(require racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-runtime-path tamer/ "samples")
(define (psd? path) (regexp-match? #px"[.]ps[bd]$" path))

(filter-not void?
            (for/list ([file.psd (in-directory tamer/)] #:when (psd? file.psd))
              (with-handlers ([exn:fail? (λ [e] (eprintf "~a~n" (exn-message e)))])
                (read-psd-bitmap file.psd #:try-@2x? #false))))
