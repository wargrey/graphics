#lang racket/base

(require geofun/vector)
(require psd/bitmap)

(require racket/path)
(require racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-runtime-path tamer/ "samples")
(define (psd? path) (regexp-match? #px"[.]ps[bd]$" path))

(filter geo?
        (for/list ([file.psd (in-directory tamer/)] #:when (psd? file.psd))
          (with-handlers ([exn:fail? (λ [e] (eprintf "~a~n" (exn-message e)))])
            (geo-vc-append #:gapsize 4.0
                           (geo-bitmap (read-psd-bitmap file.psd #:try-@2x? #false))
                           (geo-text (file-name-from-path file.psd))))))
