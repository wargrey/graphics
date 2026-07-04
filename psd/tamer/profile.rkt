#lang racket/base

(require psd/base)
(require psd/profile)
(require psd/layer)

(require racket/path)
(require racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-runtime-path tamer/ "samples")
(define (psd? path) (regexp-match? #px"[.]psd$" path))

(filter list?
        (for/list ([file.psd (in-directory tamer/)] #:when (psd? file.psd))
          (with-handlers ([exn:fail? (λ [e] (eprintf "~a~n" (exn-message e)))])
            (define tamer.psd (read-psd file.psd #:try-@2x? #false))
            
            (psd-profile tamer.psd)
            
            (list (path->string (file-name-from-path file.psd))
                  (psd-layers tamer.psd)
                  (psd-global-mask tamer.psd)
                  tamer.psd
                  (psd-image-resources tamer.psd)))))
