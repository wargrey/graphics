#lang typed/racket/base

(require geofun/vector)
(require psd/bitmap)

(require racket/path)
(require digimon/debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer/ (collection-file-path "samples" "psd" "tamer"))
(define (psd-file? [path : Path-String]) (regexp-match? #px"[.]ps[bd]$" path))

(filter geo?
        (for/list : (Listof Any) ([file.psd (in-directory tamer/)] #:when (psd-file? file.psd))
          (with-handlers ([exn:fail? (λ [[e : exn]] (eprintf "~a~n" (exn-message e)))])
            (geo-vc-append #:gapsize 4.0
                           (geo-bitmap (time* (read-psd-bitmap file.psd #:try-@2x? #false)))
                           (geo-text (file-name-from-path file.psd))))))
