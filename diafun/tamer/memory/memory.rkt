#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define swap-snapshots : Dia-Memory-Snapshots
  (parameterize ([default-memory-fixnum-radix 16]
                 [default-memory-address-mask #xFFFFFFFFFFFF]
                 [default-memory-padding-limit 0]
                 [default-memory-human-readable? #true])
    (dia-memory-snapshots #:body-limit 0 #:optimize? #false
                          (collection-file-path "memory.c" "diafun" "tamer" "memory"))))

(define swap-memory : Geo (dia-memory-snapshots->table swap-snapshots #:gapsize 16.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  swap-memory)
