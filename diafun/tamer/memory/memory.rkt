#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define swap-snapshots : Dia-RAM-Snapshots
  (parameterize ([default-ram-fixnum-radix 16]
                 [default-ram-address-mask #xFFFFFFFFFFFF]
                 [default-ram-padding-limit 0]
                 [default-ram-human-readable? #true])
    (dia-ram-snapshots #:body-limit 0 #:optimize? #false
                          (collection-file-path "memory.c" "diafun" "tamer" "memory"))))

(define swap.ram : Geo (dia-ram-snapshots->table swap-snapshots #:gapsize 16.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  swap.ram)
