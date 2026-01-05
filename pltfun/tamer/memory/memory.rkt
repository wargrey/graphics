#lang typed/racket/base

(provide (all-defined-out))

(require pltfun/memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define swap-snapshots : Plt-RAM-Snapshots
  (parameterize ([default-ram-fixnum-radix 16]
                 [default-ram-address-mask #xFFFFFFFFFFFF]
                 [default-ram-padding-limit 0]
                 [default-ram-human-readable? #true])
    (plt-ram-snapshots #:body-limit 0 #:optimize? #false
                       (collection-file-path "memory.c" "pltfun" "tamer" "memory"))))

(define swap.ram : Geo (plt-ram-snapshots->table swap-snapshots #:gapsize 16.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  swap.ram)
