#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define swap-snapshots : Dia-Memory-Snapshots
  (dia-memory-snapshots (collection-file-path "memory.c" "diafun" "tamer" "memory") #:entry 'main #:body-limit 0 #:optimize? #false
                        (λ [[snapshots : Dia-Reversed-Variables] [segment : Symbol] [state : String]]
                          (dia-memory-snapshot snapshots segment state #:no-binary-datum? #true #:integer-base 16 #:no-padding? #true))))

(define swap-memory : Geo (dia-memory-snapshots->table swap-snapshots #:gapsize 16.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  swap-memory)
