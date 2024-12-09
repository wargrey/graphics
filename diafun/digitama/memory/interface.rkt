#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "variable.rkt"))

(require digimon/predicate)

(require "variable.rkt")

(require typed/racket/unsafe)
(unsafe-require/typed/provide
 "../unsafe/memory.rkt"
 [c-variable*? (-> Any Boolean : C-Variable)]
 [c-vector*? (-> Any Boolean : C-Vector)]
 [c-rkt-run (-> Place Void)]
 [c-run (-> Place Void)]
 
 [c-run-callbacks (->* ((-> C-Reversed-Memory-Snapshot Void))
                       ((-> C-Variable Void) #:lookahead-size Byte #:lookbehind-size Byte #:body-limit Index)
                       (Values C-Take-Snapshot C-Register-Variable C-Register-Array))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type C-Reversed-Memory-Snapshot (Pairof String C-Variables))
(define-type C-Memory-Segment-Snapshot (Pairof Symbol C-Reversed-Memory-Snapshot))
(define-type C-Register-Variable (-> String Symbol Natural Symbol Void))
(define-type C-Register-Array (-> String Symbol Natural Symbol Index Void))
(define-type C-Take-Snapshot (-> String Void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-placeholder*? : (-> Any Boolean : (U C-Variable C-Vector C-Pad))
  (lambda [v]
    (or (c-padding*? v)
        (c-variable*? v)
        (c-vector*? v))))

(define c-variables? : (-> Any Boolean : C-Variables)
  (lambda [vs]
    (listof? vs c-placeholder*?)))

(define c-memory-snapshot? : (-> Any Boolean : C-Reversed-Memory-Snapshot)
  (lambda [v]
    (pairof? v string? c-variables?)))

(define c-memory-segment-snapshot? : (-> Any Boolean : C-Memory-Segment-Snapshot)
  (lambda [v]
    (pairof? v symbol? c-memory-snapshot?)))
