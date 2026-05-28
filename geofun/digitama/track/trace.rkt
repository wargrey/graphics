#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "anchor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo-trace
  ([positions : (HashTable Geo-Anchor-Name Float-Complex)]
   [ranchors : (Pairof Geo-Anchor-Name (Listof Geo-Anchor-Name))]
   [home : Float-Complex])
  #:constructor-name unsafe-geo-trace
  #:type-name Geo-Trace
  #:mutable)

(define make-geo-trace : (->* (Float-Complex) (Geo-Anchor-Name) Geo-Trace)
  (lambda [home [anchor '#:home]]
    (if (keyword? anchor)

        (unsafe-geo-trace (make-hasheq (list (cons anchor home)))
                          (list anchor)
                          home)

        (unsafe-geo-trace (make-hasheq (list (cons anchor home)
                                             (cons '#:home home)))
                          (list anchor)
                          home))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-trace-anchors : (-> Geo-Trace (Pairof Geo-Anchor-Name (Listof Geo-Anchor-Name)))
  (lambda [self]
    (assert (reverse (geo-trace-ranchors self)) pair?)))

(define geo-trace-home-anchor : (-> Geo-Trace Geo-Anchor-Name)
  (lambda [self]
    (last (geo-trace-ranchors self))))

(define #:forall (Alt) geo-trace-ref : (case-> [Geo-Trace Geo-Anchor-Name -> Float-Complex]
                                               [Geo-Trace Geo-Anchor-Name Alt -> (U Alt Float-Complex)])
  (case-lambda
    [(self anchor)
     (hash-ref (geo-trace-positions self) anchor
               (λ [] (raise-user-error 'geo-trace-ref "no such an anchor: ~a" anchor)))]
    [(self anchor defval)
     (hash-ref (geo-trace-positions self) anchor (λ [] defval))]))

(define geo-trace-set! : (-> Geo-Trace Geo-Anchor-Name Float-Complex Void)
  (lambda [self anchor pos]
    (when (hash-has-key? (geo-trace-positions self) anchor)
      (raise-user-error 'geo-trace-set! "duplicate anchor name: ~a" anchor))
    (hash-set! (geo-trace-positions self) anchor pos)
    (set-geo-trace-ranchors! self (cons anchor (geo-trace-ranchors self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-trace-anchored-positions : (-> Geo-Trace (Immutable-HashTable Float-Complex Geo-Anchor-Name))
  (lambda [self]
    (for/fold ([positions : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (hasheqv)])
              ([(anchor pos) (in-hash (geo-trace-positions self))])
      (define Eanchor : (Option Geo-Anchor-Name) (hash-ref positions pos (λ [] #false)))

      (cond [(not Eanchor) (hash-set positions pos anchor)]
            [(symbol? anchor) (hash-set positions pos anchor)] ; usually it's an alias of '#:home
            [else positions]))))
