#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "anchor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo-trail
  ([positions : (HashTable Geo-Anchor-Name Float-Complex)]
   [ranchors : (Pairof Geo-Anchor-Name (Listof Geo-Anchor-Name))]
   [home : Float-Complex])
  #:constructor-name unsafe-geo-trail
  #:type-name Geo-Trail
  #:mutable)

(define make-geo-trail : (->* (Float-Complex) (Geo-Anchor-Name) Geo-Trail)
  (lambda [home [anchor '#:home]]
    (if (keyword? anchor)

        (unsafe-geo-trail (make-hasheq (list (cons anchor home)))
                          (list anchor)
                          home)

        (unsafe-geo-trail (make-hasheq (list (cons anchor home)
                                             (cons '#:home home)))
                          (list anchor)
                          home))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-trail-anchors : (-> Geo-Trail (Pairof Geo-Anchor-Name (Listof Geo-Anchor-Name)))
  (lambda [self]
    (assert (reverse (geo-trail-ranchors self)) pair?)))

(define geo-trail-home-anchor : (-> Geo-Trail Geo-Anchor-Name)
  (lambda [self]
    (last (geo-trail-ranchors self))))

(define #:forall (Alt) geo-trail-ref : (case-> [Geo-Trail Geo-Anchor-Name -> Float-Complex]
                                               [Geo-Trail Geo-Anchor-Name Alt -> (U Alt Float-Complex)])
  (case-lambda
    [(self anchor)
     (hash-ref (geo-trail-positions self) anchor
               (λ [] (raise-user-error 'geo-trail-ref "no such an anchor: ~a" anchor)))]
    [(self anchor defval)
     (hash-ref (geo-trail-positions self) anchor (λ [] defval))]))

(define geo-trail-try-set! : (-> Geo-Trail (Option Geo-Anchor-Name) Float-Complex Void)
  (lambda [self anchor pos]
    (unless (not anchor)
      (geo-trail-set! self anchor pos))))

(define geo-trail-set! : (-> Geo-Trail Geo-Anchor-Name Float-Complex Void)
  (lambda [self anchor pos]
    (when (hash-has-key? (geo-trail-positions self) anchor)
      (raise-user-error 'geo-trail-set! "duplicate anchor name: ~a" anchor))
    (hash-set! (geo-trail-positions self) anchor pos)
    (set-geo-trail-ranchors! self (cons anchor (geo-trail-ranchors self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-trail-anchored-positions : (->* (Geo-Trail) ((Option Geo-Trusted-Anchors)) (Immutable-HashTable Float-Complex Geo-Anchor-Name))
  (lambda [self [trusted-anchors #false]]
    (for/fold ([positions : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (hasheqv)])
              ([(anchor pos) (in-hash (geo-trail-positions self))]
               #:when (geo-anchor-trusted? anchor trusted-anchors))
      (define Eanchor : (Option Geo-Anchor-Name) (hash-ref positions pos (λ [] #false)))

      (cond [(not Eanchor) (hash-set positions pos anchor)]
            [(symbol? anchor) (hash-set positions pos anchor)] ; usually it's an alias of '#:home
            [else positions]))))
