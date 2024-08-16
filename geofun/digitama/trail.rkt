#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Anchor-Name (U Symbol Keyword))

(struct geo-trail
  ([positions : (HashTable Geo-Anchor-Name Float-Complex)]
   [traces : (Pairof Keyword (Listof Keyword))]
   [ranchors : (Pairof Geo-Anchor-Name (Listof Geo-Anchor-Name))]
   [home : Float-Complex])
  #:constructor-name unsafe-geo-trail
  #:type-name Geo-Trail
  #:mutable)

(define make-geo-trail : (->* (Float-Complex) (Geo-Anchor-Name) Geo-Trail)
  (lambda [home [anchor '#:home]]
    (if (keyword? anchor)

        (unsafe-geo-trail
         ((inst make-hasheq Geo-Anchor-Name Float-Complex) (list (cons anchor home)))
         (list anchor) (list anchor) home)

        (unsafe-geo-trail
         ((inst make-hasheq Geo-Anchor-Name Float-Complex) (list (cons anchor home)
                                                                  (cons '#:home home)))
         (list '#:home) (list anchor) home))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-trail-anchors : (-> Geo-Trail (Listof Geo-Anchor-Name))
  (lambda [self]
    (reverse (geo-trail-ranchors self))))

(define #:forall (Alt) geo-trail-ref : (case-> [Geo-Trail Geo-Anchor-Name -> Float-Complex]
                                               [Geo-Trail Geo-Anchor-Name Alt -> (U Alt Float-Complex)])
  (case-lambda
    [(self anchor) (hash-ref (geo-trail-positions self) anchor (λ [] (geo-trail-home self)))]
    [(self anchor defval) (hash-ref (geo-trail-positions self) anchor (λ [] defval))]))

(define geo-trail-head-anchor : (-> Geo-Trail Geo-Anchor-Name)
  (lambda [self]
    (car (geo-trail-traces self))))

(define geo-trail-head : (-> Geo-Trail (Values Geo-Anchor-Name Float-Complex))
  (lambda [self]
    (define anchor (geo-trail-head-anchor self))    
    (values anchor (geo-trail-ref self anchor))))

(define geo-trail-set! : (-> Geo-Trail Geo-Anchor-Name Float-Complex Void)
  (lambda [self anchor pos]
    (when (hash-has-key? (geo-trail-positions self) anchor)
      (raise-user-error 'geo-trail "duplicate anchor name: ~a" anchor))
    (hash-set! (geo-trail-positions self) anchor pos)
    (set-geo-trail-ranchors! self (cons anchor (geo-trail-ranchors self)))
    (when (keyword? anchor)
      (set-geo-trail-traces! self (cons anchor (geo-trail-traces self))))))

(define geo-trail-pop! : (-> Geo-Trail Geo-Anchor-Name Void)
  (lambda [self anchor]
    (define trace-positions (geo-trail-traces self))

    (let ([trace-rest (memq anchor trace-positions)])
      (when (and trace-rest (pair? (cdr trace-rest)))
        (set-geo-trail-traces! self (cdr trace-rest))))))
