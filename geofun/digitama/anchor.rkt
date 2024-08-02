#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Anchor-Name (U Symbol Keyword))

(struct geo-path
  ([positions : (HashTable Geo-Path-Anchor-Name Float-Complex)]
   [traces : (Pairof Keyword (Listof Keyword))]
   [ranchors : (Pairof Geo-Path-Anchor-Name (Listof Geo-Path-Anchor-Name))]
   [home : Float-Complex])
  #:constructor-name unsafe-make-geo-path
  #:type-name Geo-Path
  #:mutable)

(define make-geo-path : (->* (Float-Complex) (Geo-Path-Anchor-Name) Geo-Path)
  (lambda [home [anchor '#:home]]
    (if (keyword? anchor)

        (unsafe-make-geo-path
         ((inst make-hasheq Geo-Path-Anchor-Name Float-Complex) (list (cons anchor home)))
         (list anchor) (list anchor) home)

        (unsafe-make-geo-path
         ((inst make-hasheq Geo-Path-Anchor-Name Float-Complex) (list (cons anchor home)
                                                                      (cons '#:home home)))
         (list '#:home) (list anchor) home))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-anchors : (-> Geo-Path (Listof Geo-Path-Anchor-Name))
  (lambda [self]
    (reverse (geo-path-ranchors self))))

(define #:forall (Alt) geo-path-ref : (case-> [Geo-Path Geo-Path-Anchor-Name -> Float-Complex]
                                              [Geo-Path Geo-Path-Anchor-Name Alt -> (U Alt Float-Complex)])
  (case-lambda
    [(self anchor) (hash-ref (geo-path-positions self) anchor (λ [] (geo-path-home self)))]
    [(self anchor defval) (hash-ref (geo-path-positions self) anchor (λ [] defval))]))

(define geo-path-head-anchor : (-> Geo-Path Geo-Path-Anchor-Name)
  (lambda [self]
    (car (geo-path-traces self))))

(define geo-path-head : (-> Geo-Path (Values Geo-Path-Anchor-Name Float-Complex))
  (lambda [self]
    (define anchor (geo-path-head-anchor self))    
    (values anchor (geo-path-ref self anchor))))

(define geo-path-set! : (-> Geo-Path Geo-Path-Anchor-Name Float-Complex Void)
  (lambda [self anchor pos]
    (when (hash-has-key? (geo-path-positions self) anchor)
      (raise-user-error 'geo-path "duplicate anchor name: ~a" anchor))
    (hash-set! (geo-path-positions self) anchor pos)
    (set-geo-path-ranchors! self (cons anchor (geo-path-ranchors self)))
    (when (keyword? anchor)
      (set-geo-path-traces! self (cons anchor (geo-path-traces self))))))

(define geo-path-pop! : (-> Geo-Path Geo-Path-Anchor-Name Void)
  (lambda [self anchor]
    (define trace-positions (geo-path-traces self))

    (let ([trace-rest (memq anchor trace-positions)])
      (when (and trace-rest (pair? (cdr trace-rest)))
        (set-geo-path-traces! self (cdr trace-rest))))))
