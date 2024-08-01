#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Anchor (U Symbol Keyword))

(struct (Position) geo-path
  ([anchors : (HashTable Geo-Path-Anchor Position)]
   [traces : (Pairof Keyword (Listof Keyword))]
   [home : Position])
  #:constructor-name unsafe-make-geo-path
  #:type-name Geo-Pathof
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Pos) make-geo-path : (->* (Pos) (Geo-Path-Anchor) (Geo-Pathof Pos))
  (lambda [home [anchor '#:home]]
    (if (keyword? anchor)

        ((inst unsafe-make-geo-path Pos)
         ((inst make-hasheq Geo-Path-Anchor Pos) (list (cons anchor home)))
         (list anchor) home)

        ((inst unsafe-make-geo-path Pos)
         ((inst make-hasheq Geo-Path-Anchor Pos) (list (cons anchor home)
                                                       (cons '#:home home)))
         (list '#:home) home))))

(define #:forall (Pos Alt) geo-path-ref : (case-> [(Geo-Pathof Pos) Geo-Path-Anchor -> Pos]
                                                  [(Geo-Pathof Pos) Geo-Path-Anchor Alt -> (U Alt Pos)])
  (case-lambda
    [(self anchor) (hash-ref (geo-path-anchors self) anchor (λ [] (geo-path-home self)))]
    [(self anchor defval) (hash-ref (geo-path-anchors self) anchor (λ [] defval))]))

(define #:forall (Pos) geo-path-head-anchor : (-> (Geo-Pathof Pos) Geo-Path-Anchor)
  (lambda [self]
    (car (geo-path-traces self))))

(define #:forall (Pos) geo-path-head : (-> (Geo-Pathof Pos) (Values Geo-Path-Anchor Pos))
  (lambda [self]
    (define anchor (geo-path-head-anchor self))    
    (values anchor (geo-path-ref self anchor))))

(define #:forall (Pos) geo-path-set! : (-> (Geo-Pathof Pos) (Option Geo-Path-Anchor) Pos Void)
  (lambda [self anchor pos]
    (unless (not anchor)
      (when (hash-has-key? (geo-path-anchors self) anchor)
        (raise-user-error 'geo-path "duplicate anchor name: ~a" anchor))
      (hash-set! (geo-path-anchors self) anchor pos)
      (when (keyword? anchor)
        ((inst set-geo-path-traces! Pos) self (cons anchor (geo-path-traces self)))))))

(define #:forall (Pos) geo-path-pop! : (-> (Geo-Pathof Pos) Geo-Path-Anchor Void)
  (lambda [self anchor]
    (define trace-anchors (geo-path-traces self))

    (let ([trace-rest (memq anchor trace-anchors)])
      (when (and trace-rest (pair? (cdr trace-rest)))
        (set-geo-path-traces! self (cdr trace-rest))))))
