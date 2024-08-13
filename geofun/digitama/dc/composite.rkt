#lang typed/racket/base

(provide (all-defined-out))

(require "../../paint.rkt")

(require "../convert.rkt")
(require "../composite.rkt")
(require "../unsafe/composite.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:group geo
  ([operator : (Option Symbol)]
   [layers : Geo-Layer-Group])
  #:type-name Geo:Group
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite : (->* (Geo<%> Real Real Geo<%>) (Real Real #:operator (Option Symbol) #:id (Option Symbol)) Geo:Group)
  (lambda [geo1 x1 y1 geo2 [x2 0.0] [y2 0.0] #:operator [op #false] #:id [id #false]]
    (make-geo:group id op
                    (geo-composite-layers geo1 geo2
                                          (- (real->double-flonum x1) (real->double-flonum x2))
                                          (- (real->double-flonum y1) (real->double-flonum y2))))))

(define geo-pin* : (-> Real Real Real Real Geo<%> [#:operator (Option Symbol)] [#:id (Option Symbol)] Geo<%> * Geo<%>)
  (lambda [#:operator [op #false] #:id [id #false] x1% y1% x2% y2% base . siblings]
    (cond [(null? siblings) base]
          [(null? (cdr siblings))
           (make-geo:group id op
                           (geo-composite-layers base (car siblings)
                                                 (real->double-flonum x1%) (real->double-flonum y1%)
                                                 (real->double-flonum x2%) (real->double-flonum y2%)))]
          [else
           (make-geo:group id op
                           (geo-pin-layers base siblings
                                           (real->double-flonum x1%) (real->double-flonum y1%)
                                           (real->double-flonum x2%) (real->double-flonum y2%)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define geo-superimpose-layers : (-> Geo-Pin-Port Geo<%> (Listof Geo<%>) Geo-Layer-Group)
  (lambda [port base siblings]
    (let superimpose ([width : Nonnegative-Flonum 0.0]
                      [height : Nonnegative-Flonum 0.0]
                      [sreyal : (Listof Geo-Layer) null]
                      [siblings : (Listof Geo<%>) siblings])
      (if (pair? siblings)
          (let ([geo (car siblings)])
            (define-values (w h) (geo-flsize geo))
            (superimpose (max width w) (max height h)
                         (cons (cons geo (make-layer alignment w h)) sreyal)
                         (cdr siblings)))
          (values width height (reverse sreyal))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:group : (-> (Option Symbol) (Option Symbol) Geo-Layer-Group Geo:Group)
  (lambda [id op layers]
    (create-geometry-object geo:group
                            #:with [geo-group-surface (geo-group-bbox layers)] #:id id
                            op layers)))

(define geo-group-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:group?])
      (geo_composite (geo-select-operator (geo:group-operator self) default-pin-operator)
                     (geo:group-layers self) 
                     (default-geometry-density)))))

(define geo-group-bbox : (-> Geo-Layer-Group Geo-Calculate-BBox)
  (lambda [layers]
    (define w (vector-ref layers 0))
    (define h (vector-ref layers 1))
    
    (λ [self]
      (values 0.0 0.0 w h))))
