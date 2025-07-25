#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-combiner stx)
  (syntax-case stx []
    [(_ frmt #:-> Geo #:with [geobjs extra-args ...]
        #:empty blank-expr
        #:short-path #:for anchor base geo #:if short-path-condition ([(tip) short-path-expr ...] ...)
        #:do expr ...)
     (with-syntax ([(geo-combiner ...)
                    (for/list ([<tip> (in-list (syntax->list #'(tip ...)))])
                      (datum->syntax <tip> (string->symbol (format (syntax-e #'frmt) (syntax-e <tip>)))))])
       (syntax/loc stx
         (begin (define (geo-combiner [geobjs : (Listof Geo)] extra-args ...) : Geo
                  (cond [(null? geobjs) blank-expr]
                        [(null? (cdr geobjs)) (car geobjs)]
                        [(and short-path-condition (null? (cddr geobjs)))
                         (let-values ([(base geo) (values (car geobjs) (cadr geobjs))])
                           short-path-expr ...)]
                        [else (let-values ([(base anchor) (values (car geobjs) 'tip)]) expr ...)]))
                ...)))]
    [(_ frmt #:-> Bitmap #:with [bitmaps extra-args ...]
        #:empty blank-expr
        #:config-expr fltr-expr op-expr
        #:short-path #:for anchor base bmps fltr op #:if short-path-condition ([(tip) short-path-expr ...] ...)
        #:do expr ...)
     (with-syntax ([(geo-combiner ...)
                    (for/list ([<tip> (in-list (syntax->list #'(tip ...)))])
                      (datum->syntax <tip> (string->symbol (format (syntax-e #'frmt) (syntax-e <tip>)))))])
       (syntax/loc stx
         (begin (define (geo-combiner [bitmaps : (Listof Bitmap)] extra-args ...) : Bitmap
                  (define-values (fltr op) (values fltr-expr op-expr))
                  (cond [(null? bitmaps) blank-expr]
                        [(null? (cdr bitmaps)) (car bitmaps)]
                        [(and short-path-condition (null? (cddr bitmaps)))
                         (let-values ([(base bmps) (values (car bitmaps) (cadr bitmaps))])
                           short-path-expr ...)]
                        [else (let-values ([(base anchor) (values (car bitmaps) 'tip)]) expr ...)]))
                ...)))]))

(define-syntax (define-pin stx)
  (syntax-case stx []
    [(_ id #:-> Geo TGeo #:as compose #:with op #:id)
     (syntax/loc stx
       (define id : (->* (Geo Real Real Geo) (Real Real #:id (Option Symbol)) TGeo)
         (lambda [geo1 x1 y1 geo2 [x2 0.0] [y2 0.0] #:id [id #false]]
           (compose geo1 x1 y1 geo2 x2 y2 #:id id #:operator op))))]
    [(_ id #:-> Geo #:as compose #:with op)
     (syntax/loc stx
       (define id : (->* (Geo Real Real Geo) (Real Real) Geo)
         (lambda [geo1 x1 y1 geo2 [x2 0.0] [y2 0.0]]
           (compose geo1 x1 y1 geo2 x2 y2 #:operator op))))]))

(define-enumeration* geo-pin-operator #:+> Geo-Pin-Operator ; order matters
  geo-operator->integer integer->geo-operator
  [0 clear source over in out atop dest dest-over dest-in dest-out dest-atop xor add saturate
     multiply screen overlay darken lighten color-dodge color-burn hard-light soft-light difference exclusion
     hsl-hue hsl-saturation hsl-color hsl-liminosity])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-operator->integer* : (case-> [(Option Symbol) -> (Option Byte)]
                                         [(Option Symbol) (-> Geo-Pin-Operator) -> Byte])
  (case-lambda
    [(op) (and op (geo-operator->integer op))]
    [(op fallback-op)
     (or (and op (geo-operator->integer op))
         (geo-operator->integer (fallback-op)))]))
