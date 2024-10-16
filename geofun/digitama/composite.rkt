#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-combiner stx)
  (syntax-case stx []
    [(_ frmt #:-> (Geo Extra-Type ...) #:with alignment geobjs [extra-args ...]
        #:empty blank-expr
        #:short-path #:for base geo #:if short-path-condition ([(tip) short-path-expr] ...) #:do sexp ...)
     (with-syntax ([(geo-combiner ...)
                    (for/list ([<tip> (in-list (syntax->list #'(tip ...)))])
                      (datum->syntax <tip> (string->symbol (format (syntax-e #'frmt) (syntax-e <tip>)))))])
       (syntax/loc stx
         (begin (define geo-combiner : (-> (Listof Geo) Extra-Type ... Geo)
                  (let ([alignment 'tip])
                    (λ [geobjs extra-args ...]
                      (cond [(null? geobjs) blank-expr]
                            [(null? (cdr geobjs)) (car geobjs)]
                            [(and short-path-condition (null? (cddr geobjs)))
                             (let-values ([(base geo) (values (car geobjs) (cadr geobjs))])
                               short-path-expr)]
                            [else sexp ...]))))
                ...)))]
    [(_ frmt #:-> (Geo Extra-Type ...) #:with alignment geobjs op->integer [extra-args ...]
        #:empty blank-expr
        #:operator-expr op-expr
        #:short-path #:for base geo #:if short-path-condition ([(tip) short-path-expr] ...) #:do sexp ...)
     (with-syntax ([(geo-combiner ...)
                    (for/list ([<tip> (in-list (syntax->list #'(tip ...)))])
                      (datum->syntax <tip> (string->symbol (format (syntax-e #'frmt) (syntax-e <tip>)))))])
       (syntax/loc stx
         (begin (define geo-combiner : (-> (Listof Geo) Extra-Type ... Geo)
                  (let ([alignment 'tip])
                    (λ [geobjs extra-args ...]
                      (define op->integer op-expr)
                      (cond [(null? geobjs) blank-expr]
                            [(null? (cdr geobjs)) (car geobjs)]
                            [(and short-path-condition (null? (cddr geobjs)))
                             (let-values ([(base geo) (values (car geobjs) (cadr geobjs))])
                               short-path-expr)]
                            [else sexp ...]))))
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

(define-enumeration* geo-filter-algorithm #:+> Geo-Filter-Algorithm ; order matters
  geo-filter-algorithm->integer integer->geo-filter-algorithm
  [0 fast good best nearest bilinear gaussian])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-select-operator : (-> (Option Symbol) (-> Geo-Pin-Operator) (Option Integer))
  (lambda [op fallback-op]
    (define seq (and op (geo-operator->integer op)))

    (or seq (geo-operator->integer (fallback-op)))))
