#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-combiner stx)
  (syntax-case stx []
    [(_ frmt #:-> (Geo Extra-Type ...) #:with alignment geobjs [extra-args ...]
        #:blend-mode [blend blend-expr] #:empty blank-expr
        #:short-path #:for base geo #:if short-path-condition ([(tip) short-path-expr] ...) #:do sexp ...)
     (with-syntax ([(geo-combiner ...)
                    (for/list ([<tip> (in-list (syntax->list #'(tip ...)))])
                      (datum->syntax <tip> (string->symbol (format (syntax-e #'frmt) (syntax-e <tip>)))))])
       (syntax/loc stx
         (begin (define geo-combiner : (-> (Listof Geo) Extra-Type ... Geo)
                  (let ([alignment 'tip])
                    (λ [geobjs extra-args ...]
                      (let ([blend blend-expr])
                        (cond [(null? geobjs) blank-expr]
                              [(null? (cdr geobjs)) (car geobjs)]
                              [(and short-path-condition (null? (cddr geobjs)))
                               (let-values ([(base geo) (values (car geobjs) (cadr geobjs))])
                                 short-path-expr)]
                              [else sexp ...])))))
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

(define-syntax (geo-expand-args  stx)
  (syntax-case stx []
    [(_ in type? default)
     (syntax/loc stx
       (cond [(null? in) (list default)]
             [(type? in) (list in)]
             [else in]))]
    [(_ in type? default in->out)
     (syntax/loc stx
       (cond [(null? in) (list default)]
             [(type? in) (list (in->out in))]
             [else (map in->out in)]))]))

(define-type Superimpose-Alignment (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))

(define-enumeration* geo-pin-operator #:+> Geo-Pin-Operator ; order matters
  geo-operator->integer integer->geo-operator
  [0 clear source over in out atop dest dest-over dest-in dest-out dest-atop xor add saturate
     multiply screen overlay darken lighten color-dodge color-burn hard-light soft-light difference exclusion
     hsl-hue hsl-saturation hsl-color hsl-liminosity])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-operator->integer* : (-> (Option Symbol) Symbol (Option Integer))
  (lambda [op fallback-op]
    (define seq (and op (geo-operator->integer op)))

    (or seq (geo-operator->integer fallback-op))))
