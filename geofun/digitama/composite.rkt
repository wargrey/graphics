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
    [(_ frmt #:-> Geo #:with [geobjs extra-args ...]
        #:empty blank-expr
        #:config-expr fltr-expr op-expr
        #:short-path #:for base geo #:if short-path-condition ([(tip) (fshort-path short-path-argv ...)] ...)
        #:do (fdo argv ...))
     (with-syntax ([(geo-combiner ...)
                    (for/list ([<tip> (in-list (syntax->list #'(tip ...)))])
                      (datum->syntax <tip> (string->symbol (format (syntax-e #'frmt) (syntax-e <tip>)))))])
       (syntax/loc stx
         (begin (define geo-combiner
                  (lambda [[geobjs : (Listof Geo)] extra-args ...] : Geo
                    (cond [(null? geobjs) blank-expr]
                          [(null? (cdr geobjs)) (car geobjs)]
                          [(and short-path-condition (null? (cddr geobjs)))
                           (let-values ([(base geo) (values (car geobjs) (cadr geobjs))])
                             (fshort-path fltr-expr op-expr short-path-argv ...))]
                          [else (let ([base (car geobjs)]) (fdo 'tip fltr-expr op-expr argv ...))])))
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
(define geo-select-operator : (-> (Option Symbol) (-> Geo-Pin-Operator) Byte)
  (lambda [op fallback-op]
    (define seq (and op (geo-operator->integer op)))

    (or seq (geo-operator->integer (fallback-op)))))
