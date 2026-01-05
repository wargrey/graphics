#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/adapter)

(require geofun/digitama/geometry/computation/line)

(require "style.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-expr-slot stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~alt (~optional (~seq #:slot make-slot) #:defaults ([make-slot #'expr:slot]))
              (~optional (~seq #:id name) #:defaults ([name #'#false]))
              (~optional (~seq #:desc desc) #:defaults ([desc #'#false]))
              (~optional (~seq #:fit-region hfit% vfit% (~optional (~seq lft% top%)))
                         #:defaults ([hfit% #'1.0] [vfit% #'1.0] [lft% #'+nan.0] [top% #'+nan.0]))
              (~optional (~seq #:alignment sx% sy% (~optional (~seq tx% ty%)))
                         #:defaults ([sx% #'0.5] [sy% #'0.5] [tx% #'#false] [ty% #'#false]))) ...
        #:create-with style [make-shape shape-argl ...] maybe-term slot-argl ...)
     (syntax/loc stx
       (let ([shape (make-shape #:id (expr-slot-shape-id name)
                                #:stroke (expr-slot-resolve-stroke-paint style)
                                #:fill (expr-slot-resolve-fill-paint style)
                                shape-argl ...)])
         (create-geometry-group make-slot name #false #false
                                #:outline (geo-outline shape)
                                #:desc (expr-slot-desc-from-term desc maybe-term)
                                (geo-dsfit-layers shape maybe-term lft% top% hfit% vfit%
                                                  sx% sy% (or tx% sx%) (or ty% sy%)
                                                  (default-expr-slot-margin))
                                slot-argl ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct expr:slot geo:group ()
  #:type-name Expr:Slot
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define expr-slot-desc-from-term : (-> (Option String) (Option Geo) (Option String))
  (lambda [desc maybe-term]
    (or desc
        (and maybe-term
             (cond [(geo:string? maybe-term) (geo:string-body maybe-term)]
                   [(geo:group? maybe-term) (geo:group-desc maybe-term)]
                   [else #false])))))
