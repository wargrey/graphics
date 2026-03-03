#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)

(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/adapter)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-dia-zone stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ (~alt (~optional (~seq #:zone make-zone) #:defaults ([make-zone #'dia:zone]))
              (~optional (~seq #:id name tags) #:defaults ([name #'#false] [tags #'#false]))
              (~optional (~seq #:fit-region hfit% vfit% (~optional (~seq lft%:expr top%:expr)))
                         #:defaults ([hfit% #'1.0] [vfit% #'1.0] [lft% #'+nan.0] [top% #'+nan.0]))
              (~optional (~seq #:alignment sx% sy% (~optional (~seq tx%:expr ty%:expr)))
                         #:defaults ([sx% #'0.5] [sy% #'0.5] [tx% #'#false] [ty% #'#false]))
              (~optional (~seq #:margin margin) #:defaults ([margin #'#false])))
        ...
        #:create-with style [make-shape shape-argl ...]
        zone-argl ...)
     (syntax/loc stx
       (let ([shape (make-shape #:id (dia-zone-shape-id name)
                                #:stroke (dia-zone-resolve-stroke-paint style)
                                #:fill (dia-zone-resolve-fill-paint style)
                                shape-argl ...)])
         (create-geometry-group make-zone name #false #false
                                #:outline (geo-outline shape)
                                #:desc (geo-group-desc-from-caption maybe-caption)
                                (geo-dsfit-layers shape maybe-caption
                                                  lft% top% hfit% vfit%
                                                  sx% sy% (or tx% sx%) (or ty% sy%)
                                                  (dia-zone-resolve-padding style #:padding margin))
                                (dia-zone-style-type-object style)
                                zone-argl ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Option-Zone (Option Dia:Zone))
(define-type Dia-Maybe-Zone (U Void Dia-Option-Zone))

(struct dia:zone geo:group
  ([phantom-type : Any])
  #:type-name Dia:Zone
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia:zone-same-type? : (-> Dia:Zone (Option Dia:Zone) Boolean)
  (lambda [lgt rgt]
    (and rgt
         (eq? (object-name (dia:zone-phantom-type lgt))
              (object-name (dia:zone-phantom-type rgt))))))

(define dia:zone-diff-type? : (-> Dia:Zone (Option Dia:Zone) Boolean)
  (lambda [lgt rgt]
    (if rgt
        (not (eq? (object-name (dia:zone-phantom-type lgt))
                  (object-name (dia:zone-phantom-type rgt))))
        #true)))

(define dia:zone-typeof? : (-> Dia:Zone (-> Any Boolean) Boolean)
  (lambda [self phantom-type?]
    (phantom-type? (dia:zone-phantom-type self))))

(define dia:zone*-typeof? : (-> (Option Dia:Zone) (-> Any Boolean) Boolean : #:+ Dia:Zone)
  (lambda [self phantom-type?]
    (and self (phantom-type? (dia:zone-phantom-type self)))))
