#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/track)
(require geofun/digitama/dc/track)
(require geofun/digitama/dc/composite)

(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/merge)
(require geofun/digitama/layer/combine)

(require "../interface.rkt")
(require "../block/style.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-dia-track stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo id 
        (~alt (~optional (~seq #:margin margin) #:defaults ([margin #'#false]))
              (~optional (~seq #:padding inset) #:defaults ([inset #'#false]))
              (~optional (~seq #:border border) #:defaults ([border #'#false]))
              (~optional (~seq #:background bgsource) #:defaults ([bgsource #'#false])))
        ...
        self tracks blocks argl ...)
     (syntax/loc stx
       (let ([stickers (append tracks blocks)])
         (create-geometry-group Geo id #false #false
                                #:border border #:background bgsource
                                #:margin margin #:padding inset
                                (or (and (pair? stickers)
                                         (geo-layers-try-extend stickers 0.0 0.0))
                                    #;'#:deadcode (geo-own-layers self))
                                self)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:track geo:group
  ([skeleton : Geo:Track])
  #:type-name Dia:Track
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-initial-track : (-> (Option Symbol) Real+% Real+% Real
                                Geo-Print-Datum Geo-Anchor-Name Nonnegative-Flonum
                                Gomamon)
  (lambda [id gw gh ts home anchor 100%]
    (define grid-width  (~length gw 100%))
    (define grid-height (~length gh grid-width))
    (define scale (make-rectangular ts (* ts (/ grid-width grid-height))))
    
    (make-gomamon
     #:id id #:at home #:anchor anchor
     #:T-scale scale #:U-scale scale
     grid-width grid-height)))

(define dia-register-home-name : (-> Geo-Anchor-Name (Option String) (Option Dia-Block-Describe) (Option Dia-Block-Describe))
  (lambda [home name block-desc]
    (cond [(not name) block-desc]
          [(not block-desc) (make-immutable-hash (list (cons home name)))]
          [(hash? block-desc)
           (cond [(hash-has-key? block-desc home) block-desc]
                 [else (hash-set block-desc home name)])]
          [else (λ [[anchor : Geo-Anchor-Name] [text : String]]
                  (define maybe (block-desc anchor text))
                  (cond [(not (eq? home anchor)) maybe]
                        [(void? maybe) name]
                        [else maybe]))])))
