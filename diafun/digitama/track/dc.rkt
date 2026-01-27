#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/track)
(require geofun/digitama/dc/track)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/merge)
(require geofun/digitama/layer/combine)
(require geofun/digitama/nice/box)

(require "../block/style.rkt")
(require "../block/interface.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-dia-track stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo id self (~seq #:frame frame) realize:expr argl ...)
     (syntax/loc stx
       (let*-values ([(blocks tracks) realize]
                     [(stickers) (append tracks blocks)]
                     [(border background margin padding open-sides) (geo-frame-values frame)])
         (create-geometry-group Geo id #false #false
                                #:border border #:background background
                                #:margin margin #:padding padding
                                #:open-sides open-sides
                                (or (and (pair? stickers)
                                         (geo-layers-try-extend stickers 0.0 0.0))
                                    #;'#:deadcode (geo-own-layers self))
                                self argl ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Track-Datum (U Geo:Track Dia:Track))

(struct dia:track geo:group
  ([self : Geo:Track])
  #:type-name Dia:Track
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-initial-track : (-> (Option Symbol) Length+% Length+% Real
                                Geo-Print-Datum Geo-Anchor-Name Nonnegative-Flonum
                                Gomamon)
  (lambda [id grid-width grid-height turn-scale home-position home-anchor 100%]
    (define xstep-size (~dimension grid-width 100%))
    (define ystep-size (~dimension grid-height xstep-size))
    (define radius% (make-rectangular turn-scale (* turn-scale (/ xstep-size ystep-size))))
    
    (make-gomamon
     #:id id
     #:at home-position #:anchor home-anchor
     #:turn-radius% radius%
     xstep-size ystep-size)))

(define #:forall (S M) dia-register-home-name : (-> Geo-Anchor-Name (Option String) (Option (Dia-Block-Describer S M))
                                                    (Option (Dia-Block-Describer S M)))
  (lambda [home name block-desc]
    (cond [(not name) block-desc]
          [(not block-desc) (make-immutable-hash (list (cons home name)))]
          [(hash? block-desc)
           (cond [(hash-has-key? block-desc home) block-desc]
                 [else (hash-set block-desc home name)])]
          [else (λ [[anchor : Geo-Anchor-Name] [text : String] [style : (Dia-Block-Style-Spec S)] [metadata : M]]
                  (define maybe (block-desc anchor text style metadata))
                  (cond [(not (eq? home anchor)) maybe]
                        [(void? maybe) name]
                        [else maybe]))])))
