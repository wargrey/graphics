#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require racket/math)

(require "../../font.rkt")

(require "../self.rkt")
(require "../color.rkt")

(require "../richtext/self.rkt")
(require "../richtext/plain.rkt")
(require "../richtext/realize.rkt")

(require "../dc/resize.rkt")
(require "../paint/self.rkt")
(require "../layer/type.rkt")
(require "../layer/sticker.rkt")
(require "../geometry/computation/line.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Label-Text (Option Geo-Rich-Text))
(define-type Geo-Path-Label-Datum (U Geo-Path-Label-Text (Pairof Geo-Rich-Text Geo-Rich-Text) (Listof Geo-Path-Label-Text)))

(struct geo:path:label
  ([idx : (Option Integer)]
   [sticker : Geo]
   [position : Flonum]
   [distance : (Option Real+%)]
   [rotate? : Boolean]
   [adjust-angle : (Option Flonum)])
  #:type-name Geo:Path:Label
  #:constructor-name unsafe-geo:path:label
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo-path-label
  (lambda [#:index [idx : (Option Integer) 0]
           #:font [font : (Option Font) #false]
           #:color [font-paint : Option-Fill-Paint #false]
           #:distance [distance : (Option Length+%) #false]
           #:rotate? [rotate? : Boolean #true]
           #:mirror? [mirror? : Boolean #false]
           [label : Geo-Rich-Text] [position : Flonum 0.5]
           [adjust : (Option Real) #false] [unit : Angle-Unit 'rad]] : Geo:Path:Label
    (unsafe-geo:path:label idx
                           (geo-path-label-realize label #false font font-paint) position
                           (if (~L? distance)
                               (parameterize ([default-font-metrics (font-metrics (or font (default-font)))])
                                 (~px distance))
                               distance)
                           rotate? (and adjust (~rad adjust unit)))))

(define make-geo-path-labels
  (lambda [#:index [idx : (Option Integer) 0]
           #:font [font : (Option Font) #false]
           #:color [font-paint : Option-Fill-Paint #false]
           #:reverse? [reverse? : Boolean #false]
           #:distance [distance : (Option Length+%) #false]
           #:rotate? [rotate? : Boolean #true]
           [selves : Geo-Path-Label-Datum] [rng : (U Flonum (Pairof Flonum Flonum)) 0.25]
           [adjust : (Option Real) #false] [unit : Angle-Unit 'rad]] : (Listof Geo:Path:Label)
    (define-values (bgn end)
      (if (pair? rng)
          (values (car rng) (cdr rng))
          (values rng (- 1.0 rng))))

    (define glabels : (Listof (Pairof Geo Flonum))
      (cond [(geo-rich-text? selves) (list (cons (geo-path-label-realize selves #false font font-paint) 0.5))]
            [(list? selves) ; single-item list also works as designed
             (let ([step (/ (- end bgn) (max 1 (sub1 (length selves))))])
               (for/list : (Listof (Pairof Geo Flonum)) ([lbl (in-list (if (not reverse?) selves (reverse selves)))]
                                                         [pos (in-range bgn (+ end step) step)]
                                                         #:when lbl)
                 (cons (geo-path-label-realize lbl #false font font-paint) pos)))]
            [(pair? selves)
             (if (not reverse?)
                 (list (cons (geo-path-label-realize (car selves) #false font font-paint) bgn)
                       (cons (geo-path-label-realize (cdr selves) #false font font-paint) end))
                 (list (cons (geo-path-label-realize (cdr selves) #false font font-paint) bgn)
                       (cons (geo-path-label-realize (car selves) #false font font-paint) end)))]
            [else null]))

    (for/list ([g (in-list glabels)])
      (unsafe-geo:path:label idx (car g) (cdr g)
                             (if (~L? distance)
                                 (parameterize ([default-font-metrics (font-metrics (or font (default-font)))])
                                   (~px distance))
                                 distance)
                             rotate? (and adjust (~rad adjust unit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-label-realize : (-> Geo-Rich-Text (Option Symbol) (Option Font) Option-Fill-Paint Geo)
  (lambda [text text-id font paint]
    (cond [(geo? text) text]

          ;;; TODO: transparent color makes the label clear underneath arrows but sometimes doesn't work for pdf
          [else (geo-rich-text-realize #:id text-id #:alignment 'center #:background transparent
                                       text font paint)])))

(define geo-path-label-layer+distance : (case-> [Geo:Path:Label (Pairof Float-Complex Float-Complex) -> (Values (GLayerof Geo) Flonum)]
                                                [Geo:Path:Label Float-Complex Float-Complex -> (Values (GLayerof Geo) Flonum)])
  (case-lambda
    [(self pos dir)
     (let ([label (geo:path:label-sticker self)]
           [maybe-distance (geo:path:label-distance self)]
           [adjust (geo:path:label-adjust-angle self)]
           [rotate? (geo:path:label-rotate? self)])
       (define sticker : Geo
         (cond [(not rotate?) label]
               [(and adjust) (geo-rotate label (+ (angle dir) adjust))]
               [(negative? (real-part dir)) (geo-rotate label (+ (angle dir) pi))]
               [else (geo-rotate label (angle dir))]))
       
       (define distance : Flonum
         (cond [(or maybe-distance) (~distance maybe-distance (geo-height label))]
               [(or rotate?) (* (geo-height label) (if (negative? (real-part dir)) -0.75 0.75))]
               [else (let*-values ([(lw lh) (geo-flsize label)]
                                   [(theta) (angle dir)])
                       (* (let ([Re (real-part dir)])
                            (cond [(positive? Re) 1.0]
                                  [(zero? Re) (if (positive? (imag-part dir)) 1.0 -1.0)]
                                  [else -1.0]))
                          (magnitude (make-rectangular (* (max lw 16.0) (sin theta))
                                                       (* lh (cos theta))))
                          0.75))]))
     
       (values (geo-sticker->layer sticker (geo-perpendicular-point pos dir distance 0.0))
               distance))]
    [(self pos.dir) (geo-path-label-layer+distance self (car pos.dir) (cdr pos.dir))]))

(define geo-path-label-layer : (case-> [Geo:Path:Label (Pairof Float-Complex Float-Complex) -> (GLayerof Geo)]
                                       [Geo:Path:Label Float-Complex Float-Complex -> (GLayerof Geo)])
  (case-lambda
    [(self pos dir) (let-values ([(layer distance) (geo-path-label-layer+distance self pos dir)]) layer)]
    [(self pos.dir) (geo-path-label-layer self (car pos.dir) (cdr pos.dir))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-match-any-label? : (-> (Listof Geo-Path-Label-Datum) (U Regexp Byte-Regexp) Boolean)
  (lambda [labels pattern]
    (for/or : Boolean ([label (in-list labels)])
      (cond [(string? label) (regexp-match? pattern label)]
            [(pair? label)
             (if (list? label)
                 (for/or : Boolean ([lbl (in-list label)])
                   (and lbl (geo-rich-text-match? lbl pattern)))
                 (or (geo-rich-text-match? (car label) pattern)
                     (geo-rich-text-match? (cdr label) pattern)))]
            [(geo-rich-text? label) (geo-rich-text-match? label pattern)]
            [else #false]))))

(define geo-path-label-has-stereotype? : (-> (Listof Geo-Path-Label-Datum) Boolean)
  (lambda [labels]
    (geo-path-match-any-label? labels #px"^(?:<<|«)[^<«»>]*(?:»|>>)$")))
