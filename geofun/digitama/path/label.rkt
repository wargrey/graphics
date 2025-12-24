#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/match)
(require digimon/metrics)

(require "../../font.rkt")

(require "../self.rkt")
(require "../color.rkt")
(require "../markup.rkt")

(require "../dc/text.rkt")
(require "../dc/resize.rkt")

(require "../paint/self.rkt")
(require "../layer/type.rkt")
(require "../layer/sticker.rkt")
(require "../geometry/computation/line.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Label (Option DC-Markup-Text))
(define-type Geo-Path-Labels (U Geo-Path-Label (Pairof DC-Markup-Text DC-Markup-Text) (Listof Geo-Path-Label)))

(define-type Geo-Path-Label-Bytes (U Bytes (Pairof Bytes Bytes) (Listof (Option Bytes))))

(struct geo:path:label
  ([idx : (Option Integer)]
   [sticker : Geo]
   [time : Flonum]
   [distance : (Option Real+%)]
   [rotate? : Boolean]
   [adjust-angle : (Option Flonum)])
  #:type-name Geo:Path:Label
  #:constructor-name unsafe-geo:path:label
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo-path-label : (->* (DC-Markup-Text)
                                   (Flonum #:index (Option Integer) #:font (Option Font) #:color Option-Fill-Paint
                                           #:distance (Option Real+%) #:rotate? Boolean #:adjust-angle (Option Flonum))
                                   Geo:Path:Label)
  (lambda [#:index [idx 0] #:font [font #false] #:color [font-paint #false]
           #:distance [distance #false] #:adjust-angle [adjust #false]
           #:rotate? [rotate? #true] #:mirror? [mirror? #false]
           label [position 0.5]]
    (define self : Bytes (dc-markup-datum->text label))
    (define glabel : Geo (geo-path-label-text self #false font font-paint))

    (unsafe-geo:path:label idx glabel position distance rotate? adjust)))

(define make-geo-path-labels : (->* (Geo-Path-Labels)
                                    ((U Flonum (Pairof Flonum Flonum))
                                     #:index (Option Integer) #:font (Option Font) #:color Option-Fill-Paint
                                     #:distance (Option Real+%)
                                     #:rotate? Boolean #:adjust-angle (Option Flonum) #:reverse? Boolean)
                                    (Listof Geo:Path:Label))
  (lambda [#:index [idx 0] #:font [font #false] #:color [font-paint #false] #:reverse? [reverse? #false]
           #:distance [distance #false] #:adjust-angle [adjust #false] #:rotate? [rotate? #true]
           labels [rng 0.25]]
    (define selves : (Option Geo-Path-Label-Bytes) (geo-path-label-map labels))
    (define-values (bgn end)
      (if (pair? rng)
          (values (car rng) (cdr rng))
          (values rng (- 1.0 rng))))
    
    (define glabels : (Listof (Pairof Geo Flonum))
      (cond [(bytes? selves) (list (cons (geo-path-label-text selves #false font font-paint) 0.5))]
            [(list? selves) ; single-item list also works as designed
             (let ([step (/ (- end bgn) (max 1 (sub1 (length selves))))])
               (for/list : (Listof (Pairof Geo Flonum)) ([lbl (in-list (if (not reverse?) selves (reverse selves)))]
                                                         [pos (in-range bgn (+ end step) step)]
                                                         #:when (bytes? lbl))
                 (cons (geo-path-label-text lbl #false font font-paint) pos)))]
            [(pair? selves)
             (if (not reverse?)
                 (list (cons (geo-path-label-text (car selves) #false font font-paint) bgn)
                       (cons (geo-path-label-text (cdr selves) #false font font-paint) end))
                 (list (cons (geo-path-label-text (cdr selves) #false font font-paint) bgn)
                       (cons (geo-path-label-text (car selves) #false font font-paint) end)))]
            [else null]))

    (for/list ([g (in-list glabels)])
      (unsafe-geo:path:label idx (car g) (cdr g) distance rotate? adjust))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: transparent color makes the label clear underneath arrows but sometimes doesn't work for pdf
(define geo-path-label-text : (-> DC-Markup-Text (Option Symbol) (Option Font) Option-Fill-Paint Geo)
  (lambda [text text-id font paint]
    (geo-markup #:id text-id #:alignment 'center
                #:color paint #:background transparent
                #:error-color 'GhostWhite #:error-background 'Firebrick
                text font)))

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
               [(and adjust) (geo-rotate label (+ (angle dir) adjust) #true)]
               [(negative? (real-part dir)) (geo-rotate label (+ (angle dir) pi) #true)]
               [else (geo-rotate label (angle dir) #true)]))
       
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
(define geo-path-label-map : (-> Geo-Path-Labels (Option Geo-Path-Label-Bytes))
  (lambda [info]
    (cond [(list? info) (map dc-markup-datum->maybe-text info)]
          [(pair? info) (cons (dc-markup-datum->text (car info)) (dc-markup-datum->text (cdr info)))]
          [else (dc-markup-datum->maybe-text info)])))

(define geo-path-label-filter : (-> Any (Option Geo-Path-Label-Bytes))
  (lambda [info]
    (cond [(dc-markup-text? info) (dc-markup-datum->text info)]
          [(pair? info)
           (if (list? info)
               (match info
                 [(list (? dc-markup-maybe-text? head) (? dc-markup-maybe-text? lbls) ...)
                  (cons (and head (dc-markup-datum->text head))
                        (for/list : (Listof (Option Bytes)) ([lbl (in-list lbls)])
                          (and lbl (dc-markup-datum->text lbl))))]
                 [_ #false])
               (match info
                 [(cons (? dc-markup-text? head) (? dc-markup-text? tail))
                  (cons (dc-markup-datum->text head) (dc-markup-datum->text tail))]
                 [(cons #false (? dc-markup-text? tail)) (list #false (dc-markup-datum->text tail))]
                 [(cons (? dc-markup-text? head) #false) (list (dc-markup-datum->text head) #false)]
                 [_ #false]))]
          [else #false])))

(define geo-path-label-flatten : (-> (Listof Geo-Path-Labels) (Listof Bytes))
  (lambda [labels]
    (let flatten ([bs : (Listof Bytes) null]
                  [labels : (Listof Geo-Path-Labels) labels])
      (if (pair? labels)
          (let ([self (geo-path-label-map (car labels))])
            (cond [(bytes? self) (flatten (cons self bs) (cdr labels))]
                  [(list? self) (flatten (append (filter bytes? self) bs) (cdr labels))]
                  [(pair? self) (flatten (list* (car self) (cdr self) bs) (cdr labels))]
                  [else (flatten bs (cdr labels))]))
          bs))))

(define geo-path-label-match? : (-> (Listof Bytes) (U Byte-Regexp Regexp) Boolean)
  (lambda [labels keywords]
    (for/or : Boolean ([label (in-list labels)])
      (regexp-match? keywords label))))

(define geo-path-double-angle-bracketed-label? : (-> (Listof Bytes) Boolean)
  (lambda [labels]
    (geo-path-label-match? labels #px"^&lt;&lt;\\w+&gt;&gt;$")))
