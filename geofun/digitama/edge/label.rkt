#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/match)

(require "../../paint.rkt")
(require "../../font.rkt")

(require "../path/self.rkt")

(require "../dc/text.rkt")
(require "../dc/resize.rkt")
(require "../convert.rkt")
(require "../color.rkt")
(require "../markup.rkt")

(require "../layer/type.rkt")
(require "../layer/sticker.rkt")
(require "../geometry/computation/line.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Edge-Label-Datum (U Bytes (Pairof Bytes Bytes) (Listof (Option Bytes))))

(struct geo-edge-label
  ([idx : Integer]
   [sticker : Geo]
   [t : Flonum]
   [distance : (Option Flonum)]
   [rotate? : Boolean]
   [adjust-angle : (Option Flonum)])
  #:type-name Geo-Edge-Label
  #:constructor-name unsafe-geo-edge-label
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo-edge-label : (->* (DC-Markup-Text)
                                   (Flonum #:index Integer #:font (Option Font) #:color Option-Fill-Paint #:distance (Option Flonum)
                                           #:rotate? Boolean #:adjust-angle (Option Flonum))
                                   Geo-Edge-Label)
  (lambda [#:index [idx 0] #:font [font #false] #:color [font-paint #false]
           #:distance [distance #false] #:adjust-angle [adjust #false] #:rotate? [rotate? #true]
           label [position 0.5]]
    (define self : Bytes (dc-markup-datum->text label))
    (define glabel : Geo (geo-edge-label-text self #false font font-paint))

    (unsafe-geo-edge-label idx glabel position distance rotate? adjust)))

(define make-geo-edge-labels : (->* (Geo-Path-Labels)
                                    (Flonum #:index Integer #:font (Option Font) #:color Option-Fill-Paint #:distance (Option Flonum)
                                            #:rotate? Boolean #:adjust-angle (Option Flonum))
                                    (Listof Geo-Edge-Label))
  (lambda [#:index [idx 0] #:font [font #false] #:color [font-paint #false]
           #:distance [distance #false] #:adjust-angle [adjust #false] #:rotate? [rotate? #true]
           labels [base-position 0.25]]
    (define selves : (Option Geo-Edge-Label-Datum) (geo-edge-label-map labels))
    (define glabels : (Listof (Pairof Geo Flonum))
      (cond [(bytes? selves) (list (cons (geo-edge-label-text selves #false font font-paint) 0.5))]
            [(list? selves) ; single-item list also works as designed
             (let ([step (/ (- 1.0 base-position base-position) (max 1 (sub1 (length selves))))])
               (for/list : (Listof (Pairof Geo Flonum)) ([lbl (in-list selves)]
                                                         [pos (in-range base-position 1.1 step)]
                                                         #:when (bytes? lbl))
                 (cons (geo-edge-label-text lbl #false font font-paint) pos)))]
            [(pair? selves)
             (list (cons (geo-edge-label-text (car selves) #false font font-paint) base-position)
                   (cons (geo-edge-label-text (cdr selves) #false font font-paint) (- 1.0 base-position)))]
            [else null]))

    (for/list ([g (in-list glabels)])
      (unsafe-geo-edge-label idx (car g) (cdr g) distance rotate? adjust))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: transparent color makes the label clear underneath arrows but sometimes doesn't work for pdf
(define geo-edge-label-text : (-> DC-Markup-Text (Option Symbol) (Option Font) Option-Fill-Paint Geo)
  (lambda [text text-id font paint]
    (geo-markup #:id text-id #:alignment 'center
                #:color paint #:background transparent
                #:error-color 'GhostWhite #:error-background 'Firebrick
                text font)))

(define geo-edge-label-layer+distance : (case-> [Geo-Edge-Label (Pairof Float-Complex Float-Complex) -> (Values (GLayerof Geo) Flonum)]
                                                [Geo-Edge-Label Float-Complex Float-Complex -> (Values (GLayerof Geo) Flonum)])
  (case-lambda
    [(self pos dir)
     (let ([label (geo-edge-label-sticker self)]
           [maybe-distance (geo-edge-label-distance self)]
           [adjust (geo-edge-label-adjust-angle self)]
           [rotate? (geo-edge-label-rotate? self)])
       (define sticker : Geo
         (cond [(not rotate?) label]
               [(and adjust) (geo-rotate label (+ (angle dir) adjust) #true)]
               [(negative? (real-part dir)) (geo-rotate label (+ (angle dir) pi) #true)]
               [else (geo-rotate label (angle dir) #true)]))
       
       (define distance : Flonum
         (cond [(or maybe-distance) maybe-distance]
               [(or rotate?) (* (geo-height label) (if (negative? (real-part dir)) -0.75 0.75))]
               [else (let*-values ([(lw lh) (geo-flsize label)]
                                   [(theta) (angle dir)])
                       (* (magnitude (make-rectangular (* (max lw 16.0) (sin theta)) (* lh (cos theta)))) 0.75
                          (let ([Re (real-part dir)])
                            (cond [(positive? Re) 1.0]
                                  [(zero? Re) (if (positive? (imag-part dir)) 1.0 -1.0)]
                                  [else -1.0]))))]))
       
       (values (geo-sticker->layer sticker (geo-perpendicular-point pos dir distance 0.0))
               distance))]
    [(self pos.dir) (geo-edge-label-layer+distance self (car pos.dir) (cdr pos.dir))]))

(define geo-edge-label-layer : (case-> [Geo-Edge-Label (Pairof Float-Complex Float-Complex) -> (GLayerof Geo)]
                                       [Geo-Edge-Label Float-Complex Float-Complex -> (GLayerof Geo)])
  (case-lambda
    [(self pos dir) (let-values ([(layer distance) (geo-edge-label-layer+distance self pos dir)]) layer)]
    [(self pos.dir) (geo-edge-label-layer self (car pos.dir) (cdr pos.dir))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-edge-label-map : (-> Geo-Path-Labels (Option Geo-Edge-Label-Datum))
  (lambda [info]
    (cond [(list? info) (map dc-markup-datum->maybe-text info)]
          [(pair? info) (cons (dc-markup-datum->text (car info)) (dc-markup-datum->text (cdr info)))]
          [else (dc-markup-datum->maybe-text info)])))

(define geo-edge-multiplicity-map : (-> Geo-Path-Multiplicity-Datum (Option Bytes))
  (lambda [mult]
    (and mult (string->bytes/utf-8 (format "~a" mult)))))

(define geo-edge-multiplicities-map : (-> Geo-Path-Multiplicity-Datum Geo-Path-Multiplicity-Datum (Option Geo-Edge-Label-Datum))
  (lambda [source target]
    (define src (geo-edge-multiplicity-map source))
    (define tgt (geo-edge-multiplicity-map target))

    (cond [(and src tgt) (cons src tgt)]
          [(or  src tgt) (list src tgt)]
          [else #false])))

(define geo-edge-label-filter : (-> Any (Option Geo-Edge-Label-Datum))
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

(define geo-edge-label-flatten : (-> (Listof Geo-Path-Labels) (Listof Bytes))
  (lambda [labels]
    (let flatten ([bs : (Listof Bytes) null]
                  [labels : (Listof Geo-Path-Labels) labels])
      (if (pair? labels)
          (let ([self (geo-edge-label-map (car labels))])
            (cond [(bytes? self) (flatten (cons self bs) (cdr labels))]
                  [(list? self) (flatten (append (filter bytes? self) bs) (cdr labels))]
                  [(pair? self) (flatten (list* (car self) (cdr self) bs) (cdr labels))]
                  [else (flatten bs (cdr labels))]))
          bs))))

(define geo-edge-label-match? : (-> (Listof Bytes) (U Byte-Regexp Regexp) Boolean)
  (lambda [labels keywords]
    (for/or : Boolean ([label (in-list labels)])
      (regexp-match? keywords label))))

(define geo-edge-label-double-angle-bracketed? : (-> (Listof Bytes) Boolean)
  (lambda [labels]
    (geo-edge-label-match? labels #px"^&lt;&lt;\\w+&gt;&gt;$")))
