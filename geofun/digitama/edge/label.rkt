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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Edge-Label-Datum (U Bytes (Pairof Bytes Bytes) (Listof (Option Bytes))))

(struct geo-edge-label
  ([sticker : Geo]
   [pos : Float-Complex]
   [dir : Float-Complex]
   [t : Flonum]
   [distance : Flonum])
  #:type-name Geo-Edge-Label
  #:constructor-name unsafe-geo-edge-label
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo-edge-label : (->* (Float-Complex Float-Complex Geo)
                                   (Flonum #:rotate? Boolean #:distance (Option Flonum) #:adjust-angle (Option Flonum))
                                   Geo-Edge-Label)
  (lambda [start end label [t 0.5] #:rotate? [rotate? #true] #:distance [distance #false] #:adjust-angle [adjust #false]]
    (define dir : Float-Complex (- end start))

    (define sticker : Geo
      (cond [(not rotate?) label]
            [(and adjust) (geo-rotate label (+ (angle dir) adjust) #true)]
            [(negative? (real-part dir)) (geo-rotate label (+ (angle dir) pi) #true)]
            [else (geo-rotate label (angle dir) #true)]))

    (define smart-distance : Flonum
      (cond [(or distance) distance]
            [(or rotate?) (* (geo-height label) (if (negative? (real-part dir)) -0.75 0.75))]
            [else (let*-values ([(lw lh) (geo-flsize label)]
                                [(theta) (angle dir)])
                    (* (magnitude (make-rectangular (* (max lw 16.0) (sin theta)) (* lh (cos theta)))) 0.75
                       (let ([Re (real-part dir)])
                         (cond [(positive? Re) 1.0]
                               [(zero? Re) (if (positive? (imag-part dir)) 1.0 -1.0)]
                               [else -1.0]))))]))
    
    (unsafe-geo-edge-label sticker start dir t smart-distance)))

(define make-geo-edge-labels : (->* (Float-Complex Float-Complex Geo-Edge-Label-Datum)
                                    (Flonum #:font (Option Font) #:font-paint Option-Fill-Paint #:distance (Option Flonum)
                                            #:rotate? Boolean #:adjust-angle (Option Flonum))
                                    (Listof Geo-Edge-Label))
  (lambda [#:font [font #false] #:font-paint [font-paint #false] #:distance [distance #false] #:adjust-angle [adjust #false] #:rotate? [rotate? #true]
           start end label [base-position 0.25]]
    (define glabels : (Listof (Pairof Geo Flonum))
      (cond [(bytes? label) (list (cons (geo-edge-label-text label #false font font-paint) 0.5))]
            [(list? label) ; single-item list also works as designed
             (let ([step (/ (- 1.0 base-position base-position) (max 1 (sub1 (length label))))])
               (for/list : (Listof (Pairof Geo Flonum)) ([lbl (in-list label)]
                                                         [pos (in-range base-position 1.1 step)]
                                                         #:when (bytes? lbl))
                 (cons (geo-edge-label-text lbl #false font font-paint) pos)))]
            [else (list (cons (geo-edge-label-text (car label) #false font font-paint) base-position)
                        (cons (geo-edge-label-text (cdr label) #false font font-paint) (- 1.0 base-position)))]))

    (for/list ([g (in-list glabels)])
      (make-geo-edge-label #:distance distance #:adjust-angle adjust #:rotate? rotate?
                           start end (car g) (cdr g)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: transparent color makes the label clear underneath arrows but sometimes doesn't work for pdf
(define geo-edge-label-text : (-> (U Bytes PExpr) (Option Symbol) (Option Font) Option-Fill-Paint Geo)
  (lambda [text text-id font paint]
    (geo-markup #:id text-id #:alignment 'center
                #:color paint #:background transparent
                #:error-color 'GhostWhite #:error-background 'Firebrick
                text font)))

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

(define geo-edge-label-flatten : (-> (Listof Geo-Edge-Label-Datum) (Listof Bytes))
  (lambda [labels]
    (let flatten ([bs : (Listof Bytes) null]
                  [labels : (Listof Geo-Edge-Label-Datum) labels])
      (if (pair? labels)
          (let ([self (car labels)])
            (cond [(bytes? self) (flatten (cons self bs) (cdr labels))]
                  [(list? self) (flatten (append (filter bytes? self) bs) (cdr labels))]
                  [else (flatten (list* (car self) (cdr self) bs) (cdr labels))]))
          bs))))

(define geo-edge-label-match? : (-> (Listof Bytes) (U Byte-Regexp Regexp) Boolean)
  (lambda [labels keywords]
    (for/or : Boolean ([label (in-list labels)])
      (regexp-match? keywords label))))

(define geo-edge-label-double-angle-bracketed? : (-> (Listof Bytes) Boolean)
  (lambda [labels]
    (geo-edge-label-match? labels #px"^&lt;&lt;\\w+&gt;&gt;$")))
