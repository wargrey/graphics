#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/match)

(require geofun/paint)
(require geofun/font)

(require geofun/digitama/dc/text)
(require geofun/digitama/dc/resize)
(require geofun/digitama/convert)
(require geofun/digitama/color)
(require geofun/digitama/markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Edge-Label-Datum (U Bytes (Pairof Bytes Bytes) (List #;head Bytes) (Boxof #;tail Bytes)))

(struct dia-edge-label
  ([sticker : Geo]
   [pos : Float-Complex]
   [dir : Float-Complex]
   [ratio : Flonum]
   [distance : Flonum])
  #:type-name Dia-Edge-Label
  #:constructor-name unsafe-dia-edge-label
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dia-edge-label : (->* (Float-Complex Float-Complex Geo)
                                   (Flonum #:rotate? Boolean #:distance (Option Flonum) #:adjust-angle (Option Flonum))
                                   Dia-Edge-Label)
  (lambda [start end label [t 0.5] #:rotate? [rotate? #true] #:distance [distance #false] #:adjust-angle [adjust #false]]
    (define v : Float-Complex (- end start))

    (define sticker : Geo
      (cond [(not rotate?) label]
            [(and adjust) (geo-rotate label (+ (angle v) adjust) #true)]
            [(negative? (real-part v)) (geo-rotate label (+ (angle v) pi) #true)]
            [else (geo-rotate label (angle v) #true)]))

    (define smart-distance : Flonum
      (cond [(or distance) distance]
            [(or rotate?) (* (geo-height label) (if (negative? (real-part v)) -0.75 0.75))]
            [else (let*-values ([(lw lh) (geo-flsize label)]
                                [(theta) (angle v)])
                    (* (magnitude (make-rectangular (* (max lw 16.0) (sin theta)) (* lh (cos theta)))) 0.75
                       (let ([Re (real-part v)])
                         (cond [(positive? Re) 1.0]
                               [(zero? Re) (if (positive? (imag-part v)) 1.0 -1.0)]
                               [else -1.0]))))]))
    
    (unsafe-dia-edge-label sticker start v t smart-distance)))

(define make-dia-edge-labels : (->* (Float-Complex Float-Complex Dia-Edge-Label-Datum)
                                    (Flonum #:font (Option Font) #:font-paint Option-Fill-Paint #:distance (Option Flonum)
                                            #:rotate? Boolean #:adjust-angle (Option Flonum))
                                    (Listof Dia-Edge-Label))
  (lambda [#:font [font #false] #:font-paint [font-paint #false] #:distance [distance #false] #:adjust-angle [adjust #false] #:rotate? [rotate? #true]
           start end label [head-position 0.25]]
    (define gs : (Listof (Pairof Geo Flonum))
      (cond [(bytes? label) (list (cons (dia-edge-label-text label         #false font font-paint) 0.5))]
            [(list? label)  (list (cons (dia-edge-label-text (car label)   #false font font-paint) head-position))]
            [(pair? label)  (list (cons (dia-edge-label-text (car label)   #false font font-paint) head-position)
                                  (cons (dia-edge-label-text (cdr label)   #false font font-paint) (- 1.0 head-position)))]
            [else           (list (cons (dia-edge-label-text (unbox label) #false font font-paint) (- 1.0 head-position)))]))

    (for/list ([g (in-list gs)])
      (make-dia-edge-label #:distance distance #:adjust-angle adjust #:rotate? rotate?
                           start end (car g) (cdr g)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-label-text : (-> (U Bytes PExpr) (Option Symbol) (Option Font) Option-Fill-Paint Geo)
  (lambda [text text-id font paint]
    ;;; TODO
    ; transparent makes the label clear underneath arrows
    ;   but sometimes doesn't work for pdf
    (geo-markup #:id text-id #:alignment 'center
                #:color paint #:background transparent
                #:error-color 'GhostWhite #:error-background 'Firebrick
                text font)))

(define dia-edge-label-datum-filter : (-> Any (Option Dia-Edge-Label-Datum))
  (lambda [info]
    (cond [(dc-markup-text? info) (dc-markup-datum->text info)]
          [(pair? info)
           (if (list? info)
               (match info
                 [(list #false (? dc-markup-text? tail)) (box (dc-markup-datum->text tail))]
                 [(list (? dc-markup-text? head) #false) (list (dc-markup-datum->text head))]
                 [(list (? dc-markup-text? head) (? dc-markup-text? tail))
                  (cons (dc-markup-datum->text head) (dc-markup-datum->text tail))]
                 [(list (? dc-markup-text? head)) (list (dc-markup-datum->text head))]
                 [_ #false])
               (match info
                 [(cons #false (? dc-markup-text? tail)) (box (dc-markup-datum->text tail))]
                 [(cons (? dc-markup-text? head) (? dc-markup-text? tail))
                  (cons (dc-markup-datum->text head) (dc-markup-datum->text tail))]
                 [(cons (? dc-markup-text? head) #false) (list (dc-markup-datum->text head))]
                 [_ #false]))]
          [else #false])))

(define dia-edge-label-flatten : (-> (Listof Dia-Edge-Label-Datum) (Listof Bytes))
  (lambda [labels]
    (let flatten ([strs : (Listof Bytes) null]
                  [labels : (Listof Dia-Edge-Label-Datum) labels])
      (if (pair? labels)
          (let ([self (car labels)])
            (cond [(bytes? self) (flatten (cons self strs) (cdr labels))]
                  [(list? self) (flatten (cons (car self) strs) (cdr labels))]
                  [(pair? self) (flatten (list* (car self) (cdr self) strs) (cdr labels))]
                  [else (flatten (cons (unbox self) strs) (cdr labels))]))
          strs))))

(define dia-edge-label-match? : (-> (Listof Bytes) (U Byte-Regexp Regexp) Boolean)
  (lambda [labels keywords]
    (for/or : Boolean ([label (in-list labels)])
      (regexp-match? keywords label))))

(define dia-edge-label-double-angle-bracketed? : (-> (Listof Bytes) Boolean)
  (lambda [labels]
    (dia-edge-label-match? labels #px"^&lt;&lt;\\w+&gt;&gt;$")))
