#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require geofun/paint)
(require geofun/font)

(require geofun/digitama/dc/text)
(require geofun/digitama/dc/resize)
(require geofun/digitama/convert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Edge-Label-Datum (U String (Pairof (Option String) (Option String))))

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
                       (if (negative? (real-part v)) -1.0 1.0)))]))
    
    (unsafe-dia-edge-label sticker start v t smart-distance)))

(define make-dia-edge-labels : (->* (Float-Complex Float-Complex Dia-Edge-Label-Datum)
                                    (Flonum #:font (Option Font) #:font-paint Option-Fill-Paint #:distance (Option Flonum)
                                            #:rotate? Boolean #:adjust-angle (Option Flonum))
                                    (Listof Dia-Edge-Label))
  (lambda [#:font [font #false] #:font-paint [font-paint #false] #:distance [distance #false] #:adjust-angle [adjust #false] #:rotate? [rotate? #true]
           start end label [head-position 0.25]]
    (define gs : (Listof (Pairof Geo Flonum))
      (if (pair? label)
        
          (let-values ([(head tail) (values (car label) (cdr label))])
            (cond [(and head tail)
                   (list (cons (geo-text head font #:color font-paint) head-position)
                         (cons (geo-text tail font #:color font-paint) (- 1.0 head-position)))]
                  [(or head) (list (cons (geo-text head font #:color font-paint) head-position))]
                  [(or tail) (list (cons (geo-text tail font #:color font-paint) (- 1.0 head-position)))]
                  [else null]))
          
          (list (cons (geo-text label font #:color font-paint) 0.5))))

    (for/list ([g (in-list gs)])
      (make-dia-edge-label #:distance distance #:adjust-angle adjust #:rotate? rotate?
                           start end (car g) (cdr g)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-edge-label-datum? : (-> Any Boolean : #:+ Dia-Edge-Label-Datum)
  (lambda [info]
    (or (string? info)
        (and (pair? info)
             (let ([src (car info)]
                   [tgt (cdr info)])
               (and (or (not src) (string? src))
                    (or (not tgt) (string? tgt))))))))

(define dia-edge-label-flatten : (-> (Listof Dia-Edge-Label-Datum) (Listof String))
  (lambda [labels]
    (let flatten ([strs : (Listof String) null]
                  [labels : (Listof Dia-Edge-Label-Datum) labels])
      (if (pair? labels)
          (let ([self (car labels)])
            (if (string? self)
                (flatten (cons self strs) (cdr labels))
                (flatten (append (filter string? (list (car self) (cdr self))) strs) (cdr labels))))
          strs))))

(define dia-edge-label-match? : (-> (Listof String) (U (Listof String) Regexp) Boolean)
  (lambda [labels keywords]
    (if (list? keywords)
        (for/or : Boolean ([label (in-list labels)])
          (for/or : Boolean ([keyword (in-list keywords)])
            (string-ci=? label keyword)))
        (for/or : Boolean ([label (in-list labels)])
          (regexp-match? keywords label)))))
