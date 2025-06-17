#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require racket/math)
(require racket/match)

(require geofun/paint)
(require geofun/font)

(require geofun/digitama/convert)
(require geofun/digitama/color)
(require geofun/digitama/markup)

(require geofun/digitama/dc/text)
(require geofun/digitama/dc/resize)
(require geofun/digitama/path/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Mark-Datum (U Plot-Mark Real))
(define-type Plot-Mark-Description (U DC-Markup-Text (-> Real Real DC-Markup-Text)))

(struct plot-mark
  ([x : Real]
   [desc : (Option Plot-Mark-Description)]
   [angle : (Option Flonum)]
   [distance : (Option Flonum)]
   [pin? : Boolean]
   [rotate? : Boolean])
  #:type-name Plot-Mark
  #:constructor-name unsafe-plot-mark
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-dot-mark : (->* (Real) (Real #:radian? Boolean) Plot-Mark)
  (lambda [x [angle 0.0] #:radian? [radian? #false]]
    (unsafe-plot-mark x plot-desc-point
                      (and angle (~radian angle radian?)) #false
                      #false #false)))

#;(define make-geo-edge-label : (->* (Float-Complex Float-Complex Geo)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-desc-point : (-> Real Real DC-Markup-Text)
  (lambda [x y]
    (format "(~a, ~a)" x y)))
