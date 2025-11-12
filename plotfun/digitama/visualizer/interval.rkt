#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

(require geofun/paint)
(require geofun/color)

(require geofun/digitama/base)
(require geofun/digitama/paint)
(require geofun/digitama/markup)
(require geofun/digitama/convert)

(require geofun/digitama/paint/self)

(require "dot.rkt")
(require "self.rkt")
(require "guard.rkt")
(require "interface.rkt")

(require "../sample.rkt")
(require "../calculus.rkt")

(require "../marker/self.rkt")
(require "../unsafe/line.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:function geo:line:visualizer ()
  #:type-name Plot:Function
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-function-range : (-> (-> Real (Option Number)) Positive-Index Plot-Visualizer-Data-Range)
  (let ([range-db : (Weak-HashTable Any (Pairof Real Real)) (make-weak-hash)])
    (lambda [f samples]
      (λ [xmin xmax]
        (hash-ref! range-db (list f xmin xmax)
                   (λ [] (parameterize ([plot-sampling? #false])
                           (define xs (geo-linear-samples xmin xmax samples))
                           (define maybe-range
                             (with-handlers ([exn:fail? (λ _ #false)])
                               (~y-bounds f xs)))
                           (or maybe-range
                               (~y-bounds (safe-real-function f) xs)))))))))

(define plot-linear-name : (-> Real Real Natural Symbol)
  (lambda [k b %.lf]
    (let ([K (~r k #:precision %.lf)]
          [B (~r (abs b) #:precision %.lf)])
      (string->symbol
       (cond [(string=? K "0") (format "y = ~a" B)]
             [(string=? B "0") (format "y = ~ax" (case K [("1") '||] [("-1") '-] [else K]))]
             [else (format "y = ~ax ~a ~a"
                     (case K [("1") '||] [("-1") '-] [else K])
                     (if (< b 0.0) '- '+) B)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-function : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (plot:function? self)
        (define pos (geo:visualizer-position self))
        (dc_line cr (- x0 (real-part pos)) (- y0 (imag-part pos)) width height
                 (geo:line:visualizer-dots self)
                 (geo-select-stroke-paint* alt-stroke))))))
