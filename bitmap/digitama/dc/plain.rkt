#lang typed/racket/base

(provide (all-defined-out))

(require geofun/color)
(require geofun/paint)
(require geofun/fill)

(require geofun/digitama/paint/self)
(require geofun/digitama/paint/source)
(require geofun/digitama/paint/pattern)
(require geofun/digitama/geometry/sides)
(require geofun/digitama/unsafe/frame)
(require geofun/digitama/unsafe/dc/plain)

(require "../self.rkt")
(require "../convert.rkt")

(require digimon/measure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-blank : (->* () (Real-Length (Option Length+%) #:density Positive-Flonum) Bitmap)
  (lambda [[width 0.0] [height #false] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~extent width height))
    (create-blank-bitmap flwidth flheight density)))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (define-values (flw flh) (bitmap-flsize bmp))
    (create-blank-bitmap flw flh (bitmap-density bmp))))

(define bitmap-solid : (->* () (Color Real-Length #:density Positive-Flonum) Bitmap)
  (lambda [[color transparent] [size 1] #:density [density (default-bitmap-density)]]
    (define side : Nonnegative-Flonum (~dimension size))
    (draw-bitmap dc_pattern #:with [side side density #true]
                 (desc-brush #:color color))))

(define bitmap-frame
  (lambda [#:margin [margin : Geo-Insets-Datum 0.0]
           #:padding [inset : Geo-Insets-Datum 0.0]
           #:border [border : Maybe-Stroke-Paint (default-border-paint)]
           #:background [bg-fill : Option-Fill-Paint (default-background-paint)]
           #:filter [filter : Geo-Pattern-Filter (default-pattern-filter)]
           [bmp : Bitmap]] : Bitmap
    (define-values (mtop mright mbottom mleft) (geo-inset-values margin))
    (define-values (ptop pright pbottom pleft) (geo-inset-values inset))
    (define-values (bmpw bmph) (bitmap-flsize bmp))
    (define-values (sfc density) (values (bitmap-surface bmp) (bitmap-density bmp)))
    (define s : (Option Pen) (border-paint->source* border))
    (define-values (W H bdx bdy bdw bdh bmpx bmpy)
      (dc_frame_size bmpw bmph mtop mright mbottom mleft ptop pright pbottom pleft s))

    (draw-bitmap dc_frame #:with [W H density #true]
                 sfc bdx bdy bdw bdh bmpx bmpy bmpw bmph
                 s (background->source* bg-fill)
                 (geo-pattern-filter->integer filter)
                 density)))
