#lang typed/racket/base

(provide (all-defined-out))

(require geofun/color)
(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/base)
(require geofun/digitama/source)
(require geofun/digitama/unsafe/frame)
(require geofun/digitama/unsafe/dc/plain)

(require "../convert.rkt")

(require digimon/metrics)
(require digimon/sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-blank : (->* () (Real (Option Real) #:density Positive-Flonum) Bitmap)
  (lambda [[width 0.0] [height #false] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width (or height width)))
    (create-blank-bitmap flwidth flheight density (default-pattern-filter))))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (define-values (flw flh) (bitmap-flsize bmp))
    (create-blank-bitmap flw flh (bitmap-density bmp) (default-pattern-filter))))

(define bitmap-solid : (->* () (Color Real #:density Positive-Flonum) Bitmap)
  (lambda [[color transparent] [size 1] #:density [density (default-bitmap-density)]]
    (define side : Nonnegative-Flonum (~length size))
    (draw-bitmap dc_pattern #:with [side side density #true (default-pattern-filter)]
                 (rgb* color))))

(define bitmap-frame
  (lambda [#:margin [margin : (U Nonnegative-Real (Listof Nonnegative-Real)) 0.0] #:padding [inset : (U Nonnegative-Real (Listof Nonnegative-Real)) 0.0]
           #:border [border : Maybe-Stroke-Paint (default-border-paint)] #:background [bg-fill : Option-Fill-Paint (default-background-paint)]
           #:filter [filter : Symbol (default-pattern-filter)]
           [bmp : Bitmap]] : Bitmap
    (define-values (mtop mright mbottom mleft)
      (cond [(list? margin) (list->4:values (map real->double-flonum margin) 0.0)]
            [else (let ([fl (real->double-flonum margin)]) (values fl fl fl fl))]))
    (define-values (ptop pright pbottom pleft)
      (cond [(list? inset) (list->4:values (map real->double-flonum inset) 0.0)]
            [else (let ([fl (real->double-flonum inset)]) (values fl fl fl fl))]))
    (define-values (bmpw bmph) (bitmap-flsize bmp))
    (define-values (sfc density) (values (bitmap-surface bmp) (bitmap-density bmp)))
    (define s : (Option Stroke) (border-paint->source* border))
    (define-values (W H bdx bdy bdw bdh bmpx bmpy)
      (dc_frame_size bmpw bmph mtop mright mbottom mleft ptop pright pbottom pleft s))

    (draw-bitmap dc_frame #:with [W H density #true filter]
                 sfc bdx bdy bdw bdh bmpx bmpy bmpw bmph
                 s (background->source* bg-fill) density)))
