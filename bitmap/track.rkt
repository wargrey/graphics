#lang typed/racket/base

(provide (all-defined-out) with-dryland-wani!)
(provide Track Dryland-Wani)
(provide track? dryland-wani?)

(require typed/racket/flonum)

(require "digitama/track.rkt")
(require "digitama/base.rkt")
(require "digitama/source.rkt")

(require "digitama/unsafe/path.rkt")
(require "digitama/unsafe/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dryland-wani : (->* (Real) (Real #:big-turn? Boolean #:anchor Keyword #:at Track-Node-Datum) Dryland-Wani)
  (lambda [xstepsize [ystepsize 0.0] #:big-turn? [big-turn? #false] #:anchor [anchor '#:home] #:at [home 0]]
    (define xstep : Nonnegative-Flonum (if (<= xstepsize 0.0) 1.0 (max (real->double-flonum xstepsize) 0.0)))
    (define ystep : Nonnegative-Flonum (if (<= ystepsize 0.0) xstep (max (real->double-flonum ystepsize) 0.0)))
    (define home-pos : Float-Complex (track-node-datum home))
    (define home-x : Flonum (real-part home-pos))
    (define home-y : Flonum (imag-part home-pos))
    (define-values (#{rx : Nonnegative-Flonum} #{ry : Nonnegative-Flonum})
      (cond [(and big-turn?) (values xstep ystep)]
            [else (values (* xstep 0.5) (* ystep 0.5))]))
    
    (let ([wani (dryland-wani (list (cons start-of-track home-pos)) ((inst make-hasheq Any Float-Complex)) anchor
                              home-pos home-x home-y home-x home-y
                              xstep ystep rx ry)])
      (track-anchor wani anchor)
      wani)))

(define dryland-wani-step-left! : (->* (Dryland-Wani) (Integer Track-Anchor) Void)
  (lambda [wani [step 1] [anchor #false]]
    (track-line-to wani (* (make-rectangular (real->double-flonum (- step)) 0.0) (dryland-wani-xstepsize wani)) anchor #true)))

(define dryland-wani-step-right! : (->* (Dryland-Wani) (Integer Track-Anchor) Void)
  (lambda [wani [step 1] [anchor #false]]
    (track-line-to wani (* (make-rectangular (real->double-flonum step) 0.0) (dryland-wani-xstepsize wani)) anchor #true)))

(define dryland-wani-step-up! : (->* (Dryland-Wani) (Integer Track-Anchor) Void)
  (lambda [wani [step 1] [anchor #false]]
    (track-line-to wani (* (make-rectangular 0.0 (real->double-flonum (- step))) (dryland-wani-ystepsize wani)) anchor #true)))

(define dryland-wani-step-down! : (->* (Dryland-Wani) (Integer Track-Anchor) Void)
  (lambda [wani [step 1] [anchor #false]]
    (track-line-to wani (* (make-rectangular 0.0 (real->double-flonum step)) (dryland-wani-ystepsize wani)) anchor #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-track : (-> Dryland-Wani [#:color Stroke-Paint] [#:fill (Option Fill-Paint)] [#:fill-style Symbol] [#:density Positive-Flonum] Bitmap)
  (lambda [wani #:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]]
    (define-values (bmp lt rb)
      (bitmap-track* #:color fgsource #:fill bgsource #:fill-style fstyle #:density density
                     wani))

    bmp))

(define bitmap-track* : (->* (Dryland-Wani)
                             (#:closed? Boolean #:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                             (Values Bitmap Float-Complex Float-Complex))
  (lambda [wani #:closed? [closed? #true] #:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]]
    (define xoff : Flonum (- (track-lx wani)))
    (define yoff : Flonum (- (track-rx wani)))
    
    (bitmap_crawl (track-footprints wani) closed?
                  (+ (track-rx wani) xoff) (+ (track-by wani) yoff) xoff yoff
                  (stroke-paint->source fgsource) (fill-paint->source* bgsource)
                  fstyle density)))

(define bitmap-track! : (->* (Bitmap Dryland-Wani)
                             (Real Real #:closed? Boolean #:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                             (Values Float-Complex Float-Complex))
  (lambda [#:closed? [closed? #true] #:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]
           bmp wani [dx 0.0] [dy 0.0]]
    (bitmap_crawl! (bitmap-surface bmp) (track-footprints wani) closed?
                   (stroke-paint->source fgsource) (fill-paint->source* bgsource)
                   (real->double-flonum dx) (real->double-flonum dy)
                   fstyle density)))
