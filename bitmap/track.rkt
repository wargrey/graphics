#lang typed/racket/base

(provide (all-defined-out) with-dryland-wani! track-close)
(provide Track Dryland-Wani)
(provide track? dryland-wani?)

(provide
 (rename-out [dryland-wani-step-up-right! dryland-wani-step-right-up!]
             [dryland-wani-step-right-down! dryland-wani-step-down-rigth!]
             [dryland-wani-step-down-left! dryland-wani-step-left-down!]
             [dryland-wani-step-left-up! dryland-wani-step-up-left!])

 (rename-out [dryland-wani-jump-up-right! dryland-wani-jump-right-up!]
             [dryland-wani-jump-right-down! dryland-wani-jump-down-rigth!]
             [dryland-wani-jump-down-left! dryland-wani-jump-left-down!]
             [dryland-wani-jump-left-up! dryland-wani-jump-up-left!])

 (rename-out [track-close dryland-wani-close!]))

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
    
    (let ([wani (dryland-wani (list (cons start-of-track home-pos)) ((inst make-hasheq Any Float-Complex)) (list anchor)
                              home-pos home-pos home-x home-y home-x home-y
                              xstep ystep rx ry)])
      (track-anchor wani anchor home-pos)
      wani)))

(define-dryland-wani-step/jump-move! left #:with wani step #:=> (* (make-rectangular (real->double-flonum (- step)) 0.0) (dryland-wani-xstepsize wani)))
(define-dryland-wani-step/jump-move! right #:with wani step #:=> (* (make-rectangular (real->double-flonum step) 0.0) (dryland-wani-xstepsize wani)))
(define-dryland-wani-step/jump-move! up #:with wani step #:=> (* (make-rectangular 0.0 (real->double-flonum (- step))) (dryland-wani-ystepsize wani)))
(define-dryland-wani-step/jump-move! down #:with wani step #:=> (* (make-rectangular 0.0 (real->double-flonum step)) (dryland-wani-ystepsize wani)))

(define-dryland-wani-step/jump-move! up-right #:with wani xstep ystep
  #:=> (make-rectangular (* (real->double-flonum xstep) (dryland-wani-xstepsize wani))
                         (* (real->double-flonum (- ystep)) (dryland-wani-ystepsize wani))))

(define-dryland-wani-step/jump-move! right-down #:with wani xstep ystep
  #:=> (make-rectangular (* (real->double-flonum xstep) (dryland-wani-xstepsize wani))
                         (* (real->double-flonum ystep) (dryland-wani-ystepsize wani))))

(define-dryland-wani-step/jump-move! down-left #:with wani xstep ystep
  #:=> (make-rectangular (* (real->double-flonum (- xstep)) (dryland-wani-xstepsize wani))
                         (* (real->double-flonum ystep) (dryland-wani-ystepsize wani))))

(define-dryland-wani-step/jump-move! left-up #:with wani xstep ystep
  #:=> (make-rectangular (* (real->double-flonum (- xstep)) (dryland-wani-xstepsize wani))
                         (* (real->double-flonum (- ystep)) (dryland-wani-ystepsize wani))))

(define-dryland-wani-biturn-move! up right
  #:=> [180.0 270.0 1.0  0.0 1.0 -1.0]
  #:=> [90.0  0.0   0.0 -1.0 1.0 -1.0])

(define-dryland-wani-biturn-move! right down
  #:=> [-90.0 0.0  0.0 1.0 1.0 1.0]
  #:=> [180.0 90.0 1.0 0.0 1.0 1.0])

(define-dryland-wani-biturn-move! down left
  #:=> [0.0   90.0  -1.0 0.0 -1.0 1.0]
  #:=> [270.0 180.0  0.0 1.0 -1.0 1.0])

(define-dryland-wani-biturn-move! left up
  #:=> [90.0  180.0  0.0 -1.0 -1.0 -1.0]
  #:=> [360.0 270.0 -1.0  0.0 -1.0 -1.0])

(define dryland-wani-step-to! : (-> Dryland-Wani Track-Anchor Void)
  (lambda [wani target]
    (track-connect-to wani target)))

(define dryland-wani-jump-back! : (->* (Dryland-Wani) ((Option Track-Anchor)) Void)
  (lambda [wani [target #false]]
    (track-jump-to wani target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-anchor-position : (->* (Track Track-Anchor) (#:translate? Boolean) Float-Complex)
  (lambda [self anchor #:translate? [translate? #false]]
    (define abspos : Float-Complex (track-anchor-location self anchor))

    (cond [(not translate?) abspos]
          [else (let ([xoff (- (track-lx self))]
                      [yoff (- (track-ty self))])
                  (+ abspos (make-rectangular xoff yoff)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-track : (->* (Dryland-Wani)
                            (#:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                            Bitmap)
  (lambda [#:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]
           wani]
    (define-values (bmp lt rb)
      (bitmap-track* wani #:color fgsource #:fill bgsource #:fill-style fstyle #:density density))

    bmp))

(define bitmap-track* : (->* (Dryland-Wani)
                             (#:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                             (Values Bitmap Float-Complex Float-Complex))
  (lambda [wani #:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]]
    (define xoff : Flonum (- (track-lx wani)))
    (define yoff : Flonum (- (track-ty wani)))
    
    (bitmap_crawl (track-footprints wani)
                  (+ (track-rx wani) xoff) (+ (track-by wani) yoff) xoff yoff
                  (stroke-paint->source fgsource) (fill-paint->source* bgsource)
                  fstyle density)))

(define bitmap-track! : (->* (Bitmap Dryland-Wani)
                             (Real Real #:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                             (Values Float-Complex Float-Complex))
  (lambda [#:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]
           bmp wani [dx 0.0] [dy 0.0]]
    (bitmap_crawl! (bitmap-surface bmp) (track-footprints wani)
                   (stroke-paint->source fgsource) (fill-paint->source* bgsource)
                   (real->double-flonum dx) (real->double-flonum dy)
                   fstyle density)))
