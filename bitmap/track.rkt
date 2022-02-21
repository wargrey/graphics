#lang typed/racket/base

(provide (except-out (all-defined-out) track-convert))
(provide Track Dryland-Wani)
(provide track? dryland-wani?)
(provide track-close)

(provide
 (rename-out [dryland-wani-step-up-right! dryland-wani-step-right-up!]
             [dryland-wani-step-right-down! dryland-wani-step-down-right!]
             [dryland-wani-step-down-left! dryland-wani-step-left-down!]
             [dryland-wani-step-left-up! dryland-wani-step-up-left!])

 (rename-out [dryland-wani-jump-up-right! dryland-wani-jump-right-up!]
             [dryland-wani-jump-right-down! dryland-wani-jump-down-right!]
             [dryland-wani-jump-down-left! dryland-wani-jump-left-down!]
             [dryland-wani-jump-left-up! dryland-wani-jump-up-left!])

 (rename-out [track-close dryland-wani-close!]))

(require racket/match)

(require "digitama/track.rkt")
(require "digitama/base.rkt")
(require "digitama/source.rkt")

(require "digitama/unsafe/path.rkt")
(require "digitama/unsafe/convert.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-dryland-wani! stx)
  (syntax-case stx []
    [(_ name [args ...] #:- move-expr ...)
     (syntax/loc stx
       (define name : Dryland-Wani
         (with-dryland-wani! (make-dryland-wani args ...)
           move-expr ...)))]))

(define-syntax (with-dryland-wani! stx)
  (syntax-case stx []
    [(_ wani (move argl ...) ...)
     (with-syntax* ([(dryland-wani-move ...)
                     (for/list ([<move> (in-list (syntax->list #'(move ...)))])
                       (format-id <move> "dryland-wani-~a!" (syntax->datum <move>)))])
       (syntax/loc stx
         (let ([self wani])
           (dryland-wani-move self argl ...)
           ...
           self)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dryland-wani : (->* (Real) (Real #:turn-scale Track-Print-Datum #:anchor Keyword #:at Track-Print-Datum) Dryland-Wani)
  (lambda [xstepsize [ystepsize 0.0] #:turn-scale [turn-scale +nan.0] #:anchor [anchor '#:home] #:at [home 0]]
    (define xstep : Nonnegative-Flonum (if (<= xstepsize 0.0) 1.0 (max (real->double-flonum xstepsize) 0.0)))
    (define ystep : Nonnegative-Flonum (if (<= ystepsize 0.0) xstep (max (real->double-flonum ystepsize) 0.0)))
    (define home-pos : Float-Complex (track-print-datum home))
    (define home-x : Flonum (real-part home-pos))
    (define home-y : Flonum (imag-part home-pos))
    (define-values (#{sx : Nonnegative-Flonum} #{sy : Nonnegative-Flonum})
      (cond [(flonum? turn-scale) (let ([s (track-turn-scale turn-scale)]) (values s s))]
            [(list? turn-scale) (values (track-turn-scale (car turn-scale)) (track-turn-scale (cadr turn-scale)))]
            [(pair? turn-scale) (values (track-turn-scale (car turn-scale)) (track-turn-scale (cdr turn-scale)))]
            [else (values (track-turn-scale (real-part turn-scale)) (track-turn-scale (imag-part turn-scale)))]))
    
    (let ([wani (dryland-wani track-convert
                              (list (cons start-of-track home-pos)) ((inst make-hasheq Any Float-Complex)) (list anchor)
                              home-pos home-pos home-x home-y home-x home-y
                              xstep ystep (* sx xstep) (* sy ystep))])
      (track-try-anchor! wani anchor home-pos)
      wani)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani-line-move! left            #:-> -1.0)
(define-dryland-wani-line-move! right           #:->  1.0)
(define-dryland-wani-line-move! up              #:!> -1.0)
(define-dryland-wani-line-move! down            #:!>  1.0)
(define-dryland-wani-line-move! up-right        #:+>  1.0-1.0i)
(define-dryland-wani-line-move! right-down      #:+>  1.0+1.0i)
(define-dryland-wani-line-move! down-left       #:+> -1.0+1.0i)
(define-dryland-wani-line-move! left-up         #:+> -1.0-1.0i)

(define-dryland-wani-turn-move! up-right-down   #:+> [180.0 360.0  1.0  0.0  2.0  0.0] #:boundary-guard -1.0i)
(define-dryland-wani-turn-move! up-left-down    #:-> [360.0 180.0 -1.0  0.0 -2.0  0.0] #:boundary-guard -1.0i)
(define-dryland-wani-turn-move! right-down-left #:+> [-90.0  90.0  0.0  1.0  0.0  2.0] #:boundary-guard 1.0)
(define-dryland-wani-turn-move! right-up-left   #:-> [ 90.0 -90.0  0.0 -1.0  0.0 -2.0] #:boundary-guard 1.0)
(define-dryland-wani-turn-move! down-left-up    #:+> [  0.0 180.0 -1.0  0.0 -2.0  0.0] #:boundary-guard +1.0i)
(define-dryland-wani-turn-move! down-right-up   #:-> [180.0   0.0  1.0  0.0  2.0  0.0] #:boundary-guard +1.0i)
(define-dryland-wani-turn-move! left-up-right   #:+> [ 90.0 270.0  0.0 -1.0  0.0 -2.0] #:boundary-guard -1.0)
(define-dryland-wani-turn-move! left-down-right #:-> [270.0  90.0  0.0  1.0  0.0  2.0] #:boundary-guard -1.0)

(define-dryland-wani-turn-move! up right
  #:+> [180.0 270.0 1.0  0.0 1.0 -1.0]
  #:-> [ 90.0   0.0 0.0 -1.0 1.0 -1.0])

(define-dryland-wani-turn-move! right down
  #:+> [270.0 360.0 0.0 1.0 1.0 1.0]
  #:-> [180.0  90.0 1.0 0.0 1.0 1.0])

(define-dryland-wani-turn-move! down left
  #:+> [  0.0  90.0 -1.0 0.0 -1.0 1.0]
  #:-> [270.0 180.0  0.0 1.0 -1.0 1.0])

(define-dryland-wani-turn-move! left up
  #:+> [ 90.0 180.0  0.0 -1.0 -1.0 -1.0]
  #:-> [360.0 270.0 -1.0  0.0 -1.0 -1.0])

(define dryland-wani-drift! : (->* (Dryland-Wani Track-Bezier-Datum (Listof Track-Bezier-Datum)) ((Option Track-Anchor)) Void)
  (lambda [wani end-step ctrl-steps [anchor #false]]
    (define xsize : Flonum (dryland-wani-xstepsize wani))
    (define ysize : Flonum (dryland-wani-ystepsize wani))
    (define endpt : Float-Complex (track-bezier-point wani end-step xsize ysize))
    (define controls : (Listof Float-Complex)
      (for/list : (Listof Float-Complex) ([ctrl (in-list ctrl-steps)])
        (track-bezier-point wani ctrl xsize ysize)))

    (match controls
      [(list ctrl1 ctrl2) (track-cubic-bezier wani endpt ctrl1 ctrl2 anchor)]
      [(list ctrl) (track-quadratic-bezier wani endpt ctrl anchor)]
      [(list) (track-linear-bezier wani endpt anchor)])))

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
(define bitmap-track : (->* (Track)
                            (#:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                            Bitmap)
  (lambda [#:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]
           wani]
    (define-values (bmp lt rb)
      (bitmap-track* wani #:color fgsource #:fill bgsource #:fill-style fstyle #:density density))

    bmp))

(define bitmap-track* : (->* (Track)
                             (#:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                             (Values Bitmap Float-Complex Float-Complex))
  (lambda [wani #:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]]
    (define xoff : Flonum (- (track-lx wani)))
    (define yoff : Flonum (- (track-ty wani)))
    
    (bitmap_crawl (track-footprints wani)
                  (+ (track-rx wani) xoff) (+ (track-by wani) yoff) xoff yoff
                  (stroke-paint->source fgsource) (fill-paint->source* bgsource)
                  fstyle density)))

(define bitmap-track! : (->* (Bitmap Track)
                             (Real Real #:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                             (Values Float-Complex Float-Complex))
  (lambda [#:color [fgsource black] #:fill [bgsource #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]
           bmp wani [dx 0.0] [dy 0.0]]
    (bitmap_crawl! (bitmap-surface bmp) (track-footprints wani)
                   (stroke-paint->source fgsource) (fill-paint->source* bgsource)
                   (real->double-flonum dx) (real->double-flonum dy)
                   fstyle density)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    (with-asserts ([self track?])
      (define-values (bmp lt rb)
        (bitmap-track* self #:color 'royalblue #:fill 'honeydew))

      (graphics-convert bmp mime fallback))))
