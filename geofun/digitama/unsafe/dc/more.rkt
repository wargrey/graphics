#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../../base.rkt")
(require "../source.rkt")
(require "../visual/ctype.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "../../geometry/constants.rkt")
  (require "../pangocairo.rkt")
  (require "../paint.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_stadium create-surface fllength flradius border background density)
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ fllength flheight))
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset-radius (unsafe-fl- flradius (unsafe-fl* line-width 0.5)))

    (cairo_new_path cr)
    (cairo_arc_negative cr flradius                       flradius inset-radius -pi/2 pi/2)
    (cairo_arc_negative cr (unsafe-fl+ flradius fllength) flradius inset-radius pi/2  3pi/2)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_half_stadium create-surface fllength flradius border background density)
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ fllength flradius))
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define inset-radius (unsafe-fl- flradius inset))
    (define by (unsafe-fl- flheight inset))

    (cairo_new_path cr)
    (cairo_arc cr fllength flradius inset-radius -pi/2 pi/2)
    (cairo_line_to cr inset by)
    (cairo_line_to cr inset inset)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_bullet create-surface flogive flbarrel flradius border background density)
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ flogive flbarrel))
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define tail-radius (unsafe-fl* flradius 0.384))
    (define inset-aradius (unsafe-fl- tail-radius inset))
    (define inset-bradius (unsafe-fl- flradius inset))
    (define lcx flogive)
    (define rcx (unsafe-fl- flwidth tail-radius))
    (define ctrlx (unsafe-fl* flogive 0.5))
    (define by (unsafe-fl- flheight inset))

    (cairo_new_path cr)
    (cairo-smart-elliptical-arc cr rcx flradius inset-aradius inset-bradius -pi/2 pi/2 cairo_arc)
    (cairo_line_to cr lcx by)
    (cairo_curve_to cr ctrlx by ctrlx by inset flradius)
    (cairo_curve_to cr ctrlx inset ctrlx inset lcx inset)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_sandglass create-surface flwidth flheight neck-width neck-height tube-height border background density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define-values (cy neck-a neck-b) (values (unsafe-fl* flheight 0.5) (unsafe-fl* neck-width 0.25) (unsafe-fl* neck-height 0.5)))
    (define line-width (~bdwidth border))
    (define bulb-a (unsafe-fl* (unsafe-fl- (unsafe-fl- flwidth line-width) neck-width) 0.5))
    (define bulb-b (unsafe-fl* (unsafe-fl- (unsafe-fl- flheight line-width) (unsafe-fl+ (unsafe-fl* tube-height 2.0) neck-height)) 0.5))
    (define tlset (unsafe-fl* line-width 0.5))
    (define-values (xrset ybset) (values (unsafe-fl- flwidth tlset) (unsafe-fl- flheight tlset)))
    (define-values (bulb-ty bulb-by) (values (unsafe-fl+ tlset tube-height) (unsafe-fl- ybset tube-height)))
    (define-values (neck-lx neck-rx) (values (unsafe-fl+ tlset bulb-a) (unsafe-fl- xrset bulb-a)))
    (define-values (neck-ty neck-by) (values (unsafe-fl+ bulb-ty bulb-b) (unsafe-fl- bulb-by bulb-b)))
    (define no-neck? (or (unsafe-fl= neck-a 0.0) (unsafe-fl= neck-b 0.0)))
    
    (cairo_new_path cr)
    (cairo_move_to cr tlset tlset)
    
    (cairo_line_to cr xrset tlset)
    (cairo_line_to cr xrset bulb-ty)
    (cairo-smart-arc cr neck-rx bulb-ty bulb-a bulb-b 0.0 pi/2)
    (when (not no-neck?)
      (cairo-smart-arc-negative cr neck-rx cy neck-a neck-b 3pi/2 pi/2))
    (cairo-smart-arc cr neck-rx bulb-by bulb-a bulb-b (- pi/2) 0.0)
    (cairo_line_to cr xrset ybset)
    (cairo_line_to cr tlset ybset)
    (cairo_line_to cr tlset bulb-by)
    (cairo-smart-arc cr neck-lx bulb-by bulb-a bulb-b pi 3pi/2)
    (when (not no-neck?)
      (cairo-smart-arc-negative cr neck-lx cy neck-a neck-b pi/2 (- pi/2)))
    (cairo-smart-arc cr neck-lx bulb-ty bulb-a bulb-b pi/2 pi)
    (cairo_close_path cr)
    
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_document create-surface flwidth flheight flhwave gapsize extra-n border background density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define ngap (unsafe-fl* (unsafe-fx->fl extra-n) gapsize))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define ctrl-xoff (unsafe-fl* (unsafe-fl- flwidth ngap) 0.25))
    (define ctrl-yoff (* flhwave 2.5)) ;; TODO, calculate the exact position

    (let draw-docs ([lx (unsafe-fl+ ngap inset)]
                    [rx (unsafe-fl- flwidth inset)]
                    [ty inset]
                    [by (unsafe-fl- (unsafe-fl- flheight ngap) flhwave)]
                    [i 0])
      (when (unsafe-fx<= i extra-n)
        (define ctrl-x1 (unsafe-fl+ lx ctrl-xoff))
        (define ctrl-y1 (unsafe-fl+ by ctrl-yoff))
        (define ctrl-x2 (unsafe-fl- rx ctrl-xoff))
        (define ctrl-y2 (unsafe-fl- by ctrl-yoff))
        
        (cairo_new_path cr)
        (cairo_move_to cr lx ty)
        (cairo_line_to cr lx by)
        (cairo_curve_to cr ctrl-x1 ctrl-y1 ctrl-x2 ctrl-y2 rx by)
        (cairo_line_to cr rx ty)
        (cairo_close_path cr)
        (cairo-render cr border background)
        
        (draw-docs (unsafe-fl- lx gapsize) (unsafe-fl- rx gapsize)
                   (unsafe-fl+ ty gapsize) (unsafe-fl+ by gapsize)
                   (unsafe-fx+ i 1))))
    
    (cairo_destroy cr)

    sfc)
  
  (define (dc_database create-surface flwidth flheight bradius gapsize extra-n border background density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define rx (unsafe-fl- flwidth inset))
    (define by (unsafe-fl- flheight bradius))
    (define cx (unsafe-fl* flwidth 0.5))
    (define inset-aradius (unsafe-fl- cx inset))
    (define inset-bradius (unsafe-fl- bradius inset))
    
    (cairo_new_path cr)
    (cairo_move_to cr inset bradius)
    (cairo_line_to cr inset by)
    (cairo-smart-elliptical-arc cr cx by inset-aradius inset-bradius pi 0.0 cairo_arc_negative)
    (cairo_line_to cr rx bradius)
    (cairo-smart-elliptical-arc cr cx bradius inset-aradius inset-bradius 0.0 pi cairo_arc_negative)
    (cairo_close_path cr)

    (let draw-sides ([y bradius]
                     [i 0])
      (when (unsafe-fx<= i extra-n)
        (cairo_move_to cr inset y)
        (cairo-smart-elliptical-arc cr cx y inset-aradius inset-bradius pi 0.0 cairo_arc_negative)
        (draw-sides (unsafe-fl+ y gapsize) (unsafe-fx+ i 1))))
    
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_general_storage create-surface flwidth flheight aradius border background density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define cy (unsafe-fl* flheight 0.5))
    (define inset-aradius (unsafe-fl- aradius inset))
    (define inset-bradius (unsafe-fl- cy inset))
    
    (cairo_new_path cr)
    (cairo-smart-elliptical-arc cr aradius cy inset-aradius inset-bradius -pi/2  pi/2 cairo_arc_negative)
    (cairo-smart-elliptical-arc cr flwidth cy inset-aradius inset-bradius  pi/2 3pi/2 cairo_arc)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_half_stadium (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Positive-Flonum S))]
 [dc_stadium (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Positive-Flonum S))]
 [dc_bullet (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Positive-Flonum S))]

 [dc_sandglass (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                            (Option Paint) (Option Fill-Source) Positive-Flonum
                            S))]

 [dc_general_storage (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                                  (Option Paint) (Option Fill-Source) Positive-Flonum
                                  S))]

 [dc_document (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Index
                           (Option Paint) (Option Fill-Source) Positive-Flonum
                           S))]
 
 [dc_database (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Index
                           (Option Paint) (Option Fill-Source) Positive-Flonum
                           S))])
