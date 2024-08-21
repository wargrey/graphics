#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../../base.rkt")
(require "../source.rkt")
(require "../visual/ctype.rkt")

(require "../../geometry/radius.rkt")
(require "../../geometry/constants.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "../../geometry/constants.rkt")
  (require "../pangocairo.rkt")
  (require "../paint.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_line create-surface x y dx dy width height stroke density)
    (define-values (sfc cr) (create-surface width height density #true))
    (define line-width (~bdwidth stroke))
    (define offset (unsafe-fl* line-width 0.5))
    
    (cairo_new_sub_path cr)

    (cond [(= dx 0.0) ; vline
           (cairo_move_to     cr x offset)
           (cairo_rel_line_to cr dx (unsafe-fl- dy line-width))]
          [(= dy 0.0) ; hline
           (cairo_move_to     cr offset y)
           (cairo_rel_line_to cr (unsafe-fl- dx line-width) dy)]
          [else ; TODO
           (cairo_move_to     cr  x  y)
           (cairo_rel_line_to cr dx dy)])
    
    (cairo-render cr stroke #false)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_circle create-surface radius border background density)
    (define fllength (unsafe-fl* radius 2.0))
    (define line-width (~bdwidth border))
    (define-values (sfc cr) (create-surface fllength fllength density #true))
    
    (cairo_translate cr radius radius)
    (cairo_arc cr 0.0 0.0 (unsafe-fl- radius (unsafe-fl* line-width 0.5)) 0.0 2pi)
    (cairo-render cr border background)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_sector create-surface aradius bradius rstart rend border background density)
    (define-values (sfc cr) (create-surface (unsafe-fl* aradius 2.0) (unsafe-fl* bradius 2.0) density #true))
    (define line-width (~bdwidth border))
    
    (cairo_translate cr aradius bradius)

    (unless (unsafe-fl= aradius bradius)
      (cairo_scale cr 1.0 (unsafe-fl/ bradius aradius)))
    
    (cairo_arc_negative cr 0.0 0.0 (unsafe-fl- aradius (unsafe-fl* line-width 0.5)) (unsafe-fl- 0.0 rstart) (unsafe-fl- 0.0 rend))

    (when (unsafe-fl<= (unsafe-flabs (unsafe-fl- rend rstart)) 2pi)
      (cairo_line_to cr 0.0 0.0)
      (cairo_close_path cr))

    (cairo-render cr border background)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_arc create-surface aradius bradius rstart rend stroke density)
    (define-values (sfc cr) (create-surface (unsafe-fl* aradius 2.0) (unsafe-fl* bradius 2.0) density #true))
    (define line-width (~bdwidth stroke))
    
    (cairo_translate cr aradius aradius)

    (unless (unsafe-fl= aradius bradius)
      (cairo_scale cr 1.0 (unsafe-fl/ bradius aradius)))
    
    (cairo_arc_negative cr 0.0 0.0 (unsafe-fl- aradius (unsafe-fl* line-width 0.5)) (unsafe-fl- 0.0 rstart) (unsafe-fl- 0.0 rend))

    (cairo-render cr stroke #false)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_ellipse create-surface flwidth flheight border background density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define-values (width/2 height/2) (values (unsafe-fl* flwidth 0.5) (unsafe-fl* flheight 0.5)))
    (define radius (unsafe-flmin width/2 height/2))
    (define line-width (~bdwidth border))
    
    (cairo_translate cr width/2 height/2)
    (cairo_save cr)
    (if (unsafe-fl= radius height/2)
        (cairo_scale cr (unsafe-fl/ flwidth flheight) 1.0)
        (cairo_scale cr 1.0 (unsafe-fl/ flheight flwidth)))
    (cairo_arc cr 0.0 0.0 (unsafe-fl- radius (unsafe-fl* line-width 0.5)) 0.0 2pi)
    (cairo_restore cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)
  
  (define (dc_rectangle create-surface flwidth flheight border background density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))

    (cairo_rectangle cr inset inset (unsafe-fl- flwidth line-width) (unsafe-fl- flheight line-width))
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_rounded_rectangle create-surface flwidth flheight corner-radius border background density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define flradius
      (let ([short (unsafe-flmin flwidth flheight)])
        (unsafe-flmin (unsafe-fl* short 0.5) corner-radius)))
    (define tlset (unsafe-fl+ inset flradius))
    (define xrset (unsafe-fl- (unsafe-fl- flwidth inset) flradius))
    (define ybset (unsafe-fl- (unsafe-fl- flheight inset) flradius))

    (cairo_new_sub_path cr)
    (cairo_arc cr xrset tlset flradius -pi/2 0.0)
    (cairo_arc cr xrset ybset flradius 0.0   pi/2)
    (cairo_arc cr tlset ybset flradius pi/2  pi)
    (cairo_arc cr tlset tlset flradius pi    3pi/2)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_stadium create-surface fllength flradius border background density)
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ fllength flheight))
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset-radius (unsafe-fl- flradius (unsafe-fl* line-width 0.5)))

    (cairo_new_sub_path cr)
    (cairo_arc_negative cr flradius                       flradius inset-radius -pi/2 pi/2)
    (cairo_arc_negative cr (unsafe-fl+ flradius fllength) flradius inset-radius pi/2  3pi/2)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_polyline create-surface flwidth flheight xs ys dx dy stroke close? density)
    (define line-width (~bdwidth stroke))
    (define-values (sfc cr) (create-surface (unsafe-fl+ flwidth line-width) (unsafe-fl+ flheight line-width) density #true))
    (define inset (unsafe-fl* line-width 0.5))
    (define x0 (unsafe-fl+ dx inset))
    (define y0 (unsafe-fl+ dy inset))

    (cairo_new_sub_path cr)
      
    (let draw-line ([xs xs]
                    [ys ys])
      (when (pair? xs)
        (cairo_line_to cr (unsafe-fl+ x0 (unsafe-car xs)) (unsafe-fl+ y0 (unsafe-car ys)))
        (draw-line (unsafe-cdr xs) (unsafe-cdr ys))))
      
    (when (and close?)
      (cairo_close_path cr))
    
    (cairo-render cr stroke #false)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_polygon create-surface flwidth flheight xs ys dx dy stroke background fill-rule density)
    (define line-width (~bdwidth stroke))
    (define-values (sfc cr) (create-surface (unsafe-fl+ flwidth line-width) (unsafe-fl+ flheight line-width) density #true))
    (define inset (unsafe-fl* line-width 0.5))
    (define x0 (unsafe-fl+ dx inset))
    (define y0 (unsafe-fl+ dy inset))

    (cairo_new_sub_path cr)
      
    (let draw-line ([xs xs]
                    [ys ys])
      (when (pair? xs)
        (cairo_line_to cr (unsafe-fl+ x0 (unsafe-car xs)) (unsafe-fl+ y0 (unsafe-car ys)))
        (draw-line (unsafe-cdr xs) (unsafe-cdr ys))))
      
    (cairo_close_path cr)
    (cairo-render cr stroke background fill-rule)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_regular_polygon create-surface n radius rotation border background density)
    (define fln (exact->inexact n))
    (define fllength (unsafe-fl* radius 2.0))
    (define line-width (~bdwidth border))
    (define r (unsafe-fl- radius (unsafe-fl* line-width 0.5)))
    (define delta (unsafe-fl/ 2pi fln))
    (define-values (sfc cr) (create-surface fllength fllength density #true))
    
    (cairo_translate cr radius radius)
    (cairo_new_sub_path cr)
    
    (let draw-polygon ([flidx 0.0]
                       [theta rotation])
      (when (unsafe-fl< flidx fln)
        (cairo_line_to cr (unsafe-fl* r (unsafe-flcos theta)) (unsafe-fl* r (unsafe-flsin theta)))
        (draw-polygon (unsafe-fl+ flidx 1.0) (unsafe-fl+ theta delta))))

    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_arrow create-surface head-radius0 rpoint border background density shaft-thickness shaft-length wing-angle)
    (define fllength (unsafe-fl* head-radius0 2.0))
    (define line-width (~bdwidth border))
    (define offset (unsafe-fl* line-width 0.5))
    (define head-radius (unsafe-fl- head-radius0 offset))
    (define rdelta (if (not wing-angle) (unsafe-fl* 0.6 pi #;108.0) (unsafe-fl- pi (unsafe-fl* 0.5 wing-angle))))
    (define rwing1 (unsafe-fl+ rpoint rdelta))
    (define rwing2 (unsafe-fl- rpoint rdelta))
    (define-values (rpx rpy) (values (unsafe-fl* head-radius (unsafe-flcos rpoint)) (unsafe-fl* head-radius (unsafe-flsin rpoint))))
    (define-values (wx1 wy1) (values (unsafe-fl* head-radius (unsafe-flcos rwing1)) (unsafe-fl* head-radius (unsafe-flsin rwing1))))
    (define-values (wx2 wy2) (values (unsafe-fl* head-radius (unsafe-flcos rwing2)) (unsafe-fl* head-radius (unsafe-flsin rwing2))))
    
    (define shaft-thickness/2 (unsafe-fl* shaft-thickness 0.5))
    (define draw-shaft? (unsafe-fl< shaft-thickness/2 head-radius))
    (define wing-theta (unsafe-fl- rdelta pi/2))
    (define wing-radius (unsafe-fl/ shaft-thickness/2 (unsafe-flcos wing-theta)))
    (define shaft-minlen (unsafe-fl* shaft-thickness/2 (unsafe-fltan wing-theta)))
    
    (define-values (flwidth flheight tx ty shx1 shy1 shx2 shy2)
      (if (or (not draw-shaft?) (unsafe-fl<= shaft-length shaft-minlen))
          (values fllength fllength head-radius0 head-radius0 0.0 0.0 0.0 0.0)

          (let*-values ([(shaft-radius) (unsafe-fl- (unsafe-flsqrt
                                                     (unsafe-fl+ (unsafe-fl* shaft-thickness/2 shaft-thickness/2)
                                                                 (unsafe-fl* shaft-length shaft-length)))
                                                    offset)]
                        [(shdelta) (unsafe-fl+ (unsafe-flacos (unsafe-fl/ shaft-thickness/2 shaft-radius)) pi/2)]
                        [(shtail1 shtail2) (values (unsafe-fl+ rpoint shdelta) (unsafe-fl- rpoint shdelta))]
                        [(shx1 shy1) (values (unsafe-fl* shaft-radius (unsafe-flcos shtail1)) (unsafe-fl* shaft-radius (unsafe-flsin shtail1)))]
                        [(shx2 shy2) (values (unsafe-fl* shaft-radius (unsafe-flcos shtail2)) (unsafe-fl* shaft-radius (unsafe-flsin shtail2)))])
            (if (unsafe-fl<= shaft-length head-radius0)
                (values fllength fllength head-radius0 head-radius0 shx1 shy1 shx2 shy2)

                (let*-values ([(axmin axmax) (values (unsafe-flmin rpx (unsafe-flmin wx1 wx2)) (unsafe-flmax rpx (unsafe-flmax wx1 wx2)))]
                              [(aymin aymax) (values (unsafe-flmin rpy (unsafe-flmin wy1 wy2)) (unsafe-flmax rpy (unsafe-flmax wy1 wy2)))]
                              [(shxmin shxmax) (values (unsafe-flmin shx1 shx2) (unsafe-flmax shx1 shx2))]
                              [(shymin shymax) (values (unsafe-flmin shy1 shy2) (unsafe-flmax shy1 shy2))]
                              [(xmin xmax) (values (unsafe-flmin axmin shxmin) (unsafe-flmax axmax shxmax))]
                              [(ymin ymax) (values (unsafe-flmin aymin shymin) (unsafe-flmax aymax shymax))])
                  (values (unsafe-fl- xmax xmin) (unsafe-fl- ymax ymin) (unsafe-fl- xmin) (unsafe-fl- ymin) shx1 shy1 shx2 shy2))))))
      
    (define-values (sfc cr) (create-surface (unsafe-fl+ flwidth line-width) (unsafe-fl+ flheight line-width) density #true))
    
    (cairo_translate cr (unsafe-fl+ tx offset) (unsafe-fl+ ty offset))
    (cairo_new_sub_path cr)
    (cairo_move_to cr 0.0 0.0)
    
    (when (or draw-shaft?)
      (cairo_move_to cr (unsafe-fl* wing-radius (unsafe-flcos rwing1)) (unsafe-fl* wing-radius (unsafe-flsin rwing1)))
      (cairo_line_to cr shx1 shy1)
      (cairo_line_to cr shx2 shy2)
      (cairo_line_to cr (unsafe-fl* wing-radius (unsafe-flcos rwing2)) (unsafe-fl* wing-radius (unsafe-flsin rwing2))))
    
    (cairo_line_to cr wx2 wy2)
    (cairo_line_to cr rpx rpy)
    (cairo_line_to cr wx1 wy1)
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
    
    (cairo_new_sub_path cr)
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

    sfc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_line (All (S) (-> (Cairo-Surface-Create S) Flonum Flonum Flonum Flonum Flonum Flonum Paint Flonum S))]
 [dc_arc (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Paint Flonum S))]
 [dc_polyline (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Listof Flonum) (Listof Flonum) Flonum Flonum Paint Boolean Flonum S))]
 
 [dc_circle (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum (Option Paint) (Option Fill-Source) Flonum S))]
 [dc_sector (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum (Option Paint) (Option Fill-Source) Flonum S))]
 [dc_ellipse (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Flonum S))]
 [dc_rectangle (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Flonum S))]
 [dc_rounded_rectangle (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Flonum S))]
 [dc_stadium (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Flonum S))]
 [dc_regular_polygon (All (S) (-> (Cairo-Surface-Create S) Positive-Index Nonnegative-Flonum Flonum (Option Paint) (Option Fill-Source) Flonum S))]
 [dc_sandglass (All (S) (-> (Cairo-Surface-Create S) Flonum Flonum Flonum Flonum Flonum (Option Paint) (Option Fill-Source) Flonum S))]

 [dc_polygon (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Listof Flonum) (Listof Flonum) Flonum Flonum
                          (Option Paint) (Option Fill-Source) Symbol Flonum
                          S))]
 
 [dc_arrow (All (S) (-> (Cairo-Surface-Create S)
                        Nonnegative-Flonum Flonum (Option Paint) (Option Fill-Source) Flonum
                        Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                        S))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define regular-polygon-radius->circumsphere-radius : (-> Positive-Index Nonnegative-Flonum 2D-Radius-Type Nonnegative-Flonum)
  (lambda [n R type]
    ; r = Rcos(pi/n)
    (cond [(eq? type 'vertex) R]
          [else (max (/ R (cos (/ pi (exact->inexact n)))) 0.0)])))
