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
    
    (cairo_new_path cr)

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

  (define (dc_circle create-surface radius border background diameters density)
    (define fllength (unsafe-fl* radius 2.0))
    (define line-width (~bdwidth border))
    (define-values (sfc cr) (create-surface fllength fllength density #true))
    
    (cairo_translate cr radius radius)
    (cairo_arc cr 0.0 0.0 (unsafe-fl- radius (unsafe-fl* line-width 0.5)) 0.0 2pi)

    (let draw-d ([as diameters])
      (when (pair? as)
        (define theta (unsafe-car as))
        (define x (unsafe-fl* radius (unsafe-flcos theta)))
        (define y (unsafe-fl* radius (unsafe-flsin theta)))
        (cairo-add-line cr x y (unsafe-fl- 0.0 x) (unsafe-fl- 0.0 y))
        (draw-d (unsafe-cdr as))))
    
    (cairo-render cr border background)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_ellipse create-surface flwidth flheight border background diameters density)
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
    
    (let draw-d ([as diameters])
      (when (pair? as)
        (define theta (unsafe-car as))
        (define x (unsafe-fl* width/2 (unsafe-flcos theta)))
        (define y (unsafe-fl* height/2 (unsafe-flsin theta)))
        (cairo-add-line cr x y (unsafe-fl- 0.0 x) (unsafe-fl- 0.0 y))
        (draw-d (unsafe-cdr as))))
    
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
  
  (define (dc_rectangle create-surface flwidth flheight border background vlines hlines density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define uwidth (unsafe-fl- flwidth line-width))
    (define uheight (unsafe-fl- flheight line-width))

    (cairo_rectangle cr inset inset uwidth uheight)

    (let draw-vl ([vls vlines])
      (when (pair? vls)
        (define x (unsafe-fl+ (dc-line-position uwidth (unsafe-car vls)) inset))
        (cairo-add-line cr x inset x uheight)
        (draw-vl (unsafe-cdr vls))))
    
    (let draw-hl ([hls hlines])
      (when (pair? hls)
        (define y (unsafe-fl+ (dc-line-position uheight (unsafe-car hls)) inset))
        (cairo-add-line cr inset y uwidth y)
        (draw-hl (unsafe-cdr hls))))

    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_rounded_rectangle create-surface flwidth flheight corner-radius border background vlines hlines density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define flradius
      (let ([short (unsafe-flmin flwidth flheight)])
        (unsafe-flmin (unsafe-fl* short 0.5) corner-radius)))
    (define uwidth (unsafe-fl- flwidth line-width))
    (define uheight (unsafe-fl- flheight line-width))
    (define tlset (unsafe-fl+ inset flradius))
    (define xrset (unsafe-fl- (unsafe-fl+ uwidth  inset) flradius))
    (define ybset (unsafe-fl- (unsafe-fl+ uheight inset) flradius))

    (cairo_new_path cr)
    (cairo_arc cr xrset tlset flradius -pi/2 0.0)
    (cairo_arc cr xrset ybset flradius 0.0   pi/2)
    (cairo_arc cr tlset ybset flradius pi/2  pi)
    (cairo_arc cr tlset tlset flradius pi    3pi/2)
    (cairo_close_path cr)

    (let draw-vl ([vls vlines])
      (when (pair? vls)
        (define pos (dc-line-position uwidth (unsafe-car vls)))
        (define x (unsafe-fl+ pos inset))

        (cond [(unsafe-fl< x tlset)
               (let ([off (unsafe-fl+ inset (dc-line-rounded-offset flradius (unsafe-fl- tlset x)))])
                 (cairo-add-line cr x off x (unsafe-fl- uheight off)))]
              [(unsafe-fl> x xrset)
               (let ([off (unsafe-fl+ inset (dc-line-rounded-offset flradius (unsafe-fl- x xrset)))])
                 (cairo-add-line cr x off x (unsafe-fl- uheight off)))]
              [else (cairo-add-line cr x inset x uheight)])
        
        (draw-vl (unsafe-cdr vls))))
    
    (let draw-hl ([hls hlines])
      (when (pair? hls)
        (define pos (dc-line-position uheight (unsafe-car hls)))
        (define y (unsafe-fl+ pos inset))

        (cond [(unsafe-fl< y tlset)
               (let ([off (unsafe-fl+ inset (dc-line-rounded-offset flradius (unsafe-fl- tlset y)))])
                 (cairo-add-line cr off y (unsafe-fl- uwidth off) y))]
              [(unsafe-fl> y ybset)
               (let ([off (unsafe-fl+ inset (dc-line-rounded-offset flradius (unsafe-fl- y ybset)))])
                 (cairo-add-line cr off y (unsafe-fl- uwidth off) y))]
              [else (cairo-add-line cr inset y uwidth y)])
        
        (draw-hl (unsafe-cdr hls))))

    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)
  
  (define (dc_regular_polygon create-surface n radius rotation border background density)
    (define fln (exact->inexact n))
    (define fllength (unsafe-fl* radius 2.0))
    (define line-width (~bdwidth border))
    (define r (unsafe-fl- radius (unsafe-fl* line-width 0.5)))
    (define delta (unsafe-fl/ 2pi fln))
    (define-values (sfc cr) (create-surface fllength fllength density #true))
    
    (cairo_new_path cr)
    (cairo_translate cr radius radius)
    
    (let draw-polygon ([flidx 0.0]
                       [theta rotation])
      (when (unsafe-fl< flidx fln)
        (cairo_line_to cr (unsafe-fl* r (unsafe-flcos theta)) (unsafe-fl* r (unsafe-flsin theta)))
        (draw-polygon (unsafe-fl+ flidx 1.0) (unsafe-fl+ theta delta))))

    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)
    
    sfc)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc-line-position length pos)
    (cond [(unsafe-fl>= pos 1.0) pos]
          [(unsafe-fl>= pos 0.0) (unsafe-fl* length pos)]
          [(unsafe-fl<= pos -1.0) (unsafe-fl+ length pos)]
          [else (unsafe-fl* length (unsafe-fl+ 1.0 pos))]))

  (define (dc-line-rounded-offset r d)
    (unsafe-fl- r
                (unsafe-flsqrt (unsafe-fl- (unsafe-fl* r r)
                                           (unsafe-fl* d d))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_line (All (S) (-> (Cairo-Surface-Create S) Flonum Flonum Flonum Flonum Flonum Flonum Paint Positive-Flonum S))]
 [dc_circle (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum (Option Paint) (Option Fill-Source) (Listof Flonum) Positive-Flonum S))]
 [dc_ellipse (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) (Listof Flonum) Positive-Flonum S))]
 [dc_arc (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Paint Positive-Flonum S))]
 [dc_sector (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum (Option Paint) (Option Fill-Source) Positive-Flonum S))]
 [dc_regular_polygon (All (S) (-> (Cairo-Surface-Create S) Positive-Index Nonnegative-Flonum Flonum (Option Paint) (Option Fill-Source) Positive-Flonum S))]

 [dc_rectangle (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source)
                            (Listof Flonum) (Listof Flonum) Positive-Flonum
                            S))]

 [dc_rounded_rectangle (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source)
                                    (Listof Flonum) (Listof Flonum) Positive-Flonum
                                    S))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define regular-polygon-radius->circumsphere-radius : (-> Positive-Index Nonnegative-Flonum 2D-Radius-Type Nonnegative-Flonum)
  (lambda [n R type]
    ; r = Rcos(pi/n)
    (cond [(eq? type 'vertex) R]
          [else (max (/ R (cos (/ pi (exact->inexact n)))) 0.0)])))
