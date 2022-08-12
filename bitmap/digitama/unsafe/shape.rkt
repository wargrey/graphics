#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")
(require "convert.rkt")
(require "source.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require "constants.rkt")
  (require "paint.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_line x y dx dy width height border density)
    (define-values (img cr) (make-cairo-image width height density #true))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
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
    
    (cairo-render cr border #false)
    (cairo_destroy cr)
    
    img)
  
  (define (bitmap_arrow radius start border background density radian? stem-rate)
    (define fllength (unsafe-fl* radius 2.0))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define r (unsafe-fl- radius (unsafe-fl* line-width 0.5)))
    (define rdelta (unsafe-fl/ 2pi 3.0))
    (define-values (img cr) (make-cairo-image fllength fllength density #true))
    (define rpoint (if radian? start (~radian start)))
    (define rwing1 (unsafe-fl+ rpoint rdelta))
    (define rwing2 (unsafe-fl- rpoint rdelta))
    
    (cairo_translate cr radius radius)

    (cairo_new_sub_path cr)
    
    (when (and (unsafe-fl< 0.0 stem-rate) (unsafe-fl< stem-rate 1.0))
      (define sr (unsafe-fl/ (unsafe-fl* r stem-rate) (unsafe-flcos (unsafe-fl- rdelta pi/2))))
      (define sdelta (unsafe-fl+ (unsafe-flacos stem-rate) pi/2))
      (define stail1 (unsafe-fl+ rpoint sdelta))
      (define stail2 (unsafe-fl- rpoint sdelta))

      (cairo_move_to cr (unsafe-fl* sr (unsafe-flcos rwing1)) (unsafe-fl* sr (unsafe-flsin rwing1)))
      (cairo_line_to cr (unsafe-fl* r (unsafe-flcos stail1)) (unsafe-fl* r (unsafe-flsin stail1)))
      (cairo_line_to cr (unsafe-fl* r (unsafe-flcos stail2)) (unsafe-fl* r (unsafe-flsin stail2)))
      (cairo_line_to cr (unsafe-fl* sr (unsafe-flcos rwing2)) (unsafe-fl* sr (unsafe-flsin rwing2))))

    
    (cairo_line_to cr (unsafe-fl* r (unsafe-flcos rwing2)) (unsafe-fl* r (unsafe-flsin rwing2)))
    (cairo_line_to cr (unsafe-fl* r (unsafe-flcos rpoint)) (unsafe-fl* r (unsafe-flsin rpoint)))
    (cairo_line_to cr (unsafe-fl* r (unsafe-flcos rwing1)) (unsafe-fl* r (unsafe-flsin rwing1)))
    (cairo_close_path cr)
    
    (cairo-render cr border background)
    (cairo_destroy cr)
    
    img)

  (define (bitmap_circle radius border background density)
    (define fllength (unsafe-fl* radius 2.0))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define-values (img cr) (make-cairo-image fllength fllength density #true))
    
    (cairo_translate cr radius radius)
    (cairo_arc cr 0.0 0.0 (unsafe-fl- radius (unsafe-fl* line-width 0.5)) 0.0 2pi)
    (cairo-render cr border background)
    (cairo_destroy cr)
    
    img)

  (define (bitmap_sector radius start end border background density radian?)
    (define fllength (unsafe-fl* radius 2.0))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define-values (rstart rend) (if radian? (values start end) (values (~radian start) (~radian end))))
    (define-values (img cr) (make-cairo-image fllength fllength density #true))
    
    (cairo_translate cr radius radius)
    (cairo_arc_negative cr 0.0 0.0 (unsafe-fl- radius (unsafe-fl* line-width 0.5)) (unsafe-fl- 0.0 rstart) (unsafe-fl- 0.0 rend))

    (when (unsafe-fl<= (unsafe-flabs (unsafe-fl- rend rstart)) 2pi)
      (cairo_line_to cr 0.0 0.0)
      (cairo_close_path cr))

    (cairo-render cr border background)
    (cairo_destroy cr)
    
    img)

  (define (bitmap_ellipse width height border background density)
    (define-values (img cr) (make-cairo-image width height density #true))
    (define-values (width/2 height/2) (values (unsafe-fl* width 0.5) (unsafe-fl* height 0.5)))
    (define radius (unsafe-flmin width/2 height/2))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    
    (cairo_translate cr width/2 height/2)
    (cairo_save cr)
    (if (unsafe-fl= radius height/2)
        (cairo_scale cr (unsafe-fl/ width height) 1.0)
        (cairo_scale cr 1.0 (unsafe-fl/ height width)))
    (cairo_arc cr 0.0 0.0 (unsafe-fl- radius (unsafe-fl* line-width 0.5)) 0.0 2pi)
    (cairo_restore cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    img)
  
  (define (bitmap_rectangle flwidth flheight border background density)
    (define-values (img cr) (make-cairo-image flwidth flheight density #true))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define inset (unsafe-fl* line-width 0.5))

    (cairo_rectangle cr inset inset (unsafe-fl- flwidth line-width) (unsafe-fl- flheight line-width))
    (cairo-render cr border background)
    (cairo_destroy cr)

    img)

  (define (bitmap_rounded_rectangle flwidth flheight radius border background density)
    (define-values (img cr) (make-cairo-image flwidth flheight density #true))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define inset (unsafe-fl* line-width 0.5))
    (define flradius
      (let ([short (unsafe-flmin flwidth flheight)])
        (unsafe-flmin (unsafe-fl* short 0.5) (~length radius short))))
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

    img)

  (define (bitmap_stadium fllength radius border background density)
    (define flradius (~length radius fllength))
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ fllength flheight))
    (define-values (img cr) (make-cairo-image flwidth flheight density #true))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define inset-radius (unsafe-fl- flradius (unsafe-fl* line-width 0.5)))

    (cairo_new_sub_path cr)
    (cairo_arc_negative cr flradius                       flradius inset-radius -pi/2 pi/2)
    (cairo_arc_negative cr (unsafe-fl+ flradius fllength) flradius inset-radius pi/2  3pi/2)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    img)

  (define (bitmap_polyline flwidth flheight xs ys dx dy border background fill-style density)
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define-values (img cr) (make-cairo-image (unsafe-fl+ flwidth line-width) (unsafe-fl+ flheight line-width) density #true))
    (define inset (unsafe-fl* line-width 0.5))
    (define x0 (unsafe-fl+ dx inset))
    (define y0 (unsafe-fl+ dy inset))

    (cairo_new_sub_path cr)
      
    (let draw-line ([xs xs]
                    [ys ys])
      (when (pair? xs)
        (cairo_line_to cr (unsafe-fl+ x0 (unsafe-car xs)) (unsafe-fl+ y0 (unsafe-car ys)))
        (draw-line (unsafe-cdr xs) (unsafe-cdr ys))))
      
    (when (and fill-style)
      (cairo_close_path cr)
      (unless (eq? fill-style 'winding)
        (cairo_set_fill_rule cr CAIRO_FILL_RULE_EVEN_ODD)))
    
    (cairo-render cr border background)
    (cairo_destroy cr)
    
    img)

  (define (bitmap_regular_polygon n radius0 rotation border background radian? inscribed? density)
    (define fln (exact->inexact n))
    (define radius (if (not inscribed?) radius0 (unsafe-fl/ radius0 (unsafe-flcos (unsafe-fl/ pi fln))))) ; r = Rcos(pi/n)
    (define fllength (unsafe-fl* radius 2.0))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define r (unsafe-fl- radius (unsafe-fl* line-width 0.5)))
    (define delta (unsafe-fl/ 2pi fln))
    (define-values (img cr) (make-cairo-image fllength fllength density #true))
    
    (cairo_translate cr radius radius)
    (cairo_new_sub_path cr)
    
    (let draw-polygon ([flidx 0.0]
                       [theta (if (not radian?) (~radian rotation) rotation)])
      (when (unsafe-fl< flidx fln)
        (cairo_line_to cr (unsafe-fl* r (unsafe-flcos theta)) (unsafe-fl* r (unsafe-flsin theta)))
        (draw-polygon (unsafe-fl+ flidx 1.0) (unsafe-fl+ theta delta))))

    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)
    
    img))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap_line (-> Flonum Flonum Flonum Flonum Flonum Flonum (Option Paint) Flonum Bitmap)]
 [bitmap_arrow (-> Flonum Flonum (Option Paint) (Option Bitmap-Source) Flonum Boolean Flonum Bitmap)]
 [bitmap_circle (-> Flonum (Option Paint) (Option Bitmap-Source) Flonum Bitmap)]
 [bitmap_sector (-> Flonum Flonum Flonum (Option Paint) (Option Bitmap-Source) Flonum Boolean Bitmap)]
 [bitmap_ellipse (-> Flonum Flonum (Option Paint) (Option Bitmap-Source) Flonum Bitmap)]
 [bitmap_rectangle (-> Flonum Flonum (Option Paint) (Option Bitmap-Source) Flonum Bitmap)]
 [bitmap_rounded_rectangle (-> Flonum Flonum Real (Option Paint) (Option Bitmap-Source) Flonum Bitmap)]
 [bitmap_stadium (-> Flonum Real (Option Paint) (Option Bitmap-Source) Flonum Bitmap)]
 [bitmap_polyline (-> Flonum Flonum (Listof Flonum) (Listof Flonum) Flonum Flonum (Option Paint) (Option Bitmap-Source) (Option Symbol) Flonum Bitmap)]
 [bitmap_regular_polygon (-> Positive-Index Flonum Flonum (Option Paint) (Option Bitmap-Source) Boolean Boolean Flonum Bitmap)])
