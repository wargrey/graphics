#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require geofun/digitama/unsafe/visual/ctype)

(require "../convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require geofun/digitama/unsafe/pangocairo)
  
  (require "../convert.rkt")
  (require "pixman.rkt")
  (require (submod "bitmap.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (λbitmap width height density λargb)
    (define-values (img cr) (create-argb-bitmap width height density #false))
    (define surface (cairo_get_target cr))
    (define-values (pixels _ stride w h) (bitmap-surface-metrics surface 4))

    (cairo_surface_flush surface)
    (let y-loop ([y 0])
      (when (unsafe-fx< y h)
        (let x-loop ([x 0] [idx (unsafe-fx* y stride)])
          (when (unsafe-fx< x w)
            (define-values (a r g b) (λargb x y w h))
            (unless (and (unsafe-fl<= a 0.0) (unsafe-fl<= r 0.0) (unsafe-fl<= g 0.0) (unsafe-fl<= b 0.0))
              (pixels-set-argb-flonums pixels idx a r g b))
            (x-loop (unsafe-fx+ x 1) (unsafe-fx+ idx 4))))
        (y-loop (unsafe-fx+ y 1))))

    (cairo_surface_mark_dirty surface)
    (cairo_destroy cr)
    img)

  (define (λbitmap* width height density λargb initial)
    (define-values (img cr) (create-argb-bitmap width height density #false))
    (define surface (cairo_get_target cr))
    (define-values (pixels _ stride w h) (bitmap-surface-metrics surface 4))
    (cairo_surface_flush surface)

    (let y-loop ([y 0]
                 [datum0 initial])
      (if (unsafe-fx< y h)

          (let x-loop ([x 0]
                       [idx (unsafe-fx* y stride)]
                       [datum datum0])
            (if (unsafe-fx< x w)
                (let-values ([(a r g b datum++) (λargb x y w h datum)])
                  (unless (and (unsafe-fl<= a 0.0) (unsafe-fl<= r 0.0) (unsafe-fl<= g 0.0) (unsafe-fl<= b 0.0))
                    (pixels-set-argb-flonums pixels idx a r g b))
                  (x-loop (unsafe-fx+ x 1) (unsafe-fx+ idx 4) datum++))
                (y-loop (unsafe-fx+ y 1) datum)))

          (let ()
            (cairo_surface_mark_dirty surface)
            (cairo_destroy cr)
            (values img datum0)))))

  (define (λbitmap_step width height density λargb initial)
    (define-values (img cr) (create-argb-bitmap width height density #false))
    (define surface (cairo_get_target cr))
    (define-values (pixels _ stride w h) (bitmap-surface-metrics surface 4))
    
    (cairo_surface_flush surface)
    (let step ([datum initial])
      (define-values (x y a r g b datum++) (λargb w h datum))

      (cond [(or (not x) (not y))
             (cairo_surface_mark_dirty surface)
             (cairo_destroy cr)
             (values img datum++)]
            [(and (fixnum? x) (fixnum? y) (unsafe-fx>= x 0) (unsafe-fx< x w) (unsafe-fx>= y 0) (unsafe-fx< y h))
             (pixels-set-argb-flonums pixels (unsafe-fx+ (unsafe-fx* y stride) (unsafe-fx* x 4)) a r g b)
             (step datum++)]
            [else (step datum++)])))

  (define (λbitmap_map src density argb-map)
    (define-values (flwidth flheight) (bitmap-surface-rendered-size src density))
    (define-values (img cr) (create-argb-bitmap flwidth flheight density #false))
    (define surface (cairo_get_target cr))
    (define-values (data total) (bitmap-surface-data src))
    (define-values (pixels _ stride w h) (bitmap-surface-metrics surface 4))
    
    (cairo_surface_flush surface)

    (let y-loop ([y 0])
      (when (unsafe-fx< y h)
        (let x-loop ([x 0] [idx (unsafe-fx* y stride)])
          (when (unsafe-fx< x w)
            (let*-values ([(A R G B) (pixels-get-argb-bytes data idx)]
                          [(a r g b) (argb-map x y w h A R G B)])
              (unless (and (unsafe-fl<= a 0.0) (unsafe-fl<= r 0.0) (unsafe-fl<= g 0.0) (unsafe-fl<= b 0.0))
                (pixels-set-argb-flonums pixels idx a r g b))
              (x-loop (unsafe-fx+ x 1) (unsafe-fx+ idx 4)))))
        (y-loop (unsafe-fx+ y 1))))
    
    (cairo_surface_mark_dirty surface)
    (cairo_destroy cr)
    img))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type ARGB-Map (-> Index Index Index Index Byte Byte Byte Byte (Values Flonum Flonum Flonum Flonum)))
(define-type XYWH->ARGB (-> Index Index Index Index (Values Flonum Flonum Flonum Flonum)))
(define-type (XYWH->ARGB* t) (-> Index Index Index Index t (Values Flonum Flonum Flonum Flonum t)))
(define-type (ARGB-Step t) (-> Index Index t (Values (Option Integer) (Option Integer) Flonum Flonum Flonum Flonum t)))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [λbitmap (-> Flonum Flonum Flonum XYWH->ARGB Bitmap)]
 [λbitmap* (All (t) (-> Flonum Flonum Flonum (XYWH->ARGB* t) t (Values Bitmap t)))]
 [λbitmap_step (All (t) (-> Flonum Flonum Flonum (ARGB-Step t) t (Values Bitmap t)))]
 [λbitmap_map (-> Bitmap-Surface Flonum ARGB-Map Bitmap)])
