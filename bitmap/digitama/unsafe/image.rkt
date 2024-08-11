#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require geofun/digitama/base)
(require geofun/digitama/unsafe/source)
(require geofun/digitama/unsafe/visual/ctype)

(require "../convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require geofun/digitama/unsafe/pangocairo)
  (require geofun/digitama/unsafe/paint)

  (require "../convert.rkt")
  (require (submod "pixman.rkt" unsafe))
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
    img)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_blank width height density)
    (define-values (img cr) (create-argb-bitmap width height density #true))
    (cairo_destroy cr)
    img)

  (define (bitmap_pattern width height background density)
    (define-values (img cr) (create-argb-bitmap width height density #true))
    (cairo-render-background cr background)
    (cairo_destroy cr)
    img)

  (define (bitmap_frame src mtop mright mbottom mleft ptop pright pbottom pleft border background density)
    (define line-width (~bdwidth border))
    (define line-inset (unsafe-fl/ line-width 2.0))
    (define-values (dest-width dest-height) (bitmap-surface-rendered-size src density))
    (define-values (width border-x border-width dest-x) (frame-metrics line-width line-inset mleft mright pleft pright dest-width))
    (define-values (height border-y border-height dest-y) (frame-metrics line-width line-inset mtop mbottom ptop pbottom dest-height))
    (define-values (img cr) (create-argb-bitmap width height density #true))
    (cairo_rectangle cr border-x border-y border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src dest-x dest-y dest-width dest-height CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density)
    (cairo_destroy cr)
    img)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (frame-metrics line-width line-inset flmopen flmclose flpopen flpclose size)
    (define border-position (unsafe-fl+ flmopen line-inset))
    (define position (unsafe-fl+ (unsafe-fl+ flmopen flpopen) line-width))
    (define border-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ flpopen flpclose) size) line-width))
    (define frame-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ border-position border-size) flmclose) line-inset))
    (values frame-size border-position border-size position))

  (define (list->4:values ls defval)
    (case (length ls)
      [(0) (values defval defval defval defval)]
      [(1) (let ([top (unsafe-car ls)]) (values top top top top))]
      [(2) (let ([top (unsafe-car ls)] [right (unsafe-car (unsafe-cdr ls))]) (values top right top right))]
      [(3) (let ([top (unsafe-car ls)] [right (unsafe-list-ref ls 1)] [bottom (unsafe-list-ref ls 2)]) (values top right bottom right))]
      [else (values (unsafe-car ls) (unsafe-list-ref ls 1) (unsafe-list-ref ls 2) (unsafe-list-ref ls 3))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type ARGB-Map (-> Index Index Index Index Byte Byte Byte Byte (Values Flonum Flonum Flonum Flonum)))
(define-type XYWH->ARGB (-> Index Index Index Index (Values Flonum Flonum Flonum Flonum)))
(define-type (XYWH->ARGB* t) (-> Index Index Index Index t (Values Flonum Flonum Flonum Flonum t)))
(define-type (ARGB-Step t) (-> Index Index t (Values (Option Integer) (Option Integer) Flonum Flonum Flonum Flonum t)))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [list->4:values (All (a) (-> (Listof a) a (Values a a a a)))]
 [λbitmap (-> Flonum Flonum Flonum XYWH->ARGB Bitmap)]
 [λbitmap* (All (t) (-> Flonum Flonum Flonum (XYWH->ARGB* t) t (Values Bitmap t)))]
 [λbitmap_step (All (t) (-> Flonum Flonum Flonum (ARGB-Step t) t (Values Bitmap t)))]
 [λbitmap_map (-> Bitmap-Surface Flonum ARGB-Map Bitmap)]
 [bitmap_blank (-> Flonum Flonum Flonum Bitmap)]
 [bitmap_pattern (-> Flonum Flonum Fill-Source Flonum Bitmap)]
 [bitmap_frame (-> Bitmap-Surface
                   Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                   Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                   (Option Paint) (Option Fill-Source) Flonum Bitmap)])
