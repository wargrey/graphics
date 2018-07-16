#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out) CAIRO_OPERATOR_OVER)

  (require (only-in racket/list make-list))
  (require (only-in racket/vector vector-append))
  
  (require "pangocairo.rkt")
  (require "paint.rkt")
  (require (submod "convert.rkt" unsafe))
  
  (define (bitmap_composite operator sfc1 sfc2 dx dy density)
    (define-values (w1 h1) (cairo-surface-size sfc1 density))
    (define-values (w2 h2) (cairo-surface-size sfc2 density))
    (define-values (dx1 dy1) (values (unsafe-flmax (unsafe-fl- 0.0 dx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 dy) 0.0)))
    (define-values (dx2 dy2) (values (unsafe-flmax dx 0.0) (unsafe-flmax dy 0.0)))
    (define-values (img cr)
      (make-cairo-image (unsafe-flmax (unsafe-fl+ dx1 w1) (unsafe-fl+ dx2 w2))
                        (unsafe-flmax (unsafe-fl+ dy1 h1) (unsafe-fl+ dy2 h2))
                        density #true)) 
    (cairo-composite cr sfc1 dx1 dy1 w1 h1 CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_SOURCE density #true)
    (cairo-composite cr sfc2 dx2 dy2 w2 h2 CAIRO_FILTER_BILINEAR operator density #false)
    (cairo_destroy cr)
    img)

  (define (bitmap_pin operator x1% y1% x2% y2% sfc1 sfc2 density)
    (define-values (w1 h1) (cairo-surface-size sfc1 density))
    (define-values (w2 h2) (cairo-surface-size sfc2 density))
    (bitmap_composite operator sfc1 sfc2
                      (unsafe-fl- (unsafe-fl* x1% w1) (unsafe-fl* x2% w2))
                      (unsafe-fl- (unsafe-fl* y1% h1) (unsafe-fl* y2% h2))
                      density))
  
  (define (bitmap_pin* operator x1% y1% x2% y2% sfc1 sfcs density)
    (define-values (min-width min-height) (cairo-surface-size sfc1 density))
    (define-values (flwidth flheight all)
      (let compose ([width min-width] [height min-height] [lla (list (vector sfc1 0.0 0.0 min-width min-height))]
                                      [dx 0.0] [dy 0.0]  ; offsets passed to (bitmap_composite), also see (flomap-pin*)
                                      [width1 min-width] [height1 min-height] [children sfcs])
        (cond [(null? children) (values width height (reverse lla))]
              [else (let ([sfc2 (unsafe-car children)])
                      (define-values (width2 height2) (cairo-surface-size sfc2 density))
                      (define nx (unsafe-fl+ dx (unsafe-fl- (unsafe-fl* width1 x1%) (unsafe-fl* width2 x2%))))
                      (define ny (unsafe-fl+ dy (unsafe-fl- (unsafe-fl* height1 y1%) (unsafe-fl* height2 y2%))))
                      (define-values (xoff1 yoff1) (values (unsafe-flmax (unsafe-fl- 0.0 nx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 ny) 0.0)))
                      (define-values (xoff2 yoff2) (values (unsafe-flmax nx 0.0) (unsafe-flmax ny 0.0)))
                      (compose (unsafe-flmax (unsafe-fl+ xoff1 width) (unsafe-fl+ xoff2 width2))
                               (unsafe-flmax (unsafe-fl+ yoff1 height) (unsafe-fl+ yoff2 height2))
                               (unsafe-cons-list (vector sfc2 xoff2 yoff2 width2 height2) lla)
                               (unsafe-flmax nx 0.0) (unsafe-flmax ny 0.0) width2 height2
                               (unsafe-cdr children)))])))
    
    (define-values (bmp cr) (make-cairo-image flwidth flheight density #true))
    (let combine ([all all])
      (unless (null? all)
        (define child (unsafe-car all))
        (cairo-composite cr (unsafe-vector*-ref child 0)
                         (unsafe-vector*-ref child 1) (unsafe-vector*-ref child 2)
                         (unsafe-vector*-ref child 3) (unsafe-vector*-ref child 4)
                         CAIRO_FILTER_BILINEAR operator density #true)
        (combine (unsafe-cdr all))))
    bmp)

  (define (bitmap_append alignment operator base others gapsize density) ; slight but more efficient than (bitmap_pin*)
    (define-values (min-width min-height) (cairo-surface-size base density))
    (define-values (flwidth flheight all)
      (let compose ([width min-width] [height min-height] [lla (list (vector base min-width min-height))] [children others])
        (cond [(null? children) (values width height (reverse lla))]
              [else (let-values ([(child rest) (values (unsafe-car children) (unsafe-cdr children))])
                      (define-values (chwidth chheight) (cairo-surface-size child density))
                      (define ++ (unsafe-cons-list (vector child chwidth chheight) lla))
                      (case alignment
                        [(vl vc vr) (compose (unsafe-flmax width chwidth) (unsafe-fl+ gapsize (unsafe-fl+ height chheight)) ++ rest)]
                        [(ht hc hb) (compose (unsafe-fl+ gapsize (unsafe-fl+ width chwidth)) (unsafe-flmax height chheight) ++ rest)]
                        [else #|unreachable|# (compose (unsafe-flmax width chwidth) (unsafe-flmax height chheight) ++ rest)]))])))
    
    (define-values (bmp cr) (make-cairo-image flwidth flheight density #true))
    (let combine ([all all] [maybe-used-x 0.0] [maybe-used-y 0.0])
      (unless (null? all)
        (define child (unsafe-car all))
        (define-values (chwidth chheight) (values (unsafe-vector*-ref child 1) (unsafe-vector*-ref child 2)))
        (define-values (dest-x dest-y)
          (case alignment
            [(vl) (values 0.0                                           maybe-used-y)]
            [(vc) (values (unsafe-fl* (unsafe-fl- flwidth chwidth) 0.5) maybe-used-y)]
            [(vr) (values (unsafe-fl- flwidth chwidth)                  maybe-used-y)]
            [(ht) (values maybe-used-x                                  0.0)]
            [(hc) (values maybe-used-x                                  (unsafe-fl* (unsafe-fl- flheight chheight) 0.5))]
            [(hb) (values maybe-used-x                                  (unsafe-fl- flheight chheight))]
            [else #|unreachable|# (values maybe-used-x                  maybe-used-y)]))
        (cairo-composite cr (unsafe-vector*-ref child 0) dest-x dest-y chwidth chheight CAIRO_FILTER_BILINEAR operator density #true)
        (combine (unsafe-cdr all)
                 (unsafe-fl+ maybe-used-x (unsafe-fl+ chwidth gapsize))
                 (unsafe-fl+ maybe-used-y (unsafe-fl+ chheight gapsize)))))
    bmp)

  (define (bitmap_superimpose alignment operator sfcs density)
    (define-values (flwidth flheight layers)
      (let compose ([width 0.0] [height 0.0] [sreyal null] [sfcs sfcs])
        (cond [(null? sfcs) (values width height (reverse sreyal))]
              [else (let ([sfc (unsafe-car sfcs)])
                      (define-values (w h) (cairo-surface-size sfc density))
                      (compose (unsafe-flmax width w)
                               (unsafe-flmax height h)
                               (unsafe-cons-list (cons sfc (make-layer alignment w h)) sreyal)
                               (unsafe-cdr sfcs)))])))
    
    (define-values (bmp cr) (make-cairo-image flwidth flheight density #true))
    (let combine ([all layers])
      (unless (null? all)
        (define layer (unsafe-car all))
        (define-values (x y) ((unsafe-cdr layer) flwidth flheight))
        (cairo-composite cr (unsafe-car layer) x y flwidth flheight CAIRO_FILTER_BILINEAR operator density #true)
        (combine (unsafe-cdr all))))
    bmp)

  (define (bitmap_table sfcs ncols nrows col-aligns row-aligns col-gaps row-gaps density)
    (define alcols (list->n:vector col-aligns ncols))
    (define alrows (list->n:vector row-aligns nrows))
    (define gcols (list->n:vector (map real->double-flonum col-gaps) ncols))
    (define grows (list->n:vector (map real->double-flonum row-gaps) nrows))
    (define table (list->table sfcs nrows ncols density))
    (define table-ref (λ [c r] (unsafe-vector*-ref table (unsafe-fx+ (unsafe-fx* r ncols) c))))
    (define-values (pbcols pbrows)
      (values (for/vector ([c (in-range ncols)])
                (superimpose* (unsafe-vector*-ref alcols c)
                              (let ++ ([swor null] [r 0])
                                (cond [(unsafe-fx= r nrows) swor]
                                      [else (++ (unsafe-cons-list (table-ref c r) swor)
                                                (unsafe-fx+ r 1))]))))
              (for/vector ([r (in-range nrows)])
                (superimpose* (unsafe-vector*-ref alrows r)
                              (let ++ ([sloc null] [c 0])
                                (cond [(unsafe-fx= c ncols) sloc]
                                      [else (++ (unsafe-cons-list (table-ref c r) sloc)
                                                (unsafe-fx+ c 1))]))))))
    
    (unsafe-vector*-set! gcols (unsafe-fx- ncols 1) 0.0)
    (unsafe-vector*-set! grows (unsafe-fx- nrows 1) 0.0)

    (define-values (flwidth flheight)
      (let compose-row ([width 0.0] [height 0.0] [row 0])
        (cond [(unsafe-fx= row nrows) (values width height)]
              [else (let ([pbrow (unsafe-vector*-ref pbrows row)])
                      (define hrow (unsafe-fl+ (unsafe-car (unsafe-cdr pbrow)) (unsafe-vector*-ref grows row)))
                      (let compose-col ([xoff 0.0] [col 0])
                        (cond [(unsafe-fx= col ncols) (compose-row xoff (unsafe-fl+ height hrow) (unsafe-fx+ row 1))]
                              [else (let ([cell (table-ref col row)])
                                      (define pbcol (unsafe-vector*-ref pbcols col))
                                      (define wcol (unsafe-fl+ (unsafe-car pbcol) (unsafe-vector*-ref gcols col)))
                                      (define-values (x y) (values (find-position cell pbcol 1) (find-position cell pbrow 2)))
                                      (unsafe-vector*-set! cell 1 (unsafe-fl+ x xoff))
                                      (unsafe-vector*-set! cell 2 (unsafe-fl+ y height))
                                      (compose-col (unsafe-fl+ xoff wcol) (unsafe-fx+ col 1)))])))])))

    (define-values (bmp cr) (make-cairo-image flwidth flheight density #true))
    (let combine ([idx (unsafe-fx- (unsafe-fx* ncols nrows) 1)])
      (when (unsafe-fx>= idx 0)
        (define cell (unsafe-vector*-ref table idx))
        (cairo-composite cr (unsafe-vector*-ref cell 0) (unsafe-vector*-ref cell 1) (unsafe-vector*-ref cell 2)
                         flwidth flheight CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density #true)
        (combine (unsafe-fx- idx 1))))
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define ((make-layer alignment w h) W H) (superimpose-position alignment W H w h))
  
  (define (superimpose-position alignment width height w h)
    (define-values (rx by) (values (unsafe-fl- width w) (unsafe-fl- height h)))
    (define-values (cx cy) (values (unsafe-fl/ rx 2.0) (unsafe-fl/ by 2.0)))
    (case alignment
      [(lt) (values 0.0 0.0)] [(lc) (values 0.0 cy)] [(lb) (values 0.0 by)]
      [(ct) (values  cx 0.0)] [(cc) (values  cx cy)] [(cb) (values  cx by)]
      [(rt) (values  rx 0.0)] [(rc) (values  rx cy)] [(rb) (values  rx by)]
      [else #|unreachable|# (values 0.0 0.0)]))

  (define (superimpose* alignment sllec)
    (define-values (width height)
      (let compose ([width 0.0] [height 0.0] [rest sllec])
        (cond [(null? rest) (values width height)]
              [else (let ([cell (unsafe-car rest)])
                      (compose (unsafe-flmax width (unsafe-vector*-ref cell 1))
                               (unsafe-flmax height (unsafe-vector*-ref cell 2))
                               (unsafe-cdr rest)))])))
    (list width height
          (let locate ([cells null] [rest sllec])
            (cond [(null? rest) cells]
                  [else (let ([cell (unsafe-car rest)])
                          (define-values (w h) (values (unsafe-vector*-ref cell 1) (unsafe-vector*-ref cell 2)))
                          (define-values (x y) (superimpose-position alignment width height w h))
                          (locate (unsafe-cons-list (vector cell x y) cells)
                                  (unsafe-cdr rest)))]))))

  (define (find-position sfc psfcs which)
    (let find ([rest (unsafe-list-ref psfcs 2)])
      (cond [(null? rest) (values 0.0 0.0)]
            [else (let ([cell (unsafe-car rest)])
                    (if (eq? sfc (unsafe-vector*-ref cell 0))
                        (unsafe-vector*-ref cell which)
                        (find (unsafe-cdr rest))))])))

  (define (list->table sfcs nrows ncols density)
    (define cells (for/vector ([sfc (in-list sfcs)])
                    (define-values (w h) (cairo-surface-size sfc density))
                    (vector sfc w h)))
    (define diff (unsafe-fx- (unsafe-fx* nrows ncols) (unsafe-vector-length cells)))
    (cond [(unsafe-fx<= diff 0) cells]
          [else (let ([filling (vector the-surface 1.0 1.0)])
                  (vector-append cells (make-vector diff filling)))]))

  (define (list->n:vector src total) ; NOTE: (length src) is usually very small.
    (define count (length src))
    (cond [(unsafe-fx= total count) (list->vector src)]
          [else (let ([supplement (unsafe-list-ref src (unsafe-fx- count 1))])
                  (list->vector (append src (make-list (unsafe-fx- total count) supplement))))])))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [CAIRO_OPERATOR_OVER Integer]
 [bitmap_composite (-> Integer Bitmap-Surface Bitmap-Surface Flonum Flonum Flonum Bitmap)]
 [bitmap_pin (-> Integer Flonum Flonum Flonum Flonum Bitmap-Surface Bitmap-Surface Flonum Bitmap)]
 [bitmap_pin* (-> Integer Flonum Flonum Flonum Flonum Bitmap-Surface (Listof Bitmap-Surface) Flonum Bitmap)]
 [bitmap_append (-> Symbol Integer Bitmap-Surface (Listof Bitmap-Surface) Flonum Flonum Bitmap)]
 [bitmap_superimpose (-> Symbol Integer (Listof Bitmap-Surface) Flonum Bitmap)]
 [bitmap_table (-> (Listof Bitmap-Surface) Integer Integer (Listof Symbol) (Listof Symbol) (Listof Flonum) (Listof Flonum) Flonum Bitmap)])
