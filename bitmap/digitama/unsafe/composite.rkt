#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "draw.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require (only-in racket/vector vector-append))
  
  (require "pangocairo.rkt")
  (require "paint.rkt")
  
  (define (bitmap_composite operator sfc1 sfc2 dx dy density)
    (define-values (w1 h1) (cairo-image-size sfc1 density))
    (define-values (w2 h2) (cairo-image-size sfc2 density))
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

  (define (bitmap_pin x1% y1% x2% y2% sfc1 sfc2 density)
    (define-values (w1 h1) (cairo-image-size sfc1 density))
    (define-values (w2 h2) (cairo-image-size sfc2 density))
    (bitmap_composite CAIRO_OPERATOR_OVER sfc1 sfc2
                      (unsafe-fl- (unsafe-fl* x1% w1) (unsafe-fl* x2% w2))
                      (unsafe-fl- (unsafe-fl* y1% h1) (unsafe-fl* y2% h2))
                      density))
  
  (define (bitmap_pin* x1% y1% x2% y2% sfc1 sfcs density)
    (define-values (min-width min-height) (cairo-image-size sfc1 density))
    (define-values (flwidth flheight all)
      (let compose ([width min-width] [height min-height] [lla (list (vector sfc1 0.0 0.0 min-width min-height))]
                                      [dx 0.0] [dy 0.0]  ; offsets passed to (bitmap_composite), also see (flomap-pin*)
                                      [width1 min-width] [height1 min-height] [children sfcs])
        (cond [(null? children) (values width height (reverse lla))]
              [else (let ([sfc2 (unsafe-car children)])
                      (define-values (width2 height2) (cairo-image-size sfc2 density))
                      (define nx (unsafe-fl+ dx (unsafe-fl- (unsafe-fl* width1 x1%) (unsafe-fl* width2 x2%))))
                      (define ny (unsafe-fl+ dy (unsafe-fl- (unsafe-fl* height1 y1%) (unsafe-fl* height2 y2%))))
                      (define-values (xoff1 yoff1) (values (unsafe-flmax (unsafe-fl- 0.0 nx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 ny) 0.0)))
                      (define-values (xoff2 yoff2) (values (unsafe-flmax nx 0.0) (unsafe-flmax ny 0.0)))
                      (compose (unsafe-flmax (unsafe-fl+ xoff1 width) (unsafe-fl+ xoff2 width2))
                               (unsafe-flmax (unsafe-fl+ yoff1 height) (unsafe-fl+ yoff2 height2))
                               (cons (vector sfc2 xoff2 yoff2 width2 height2) lla)
                               (unsafe-flmax nx 0.0) (unsafe-flmax ny 0.0) width2 height2
                               (unsafe-cdr children)))])))
    
    (define-values (bmp cr) (make-cairo-image flwidth flheight density #true))
    (let combine ([all all])
      (unless (null? all)
        (define child (unsafe-car all))
        (cairo-composite cr (unsafe-vector*-ref child 0)
                         (unsafe-vector*-ref child 1) (unsafe-vector*-ref child 2)
                         (unsafe-vector*-ref child 3) (unsafe-vector*-ref child 4)
                         CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density #true)
        (combine (unsafe-cdr all))))
    bmp)

  (define (bitmap_append alignment base others gapsize density) ; slight but more efficient than (bitmap_pin*)
    (define-values (min-width min-height) (cairo-image-size base density))
    (define-values (flwidth flheight all)
      (let compose ([width min-width] [height min-height] [lla (list (vector base min-width min-height))] [children others])
        (cond [(null? children) (values width height (reverse lla))]
              [else (let-values ([(child rest) (values (unsafe-car children) (unsafe-cdr children))])
                      (define-values (chwidth chheight) (cairo-image-size child density))
                      (define ++ (cons (vector child chwidth chheight) lla))
                      (case alignment
                        [(vl vc vr) (compose (unsafe-flmax width chwidth) (unsafe-fl+ gapsize (unsafe-fl+ height chheight)) ++ rest)]
                        [(ht hc hb) (compose (unsafe-fl+ gapsize (unsafe-fl+ width chwidth)) (unsafe-flmax height chheight) ++ rest)]
                        [else #|unreachable|# (compose (unsafe-flmax width chwidth) (unsafe-flmax height chheight) ++ rest)]))])))
    
    (define-values (bmp cr) (make-cairo-image flwidth flheight density #true))
    (let combine ([all all] [maybe-used-xoff (unsafe-fl- 0.0 gapsize)] [maybe-used-yoff (unsafe-fl- 0.0 gapsize)])
      (unless (null? all)
        (define child (unsafe-car all))
        (define-values (maybe-used-x maybe-used-y) (values (unsafe-fl+ maybe-used-xoff gapsize) (unsafe-fl+ maybe-used-yoff gapsize)))
        (define-values (chwidth chheight) (values (unsafe-vector*-ref child 1) (unsafe-vector*-ref child 2)))
        (define-values (dest-x dest-y)
          (case alignment
            [(vl) (values 0.0                                           maybe-used-y)]
            [(vc) (values (unsafe-fl/ (unsafe-fl- flwidth chwidth) 2.0) maybe-used-y)]
            [(vr) (values (unsafe-fl- flwidth chwidth)                  maybe-used-y)]
            [(ht) (values maybe-used-x                                  0.0)]
            [(hc) (values maybe-used-x                                  (unsafe-fl/ (unsafe-fl- flheight chheight) 2.0))]
            [(hb) (values maybe-used-x                                  (unsafe-fl- flheight chheight))]
            [else #|unreachable|# (values maybe-used-x                  maybe-used-y)]))
        (cairo-composite cr (unsafe-vector*-ref child 0) dest-x dest-y chwidth chheight
                         CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density #true)
        (combine (unsafe-cdr all) (unsafe-fl+ maybe-used-x chwidth) (unsafe-fl+ maybe-used-y chheight))))
    bmp)

  (define (bitmap_superimpose alignment sfcs density)
    (define-values (flwidth flheight layers)
      (let compose ([width 0.0] [height 0.0] [sreyal null] [sfcs sfcs])
        (cond [(null? sfcs) (values width height (reverse sreyal))]
              [else (let ([sfc (unsafe-car sfcs)])
                      (define-values (w h) (cairo-image-size sfc density))
                      (values (unsafe-flmax width w) (unsafe-flmax height h)
                              (cons (cons bmp (make-layer alignment w h)) layers)))])))
    
    (define-values (bmp cr) (make-cairo-image flwidth flheight density #true))
    (let combine ([all layers])
      (unless (null? all)
        (define layer (unsafe-car all))
        (define-values (x y) ((unsafe-cdr layer) flwidth flheight))
        (cairo-composite cr (unsafe-car layer) x y flwidth flheight CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density #true)
        (combine (unsafe-cdr all))))
    bmp)

  (define (bitmap_table sfcs ncols nrows alcols alrows gcols grows density)
    (define table (list->table sfcs nrows ncols density))
    (define table-ref (λ [c r] (unsafe-vector*-ref table (unsafe-fx+ (unsafe-fx* r ncols) c))))
    (define-values (pbcols pbrows)
      (values (for/vector ([c (in-range ncols)])
                (superimpose* (vector-ref alcols c)
                              (for/list ([r (in-range nrows)])
                                (table-ref c r))))
              (for/vector ([r (in-range nrows)])
                (superimpose* (vector-ref alrows r)
                              (for/list ([c (in-range ncols)])
                                (table-ref c r))))))
    
    (unless (zero? nrows)
      (vector-set! gcols (sub1 ncols) 0.0)
      (vector-set! grows (sub1 nrows) 0.0))

    (define-values (flwidth flheight cells)
      (let compose-row ([width 0.0] [height 0.0] [psfcs null] [row 0])
        (cond [(unsafe-fx= row nrows) (values width height psfcs)]
              [else (let ([pbrow (unsafe-vector*-ref pbrows row)])
                      (define hrow (unsafe-fl+ (cadr pbrow) (unsafe-vector*-ref grows row)))
                      (define-values (wcols cells)
                        (let compose-col ([xoff 0.0] [psfcs psfcs] [col 0])
                          (cond [(unsafe-fx= col ncols) (values xoff psfcs)]
                                [else (let ([cell (table-ref col row)])
                                        (define pbcol (vector-ref pbcols col))
                                        (define wcol (unsafe-fl+ (car pbcol) (unsafe-vector*-ref gcols col)))
                                        (define-values (x _y) (find-xy cell pbcol))
                                        (define-values (_x y) (find-xy cell pbrow))
                                        (compose-col (unsafe-fl+ xoff wcol)
                                                     (cons (vector cell (unsafe-fl+ x xoff) (unsafe-fl+ y height)) psfcs)
                                                     (unsafe-fx+ col 1)))])))
                      (compose-row wcols (unsafe-fl+ height hrow) cells (unsafe-fx+ row 1)))])))

    (define-values (bmp cr) (make-cairo-image flwidth flheight density #true))
    (let combine ([rest cells])
      (unless (null? rest)
        (define cell+xy (unsafe-car rest))
        (define cell+size (unsafe-vector*-ref cell+xy 0))
        (cairo-composite cr (unsafe-vector*-ref cell+size 0)
                         (unsafe-vector*-ref cell+xy 1) (unsafe-vector*-ref cell+xy 2)
                         (unsafe-vector*-ref cell+size 1) (unsafe-vector*-ref cell+size 2)
                         CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density #true)
        (combine (unsafe-cdr rest))))
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define ((make-layer alignment w h) W H) (superimpose-xy alignment W H w h))
  
  (define (superimpose-xy alignment width height w h)
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
                          (define-values (x y) (superimpose-xy alignment width height w h))
                          (locate (unsafe-cons-list (vector cell x y) cells)
                                  (unsafe-cdr rest)))]))))

  (define (find-xy sfc psfcs)
    (let find ([rest (unsafe-list-ref psfcs 2)])
      (cond [(null? rest) (values 0.0 0.0)]
            [else (let ([cell (unsafe-car rest)])
                    (if (eq? sfc (unsafe-vector*-ref cell 0))
                        (values (unsafe-vector*-ref cell 1) (unsafe-vector*-ref cell 2))
                        (find (unsafe-cdr rest))))])))

  (define (list->table sfcs nrows ncols density)
    (define cells (for/vector ([sfc (in-list sfcs)])
                    (define-values (w h) (cairo-image-size sfc density))
                    (vector sfc w h)))
    (define diff (unsafe-fx- (unsafe-fx* nrows ncols) (unsafe-vector-length cells)))
    (cond [(unsafe-fx<= diff 0) cells]
          [else (let ([filling (vector the-surface 1.0 1.0)])
                  (vector-append cells (make-vector diff filling)))])))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_composite (-> Integer Bitmap-Surface Bitmap-Surface Flonum Flonum Flonum Bitmap)]
 [bitmap_pin (-> Flonum Flonum Flonum Flonum Bitmap-Surface Bitmap-Surface Flonum Bitmap)]
 [bitmap_pin* (-> Flonum Flonum Flonum Flonum Bitmap-Surface (Listof Bitmap-Surface) Flonum Bitmap)]
 [bitmap_append (-> Symbol Bitmap-Surface (Listof Bitmap-Surface) Flonum Flonum Bitmap)]
 [bitmap_superimpose (-> Symbol (Listof Bitmap-Surface) Flonum Bitmap)]
 [bitmap_table (-> (Listof Bitmap-Surface) Integer Integer
                   (Vectorof Symbol) (Vectorof Symbol)
                   (Vectorof Flonum) (Vectorof Flonum)
                   Flonum Bitmap)])
