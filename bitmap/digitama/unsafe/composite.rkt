#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require geofun/digitama/unsafe/visual/ctype)
(require "../convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require (only-in racket/list make-list))
  (require (only-in racket/vector vector-append))
  (require (only-in racket/flonum make-flvector))

  (require geofun/digitama/unsafe/pangocairo)
  (require geofun/digitama/unsafe/surface/image)
  (require geofun/digitama/unsafe/paint)

  (require "../convert.rkt")
  
  (require (submod "bitmap.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_composite operator sfc1 sfc2 dx dy density)
    (define-values (w1 h1) (bitmap-surface-rendered-size sfc1 density))
    (define-values (w2 h2) (bitmap-surface-rendered-size sfc2 density))
    (define-values (dx1 dy1) (values (unsafe-flmax (unsafe-fl- 0.0 dx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 dy) 0.0)))
    (define-values (dx2 dy2) (values (unsafe-flmax dx 0.0) (unsafe-flmax dy 0.0)))
    (define-values (img cr)
      (create-argb-bitmap (unsafe-flmax (unsafe-fl+ dx1 w1) (unsafe-fl+ dx2 w2))
                          (unsafe-flmax (unsafe-fl+ dy1 h1) (unsafe-fl+ dy2 h2))
                          density #true))

    (cairo-composite cr sfc1 dx1 dy1 w1 h1 CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_SOURCE density)
    (cairo-composite cr sfc2 dx2 dy2 w2 h2 CAIRO_FILTER_BILINEAR (or operator CAIRO_OPERATOR_OVER) density)
    
    (cairo_destroy cr)
    img)

  (define (bitmap_pin operator x1% y1% x2% y2% sfc1 sfc2 density)
    (define-values (w1 h1) (bitmap-surface-rendered-size sfc1 density))
    (define-values (w2 h2) (bitmap-surface-rendered-size sfc2 density))

    (bitmap_composite operator sfc1 sfc2
                      (unsafe-fl- (unsafe-fl* x1% w1) (unsafe-fl* x2% w2))
                      (unsafe-fl- (unsafe-fl* y1% h1) (unsafe-fl* y2% h2))
                      density))
  
  (define (bitmap_pin* operator0 x1% y1% x2% y2% sfc1 sfcs density)
    (define-values (min-width min-height) (bitmap-surface-rendered-size sfc1 density))
    (define-values (flwidth flheight xoff yoff all)
      (let compose ([lla (list (vector sfc1 0.0 0.0 min-width min-height))]
                    [dx 0.0] [dy 0.0] [width1 min-width] [height1 min-height]
                    [lx 0.0] [ty 0.0] [rx min-width] [by min-height]
                    [children sfcs])
        (cond [(pair? children)
               (let ([sfc2 (unsafe-car children)])
                 (define-values (width2 height2) (bitmap-surface-rendered-size sfc2 density))
                 (define nx (unsafe-fl+ dx (unsafe-fl- (unsafe-fl* width1 x1%) (unsafe-fl* width2 x2%))))
                 (define ny (unsafe-fl+ dy (unsafe-fl- (unsafe-fl* height1 y1%) (unsafe-fl* height2 y2%))))
                 
                 (compose (unsafe-cons-list (vector sfc2 nx ny width2 height2) lla)
                          nx ny width2 height2
                          (unsafe-flmin lx nx) (unsafe-flmin ty ny)
                          (unsafe-flmax rx (unsafe-fl+ nx width2))
                          (unsafe-flmax by (unsafe-fl+ ny height2))
                          (unsafe-cdr children)))]
              [else (values (unsafe-flmax rx min-width) (unsafe-flmax by min-height)
                            (if (unsafe-fl< lx 0.0) (unsafe-fl- 0.0 lx) 0.0)
                            (if (unsafe-fl< ty 0.0) (unsafe-fl- 0.0 ty) 0.0)
                            (reverse lla))])))
    
    (define-values (bmp cr) (create-argb-bitmap (unsafe-fl+ flwidth xoff) (unsafe-fl+ flheight yoff) density #true))
    (define operator (or operator0 CAIRO_OPERATOR_OVER))
    (let combine ([all all])
      (unless (null? all)
        (define child (unsafe-car all))
        (cairo-composite cr (unsafe-vector*-ref child 0)
                         (unsafe-fl+ (unsafe-vector*-ref child 1) xoff)
                         (unsafe-fl+ (unsafe-vector*-ref child 2) yoff)
                         (unsafe-vector*-ref child 3) (unsafe-vector*-ref child 4)
                         CAIRO_FILTER_BILINEAR operator density)
        (combine (unsafe-cdr all))))
    bmp)

  (define (bitmap_append alignment operator0 base others gapsize density) ; slightly but more efficient than (bitmap_pin*)
    (define-values (min-width min-height) (bitmap-surface-rendered-size base density))
    (define-values (lx ty rx by all)
      (let compose ([lla (list (vector base min-width min-height 0.0 0.0))]
                    [lx 0.0] [ty 0.0] [rx min-width] [by min-height]
                    [x (unsafe-fl+ min-width gapsize)] [y (unsafe-fl+ min-height gapsize)]
                    [children others])
        (cond [(null? children) (values lx ty rx by (reverse lla))]
              [else (let-values ([(child rest) (values (unsafe-car children) (unsafe-cdr children))])
                      (define-values (chwidth chheight) (bitmap-surface-rendered-size child density))
                      (case alignment
                        [(vl vc vr)
                         (let ([delta (unsafe-fl+ chheight gapsize)])
                           (compose (unsafe-cons-list (vector child chwidth chheight x y) lla)
                                    lx (unsafe-flmin ty y) (unsafe-flmax rx chwidth) (unsafe-flmax by (unsafe-fl+ y chheight))
                                    x (unsafe-fl+ y delta) rest))]
                        [(ht hc hb)
                         (let ([delta (unsafe-fl+ chwidth gapsize)])
                           (compose (unsafe-cons-list (vector child chwidth chheight x y) lla)
                                    (unsafe-flmin lx x) ty (unsafe-flmax rx (unsafe-fl+ x chwidth)) (unsafe-flmax by chheight)
                                    (unsafe-fl+ x delta) y rest))]
                        [else #| deadcode |# (compose lla lx ty rx by x y rest)]))])))

    (define flwidth (unsafe-fl- rx lx))
    (define flheight (unsafe-fl- by ty))    
    (define xoff (if (unsafe-fl< lx 0.0) (unsafe-fl- lx) 0.0))
    (define yoff (if (unsafe-fl< ty 0.0) (unsafe-fl- ty) 0.0))
    
    (define-values (bmp cr) (create-argb-bitmap flwidth flheight density #true))
    (define operator (or operator0 CAIRO_OPERATOR_OVER))
    (let combine ([all all])
      (unless (null? all)
        (define child (unsafe-car all))
        (define-values (chwidth chheight) (values (unsafe-vector*-ref child 1) (unsafe-vector*-ref child 2)))
        (define-values (maybe-x maybe-y) (values (unsafe-vector*-ref child 3) (unsafe-vector*-ref child 4)))
        (define-values (dest-x dest-y)
          (case alignment
            [(vl) (values 0.0                                           maybe-y)]
            [(vc) (values (unsafe-fl* (unsafe-fl- flwidth chwidth) 0.5) maybe-y)]
            [(vr) (values (unsafe-fl- flwidth chwidth)                  maybe-y)]
            [(ht) (values maybe-x                                       0.0)]
            [(hc) (values maybe-x                                       (unsafe-fl* (unsafe-fl- flheight chheight) 0.5))]
            [(hb) (values maybe-x                                       (unsafe-fl- flheight chheight))]
            [else #| deadcode |# (values maybe-x                        maybe-y)]))
        (cairo-composite cr (unsafe-vector*-ref child 0)
                         (unsafe-fl+ dest-x xoff) (unsafe-fl+ dest-y yoff)
                         chwidth chheight CAIRO_FILTER_BILINEAR operator density)
        (combine (unsafe-cdr all))))
    bmp)

  (define (bitmap_superimpose alignment operator0 sfcs density)
    (define-values (flwidth flheight layers) (list->layers alignment sfcs density))
    (define-values (bmp cr) (create-argb-bitmap flwidth flheight density #true))
    (define operator (or operator0 CAIRO_OPERATOR_OVER))
    (let combine ([all layers])
      (unless (null? all)
        (define layer (unsafe-car all))
        (define-values (x y) ((unsafe-cdr layer) flwidth flheight))
        (cairo-composite cr (unsafe-car layer) x y flwidth flheight CAIRO_FILTER_BILINEAR operator density)
        (combine (unsafe-cdr all))))
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_table sfcs ncols nrows col-aligns row-aligns col-gaps row-gaps density)
    (define alcols (list->n:vector col-aligns ncols))
    (define alrows (list->n:vector row-aligns nrows))
    (define gcols (list->n:vector col-gaps ncols))
    (define grows (list->n:vector row-gaps nrows))
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
      (let compose-row ([width 0.0]
                        [height 0.0]
                        [row 0])
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

    (define-values (bmp cr) (create-argb-bitmap flwidth flheight density #true))
    (let combine ([idx (unsafe-fx- (unsafe-fx* ncols nrows) 1)])
      (when (unsafe-fx>= idx 0)
        (define cell (unsafe-vector*-ref table idx))
        (cairo-composite cr (unsafe-vector*-ref cell 0) (unsafe-vector*-ref cell 1) (unsafe-vector*-ref cell 2)
                         flwidth flheight CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density)
        (combine (unsafe-fx- idx 1))))
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_pyramid sfcs sub-gaps sibling-gaps node-aligns density)
    (define n (length sfcs))
    (define depth (unsafe-fx->fl (pyramid-depth n)))
    (define nleaves (unsafe-fl+ depth 1.0))
    (define-values (nwidth nheight nodes) (list->layers* (list->n:vector node-aligns n) sfcs density))
    (define flwidth (unsafe-fl+ (unsafe-fl* nwidth nleaves) (unsafe-fl* sibling-gaps (unsafe-fl- nleaves 1.0))))
    (define flheight (unsafe-fl+ (unsafe-fl* nheight (unsafe-fl+ depth 1.0)) (unsafe-fl* sub-gaps depth)))
    (define 2translate (unsafe-fl- flwidth nwidth))
    (define xtranslate (unsafe-fl* 2translate 0.5))
    (define xstep (unsafe-fl* (unsafe-fl+ nwidth sibling-gaps) 0.5))
    (define ystep (unsafe-fl+ nheight sub-gaps))
    (define dx (unsafe-fl+ nwidth sibling-gaps))
    
    (define-values (bmp cr) (create-argb-bitmap flwidth flheight density #true))

    ;;; TODO
    ; Arithmetic here only involves in addition and subtraction,
    ; working with flonums directly seems do not lose precision.
    
    (let combine ([nodes nodes]
                  [y0 0.0]
                  [x0 xtranslate]
                  [boundary xtranslate])
      (unless (null? nodes)
        (define node (unsafe-car nodes))
        (define-values (xoff yoff) ((unsafe-cdr node) nwidth nheight))
        (cairo-composite cr (unsafe-car node) (unsafe-fl+ x0 xoff) (unsafe-fl+ y0 yoff)
                         flwidth flheight CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density)
        (if (unsafe-fl< x0 boundary)
            (combine (unsafe-cdr nodes) y0 (unsafe-fl+ x0 dx) boundary)
            (let ([boundary++ (unsafe-fl+ boundary xstep)])
              (combine (unsafe-cdr nodes) (unsafe-fl+ y0 ystep)
                       (unsafe-fl- 2translate boundary++)  ; (t - [b - t]) = 2t - b
                       boundary++)))))
    bmp)

  (define (bitmap_heap sfcs ary sub-gaps sibling-gaps node-aligns density)
    (define n (length sfcs))
    (define ary-1 (unsafe-fx- ary 1))
    (define flary (unsafe-fx->fl ary))
    (define depth (unsafe-flfloor (log (unsafe-fx* n ary-1) ary)))
    (define nleaves (unsafe-flexpt flary depth))
    (define-values (nwidth nheight nodes) (list->layers* (list->n:vector node-aligns n) sfcs density))
    (define flwidth (unsafe-fl+ (unsafe-fl* nwidth nleaves) (unsafe-fl* sibling-gaps (unsafe-fl- nleaves 1.0))))
    (define flheight (unsafe-fl+ (unsafe-fl* nheight (unsafe-fl+ depth 1.0)) (unsafe-fl* sub-gaps depth)))
    (define xstep (unsafe-fl+ nwidth sibling-gaps))
    (define ystep (unsafe-fl+ nheight sub-gaps))

    ; precompute x positions
    (define leaf0-idx (unsafe-fxquotient (unsafe-fx- (unsafe-fl->fx (unsafe-flexpt flary depth)) 1) ary-1)) ; N = (m ^ (h + 1) - 1) / (m - 1)
    (define vs (make-flvector (unsafe-fx+ leaf0-idx (unsafe-fl->fx nleaves)) 0.0))

    (let location-precompute ([pos 0])
      (cond [(unsafe-fx< pos leaf0-idx)
             (let* ([child-start (unsafe-fx+ (unsafe-fx* pos ary) 1)]
                    [child-end (unsafe-fx+ child-start ary)])
               (let locate-child ([child-idx child-start]
                                  [x-acc 0.0])
                 (if (unsafe-fx< child-idx child-end)
                     (let ([x (location-precompute child-idx)])
                       (locate-child (unsafe-fx+ child-idx 1) (unsafe-fl+ x-acc x)))
                     (let ([x (unsafe-fl/ x-acc flary)])
                       (unsafe-flvector-set! vs pos x) x))))]
            [(unsafe-fx> pos leaf0-idx)
             (let ([x (unsafe-fl* xstep (unsafe-fx->fl (unsafe-fx- pos leaf0-idx)))])
               (unsafe-flvector-set! vs pos x) x)]
            [else 0.0 #| the first leaf is already located at 0 |#]))
    
    (define-values (bmp cr) (create-argb-bitmap flwidth flheight density #true))
    (let combine ([nodes nodes]
                  [y0 0.0]
                  [idx 0]
                  [boundary 1]
                  [siblings 1])
      (unless (null? nodes)
        (define node (unsafe-car nodes))
        (define-values (xoff yoff) ((unsafe-cdr node) nwidth nheight))
        (define idx++ (unsafe-fx+ idx 1))

        (cairo-composite cr (unsafe-car node) (unsafe-fl+ (unsafe-flvector-ref vs idx) xoff) (unsafe-fl+ y0 yoff)
                         flwidth flheight CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density)

        (if (unsafe-fx< idx++ boundary)
            (combine (unsafe-cdr nodes) y0 idx++ boundary siblings)
            (let ([siblings++ (unsafe-fx* siblings ary)])
              (combine (unsafe-cdr nodes) (unsafe-fl+ y0 ystep) idx++
                       (unsafe-fx+ boundary siblings++) siblings++)))))
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define ((make-layer alignment w h) W H) (superimpose-position alignment W H w h))
  
  (define (superimpose-position alignment width height w h)
    (define-values (rx by) (values (unsafe-fl- width w) (unsafe-fl- height h)))
    (define-values (cx cy) (values (unsafe-fl* rx 0.5) (unsafe-fl* by 0.5)))
    (case alignment
      [(lt) (values 0.0 0.0)] [(lc) (values 0.0 cy)] [(lb) (values 0.0 by)]
      [(ct) (values  cx 0.0)] [(cc) (values  cx cy)] [(cb) (values  cx by)]
      [(rt) (values  rx 0.0)] [(rc) (values  rx cy)] [(rb) (values  rx by)]
      [else #| deadcode |# (values 0.0 0.0)]))

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
                    (define-values (w h) (bitmap-surface-rendered-size sfc density))
                    (vector sfc w h)))
    (define diff (unsafe-fx- (unsafe-fx* nrows ncols) (unsafe-vector*-length cells)))
    (cond [(unsafe-fx<= diff 0) cells]
          [else (let ([filling (vector the-image-surface 1.0 1.0)])
                  (vector-append cells (make-vector diff filling)))]))

  (define (list->n:vector src total)
    ; NOTE: `src` is usually very small.
    (define count (length src))
    (cond [(unsafe-fx= total count) (list->vector src)]
          [else (let ([supplement (unsafe-list-ref src (unsafe-fx- count 1))])
                  (list->vector (append src (make-list (unsafe-fx- total count) supplement))))]))

  (define (list->layers alignment sfcs density)
    (let compose ([width 0.0]
                  [height 0.0]
                  [sreyal null]
                  [sfcs sfcs])
      (cond [(null? sfcs) (values width height (reverse sreyal))]
            [else (let ([sfc (unsafe-car sfcs)])
                    (define-values (w h) (bitmap-surface-rendered-size sfc density))
                    (compose (unsafe-flmax width w)
                             (unsafe-flmax height h)
                             (unsafe-cons-list (cons sfc (make-layer alignment w h)) sreyal)
                             (unsafe-cdr sfcs)))])))

  (define (list->layers* aligns sfcs density)
    (let compose ([width 0.0]
                  [height 0.0]
                  [sreyal null]
                  [sfcs sfcs]
                  [aidx 0])
      (cond [(null? sfcs) (values width height (reverse sreyal))]
            [else (let ([sfc (unsafe-car sfcs)])
                    (define-values (w h) (bitmap-surface-rendered-size sfc density))
                    (compose (unsafe-flmax width w)
                             (unsafe-flmax height h)
                             (unsafe-cons-list (cons sfc (make-layer (unsafe-vector*-ref aligns aidx) w h)) sreyal)
                             (unsafe-cdr sfcs)
                             (unsafe-fx+ aidx 1)))])))

  (define (pyramid-depth n)
    (let step ([depth 0] ; the root is in the 0th layer
               [sibling 1]
               [rest n])
      (cond [(unsafe-fx<= rest sibling) depth]
            [else (step (unsafe-fx+ depth 1)
                        (unsafe-fx+ sibling 1)
                        (unsafe-fx- rest sibling))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap_composite (-> (Option Integer) Bitmap-Surface Bitmap-Surface Flonum Flonum Flonum Bitmap)]
 [bitmap_pin (-> (Option Integer) Flonum Flonum Flonum Flonum Bitmap-Surface Bitmap-Surface Flonum Bitmap)]
 [bitmap_pin* (-> (Option Integer) Flonum Flonum Flonum Flonum Bitmap-Surface (Listof Bitmap-Surface) Flonum Bitmap)]
 [bitmap_append (-> Symbol (Option Integer) Bitmap-Surface (Listof Bitmap-Surface) Flonum Flonum Bitmap)]
 [bitmap_superimpose (-> Symbol (Option Integer) (Listof Bitmap-Surface) Flonum Bitmap)]
 [bitmap_table (-> (Listof Bitmap-Surface) Integer Integer (Listof Symbol) (Listof Symbol) (Listof Flonum) (Listof Flonum) Flonum Bitmap)]
 [bitmap_heap (-> (Listof Bitmap-Surface) Positive-Index Flonum Flonum (Listof Symbol) Flonum Bitmap)]
 [bitmap_pyramid (-> (Listof Bitmap-Surface) Flonum Flonum (Listof Symbol) Flonum Bitmap)])
