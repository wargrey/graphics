#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require geofun/digitama/unsafe/typed/c)

(require "../self.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require (only-in racket/flonum make-flvector))
  (require racket/unsafe/ops)
  (require racket/draw/unsafe/cairo)
  (require digimon/sequence)
  
  (require geofun/digitama/layer/position)
  (require geofun/digitama/layer/table)

  (require (submod geofun/digitama/unsafe/surface/image unsafe))
  (require (submod geofun/digitama/unsafe/paint unsafe))
  
  (require "../convert.rkt")
  
  (require (submod "bitmap.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_composite filter operator sfc1 sfc2 dx dy density)
    (define-values (w1 h1) (bitmap-surface-rendered-size sfc1 density))
    (define-values (w2 h2) (bitmap-surface-rendered-size sfc2 density))
    (define-values (dx1 dy1) (values (unsafe-flmax (unsafe-fl- 0.0 dx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 dy) 0.0)))
    (define-values (dx2 dy2) (values (unsafe-flmax dx 0.0) (unsafe-flmax dy 0.0)))
    (define-values (img cr)
      (create-argb-bitmap (unsafe-flmax (unsafe-fl+ dx1 w1) (unsafe-fl+ dx2 w2))
                          (unsafe-flmax (unsafe-fl+ dy1 h1) (unsafe-fl+ dy2 h2))
                          density #true))
    
    (let ([s (unsafe-fl/ 1.0 density)])
      (cairo_set_operator cr CAIRO_OPERATOR_SOURCE)
      (cairo-composite cr sfc1 dx1 dy1 w1 h1 filter s s)
      (cairo_set_operator cr operator)
      (cairo-composite cr sfc2 dx2 dy2 w2 h2 filter s s))

    img)

  (define (bitmap_pin filter operator x1% y1% x2% y2% sfc1 sfc2 density)
    (define-values (w1 h1) (bitmap-surface-rendered-size sfc1 density))
    (define-values (w2 h2) (bitmap-surface-rendered-size sfc2 density))

    (bitmap_composite filter operator sfc1 sfc2
                      (unsafe-fl- (unsafe-fl* x1% w1) (unsafe-fl* x2% w2))
                      (unsafe-fl- (unsafe-fl* y1% h1) (unsafe-fl* y2% h2))
                      density))
  
  (define (bitmap_pin* filter operator x1% y1% x2% y2% sfc1 sfcs density)
    (define-values (min-width min-height) (bitmap-surface-rendered-size sfc1 density))
    (define-values (flwidth flheight xoff yoff all)
      (let compose ([lla (list (vector-immutable sfc1 0.0 0.0 min-width min-height))]
                    [dx 0.0] [dy 0.0] [width1 min-width] [height1 min-height]
                    [lx 0.0] [ty 0.0] [rx min-width] [by min-height]
                    [children sfcs])
        (cond [(pair? children)
               (let ([sfc2 (unsafe-car children)])
                 (define-values (width2 height2) (bitmap-surface-rendered-size sfc2 density))
                 (define nx (unsafe-fl+ dx (unsafe-fl- (unsafe-fl* width1 x1%) (unsafe-fl* width2 x2%))))
                 (define ny (unsafe-fl+ dy (unsafe-fl- (unsafe-fl* height1 y1%) (unsafe-fl* height2 y2%))))
                 
                 (compose (unsafe-cons-list (vector-immutable sfc2 nx ny width2 height2) lla)
                          nx ny width2 height2
                          (unsafe-flmin lx nx) (unsafe-flmin ty ny)
                          (unsafe-flmax rx (unsafe-fl+ nx width2))
                          (unsafe-flmax by (unsafe-fl+ ny height2))
                          (unsafe-cdr children)))]
              [else (values (unsafe-flmax rx min-width) (unsafe-flmax by min-height)
                            (if (unsafe-fl< lx 0.0) (unsafe-fl- 0.0 lx) 0.0)
                            (if (unsafe-fl< ty 0.0) (unsafe-fl- 0.0 ty) 0.0)
                            (reverse lla))])))
    
    (let-values ([(bmp cr) (create-argb-bitmap (unsafe-fl+ flwidth xoff) (unsafe-fl+ flheight yoff) density #true)]
                 [(scale) (unsafe-fl/ 1.0 density)])
      (cairo_set_operator cr operator)
      (let combine ([all all])
        (unless (null? all)
          (layer-composite cr (unsafe-car all) filter scale xoff yoff)
          (combine (unsafe-cdr all))))
      bmp))

  (define (bitmap_append alignment filter operator base others gapsize density)
    (define-values (min-width min-height) (bitmap-surface-rendered-size base density))
    (define-values (lx ty rx by all)
      (let compose ([lla (list (vector-immutable base 0.0 0.0 min-width min-height))]
                    [lx 0.0] [ty 0.0] [rx min-width] [by min-height]
                    [x (unsafe-fl+ min-width gapsize)] [y (unsafe-fl+ min-height gapsize)]
                    [children others])
        (cond [(null? children) (values lx ty rx by (reverse lla))]
              [else (let-values ([(child rest) (values (unsafe-car children) (unsafe-cdr children))])
                      (define-values (chwidth chheight) (bitmap-surface-rendered-size child density))
                      (case alignment
                        [(vl vc vr)
                         (let ([delta (unsafe-fl+ chheight gapsize)])
                           (compose (unsafe-cons-list (vector-immutable child x y chwidth chheight) lla)
                                    lx (unsafe-flmin ty y) (unsafe-flmax rx chwidth) (unsafe-flmax by (unsafe-fl+ y chheight))
                                    x (unsafe-fl+ y delta) rest))]
                        [(ht hc hb)
                         (let ([delta (unsafe-fl+ chwidth gapsize)])
                           (compose (unsafe-cons-list (vector-immutable child x y chwidth chheight) lla)
                                    (unsafe-flmin lx x) ty (unsafe-flmax rx (unsafe-fl+ x chwidth)) (unsafe-flmax by chheight)
                                    (unsafe-fl+ x delta) y rest))]
                        [else #| deadcode |# (compose lla lx ty rx by x y rest)]))])))

    (define flwidth (unsafe-fl- rx lx))
    (define flheight (unsafe-fl- by ty))    
    (define xoff (if (unsafe-fl< lx 0.0) (unsafe-fl- lx) 0.0))
    (define yoff (if (unsafe-fl< ty 0.0) (unsafe-fl- ty) 0.0))
    
    (let-values ([(bmp cr) (create-argb-bitmap flwidth flheight density #true)]
                 [(scale) (unsafe-fl/ 1.0 density)])
      (cairo_set_operator cr operator)
      (let combine ([all all])
        (unless (null? all)
          (layer-composite cr (unsafe-append-layer alignment flwidth flheight (unsafe-car all) xoff yoff) filter scale)
          (combine (unsafe-cdr all))))
      bmp))

  (define (bitmap_superimpose alignment filter operator sfcs density)
    (define-values (flwidth flheight layers) (list->layers alignment sfcs density))
    (define-values (bmp cr) (create-argb-bitmap flwidth flheight density #true))
    (define scale (unsafe-fl/ 1.0 density))
    
    (cairo_set_operator cr operator)
    (let combine ([all layers])
      (unless (null? all)
        (layer-composite cr ((unsafe-car all) flwidth flheight) filter scale)
        (combine (unsafe-cdr all))))
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_table sfcs ncols nrows alcols alrows gcols grows filter density)
    (define table (list->n:vector* sfcs (unsafe-fx* nrows ncols) (table-filling-layer) (bitmap-surface->layer density)))
    (define size (unsafe-vector*-length table))
    (define cwidths (unsafe-table-column-widths table nrows ncols size))
    (define rheights (unsafe-table-row-heights table nrows ncols size))
    (define layers (unsafe-table-layers table ncols nrows alcols alrows gcols grows cwidths rheights))
    
    (let-values ([(bmp cr) (create-argb-bitmap (unsafe-struct*-ref layers 0) (unsafe-struct*-ref layers 1) density #true)]
                 [(scale) (unsafe-fl/ 1.0 density)])    
      (let combine ([all (unsafe-struct*-ref layers 2)])
        (unless (null? all)
          (layer-composite cr (unsafe-car all) filter scale)
          (combine (unsafe-cdr all))))
      bmp))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_pyramid sfcs sub-gaps sibling-gaps node-aligns filter density)
    (define n (length sfcs))
    (define depth (unsafe-fx->fl (pyramid-depth n)))
    (define nleaves (unsafe-fl+ depth 1.0))
    (define-values (nwidth nheight nodes) (list->layers* node-aligns sfcs density))
    (define flwidth (unsafe-fl+ (unsafe-fl* nwidth nleaves) (unsafe-fl* sibling-gaps (unsafe-fl- nleaves 1.0))))
    (define flheight (unsafe-fl+ (unsafe-fl* nheight (unsafe-fl+ depth 1.0)) (unsafe-fl* sub-gaps depth)))
    (define 2translate (unsafe-fl- flwidth nwidth))
    (define xtranslate (unsafe-fl* 2translate 0.5))
    (define xstep (unsafe-fl* (unsafe-fl+ nwidth sibling-gaps) 0.5))
    (define ystep (unsafe-fl+ nheight sub-gaps))
    (define dx (unsafe-fl+ nwidth sibling-gaps))
    
    (define-values (bmp cr) (create-argb-bitmap flwidth flheight density #true))
    (define scale (unsafe-fl/ 1.0 density))

    ;;; TODO
    ; Arithmetic here only involves in addition and subtraction,
    ; working with flonums directly seems do not lose precision.
    
    (let combine ([nodes nodes]
                  [y0 0.0]
                  [x0 xtranslate]
                  [boundary xtranslate])
      (unless (null? nodes)
        (define node (unsafe-car nodes))
        (define layer ((unsafe-cdr node) nwidth nheight))
        (cairo-composite cr (unsafe-car node)
                         (unsafe-fl+ x0 (unsafe-struct*-ref layer 1))
                         (unsafe-fl+ y0 (unsafe-struct*-ref layer 2))
                         flwidth flheight filter scale scale)
        (if (unsafe-fl< x0 boundary)
            (combine (unsafe-cdr nodes) y0 (unsafe-fl+ x0 dx) boundary)
            (let ([boundary++ (unsafe-fl+ boundary xstep)])
              (combine (unsafe-cdr nodes) (unsafe-fl+ y0 ystep)
                       (unsafe-fl- 2translate boundary++)  ; (t - [b - t]) = 2t - b
                       boundary++)))))
    bmp)

  (define (bitmap_heap sfcs ary sub-gaps sibling-gaps node-aligns filter density)
    (define n (length sfcs))
    (define ary-1 (unsafe-fx- ary 1))
    (define flary (unsafe-fx->fl ary))
    (define depth (unsafe-flfloor (log (unsafe-fx* n ary-1) ary)))
    (define nleaves (unsafe-flexpt flary depth))
    (define-values (nwidth nheight nodes) (list->layers* node-aligns sfcs density))
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
    (define scale (unsafe-fl/ 1.0 density))
    
    (let combine ([nodes nodes]
                  [y0 0.0]
                  [idx 0]
                  [boundary 1]
                  [siblings 1])
      (unless (null? nodes)
        (define node (unsafe-car nodes))
        (define layer ((unsafe-cdr node) nwidth nheight))
        (define idx++ (unsafe-fx+ idx 1))

        (cairo-composite cr (unsafe-car node)
                         (unsafe-fl+ (unsafe-flvector-ref vs idx) (unsafe-struct*-ref layer 1))
                         (unsafe-fl+ y0 (unsafe-struct*-ref layer 2))
                         flwidth flheight filter scale scale)

        (if (unsafe-fx< idx++ boundary)
            (combine (unsafe-cdr nodes) y0 idx++ boundary siblings)
            (let ([siblings++ (unsafe-fx* siblings ary)])
              (combine (unsafe-cdr nodes) (unsafe-fl+ y0 ystep) idx++
                       (unsafe-fx+ boundary siblings++) siblings++)))))
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define layer-composite
    (case-lambda
      [(cr self filter scale)
       (cairo-composite cr (unsafe-struct*-ref self 0)
                        (unsafe-struct*-ref self 1) (unsafe-struct*-ref self 2)
                        (unsafe-struct*-ref self 3) (unsafe-struct*-ref self 4)
                        filter scale scale)]
      [(cr self filter scale xoff yoff)
       (cairo-composite cr (unsafe-struct*-ref self 0)
                        (unsafe-fl+ (unsafe-struct*-ref self 1) xoff)
                        (unsafe-fl+ (unsafe-struct*-ref self 2) yoff)
                        (unsafe-struct*-ref self 3) (unsafe-struct*-ref self 4)
                        filter scale scale)]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (table-filling-layer)
    (vector-immutable the-image-surface 0.0 0.0 1.0 1.0))
  
  (define ((bitmap-surface->layer density) sfc)
    (define-values (w h) (bitmap-surface-rendered-size sfc density))
    (vector-immutable sfc 0.0 0.0 w h))

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
                             (unsafe-cons-list (unsafe-superimpose-layer-make alignment sfc w h) sreyal)
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
                             (unsafe-cons-list (cons sfc (unsafe-superimpose-layer-make (unsafe-vector*-ref aligns aidx) sfc w h)) sreyal)
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
 [bitmap_composite (-> Byte Byte Bitmap-Surface Bitmap-Surface Flonum Flonum Positive-Flonum Bitmap)]
 [bitmap_pin (-> Byte Byte Flonum Flonum Flonum Flonum Bitmap-Surface Bitmap-Surface Positive-Flonum Bitmap)]
 [bitmap_pin* (-> Byte Byte Flonum Flonum Flonum Flonum Bitmap-Surface (Listof Bitmap-Surface) Positive-Flonum Bitmap)]
 [bitmap_append (-> Symbol Byte Byte Bitmap-Surface (Listof Bitmap-Surface) Flonum Positive-Flonum Bitmap)]
 [bitmap_superimpose (-> Symbol Byte Byte (Listof Bitmap-Surface) Positive-Flonum Bitmap)]

 [bitmap_table (-> (Listof Bitmap-Surface) Natural Natural (Vectorof Symbol) (Vectorof Symbol) (Vectorof Flonum) (Vectorof Flonum)
                   Byte Positive-Flonum
                   Bitmap)]

 [bitmap_heap (-> (Listof Bitmap-Surface) Positive-Index Flonum Flonum (Vectorof Symbol) Byte Positive-Flonum Bitmap)]
 [bitmap_pyramid (-> (Listof Bitmap-Surface) Flonum Flonum (Vectorof Symbol) Byte Positive-Flonum Bitmap)])
