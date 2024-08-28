#lang typed/racket/base

(provide (all-defined-out))
(provide Bitmap XYWH->ARGB XYWH->ARGB* ARGB-Step)
(provide (rename-out [bitmap-polyline bitmap-lines]
                     [bitmap-sandglass bitmap-hourglass]
                     [bitmap-arrowhead bitmap-dart]))

(require geofun/font)
(require geofun/color)
(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/base)
(require geofun/digitama/font)
(require geofun/digitama/color)
(require geofun/digitama/source)

(require geofun/digitama/geometry/dot)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/unsafe/path)
(require geofun/digitama/unsafe/dc/plain)
(require geofun/digitama/unsafe/dc/shape)
(require geofun/digitama/unsafe/dc/arrow)
(require geofun/digitama/unsafe/dc/text)

(require geofun/digitama/geometry/polygon/quadrilateral)

(require "digitama/convert.rkt")
(require "digitama/unsafe/image.rkt")

(require racket/math)
(require racket/string)
(require racket/format)

(require digimon/metrics)
(require digimon/sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-rectangular : (-> Real Real XYWH->ARGB [#:density Positive-Flonum] Bitmap)
  (lambda [width height λargb #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (λbitmap w h density λargb)))

(define bitmap-rectangular* : (All (t) (-> Real Real (XYWH->ARGB* t) t [#:density Positive-Flonum] (Values Bitmap t)))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (λbitmap* w h density λargb initial)))

(define bitmap-irregular : (All (t) (-> Real Real (ARGB-Step t) t [#:density Positive-Flonum] Bitmap))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (define-values (bmp _) (bitmap-irregular* w h λargb initial #:density density))
    bmp))

(define bitmap-irregular* : (All (t) (-> Real Real (ARGB-Step t) t [#:density Positive-Flonum] (Values Bitmap t)))
  (lambda [width height λargb initial #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (λbitmap_step w h density λargb initial)))

(define bitmap-blank : (->* () (Real (Option Real) #:density Positive-Flonum) Bitmap)
  (lambda [[width 0.0] [height #false] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width (or height width)))
    (dc_blank create-argb-bitmap flwidth flheight density)))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (define-values (flw flh) (bitmap-flsize bmp))
    (dc_blank create-argb-bitmap flw flh (bitmap-density bmp))))

(define bitmap-solid : (->* () (Color Real #:density Positive-Flonum) Bitmap)
  (lambda [[color transparent] [size 1] #:density [density (default-bitmap-density)]]
    (define side : Flonum (real->double-flonum size))
    (dc_pattern create-argb-bitmap side side (rgb* color) density)))

(define bitmap-frame : (-> Bitmap [#:border Maybe-Stroke-Paint] [#:background Option-Fill-Paint]
                           [#:margin (U Nonnegative-Real (Listof Nonnegative-Real))]
                           [#:padding (U Nonnegative-Real (Listof Nonnegative-Real))]
                           Bitmap)
  (lambda [bmp #:margin [margin 0.0] #:padding [inset 0.0] #:border [border (default-border-paint)] #:background [bg-fill (default-background-paint)]]
    (define-values (mtop mright mbottom mleft)
      (cond [(list? margin) (list->4:values (map real->double-flonum margin) 0.0)]
            [else (let ([fl (real->double-flonum margin)]) (values fl fl fl fl))]))
    (define-values (ptop pright pbottom pleft)
      (cond [(list? inset) (list->4:values (map real->double-flonum inset) 0.0)]
            [else (let ([fl (real->double-flonum inset)]) (values fl fl fl fl))]))
    (define-values (flwidth flheight) (bitmap-flsize bmp))
    (dc_frame create-argb-bitmap (bitmap-surface bmp) flwidth flheight
              mtop mright mbottom mleft ptop pright pbottom pleft
              (border-paint->source* border) (background->source* bg-fill) (bitmap-density bmp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-art-text : (->* (Any)
                               (Font #:stroke Option-Stroke-Paint #:fill Option-Fill-Paint #:background Option-Fill-Paint
                                     #:lines (Listof Symbol) #:density Positive-Flonum)
                               Bitmap)
  (lambda [text [font (default-art-font)] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
                #:background [bgsource (default-background-paint)] #:lines [lines null] #:density [density (default-bitmap-density)]]
    (dc_art_text create-argb-bitmap
                 (~a text) (font-description font) lines
                 (stroke-paint->source* outline) (fill-paint->source* pattern)
                 (background->source* bgsource) density)))

(define bitmap-text : (->* (Any)
                           (Font #:color Fill-Paint #:background Option-Fill-Paint #:lines (Listof Symbol)
                                 #:baseline Maybe-Stroke-Paint #:capline Maybe-Stroke-Paint #:meanline Maybe-Stroke-Paint
                                 #:ascent Maybe-Stroke-Paint #:descent Maybe-Stroke-Paint
                                 #:density Positive-Flonum)
                           Bitmap)
  (lambda [text [font (default-font)] #:color [ftsource (default-font-paint)] #:background [bgsource (default-background-paint)] #:lines [lines null]
                #:ascent [alsource #false] #:descent [dlsource #false] #:capline [clsource #false] #:meanline [mlsource #false]
                #:baseline [blsource #false] #:density [density (default-bitmap-density)]]
    (dc_text create-argb-bitmap
             (~a text) (font-description font) lines (font-paint->source ftsource) (background->source* bgsource)
             (stroke-paint->source* alsource) (stroke-paint->source* clsource) (stroke-paint->source* mlsource)
             (stroke-paint->source* blsource) (stroke-paint->source* dlsource) density)))

(define bitmap-paragraph : (->* ((U String (Listof String)))
                                (Font #:color Fill-Paint #:background Option-Fill-Paint #:lines (Listof Symbol)
                                      #:max-width Real #:max-height Real #:indent Real #:spacing Real
                                      #:wrap-mode Paragraph-Wrap-Mode #:ellipsize-mode Paragraph-Ellipsize-Mode
                                      #:density Positive-Flonum)
                                Bitmap)
  (lambda [texts [font (default-font)] #:color [ftsource (default-font-paint)] #:background [bgsource (default-background-paint)] #:lines [lines null]
                 #:max-width [max-width +inf.0] #:max-height [max-height +inf.0] #:indent [indent 0.0] #:spacing [spacing 0.0]
                 #:wrap-mode [wrap-mode 'word-char] #:ellipsize-mode [ellipsize-mode 'end]
                 #:density [density (default-bitmap-density)]]
    (define-values (smart-height smart-emode)
      (cond [(or (infinite? max-height) (nan? max-height)) (values -1 'none)]
            [(negative? max-height) (values (exact-round max-height) ellipsize-mode)]
            [else (values (real->double-flonum max-height) ellipsize-mode)]))
    (dc_paragraph create-argb-bitmap
                  (if (list? texts) (string-join texts "\n") texts) (font-description font) lines
                  (if (or (infinite? max-width) (nan? max-width)) #false (real->double-flonum max-width)) smart-height
                  (real->double-flonum indent) (real->double-flonum spacing)
                  (paragraph-wrap-mode->integer wrap-mode raise-argument-error)
                  (paragraph-ellipsize-mode->integer smart-emode raise-argument-error)
                  (font-paint->source ftsource) (background->source* bgsource) density)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-square : (->* (Real) (Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width [corner-radius 0.0] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define w : Nonnegative-Flonum (~length width))
    (if (zero? corner-radius)
        (dc_rectangle create-argb-bitmap w w (stroke-paint->source* outline) (fill-paint->source* pattern) density)
        (dc_rounded_rectangle create-argb-bitmap
                              w w (~length corner-radius w)
                              (stroke-paint->source* outline) (fill-paint->source* pattern)
                              density))))

(define bitmap-rectangle : (->* (Real) (Real Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum) Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]
           width [height -0.618] [corner-radius 0.0]]
    (define-values (w h) (~size width height))
    
    (if (zero? corner-radius)
        (dc_rectangle create-argb-bitmap w h (stroke-paint->source* outline) (fill-paint->source* pattern) density)
        (dc_rounded_rectangle create-argb-bitmap
                              w h (~length corner-radius (min w h))
                              (stroke-paint->source* outline) (fill-paint->source* pattern)
                              density))))

(define bitmap-polyline : (->* ((U Point2D (Listof Point2D)))
                               (Real Real #:scale Point2D #:window Point2D #:stroke Maybe-Stroke-Paint #:close? Boolean #:density Positive-Flonum)
                               Bitmap)
  (lambda [#:scale [scale 1.0] #:stroke [stroke (default-stroke-paint)] #:close? [close? #false] #:density [density (default-bitmap-density)]
           #:window [window -1.0-1.0i]
           pts [dx 0.0] [dy 0.0]]
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height) (point2d->window window lx ty rx by))

    (dc_polyline create-argb-bitmap
                 width height prints xoff yoff
                 (stroke-paint->source stroke) close?
                 density)))

(define bitmap-regular-polygon : (->* (Integer Real)
                                      (Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum
                                            #:radian? Boolean #:inscribed? Boolean)
                                      Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]
           #:radian? [radian? #true] #:inscribed? [inscribed? #false]
           n radius [rotation 0.0]]
    (if (and (index? n) (> n 0))
        (dc_regular_polygon create-argb-bitmap
                            n (regular-polygon-radius->circumsphere-radius n (~length radius) (if inscribed? 'edge 'vertex))
                            (~radian rotation radian?)
                            (stroke-paint->source* outline) (fill-paint->source* pattern)
                            density)
        (dc_circle create-argb-bitmap
                   (~length radius) (stroke-paint->source* outline) (fill-paint->source* pattern)
                   density))))

(define bitmap-polygon : (->* ((U Point2D (Listof Point2D)))
                              (Real Real #:scale Point2D #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:fill-rule Symbol
                                    #:density Positive-Flonum #:window Point2D)
                              Bitmap)
  (lambda [#:scale [scale 1.0] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:density [density (default-bitmap-density)] #:window [window -1.0-1.0i]
           pts [dx 0.0] [dy 0.0]]
    (define-values (prints lx ty rx by) (~point2ds (if (list? pts) pts (list pts)) dx dy scale))
    (define-values (xoff yoff width height) (point2d->window window lx ty rx by))

    (dc_polygon create-argb-bitmap
                width height prints xoff yoff
                (stroke-paint->source* outline) (fill-paint->source* pattern) rule
                density)))

(define bitmap-parallelogram : (-> Real Real Real
                                   [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] [#:radian? Boolean]
                                   Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)] #:radian? [radian? #true]
           width height angle]
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window -1.0-1.0i
                    (geo-parallelogram-vertices flwidth flheight (~cycle (~radian angle radian?) 2pi 0.0)))))

(define bitmap-rhombus : (-> Real Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] Bitmap)
  (lambda [width height #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width height))
    
    (bitmap-polygon #:stroke outline #:fill pattern #:density density #:window -1.0-1.0i
                    (geo-rhombus-vertices flwidth flheight))))

(define bitmap-stadium : (-> Real Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] Bitmap)
  (lambda [length radius #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define flength : Nonnegative-Flonum (~length length))
    (dc_stadium create-argb-bitmap
                flength (~length radius flength)
                (stroke-paint->source* outline) (fill-paint->source* pattern)
                density)))

(define bitmap-circle : (-> Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] Bitmap)
  (lambda [radius #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (dc_circle create-argb-bitmap
               (~length radius) (stroke-paint->source* outline) (fill-paint->source* pattern)
               density)))

(define bitmap-sector : (->* (Real Real Real)
                             (#:ratio Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:radian? Boolean #:density Positive-Flonum)
                             Bitmap)
  (lambda [#:ratio [ratio 1.0] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius start end]
    (define r : Nonnegative-Flonum (~length radius))
    (dc_sector create-argb-bitmap
               r (max (if (> ratio 0.0) (/ r ratio) r) 0.0)
               (~radian start radian?) (~radian end radian?)
               (stroke-paint->source* outline) (fill-paint->source* pattern) density)))

(define bitmap-arc : (->* (Real Real Real) (#:ratio Real #:stroke Maybe-Stroke-Paint #:radian? Boolean #:density Positive-Flonum) Bitmap)
  (lambda [#:ratio [ratio 1.0] #:stroke [stroke (default-stroke-paint)] #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           radius start end]
    (define r : Nonnegative-Flonum (~length radius))
    (dc_arc create-argb-bitmap
            r (max (if (> ratio 0.0) (/ r ratio) r) 0.0)
            (~radian start radian?) (~radian end radian?)
            (stroke-paint->source stroke) density)))

(define bitmap-ellipse : (->* (Real) (Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width [height -0.618] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (w h) (~size width height))
    (if (= w h)
        (dc_circle create-argb-bitmap (* w 0.5) (stroke-paint->source* outline) (fill-paint->source* pattern) density)
        (dc_ellipse create-argb-bitmap w h (stroke-paint->source* outline) (fill-paint->source* pattern) density))))

(define bitmap-arrow : (->* (Real Real)
                            (Real #:shaft-thickness Real #:wing-angle (Option Real) #:radian? Boolean
                                  #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum)
                            Bitmap)
  (lambda [#:shaft-thickness [shaft-thickness -0.3] #:wing-angle [wing-angle #false] #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:radian? [radian? #true] #:density [density (default-bitmap-density)]
           head-radius shaft-length [start 0.0]]
    (define rhead : Nonnegative-Flonum (~length head-radius))
    
    (dc_arrow create-argb-bitmap
              (dc-arrow-metrics rhead (~radian start radian?)
                                (~length shaft-thickness rhead) (~length shaft-length rhead)
                                (and wing-angle (~radian wing-angle radian?)))
              (stroke-paint->source* outline) (fill-paint->source* pattern) density)))

(define bitmap-arrowhead : (->* (Real)
                                (Real #:shaft-thickness Real #:wing-angle (Option Real) #:radian? Boolean
                                      #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum)
                                Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]
           #:shaft-thickness [shaft-thickness 0.0] #:wing-angle [wing-angle #false] #:radian? [radian? #true]
           radius [start 0.0]]
    (define r : Nonnegative-Flonum (~length radius))
    
    (dc_arrow create-argb-bitmap
              (dc-arrow-metrics r (~radian start radian?)
                                (~length shaft-thickness r) 0.0
                                (and wing-angle (~radian wing-angle radian?)))
              (stroke-paint->source* outline) (fill-paint->source* pattern) density)))

(define bitmap-hline : (->* (Real Real) (#:stroke Maybe-Stroke-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height #:stroke [stroke (default-stroke-paint)] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width height))
    (dc_line create-argb-bitmap
             0.0 (* flheight 0.5) flwidth 0.0
             flwidth flheight (stroke-paint->source stroke)
             density)))

(define bitmap-vline : (->* (Real Real) (#:stroke Maybe-Stroke-Paint #:density Positive-Flonum) Bitmap)
  (lambda [width height #:stroke [stroke (default-stroke-paint)] #:density [density (default-bitmap-density)]]
    (define-values (flwidth flheight) (~size width height))
    (dc_line create-argb-bitmap
             (* flwidth 0.5) 0.0 0.0 flheight
             flwidth flheight (stroke-paint->source stroke)
             density)))

(define bitmap-sandglass : (->* (Real)
                                (Real #:neck-width Real #:neck-height Real #:tube-height Real
                                      #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum)
                                Bitmap)
  (lambda [#:neck-width [neck-width -0.1618] #:neck-height [neck-height -0.0618] #:tube-height [tube-height 0]
           #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]
           width [height -1.618]]
    (define-values (flwidth flheight) (~size width height))
    (define neck-flwidth (~length neck-width flwidth))
    (define neck-flheight (~length neck-height flheight))
    (define tube-flheight (~length tube-height flheight))
    
    (dc_sandglass create-argb-bitmap
                  flwidth flheight neck-flwidth neck-flheight tube-flheight
                  (stroke-paint->source* outline) (fill-paint->source* pattern) density)))
