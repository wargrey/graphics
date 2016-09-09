#lang at-exp typed/racket

(provide (all-defined-out))
(provide (all-from-out "colorspace.rkt"))
(provide (all-from-out typed/racket/draw images/flomap typed/images/logos typed/images/icons))

@require{css.rkt}
@require{colorspace.rkt}

(require racket/fixnum)
(require racket/flonum)
(require racket/unsafe/ops)

(require typed/racket/draw)

(require (except-in images/flomap flomap->bitmap bitmap->flomap))
(require typed/images/logos)
(require typed/images/icons)

(define-type Flomap flomap)
(define-type Bitmap (Instance Bitmap%))
(define-type Color+sRGB (U Nonnegative-Fixnum Symbol String (Instance Color%)))

(define bitmap/2x : (-> (U Path-String Input-Port) Bitmap)
  (lambda [src]
    (define raw : Bitmap (read-bitmap src #:try-@2x? #true))
    (bitmap-scale raw (/ (send raw get-backing-scale) (default-icon-backing-scale)))))

(define bitmap-size : (-> Bitmap (Values Positive-Integer Positive-Integer))
  (lambda [bmp]
    (values (send bmp get-width)
            (send bmp get-height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bitmap->flomap : (-> Bitmap Flomap)
  (lambda [bmp]
    (define backing-scale : Positive-Real (send bmp get-backing-scale))
    (define w : Positive-Integer (max 1 (exact-ceiling (* (send bmp get-width) backing-scale))))
    (define h : Positive-Integer (max 1 (exact-ceiling (* (send bmp get-height) backing-scale))))
    (define bs : Bytes (make-bytes (* 4 w h)))
    (send bmp get-argb-pixels 0 0 w h bs #true #true #:unscaled? #true)
    (send bmp get-argb-pixels 0 0 w h bs #false #true #:unscaled? #true)
    
    (define flmp (make-flomap 4 w h))
    (define flvs (flomap-values flmp))
    (for ([ia (in-range 0 (* 4 w h) 4)])
      (define-values (ir ig ib) (values (fx+ ia 1) (fx+ ia 2) (fx+ ia 3)))
      (flvector-set! flvs ia (fl/ (fx->fl (bytes-ref bs ia)) 255.0))
      (flvector-set! flvs ir (fl/ (fx->fl (bytes-ref bs ir)) 255.0))
      (flvector-set! flvs ig (fl/ (fx->fl (bytes-ref bs ig)) 255.0))
      (flvector-set! flvs ib (fl/ (fx->fl (bytes-ref bs ib)) 255.0)))
    flmp))

(define flomap->bitmap : (-> Flomap Bitmap)
  (lambda [fmp]
    (match-define (flomap vs 4 w h) fmp)
    (define bs : Bytes (make-bytes (* 4 w h)))
    (for ([ia (in-range 0 (* 4 w h) 4)])
      (define-values (ir ig ib) (values (fx+ ia 1) (fx+ ia 2) (fx+ ia 3)))
      (bytes-set! bs ia (gamut->byte (flabs (flvector-ref vs ia))))
      (bytes-set! bs ir (gamut->byte (flabs (flvector-ref vs ir))))
      (bytes-set! bs ig (gamut->byte (flabs (flvector-ref vs ig))))
      (bytes-set! bs ib (gamut->byte (flabs (flvector-ref vs ib)))))
    
    (define bmp : Bitmap
      (make-bitmap (max 1 (exact-ceiling (/ w (default-icon-backing-scale))))
                   (max 1 (exact-ceiling (/ h (default-icon-backing-scale))))
                   #:backing-scale (default-icon-backing-scale)))
    (send bmp set-argb-pixels 0 0 (abs w) (abs h) bs #true #true #:unscaled? #true)
    (send bmp set-argb-pixels 0 0 (abs w) (abs h) bs #false #true #:unscaled? #true)
    bmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-font+ : (->* () ((Instance Font%)
                              #:size Real #:face (Option String) #:family (Option Font-Family)
                              #:style (Option Font-Style) #:weight (Option Font-Weight) #:hinting (Option Font-Hinting)
                              #:underlined? (U 'default Boolean) #:size-in-pixels? (U 'default Boolean)) (Instance Font%))
  (let ([default-font : (Instance Font%) (make-font)])
    (lambda [[basefont default-font] #:size [size -1.0] #:face [face #false] #:family [family #false]
             #:style [style #false] #:weight [weight #false] #:hinting [hinting #false]
             #:underlined? [underlined 'default] #:size-in-pixels? [size-in-pixels? 'default]]
      (define maybe-face : (Option String) (or face (send basefont get-face)))
      (define underlined? : Boolean (if (boolean? underlined) underlined (send basefont get-underlined)))
      (define pixels? : Boolean (if (boolean? size-in-pixels?) size-in-pixels? (send basefont get-size-in-pixels)))
      (define fontsize : Real (let ([default-size (send basefont get-size)])
                                (cond [(positive? size) size]
                                      [(zero? size) default-size]
                                      [else (* (- size) default-size)])))
      (if (string? maybe-face)
          (send the-font-list find-or-create-font fontsize maybe-face
                (or family (send basefont get-family)) (or style (send basefont get-style))
                (or weight (send basefont get-weight)) underlined? 'smoothed pixels?
                (or hinting (send basefont get-hinting)))
          (send the-font-list find-or-create-font fontsize
                (or family (send basefont get-family)) (or style (send basefont get-style))
                (or weight (send basefont get-weight)) underlined? 'smoothed pixels?
                (or hinting (send basefont get-hinting)))))))

(define bitmap-blank : (->* () (Nonnegative-Real (Option Nonnegative-Real) #:backing-scale Positive-Real) Bitmap)
  (lambda [[w 0] [h #false] #:backing-scale [backing-scale (default-icon-backing-scale)]]
    (define width : Positive-Integer (max 1 (exact-ceiling w)))
    (define height : Positive-Integer (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale backing-scale)))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-blank #:backing-scale (send bmp get-backing-scale)
                  (send bmp get-width) (send bmp get-height))))

(define bitmap-text : (->* (String) ((Instance Font%) #:combine? Boolean #:color (Option Color+sRGB)
                                                      #:background-color (Option Color+sRGB)) Bitmap)
  (lambda [content [font (make-font+)] #:combine? [combine? #false] #:color [fgcolor #false]
           #:background-color [bgcolor #false]]
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank)))
    (define-values (width height descent ascent) (send dc get-text-extent content font combine?))
    (send dc set-bitmap (bitmap-blank width height))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-rgba-color fgcolor)))
    (when bgcolor (send dc set-text-background (select-rgba-color bgcolor)))
    (when bgcolor (send dc set-text-mode 'solid))
    (send dc draw-text content 0 0 combine?)
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-desc : (->* (String Positive-Real)
                           ((Instance Font%) #:combine? Boolean #:color (Option Color+sRGB)
                                             #:background-color (Option Color+sRGB)) Bitmap)
  (lambda [description max-width.0 [font (make-font+)] #:combine? [combine? #false] #:color [fgcolor #false]
           #:background-color [bgcolor #false]]
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank)))
    (define max-width : Positive-Integer (exact-ceiling max-width.0))
    (define desc-extent : (-> String Integer Integer (Values String Natural Nonnegative-Real))
      (lambda [desc start end]
        (define subdesc : String (substring desc start end))
        (define-values (width height descent ascent) (send dc get-text-extent subdesc font combine?))
        (values subdesc (exact-ceiling width) height)))

    (define-values (_ char-width phantom-height) (desc-extent " " 0 1))
    (define-values (descs ys width height)
      (for/fold ([descs : (Listof String) null] [ys : (Listof Real) null] [width : Natural 0] [height : Real 0])
                ([desc : String (in-list (string-split description (string #\newline)))])
        (define terminal : Index (string-length desc))
        (let desc-row : (Values (Listof String) (Listof Real) Natural Real)
          ([idx0 : Nonnegative-Fixnum 0] [descs : (Listof String) descs] [ys : (Listof Real) ys]
                                         [Widthn : Natural width] [yn : Real height])
          (define-values (descn widthn heightn idx)
            (let desc-col-expt : (Values String Natural Real Nonnegative-Fixnum)
              ([interval : Nonnegative-Fixnum 1] [open : Nonnegative-Fixnum idx0] [close : Nonnegative-Fixnum terminal]
                                                 [backtracking : String ""] ; char-width is bad for forecasting the next width 
                                                 [back-width : Natural max-width] [back-height : Nonnegative-Real phantom-height])
              (define idx : Nonnegative-Fixnum (fxmin close (fx+ open interval)))
              (define next-open : Nonnegative-Fixnum (fx+ open (fxquotient interval 2)))
              (define-values (line width height) (desc-extent desc idx0 idx))
              (define found? : Boolean (and (> width max-width) (fx= interval 1) (<= back-width max-width)))
              (cond [found? (values backtracking back-width back-height (if (zero? open) terminal (max 0 (sub1 idx))))]
                    [(> width max-width) (desc-col-expt 1 next-open idx backtracking back-width back-height)]
                    [(= close idx) (values line width height idx)]
                    [else (desc-col-expt (fxlshift interval 1) open close line width height)])))
          (if (fx= idx terminal)
              (values (cons descn descs) (cons yn ys) (max widthn Widthn) (+ yn heightn))
              (desc-row idx (cons descn descs) (cons yn ys) (max widthn Widthn) (+ yn heightn))))))
    
    (send dc set-bitmap (bitmap-blank (max 1 width) (max 1 phantom-height height)))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-rgba-color fgcolor)))
    (unless (false? bgcolor)
      (define color : (Instance Color%) (select-rgba-color bgcolor))
      (send dc set-smoothing 'aligned)
      (send dc set-pen color 0 'transparent)
      (send dc set-brush color 'solid)
      (send dc draw-rectangle 0 0 (max 1 width) (max 1 phantom-height height)))
    (for ([desc (in-list (reverse descs))] [y (in-list (reverse ys))])
      (send dc draw-text desc 0 y combine?))
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-frame : (-> Bitmap [#:margin Index] [#:color Color+sRGB] [#:style Brush-Style]
                           [#:border-color Color+sRGB] [#:border-width Index] [#:border-style Pen-Style] Bitmap)
  (lambda [bmp #:margin [margin 0] #:color [brush-color #xFFFFFF] #:style [brush-style 'transparent]
           #:border-color [pen-color #x000000] #:border-width [pen-size 1] #:border-style [pen-style 'solid]]
    (define width : Positive-Integer (+ margin margin (send bmp get-width) pen-size pen-size))
    (define height : Positive-Integer (+ margin margin (send bmp get-height) pen-size pen-size))
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
    (send dc set-smoothing 'aligned)
    (send dc set-pen (select-rgba-color pen-color) pen-size (if (zero? pen-size) 'transparent pen-style))
    (send dc set-brush (select-rgba-color brush-color) brush-style)
    (send dc draw-rectangle 0 0 width height)
    (send dc draw-bitmap bmp margin margin)
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-scale : (->* (Bitmap Real) (Real) Bitmap)
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (let ([w : Positive-Integer (max 1 (exact-ceiling (* (send bmp get-width) scale-x)))]
                       [h : Positive-Integer (max 1 (exact-ceiling (* (send bmp get-height) scale-y)))])
                   (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank w h)))
                   (send dc set-smoothing 'aligned)
                   (send dc set-scale scale-x scale-y)
                   (send dc draw-bitmap bmp 0 0)
                   (or (send dc get-bitmap) (bitmap-blank)))])]))

(define bitmap-resize : (case-> [Bitmap (U Bitmap Nonnegative-Real) -> Bitmap]
                                [Bitmap Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp refer)
     (cond [(real? refer) (bitmap-scale bmp (/ refer (min (send bmp get-width) (send bmp get-height))))]
           [else (bitmap-resize bmp (send refer get-width) (send refer get-height))])]
    [(bmp w h)
     (bitmap-scale bmp (/ w (send bmp get-width)) (/ h (send bmp get-height)))]))

(define-values (bitmap-pin-over bitmap-pin-under)
  (let ([make-pin
         (lambda [order]
           (define bitmap-pin : (->* (Bitmap Real Real Bitmap) (Real Real) Bitmap)
             (lambda [bmp1 x1 y1 bmp2 [x2 0] [y2 0]]
               (define dx : Integer (exact-round (- x1 x2)))
               (define dy : Integer (exact-round (- y1 y2)))
               (define dx1 : Natural (fxmax 0 (fx- 0 dx)))
               (define dy1 : Natural (fxmax 0 (fx- 0 dy)))
               (define dx2 : Natural (fxmax 0 dx))
               (define dy2 : Natural (fxmax 0 dy))
               (define dc : (Instance Bitmap-DC%)
                 (make-object bitmap-dc%
                   (bitmap-blank (fxmax (fx+ dx1 (send bmp1 get-width)) (fx+ dx2 (send bmp2 get-width)))
                                 (fxmax (fx+ dy1 (send bmp1 get-height)) (fx+ dy2 (send bmp2 get-height))))))
               (send dc set-smoothing 'aligned)
               (case order
                 [(over)  (void (send dc draw-bitmap bmp1 dx1 dy1)
                                (send dc draw-bitmap bmp2 dx2 dy2))]
                 [(under) (void (send dc draw-bitmap bmp2 dx2 dy2)
                                (send dc draw-bitmap bmp1 dx1 dy1))])
               (or (send dc get-bitmap) (bitmap-blank))))
           bitmap-pin)])
    (values (make-pin 'over) (make-pin 'under))))

(define bitmap-pin : (-> Real Real Real Real Bitmap Bitmap * Bitmap)
  (lambda [x1-frac y1-frac x2-frac y2-frac bmp0 . bmps]
    (define-values (bmp _who _cares)
      (for/fold ([bmp : Bitmap  bmp0]
                 [x : Exact-Rational  0]
                 [y : Exact-Rational  0])
                ([bmp1 (in-list (cons bmp0 bmps))]
                 [bmp2 (in-list bmps)])
        (define-values (w1 h1) (bitmap-size bmp1))
        (define-values (w2 h2) (bitmap-size bmp2))
        (define x1 (+ x (- (inexact->exact (* x1-frac w1)) (inexact->exact (* x2-frac w2)))))
        (define y1 (+ y (- (inexact->exact (* y1-frac h1)) (inexact->exact (* y2-frac h2)))))
        (values (bitmap-pin-over bmp x1 y1 bmp2) (max 0 x1) (max 0 y1))))
    bmp))

(define-values (bitmap-vl-append bitmap-vc-append bitmap-vr-append bitmap-ht-append bitmap-hc-append bitmap-hb-append
                                 bitmap-lt-superimpose bitmap-lc-superimpose bitmap-lb-superimpose
                                 bitmap-ct-superimpose bitmap-cc-superimpose bitmap-cb-superimpose
                                 bitmap-rt-superimpose bitmap-rc-superimpose bitmap-rb-superimpose)
  (let ([make-append
         (lambda [alignment] : (-> [#:gapsize Real] Bitmap * Bitmap)
          (lambda [#:gapsize [delta 0.0] . bitmaps]
            (cond [(null? bitmaps) (bitmap-blank)]
                  [(null? (cdr bitmaps)) (car bitmaps)]
                  [else (let*-values ([(base others) (values (car bitmaps) (cdr bitmaps))]
                                      [(min-width min-height) (bitmap-size base)])
                          (define-values (width0 height0)
                            (for/fold ([width : Real (send base get-width)]
                                       [height : Real (send base get-height)])
                                      ([child : Bitmap (in-list others)])
                              (define w : Positive-Integer (send child get-width))
                              (define h : Positive-Integer (send child get-height))
                              (case alignment
                                [(vl vc vr) (values (max width w) (+ height h delta))]
                                [(ht hc hb) (values (+ width w delta) (max height h))]
                                [else #|unreachable|# (values (max width w) (max height h))])))
                 
                          (define width : Positive-Real (max min-width width0))
                          (define height : Positive-Real (max min-height height0))
                          (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
                          (send dc set-smoothing 'aligned)
                          
                          (let append-bitmap : Void ([bmps : (Listof Bitmap) bitmaps]
                                                     [xoff : Real (- delta)]
                                                     [yoff : Real (- delta)])
                            (unless (null? bmps)
                              (define bmp : Bitmap (car bmps))
                              (define-values (w h) (bitmap-size bmp))
                              (define this-x-if-use : Real (+ xoff delta))
                              (define this-y-if-use : Real (+ yoff delta))
                              (define-values (x y)
                                (case alignment
                                  [(vl) (values 0                 this-y-if-use)]
                                  [(vc) (values (/ (- width w) 2) this-y-if-use)]
                                  [(vr) (values (- width w)       this-y-if-use)]
                                  [(ht) (values this-x-if-use     0)]
                                  [(hc) (values this-x-if-use     (/ (- height h) 2))]
                                  [(hb) (values this-x-if-use     (- height h))]
                                  [else #|unreachable|# (values this-x-if-use this-y-if-use)]))
                              (send dc draw-bitmap bmp x y)
                              (append-bitmap (cdr bmps) (+ this-x-if-use w) (+ this-y-if-use h))))
                          (or (send dc get-bitmap) (bitmap-blank)))])))]
        [make-superimpose
         (lambda [alignment] : (-> Bitmap * Bitmap)
           (lambda bitmaps
             (cond [(null? bitmaps) (bitmap-blank)]
                   [(null? (cdr bitmaps)) (car bitmaps)]
                   [(null? (cddr bitmaps))
                    (let-values ([(base bmp) (values (car bitmaps) (cadr bitmaps))])
                      (case alignment
                        [(lt) (bitmap-pin  0   0   0   0  base bmp)]
                        [(lc) (bitmap-pin  0  1/2  0  1/2 base bmp)]
                        [(lb) (bitmap-pin  0   1   0   1  base bmp)]
                        [(ct) (bitmap-pin 1/2  0  1/2  0  base bmp)]
                        [(cc) (bitmap-pin 1/2 1/2 1/2 1/2 base bmp)]
                        [(cb) (bitmap-pin 1/2  1  1/2  1  base bmp)]
                        [(rt) (bitmap-pin  1   0   1   0  base bmp)]
                        [(rc) (bitmap-pin  1  1/2  1  1/2 base bmp)]
                        [(rb) (bitmap-pin  1   1   1   1  base bmp)]
                        [else base]))]
                   [else (let-values ([(width height) (for/fold ([width : Positive-Integer 1]
                                                                 [height : Positive-Integer 1])
                                                                ([bmp : Bitmap (in-list bitmaps)])
                                                        (values (max width (send bmp get-width))
                                                                (max height (send bmp get-height))))])
                           (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
                           (send dc set-smoothing 'aligned)
                
                           (for ([bmp : Bitmap (in-list bitmaps)])
                             (define w : Positive-Integer (send bmp get-width))
                             (define h : Positive-Integer (send bmp get-height))
                             (define-values (rx by) (values (- width w) (- height h)))
                             (define-values (cx cy) (values (/ rx 2) (/ by 2)))
                             (define-values (x y)
                               (case alignment
                                 [(lt) (values  0 0)] [(lc) (values  0 cy)] [(lb) (values  0 by)]
                                 [(ct) (values cx 0)] [(cc) (values cx cy)] [(cb) (values cx by)]
                                 [(rt) (values rx 0)] [(rc) (values rx cy)] [(rb) (values rx by)]
                                 [else #|unreachable|# (values 0 0)]))
                             (send dc draw-bitmap bmp x y))
                           (or (send dc get-bitmap) (bitmap-blank)))])))])
    (values (make-append 'vl) (make-append 'vc) (make-append 'vr)
            (make-append 'ht) (make-append 'hc) (make-append 'hb)
            (make-superimpose 'lt) (make-superimpose 'lc) (make-superimpose 'lb)
            (make-superimpose 'ct) (make-superimpose 'cc) (make-superimpose 'cb)
            (make-superimpose 'rt) (make-superimpose 'rc) (make-superimpose 'rb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module css typed/racket
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; http://www.w3.org/Style/CSS/specs.en.html                                                 ;;;
  ;;;                                                                                           ;;;
  ;;; https://drafts.csswg.org/css-color                                                        ;;;
  ;;; https://drafts.csswg.org/css-fonts                                                        ;;;
  ;;; https://drafts.csswg.org/css-images                                                       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require "css.rkt")
  (require "colorspace.rkt")

  (require racket/fixnum)
  (require typed/racket/draw)
  
  (define the-color-pool : (HashTable Fixnum (Instance Color%)) (make-hasheq))
  
  (define css-named-colors : (HashTable Symbol Nonnegative-Fixnum)
    #hasheq((black . 0) (gold . #xFFD700) (palegoldenrod . #xEEE8AA) (hotpink . #xFF69B4) (darksalmon . #xE9967A) (yellow . #xFFFF00)
                        (moccasin . #xFFE4B5) (white . #xFFFFFF) (plum . #xDDA0DD) (teal . #x008080) (whitesmoke . #xF5F5F5)
                        (lightsalmon . #xFFA07A) (aquamarine . #x7FFFD4) (lavenderblush . #xFFF0F5) (palevioletred . #xDB7093)
                        (olivedrab . #x6B8E23) (dimgrey . #x696969) (navajowhite . #xFFDEAD) (darkblue . #x00008B) (coral . #xFF7F50)
                        (indigo . #x4B0082) (lightcyan . #xE0FFFF) (limegreen . #x32CD32) (oldlace . #xFDF5E6) (grey . #x808080)
                        (darkslateblue . #x483D8B) (sandybrown . #xF4A460) (mediumblue . #x0000CD) (darkolivegreen . #x556B2F)
                        (sienna . #xA0522D) (springgreen . #x00FF7F) (dimgray . #x696969) (royalblue . #x4169E1) (ivory . #xFFFFF0)
                        (rebeccapurple . #x663399) (crimson . #xDC143C) (goldenrod . #xDAA520) (gray . #x808080) (purple . #x800080)
                        (antiquewhite . #xFAEBD7) (cyan . #x00FFFF) (aliceblue . #xF0F8FF) (darkviolet . #x9400D3) (orchid . #xDA70D6)
                        (palegreen . #x98FB98) (green . #x008000) (peachpuff . #xFFDAB9) (snow . #xFFFAFA) (mediumseagreen . #x3CB371)
                        (paleturquoise . #xAFEEEE) (lightslategray . #x778899) (lightcoral . #xF08080) (ghostwhite . #xF8F8FF)
                        (azure . #xF0FFFF) (seashell . #xFFF5EE) (darkcyan . #x008B8B) (darkorchid . #x9932CC) (burlywood . #xDEB887)
                        (lightslategrey . #x778899) (thistle . #xD8BFD8) (bisque . #xFFE4C4) (darkred . #x8B0000) (darkgrey . #xA9A9A9)
                        (dodgerblue . #x1E90FF) (lavender . #xE6E6FA) (deeppink . #xFF1493) (cornflowerblue . #x6495ED) (peru . #xCD853F)
                        (orangered . #xFF4500) (darkgray . #xA9A9A9) (lightseagreen . #x20B2AA) (tomato . #xFF6347) (darkgreen . #x006400)
                        (blueviolet . #x8A2BE2) (forestgreen . #x228B22) (mediumvioletred . #xC71585) (lightyellow . #xFFFFE0)
                        (lightgray . #xD3D3D3) (mediumorchid . #xBA55D3) (darkturquoise . #x00CED1) (papayawhip . #xFFEFD5) 
                        (yellowgreen . #x9ACD32) (lawngreen . #x7CFC00) (firebrick . #xB22222) (rosybrown . #xBC8F8F) (navy . #x000080)
                        (mediumpurple . #x9370DB) (skyblue . #x87CEEB) (lightgreen . #x90EE90) (lemonchiffon . #xFFFACD) (tan . #xD2B48C)
                        (honeydew . #xF0FFF0) (seagreen . #x2E8B57) (darkseagreen . #x8FBC8F) (darkmagenta . #x8B008B) (pink . #xFFC0CB)
                        (blanchedalmond . #xFFEBCD) (darkslategrey . #x2F4F4F) (maroon . #x800000) (darkgoldenrod . #xB8860B)
                        (chocolate . #xD2691E) (mediumaquamarine . #x66CDAA) (darkkhaki . #xBDB76B) (indianred . #xCD5C5C)
                        (floralwhite . #xFFFAF0) (darkslategray . #x2F4F4F) (mediumslateblue . #x7B68EE) (chartreuse . #x7FFF00)
                        (deepskyblue . #x00BFFF) (blue . #x0000FF) (lime . #x00FF00) (darkorange . #xFF8C00) (red . #xFF0000)
                        (violet . #xEE82EE) (mintcream . #xF5FFFA) (beige . #xF5F5DC) (cornsilk . #xFFF8DC) (turquoise . #x40E0D0)
                        (brown . #xA52A2A) (magenta . #xFF00FF) (lightgoldenrodyellow . #xFAFAD2) (saddlebrown . #x8B4513)
                        (slategrey . #x708090) (lightblue . #xADD8E6) (steelblue . #x4682B4) (mediumturquoise . #x48D1CC)
                        (mistyrose . #xFFE4E1) (lightgrey . #xD3D3D3) (lightpink . #xFFB6C1) (wheat . #xF5DEB3) (linen . #xFAF0E6)
                        (powderblue . #xB0E0E6) (aqua . #x00FFFF) (khaki . #xF0E68C) (slategray . #x708090) (greenyellow . #xADFF2F)
                        (cadetblue . #x5F9EA0) (slateblue . #x6A5ACD) (olive . #x808000) (orange . #xFFA500) (lightsteelblue . #xB0C4DE)
                        (lightskyblue . #x87CEFA) (gainsboro . #xDCDCDC) (fuchsia . #xFF00FF) (mediumspringgreen . #x00FA9A)
                        (midnightblue . #x191970) (salmon . #xFA8072) (silver . #xC0C0C0)))

  (define rgb-bytes->ufixnum : (-> Byte Byte Byte Nonnegative-Fixnum)
    (lambda [r g b]
      (fxand #xFFFFFF
             (fxior (fxlshift r 16)
                    (fxior (fxlshift g 8)
                           b)))))

  (define ufixnum->rgb-bytes : (-> Nonnegative-Fixnum (Values Byte Byte Byte))
    (lambda [rgb]
      (values (fxand (fxrshift rgb 16) #xFF)
              (fxand (fxrshift rgb 8) #xFF)
              (fxand rgb #xFF))))
  
  (define css-hex-color->rgba : (-> Keyword (Values (Option Nonnegative-Fixnum) Nonnegative-Flonum))
    (lambda [hash-color]
      (define color : String (keyword->string hash-color))
      (define digits : Index (string-length color))
      (define maybe-hexcolor : (Option Number)
        (case digits
          [(6 8) (string->number color 16)]
          [(3 4) (for/fold ([hexcolor : (Option Nonnegative-Fixnum) 0])
                           ([ch : Char (in-string color)])
                   (define digit : (U Integer Void)
                     (cond [(char-numeric? ch)   (fx- (char->integer ch) #x30)]
                           [(char<=? #\a ch #\f) (fx- (char->integer ch) #x37)]
                           [(char<=? #\A ch #\F) (fx- (char->integer ch) #x57)]))
                   (and hexcolor (byte? digit)
                        (fxior (fxlshift hexcolor 8)
                               (fxior (fxlshift digit 4)
                                      digit))))]
          [else #false]))
      (cond [(false? (index? maybe-hexcolor)) (values #false 1.0)]
            [(or (fx= digits 3) (fx= digits 6)) (values maybe-hexcolor 1.0)]
            [else (values (fxrshift maybe-hexcolor 8)
                          (flabs (fl/ (fx->fl (fxand maybe-hexcolor #xFF)) 255.0)))])))
  
  (define css-rgb->scalar : (-> CSS-Token (U Byte CSS-Declared-Result))
    (lambda [t]
      (cond [(css:byte? t) (css:byte-datum t)]
            [(css:fraction%? t)  (min (css:fraction%=> t fl* 255.0 exact-round) #xFF)]
            [(css:flunum=<-? t fl<= 255.0) (min (css:flunum=> t exact-round) #xFF)]
            [(css-number? t) (vector exn:css:range t)]
            [else (vector exn:css:type t)])))

  (define css-alpha->scalar : (-> CSS-Token (U Gamut CSS-Declared-Result))
    (lambda [t]
      (cond [(css:fraction%? t) (css:fraction%-datum t)]
            [(css:flunum=<-? t fl<= 1.0) (css:flunum-datum t)]
            [(css:byte=<-? t '(0 1)) (css:byte=> t fx->fl)]
            [(css-number? t) (vector exn:css:range t)]
            [else (vector exn:css:type t)])))

  (define css-hue->scalar : (-> CSS-Token (U Hue CSS-Declared-Result))
    (lambda [t]
      (cond [(css:angle? t) (real->hue (css:angle->datum t))]
            [(css:integer? t) (css:integer=> t real->hue)]
            [(css:flonum? t) (css:flonum=> t real->hue)]
            [(css:dimension? t) (vector exn:css:unit t)]
            [else (vector exn:css:type t)])))

  (define css-color-filter-delimiter : (->* (CSS-Token (Listof CSS-Token) Index) (Boolean)
                                            (Values CSS-Declared-Result Nonnegative-Fixnum))
    ;;; https://github.com/w3c/csswg-drafts/issues/266
    (lambda [id args n:min [comma? #true]]
      (define n:max : Positive-Fixnum (fx+ n:min 1))
      (let fold ([return : (Listof CSS-Token) null]
                 [size : Nonnegative-Fixnum 0]
                 [source : (Listof CSS-Token) args]
                 [podd? : Boolean #false]
                 [/? : Boolean #false])
        (cond [(null? source) (if (fx>= size n:min) (values (reverse return) size) (values (vector exn:css:missing-value id) size))]
              [else (let-values ([(token rest) (values (car source) (cdr source))])
                      (cond [(fx>= size n:max) (values (vector exn:css:overconsumption source) size)]
                            [(not (css:delim? token)) (fold (cons token return) (fx+ size 1) rest (not podd?) /?)]
                            [else (let ([d : Char (css:delim-datum token)]
                                        [following? : Boolean (pair? rest)])
                                    (cond [(and (char=? d #\,) podd? following? (or comma? /?)) (fold return size rest (not podd?) /?)]
                                          [(and (char=? d #\/) podd? following? (not /?)) (fold return size rest (not podd?) #true)]
                                          [(not following?) (values (vector exn:css:missing-value token) size)]
                                          [else (values (vector exn:css:unrecognized token) size)]))]))]))))
  
  (define css-apply-rgba : (-> (U CSS:Function CSS:Ident) (Listof CSS-Token) CSS-Declared-Result)
    ;;; https://drafts.csswg.org/css-color/#rgb-functions
    (lambda [frgba args]
      (define-values (rgba-tokens argsize) (css-color-filter-delimiter frgba args 3))
      (cond [(not (pair? rgba-tokens)) rgba-tokens]
            [else (let ([r (css-rgb->scalar (list-ref rgba-tokens 0))]
                        [g (css-rgb->scalar (list-ref rgba-tokens 1))]
                        [b (css-rgb->scalar (list-ref rgba-tokens 2))]
                        [a (if (fx= argsize 4) (css-alpha->scalar (last rgba-tokens)) 1.0)])
                    (cond [(not (byte? r)) r]
                          [(not (byte? g)) g]
                          [(not (byte? b)) b]
                          [(not (flonum? a)) a]
                          [else (css-remake-token [frgba (last args)] css:rgba (rgb-bytes->ufixnum r g b) a)]))])))

  (define css-apply-hsba : (-> (U CSS:Function CSS:Ident) (Listof CSS-Token) HSB->RGB CSS-Declared-Result)
    ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
    (lambda [fhsba args ->rgb]
      (define-values (hsba-tokens argsize) (css-color-filter-delimiter fhsba args 3))
      (cond [(not (pair? hsba-tokens)) hsba-tokens]
            [else (let ([h (css-hue->scalar (list-ref hsba-tokens 0))]
                        [s (list-ref hsba-tokens 1)]
                        [b (list-ref hsba-tokens 2)]
                        [a (if (fx= argsize 4) (css-alpha->scalar (last hsba-tokens)) 1.0)])
                    (cond [(not (flonum? h)) h]
                          [(not (css:fraction%? s)) (vector exn:css:type s)]
                          [(not (css:fraction%? b)) (vector exn:css:type b)]
                          [(not (flonum? a)) a]
                          [else (let-values ([(flr flg flb) (->rgb h (css:fraction%-datum s) (css:fraction%-datum b))])
                                  (css-remake-token [fhsba (last args)] css:rgba
                                                    (rgb-bytes->ufixnum (gamut->byte flr) (gamut->byte flg) (gamut->byte flb))
                                                    a))]))]))))

(require (submod "." css))

(define select-rgba-color : (->* (Color+sRGB) (Nonnegative-Flonum) (Instance Color%))
  (lambda [color-representation [alpha 1.0]]
    (define abyte : Byte (min (exact-round (fl* alpha 255.0)) #xFF))
    (define opaque? : Boolean (fx= abyte #xFF))
    (cond [(fixnum? color-representation)
           (define hashcode : Nonnegative-Fixnum (fxand color-representation #xFFFFFF))
           (hash-ref! the-color-pool
                      (if opaque? hashcode (eqv-hash-code (make-rectangular hashcode abyte)))
                      (thunk (let-values ([(r g b) (ufixnum->rgb-bytes color-representation)])
                               (make-color r g b (fl/ (fx->fl abyte) 255.0)))))]
          [(symbol? color-representation)
           (let try-again ([color-name : Symbol color-representation]
                           [downcased? : Boolean #false])
             (cond [(hash-has-key? css-named-colors color-name)
                    (hash-ref! the-color-pool
                               (cond [(and opaque?) (eq-hash-code color-name)]
                                     [else (equal-hash-code (cons color-name abyte))])
                               (thunk (select-rgba-color (hash-ref css-named-colors color-name) alpha)))]
                   [(not downcased?) (try-again (string->symbol (string-downcase (symbol->string color-name))) #true)]
                   [else (select-rgba-color 0 alpha)]))]
          [(string? color-representation)
           (let* ([color-name (string-downcase (string-replace color-representation #px"(?i:grey)" "gray"))]
                  [color (send the-color-database find-color color-name)])
             (cond [(false? color) (select-rgba-color 0 alpha)]
                   [else (hash-ref! the-color-pool
                                    (equal-hash-code (if opaque? color-name (cons color-name abyte)))
                                    (thunk (select-rgba-color (rgb-bytes->ufixnum (send color red) (send color green) (send color blue))
                                                              alpha)))]))]
          [else color-representation])))

(define css-declared-color-filter : (-> CSS-Token (Listof CSS-Token) CSS-Declared-Result)
  ;;; https://drafts.csswg.org/css-color/#color-type
  ;;; https://drafts.csswg.org/css-color/#named-colors
  ;;; https://drafts.csswg.org/css-color/#numeric-rgb
  ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
  ;;; https://drafts.csswg.org/css-color/#the-hwb-notation
  (lambda [color-value rest]
    (cond [(pair? rest) (vector exn:css:overconsumption rest)]
          [(css:ident? color-value)
           (define name : Symbol (css:ident-norm color-value))
           (cond [(hash-has-key? css-named-colors name) color-value]
                 [(memq name '(transparent currentcolor)) color-value]
                 [else exn:css:range])]
          [(css:hash? color-value)
           (define-values (maybe-rgb alpha) (css-hex-color->rgba (css:hash-norm color-value)))
           (cond [(false? maybe-rgb) exn:css:range]
                 [else (css-remake-token color-value css:rgba maybe-rgb alpha)])]
          [(css:function? color-value)
           (case (css:function-norm color-value)
             [(rgb rgba) (css-apply-rgba color-value (css:function-arguments color-value))]
             [(hsl hsla) (css-apply-hsba color-value (css:function-arguments color-value) hsl->rgb)]
             [(hsv hsva) (css-apply-hsba color-value (css:function-arguments color-value) hsv->rgb)]
             [(hsi hsia) (css-apply-hsba color-value (css:function-arguments color-value) hsi->rgb)]
             [(hwb hwba) (css-apply-hsba color-value (css:function-arguments color-value) hwb->rgb)]
             [else (vector exn:css:unrecognized color-value)])]
          [(css:string? color-value)
           (define name-raw : String (css:string-datum color-value))
           (define grey : (Option (Pairof String (Listof (Option String)))) (regexp-match #px"(?i:grey)" name-raw))
           (define name : String (if (pair? grey) (string-replace name-raw (car grey) "gray") name-raw))
           (cond [(not (send the-color-database find-color name)) exn:css:range]
                 [(pair? grey) (css-remake-token color-value css:string name)]
                 [else color-value])]
          [else exn:css:type])))

(define css-font-property-filter : (-> Symbol CSS-Token (Listof CSS-Token) CSS-Declared+Longhand-Result)
  ;;; https://drafts.csswg.org/css-fonts/#basic-font-props
  (lambda [suitcased-name font-value rest]
    (case suitcased-name
      [(font-weight)
       (cond [(pair? rest) (vector exn:css:overconsumption rest)]
             [(css:ident=<-? font-value '(normal bold bolder lighter)) font-value]
             [else exn:css:type])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (apply bitmap-vl-append
         (for/fold ([bitmap-descs : (Listof Bitmap) null])
                   ([width (in-list (list 32 16  2   2             32  32               32               256 256))]
                    [words (in-list (list "" " " "!" "bitmap-desc" "!" "Hello, Racket!" "Hello,\nWorld!"
                                          (string-join (list "That last testcase is not a mysterious phenomena:"
                                                             "It seemed to do some strange optimizing so that"
                                                             "no single letter would be printed in a single line."
                                                             "\n\nBug Fixed!"))
                                          "最后一个例子不是谜之现象，之前误以为它会自己优化而不会出现一行只有一个字母的情况。此缺陷已修正！"))])
           (define desc (bitmap-desc words width #:background-color "Honeydew" #:combine? #false))
           (define combined-desc (bitmap-desc words width #:background-color "Honeydew" #:combine? #true))
           (define-values (normal-width combined-width) (values (send desc get-width) (send combined-desc get-width)))
           (define-values (height combined-height) (values (send desc get-height) (send combined-desc get-height)))
           (append bitmap-descs
                   (list (bitmap-pin 1 1/2 0 1/2
                                     (bitmap-text "> ")
                                     (bitmap-text "(" #:color 'Sienna)
                                     (bitmap-hc-append #:gapsize 7
                                                       (bitmap-text "bitmap-desc" #:color 'Blue)
                                                       (bitmap-text (~s words) #:color 'Orange)
                                                       (bitmap-text (~a width) #:color 'Tomato))
                                     (bitmap-text ")" #:color 'Sienna))
                         (bitmap-text (format "- : (Bitmap ~a ~a)" normal-width height) #:color 'Purple)
                         (bitmap-pin 0 0 0 0
                                     (bitmap-frame desc #:margin 1 #:border-style 'transparent #:style 'transparent)
                                     (bitmap-frame (bitmap-blank width height) #:border-color 'Lavender))
                         (bitmap-text (format "- : (Bitmap ~a ~a #:combined)" combined-width combined-height) #:color 'Purple)
                         (bitmap-lt-superimpose (bitmap-frame combined-desc #:margin 1 #:border-style 'transparent #:style 'transparent)
                                                (bitmap-frame (bitmap-blank width combined-height) #:border-color 'Lavender)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require "css.rkt")
  (require (submod ".."))
  (require (submod "css.rkt" test/digitama))

  (current-css-media-type 'screen)
  (current-css-media-preferences
   ((inst make-hash Symbol CSS-Media-Datum)
    (list (cons 'orientation 'landscape)
          (cons 'width 1440)
          (cons 'height 820))))
  
  (define tamer-sheet : CSS-StyleSheet
    (time-run (read-css-stylesheet (simplify-path (build-path (find-system-path 'orig-dir)
                                                              (find-system-path 'run-file)
                                                              'up 'up "tamer" "tamer.css")))))
  
  (define tamer-subject : CSS-Subject (make-css-subject #:type 'body #:id '#:header))

  (define-configuration descriptors #:as Descriptors
    ([color : (Instance Color%)]
     [background-color : (Instance Color%)]
     [border-color : (Instance Color%)]
     [otherwise : (Option (HashTable Symbol Datum))])
    #:transparent)

  (define token->color : (-> Symbol CSS-Cascaded-Value (Instance Color%))
    (lambda [desc-name maybe-value]
      (cond [(css:rgba? maybe-value) (select-rgba-color (css:rgba-datum maybe-value) (css:rgba-alpha maybe-value))]
            [(css:ident? maybe-value) (css:ident=> maybe-value select-rgba-color)]
            [(css:string? maybe-value) (css:string=> maybe-value select-rgba-color)]
            [else (make-object color%)])))
  
  (define css-descriptor-filter : CSS-Declared-Value-Filter
    (lambda [suitcased-name desc-value rest]
      (define maybe-font-property : CSS-Declared+Longhand-Result (css-font-property-filter suitcased-name desc-value rest))
      (cond [(not (void? maybe-font-property)) (values maybe-font-property #false)]
            [else (values (case suitcased-name
                            [(color background-color border-color) (css-declared-color-filter desc-value rest)]
                            [else desc-value])
                          #false)])))

  (define css-filter : (CSS-Cascaded-Value-Filter (Option Descriptors))
    (lambda [declared-values all default-values inherit-values]
      (make-descriptors #:color (css-ref declared-values 'color token->color)
                        #:background-color (css-ref declared-values 'background-color token->color)
                        #:border-color (css-ref declared-values 'border-color token->color)
                        #:otherwise (for/hash : (HashTable Symbol Datum)
                                      ([desc-name (in-hash-keys declared-values)]
                                       #:when (not (memq desc-name '(color background-color border-color))))
                                      (values desc-name
                                              (css-ref declared-values desc-name
                                                       (λ [[desc-name : Symbol] [maybe-value : CSS-Cascaded-Value]]
                                                         (cond [(css-token? maybe-value) (css-token->datum maybe-value)]
                                                               [else (and maybe-value (map css-token->datum maybe-value))]))))))))
  
  tamer-subject
  (time-run (css-cascade (list tamer-sheet)
                         tamer-subject
                         css-descriptor-filter
                         css-filter
                         #false
                         #false)))
