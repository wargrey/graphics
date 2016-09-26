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
(define-type Color+sRGB (U Index Symbol String (Instance Color%)))

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
  (lambda [[basefont css-default-font] #:size [size -1.0] #:face [face #false] #:family [family #false]
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
              (or hinting (send basefont get-hinting))))))

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
  ;;; https://drafts.csswg.org/css-images                                                       ;;;
  ;;; https://drafts.csswg.org/css-fonts                                                        ;;;
  ;;; https://drafts.csswg.org/css-fonts-4                                                      ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require "css.rkt")
  (require "colorspace.rkt")

  (require racket/fixnum)
  (require typed/racket/draw)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-css-value css-color #:as CSS-Color ())
  (struct hexa css-color ([hex : Index] [a : Nonnegative-Flonum]) #:transparent #:type-name HEXA)
  (struct rgba css-color ([r : Byte] [g : Byte] [b : Byte] [a : Nonnegative-Flonum]) #:transparent #:type-name RGBA)
  (struct hsba css-color ([>rgb : HSB->RGB] [h : Real] [s : Real] [b : Real] [a : Nonnegative-Flonum]) #:transparent #:type-name HSBA)
  
  (define the-color-pool : (HashTable Fixnum (Instance Color%)) (make-hasheq))
  
  (define css-named-colors : (HashTable Symbol Index)
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

  (define css-hex-color->rgba : (-> Keyword (Values (Option Index) Nonnegative-Flonum))
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
      (cond [(or (fx= digits 3) (fx= digits 6)) (values (and (index? maybe-hexcolor) maybe-hexcolor) 1.0)]
            [(not (exact-integer? maybe-hexcolor)) (values #false 1.0)]
            [else (let ([hex-rgb (arithmetic-shift maybe-hexcolor -8)])
                    (values (and (index? hex-rgb) hex-rgb)
                            (flabs (fl/ (fx->fl (fxand maybe-hexcolor #xFF)) 255.0))))])))
  
  (define css-rgb->scalar : (-> CSS-Token (U Integer CSS-Syntax-Error))
    (lambda [t]
      (cond [(css:integer=<-? t byte?) => values]
            [(css:percentage=<-? t 0.0f0 <= 1.0f0) => (λ [%] (exact-round (* % 255.0)))]
            [(css:flonum=<-? t fl<= 255.0) => exact-round]
            [(css-number? t) (make-exn:css:range t)]
            [else (make-exn:css:type t)])))

  (define css-hue->scalar : (-> CSS-Token (U Real CSS-Syntax-Error))
    (lambda [t]
      (cond [(css:integer? t) (css:integer-datum t)]
            [(css:flonum? t) (css:flonum-datum t)]
            [else (css-declared-angle-filter t)])))

  (define css-sb%->scalar : (-> CSS-Token (U Single-Flonum CSS-Syntax-Error))
    (lambda [t]
      (cond [(css:percentage? t) (css:percentage-datum t)]
            [else (make-exn:css:type t)])))

  (define css-color-delimiter-filter : (->* (CSS-Token (Listof CSS-Token) Index) (Boolean)
                                            (Values (U (Listof CSS-Token) CSS-Syntax-Error) Nonnegative-Fixnum))
    ;;; https://github.com/w3c/csswg-drafts/issues/266
    (lambda [id args n:min [comma? #true]]
      (define n:max : Positive-Fixnum (fx+ n:min 1))
      (let fold ([return : (Listof CSS-Token) null]
                 [size : Nonnegative-Fixnum 0]
                 [source : (Listof CSS-Token) (filter-not css:whitespace? args)]
                 [podd? : Boolean #false]
                 [/? : Boolean #false])
        (cond [(null? source) (if (fx>= size n:min) (values (reverse return) size) (values (make-exn:css:missing-value id) size))]
              [else (let-values ([(token rest) (values (car source) (cdr source))])
                      (cond [(fx>= size n:max) (values (make-exn:css:overconsumption source) size)]
                            [(not (css:delim? token)) (fold (cons token return) (fx+ size 1) rest (not podd?) /?)]
                            [else (let ([d : Char (css:delim-datum token)]
                                        [following? : Boolean (pair? rest)])
                                    (cond [(and (char=? d #\,) podd? following? (or comma? /?)) (fold return size rest (not podd?) /?)]
                                          [(and (char=? d #\/) podd? following? (not /?)) (fold return size rest (not podd?) #true)]
                                          [(not following?) (values (make-exn:css:missing-value token) size)]
                                          [else (values (make-exn:css:unrecognized token) size)]))]))]))))
  
  (define css-extract-rgba : (-> (U CSS:Function CSS:Ident) (Listof CSS-Token) (U RGBA Void CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-color/#rgb-functions
    (lambda [frgba args]
      (define-values (rgba-tokens argsize) (css-color-delimiter-filter frgba args 3))
      (when (pair? rgba-tokens)
        (define r : (U Integer CSS-Syntax-Error) (css-rgb->scalar (list-ref rgba-tokens 0)))
        (define g : (U Integer CSS-Syntax-Error) (css-rgb->scalar (list-ref rgba-tokens 1)))
        (define b : (U Integer CSS-Syntax-Error) (css-rgb->scalar (list-ref rgba-tokens 2)))
        (define a : (U Nonnegative-Flonum CSS-Syntax-Error) (if (fx= argsize 4) (css-declared+percentage%-filter (last rgba-tokens)) 1.0))
        (cond [(and (byte? r) (byte? g) (byte? b) (nonnegative-flonum? a)) (rgba r g b a)]
              [(exn:css? r) r]
              [(exn:css? g) g]
              [(exn:css? b) b]
              [(exn:css? a) a]))))

  (define css-extract-hsba : (-> (U CSS:Function CSS:Ident) (Listof CSS-Token) HSB->RGB (U HSBA Void CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
    (lambda [fhsba args ->rgb]
      (define-values (hsba-tokens argsize) (css-color-delimiter-filter fhsba args 3))
      (when (pair? hsba-tokens)
        (define h : (U Real CSS-Syntax-Error) (css-hue->scalar (list-ref hsba-tokens 0)))
        (define s : (U Single-Flonum CSS-Syntax-Error) (css-sb%->scalar (list-ref hsba-tokens 1)))
        (define b : (U Single-Flonum CSS-Syntax-Error) (css-sb%->scalar (list-ref hsba-tokens 2)))
        (define a : (U Nonnegative-Flonum CSS-Syntax-Error) (if (fx= argsize 4) (css-declared+percentage%-filter (last hsba-tokens)) 1.0))
        (cond [(and (real? h) (single-flonum? s) (single-flonum? b) (nonnegative-flonum? a)) (hsba ->rgb h s b a)]
              [(exn:css? h) h]
              [(exn:css? s) s]
              [(exn:css? b) b]
              [(exn:css? a) a]))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-default-font : (Instance Font%) (make-font))
  
  (define css-font-style-option : (Listof Symbol) '(normal italic oblique slant))
  (define css-font-kerning-option : (Listof Symbol) '(auto normal none))
  (define css-font-position-option : (Listof Symbol) '(normal sub super))
  (define css-font-caps-option : (Listof Symbol) '(normal small-caps all-small-caps petite-caps all-petite-caps unicase titling-caps))
  (define css-font-weight-option : (Listof Symbol) '(normal bold bolder light lighter))
  (define css-font-size-option : (Listof Symbol) '(xx-small x-small small medium large x-large xx-large smaller larger))
  (define css-font-synthesis-options : (Listof Symbol) '(weight style small-caps))
  (define css-font-variant-options/21 : (Listof Symbol) '(normal small-caps))
  (define css-font-stretch-option : (Listof Symbol) '(normal condensed expanded ultra-condensed extra-condensed semi-condensed
                                                             ultra-expanded extra-expanded semi-expanded))
    
  (define css-system-font-names : (Listof Symbol) '(caption icon menu message-box small-caption status-bar))
  (define current-css-caption-font : (Parameterof (Instance Font%)) (make-parameter css-default-font))
  (define current-css-icon-font : (Parameterof (Instance Font%)) (make-parameter css-default-font))
  (define current-css-menu-font : (Parameterof (Instance Font%)) (make-parameter css-default-font))
  (define current-css-message-box-font : (Parameterof (Instance Font%)) (make-parameter css-default-font))
  (define current-css-small-caption-font : (Parameterof (Instance Font%)) (make-parameter css-default-font))
  (define current-css-status-bar-font : (Parameterof (Instance Font%)) (make-parameter css-default-font))
  
  (define css-reset!-other-font-properties : (-> CSS-Longhand-Values (Instance Font%) CSS-Longhand-Values)
    (lambda [font-longhand font]
      (hash-ref! font-longhand 'font-family (thunk (list (or (send font get-face) (send font get-family)))))
      (hash-ref! font-longhand 'font-weight (thunk (send font get-weight)))
      (hash-ref! font-longhand 'font-style (thunk (send font get-style)))
      (hash-ref! font-longhand 'font-stretch (thunk 'normal))
      (hash-ref! font-longhand 'line-height (thunk 'normal))
      (hash-ref! font-longhand 'font-size-adjust (thunk 'none))
      (hash-ref! font-longhand 'font-kerning (thunk 'auto))
      (hash-ref! font-longhand 'font-variant (thunk 'normal))
      (hash-ref! font-longhand 'font-kerning (thunk 'auto))
      (hash-ref! font-longhand 'font-language-override (thunk 'normal))
      (hash-ref! font-longhand 'font-size
                 (thunk (css-length->scalar (real->double-flonum (send font get-size))
                                            (if (or (send font get-size-in-pixels) (eq? (system-type 'os) 'macosx))
                                                'px 'pt))))
      font-longhand))

  (define css-set!-font-property : (-> CSS-Longhand-Values Symbol CSS-Datum CSS-Token (Listof CSS-Token) CSS+Longhand-Values)
    (lambda [font-longhand font-option property ptoken rest]
      (cond [(hash-has-key? font-longhand font-option) (make-exn:css:duplicate ptoken)]
            [(hash-set! font-longhand font-option property)
             (cond [(null? rest) (css-reset!-other-font-properties font-longhand css-default-font)]
                   [(not (eq? font-option 'font-size)) (css-font-shorthand-filter (car rest) (cdr rest) font-longhand #false)]
                   [else (let-values ([(maybe-/ rst) (values (car rest) (cdr rest))])
                           (cond [(not (css:delim=:=? maybe-/ #\/)) (css-font-shorthand-filter maybe-/ rst font-longhand #false)]
                                 [(null? rst) (make-exn:css:missing-value maybe-/)]
                                 [else (let* ([<line-height> (car rst)]
                                              [height (css-line-height-filter <line-height> null)])
                                         (unless (exn? height)
                                           (css-set!-font-property font-longhand 'line-height height <line-height> (cdr rst))))]))])])))

  (define css-font-family-filter : (-> CSS-Token (Listof CSS-Token) (U (Listof (U String Symbol)) CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-fonts/#font-family-prop
    ;;; https://drafts.csswg.org/css-fonts-4/#extended-generics
    (lambda [font-value rest]
      (define (identifier-join [tail : CSS:Ident] [family-heads : (Listof CSS:Ident)]) : String
        (string-join (for/list : (Listof String) ([part (in-list (reverse (cons tail family-heads)))])
                       (symbol->string (css:ident-datum part)))))
      (let font-fold ([seilimaf : (Listof (U String Symbol)) null]
                      [ylimaf : (Listof CSS:Ident) null]
                      [cvalue : CSS-Token font-value]
                      [rvalues : (Listof CSS-Token) rest])
        (define maybe-family : (U String Symbol False)
          (or (and (css:string? cvalue) (css:string-datum cvalue))
              (css:ident-norm=<-? cvalue '(default decorative roman script  swiss      modern    system    symbol
                                            emoji  fantasy    serif cursive sans-serif monospace system-ui math fangsong))))
        (cond [(and maybe-family)
               (define-values (maybe-term rst) (css-car rvalues))
               (cond [(pair? ylimaf) (make-exn:css:missing-delimiter cvalue)]
                     [(eof-object? maybe-term) (reverse (cons maybe-family seilimaf))]
                     [(not (css:delim=:=? maybe-term #\,)) (make-exn:css:missing-delimiter rvalues)]
                     [(null? rst) (make-exn:css:overconsumption rvalues)]
                     [else (font-fold (cons maybe-family seilimaf) null (car rst) (cdr rst))])]
              [(css:ident? cvalue)
               (define-values (maybe-term rst) (css-car rvalues))
               (cond [(eof-object? maybe-term) (reverse (cons (identifier-join cvalue ylimaf) seilimaf))]
                     [(not (css:delim=:=? maybe-term #\,)) (font-fold seilimaf (cons cvalue ylimaf) maybe-term rst)]
                     [(null? rst) (make-exn:css:overconsumption rvalues)]
                     [else (font-fold (cons (identifier-join cvalue ylimaf) seilimaf) null (car rst) (cdr rst))])]
              [else (make-exn:css:type cvalue)]))))

  (define css-line-height-filter : (-> CSS-Token (Listof CSS-Token) (U Symbol CSS-Wide-Keyword Nonnegative-Flonum CSS-Syntax-Error))
    ;;; http://www.w3.org/TR/CSS2/visudet.html#propdef-line-height
    (lambda [font-value rest]
      (cond [(css-declared-keyword-filter font-value rest '(normal inherit) #false) => css-wide-keywords-filter-map]
            [(css-declared+number%-filter font-value #false) => (make-css->racket real->double-flonum)]
            [else (css-declared+length-filter font-value)])))

  (define css-font-numeric-size-filter : (case-> [CSS-Token -> (U Nonnegative-Inexact-Real CSS-Syntax-Error)]
                                                 [CSS-Token True -> (U Nonnegative-Inexact-Real CSS-Syntax-Error)]
                                                 [CSS-Token False -> (U Nonnegative-Inexact-Real CSS-Syntax-Error False)])
    ;;; https://drafts.csswg.org/css-fonts/#font-size-prop
    (lambda [font-value [terminate? #true]]
      (cond [(css-declared+number-filter font-value #false) => (make-css->racket real->double-flonum)]
            [(css-declared+number%-filter font-value #false) => (make-css->racket real->single-flonum)]
            [else (css-declared+length-filter font-value terminate?)])))

  (define css-font-size-filter : (case-> [CSS-Token (Listof CSS-Token) -> (U Symbol Nonnegative-Inexact-Real CSS-Syntax-Error)]
                                         [CSS-Token (Listof CSS-Token) True -> (U Symbol Nonnegative-Inexact-Real CSS-Syntax-Error)]
                                         [CSS-Token (Listof CSS-Token) False -> (U Symbol Nonnegative-Inexact-Real
                                                                                   CSS-Syntax-Error False)])
    ;;; https://drafts.csswg.org/css-fonts/#font-size-prop
    (lambda [font-value rest [terminate? #true]]
      (cond [(css-declared-keyword-filter font-value rest css-font-size-option #false) => values]
            [else (css-font-numeric-size-filter font-value terminate?)])))

  (define css-font-shorthand-filter : (->* (CSS-Token (Listof CSS-Token)) (CSS-Longhand-Values Boolean) CSS+Longhand-Values)
    ;;; https://drafts.csswg.org/css-fonts/#font-prop
    (lambda [property rst [font-longhand ((inst make-hasheq Symbol CSS-Datum))] [initial? #true]]
      (define keyword : (Option Symbol) (and (css:ident? property) (css:ident-norm property)))
      (define font-hint : (U Symbol Nonnegative-Flonum Nonnegative-Single-Flonum Integer (Listof (U String Symbol)) CSS-Syntax-Error)
        (cond [(and keyword)
               (cond [(eq? keyword 'normal) keyword]
                     [(memq keyword css-font-style-option) 'font-style]
                     [(memq keyword css-font-variant-options/21) 'font-variant]
                     [(memq keyword css-font-weight-option) 'font-weight]
                     [(memq keyword css-font-stretch-option) 'font-stretch]
                     [(memq keyword css-font-size-option) 'font-size]
                     [(and initial? (memq keyword css-system-font-names) (if (null? rst) keyword (make-exn:css:overconsumption rst)))]
                     [else (css-font-family-filter property rst)])]
              [(css:string? property) (css-font-family-filter property rst)]
              [(css:integer=<-? property 1 <= 999) => values]
              [(css-font-numeric-size-filter property #false) => values]
              [else (make-exn:css:unrecognized property)]))
      (cond [(symbol? font-hint)
             (if (eq? font-hint 'normal) ; ignore the default option (also, this option can be applied to 3 properties)
                 (cond [(null? rst) (css-reset!-other-font-properties font-longhand css-default-font)]
                       [else (css-font-shorthand-filter (car rst) (cdr rst) font-longhand #false)])
                 (case font-hint
                   [(caption) (css-reset!-other-font-properties font-longhand (current-css-caption-font))]
                   [(small-caption) (css-reset!-other-font-properties font-longhand (current-css-small-caption-font))]
                   [(icon) (css-reset!-other-font-properties font-longhand (current-css-icon-font))]
                   [(menu) (css-reset!-other-font-properties font-longhand (current-css-menu-font))]
                   [(message-box) (css-reset!-other-font-properties font-longhand (current-css-message-box-font))]
                   [(status-bar) (css-reset!-other-font-properties font-longhand (current-css-status-bar-font))]
                   [(font-size) (css-set!-font-property font-longhand 'font-size font-hint property rst)]
                   [else (css-set!-font-property font-longhand font-hint (or keyword 'should-not-happen) property rst)]))]
            [(integer? font-hint) (css-set!-font-property font-longhand 'font-weight font-hint property rst)]
            [(real? font-hint) (css-set!-font-property font-longhand 'font-size font-hint property rst)]
            [(list? font-hint) ; this branch will exist only once
             (hash-set! font-longhand 'font-family font-hint)
             (css-reset!-other-font-properties font-longhand css-default-font)]
            [else font-hint]))))

(require (submod "." css))

(define select-rgba-color : (->* (Color+sRGB) (Nonnegative-Flonum) (Instance Color%))
  (lambda [color-representation [alpha 1.0]]
    (define abyte : Byte (min (exact-round (fl* alpha 255.0)) #xFF))
    (define opaque? : Boolean (fx= abyte #xFF))
    (cond [(fixnum? color-representation)
           (define hashcode : Nonnegative-Fixnum (fxand color-representation #xFFFFFF))
           (hash-ref! the-color-pool
                      (if opaque? hashcode (eqv-hash-code (make-rectangular hashcode abyte)))
                      (thunk (let-values ([(r g b) (hex->rgb-bytes color-representation)])
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
                                    (thunk (select-rgba-color (rgb-bytes->hex (send color red) (send color green) (send color blue))
                                                              alpha)))]))]
          [else color-representation])))

(define css-declared-color-filter : (-> CSS-Token (Listof CSS-Token) (U Color+sRGB CSS-Color Void CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-color/#color-type
  ;;; https://drafts.csswg.org/css-color/#named-colors
  ;;; https://drafts.csswg.org/css-color/#numeric-rgb
  ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
  ;;; https://drafts.csswg.org/css-color/#the-hwb-notation
  (lambda [color-value rest]
    (cond [(pair? rest) (make-exn:css:overconsumption rest)]
          [(css:ident? color-value)
           (define name : Symbol (css:ident-norm color-value))
           (cond [(hash-has-key? css-named-colors name) name]
                 [(memq name '(transparent currentcolor)) name]
                 [else (make-exn:css:range color-value)])]
          [(css:hash? color-value)
           (define-values (maybe-rgb alpha) (css-hex-color->rgba (css:hash-norm color-value)))
           (cond [(false? maybe-rgb) (make-exn:css:range color-value)]
                 [(fl= alpha 1.0) maybe-rgb]
                 [else (hexa maybe-rgb alpha)])]
          [(css:function? color-value)
           (case (css:function-norm color-value)
             [(rgb rgba) (css-extract-rgba color-value (css:function-arguments color-value))]
             [(hsl hsla) (css-extract-hsba color-value (css:function-arguments color-value) hsl->rgb)]
             [(hsv hsva) (css-extract-hsba color-value (css:function-arguments color-value) hsv->rgb)]
             [(hsi hsia) (css-extract-hsba color-value (css:function-arguments color-value) hsi->rgb)]
             [(hwb hwba) (css-extract-hsba color-value (css:function-arguments color-value) hwb->rgb)]
             [else (make-exn:css:range color-value)])]
          [(css:string? color-value)
           (define name-raw : String (css:string-datum color-value))
           (define grey : (Option (Pairof String (Listof (Option String)))) (regexp-match #px"(?i:grey)" name-raw))
           (define name : String (if (pair? grey) (string-replace name-raw (car grey) "gray") name-raw))
           (cond [(send the-color-database find-color name) name]
                 [else (make-exn:css:range color-value)])]
          [else (make-exn:css:type color-value)])))

(define css-color->color% : (case-> [CSS-Datum -> (Instance Color%)]
                                    [CSS-Datum True -> (Instance Color%)]
                                    [CSS-Datum False -> (Option (Instance Color%))])
  (lambda [color [terminate? #true]]
    (cond [(or (index? color) (string? color) (symbol? color)) (select-rgba-color color)]
          [(hexa? color) (select-rgba-color (hexa-hex color) (hexa-a color))]
          [(rgba? color) (select-rgba-color (rgb-bytes->hex (rgba-r color) (rgba-g color) (rgba-b color)) (rgba-a color))]
          [(hsba? color) (select-rgba-color (hsb->rgb-hex (hsba->rgb color) (hsba-h color) (hsba-s color) (hsba-b color)) (hsba-a color))]
          [(object? color) (cast color (Instance Color%))]
          [else (and terminate? (select-rgba-color #x000000))])))

(define css-font-property-filter : (-> Symbol CSS-Token (Listof CSS-Token) (Option CSS+Longhand-Values))
  ;;; https://drafts.csswg.org/css-fonts/#basic-font-props
  ;;; https://drafts.csswg.org/css-fonts-4/#expanded-font-weight-scale
  (lambda [suitcased-name font-value rest [terminate? #true]]
    (case suitcased-name
      [(font) (css-font-shorthand-filter font-value rest)]
      [(font-family) (css-font-family-filter font-value rest)]
      [(font-style) (css-declared-keyword-filter font-value rest css-font-style-option)]
      [(font-kerning) (css-declared-keyword-filter font-value rest css-font-kerning-option)]
      [(font-variant-position) (css-declared-keyword-filter font-value rest css-font-position-option)]
      [(font-variant-caps) (css-declared-keyword-filter font-value rest css-font-caps-option)]
      [(font-stretch) (css-declared-keyword-filter font-value rest css-font-stretch-option)]
      [(font-size min-font-size max-font-size) (css-font-size-filter font-value rest)]
      [(line-height) (css-line-height-filter font-value rest)]
      [(font-synthesis) (css-declared-keywords-filter font-value rest css-font-synthesis-options 'none)]
      [(font-weight)
       (cond [(css-declared-keyword-filter font-value rest css-font-weight-option #false) => values]
             [(css:integer=<-? font-value 1 <= 999) => values]
             [(css:integer? font-value) (make-exn:css:range font-value)]
             [else (make-exn:css:type font-value)])]
      [(font-size-adjust)
       (cond [(pair? rest) (make-exn:css:overconsumption rest)]
             [(css:ident=:=? font-value 'none) => values]
             [(css-declared+number-filter font-value) => values]
             [(css:ident? font-value) (make-exn:css:range font-value)]
             [else (make-exn:css:type font-value)])]
      [else #false])))

(define css-font-filter : (CSS-Cascaded-Value-Filter (Instance Color%))
  (lambda [declared-values initial-font inherit-font]
    initial-font))

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
  
  (define tamer-sheet : CSS-StyleSheet (time-run (read-css-stylesheet tamer.css)))
  (define tamer-bitmap-test : CSS-Subject (make-css-subject #:type 'module #:id '#:header #:classes '(main)))

  (define-preference bmp #:as Bitmap-Test-Preference
    ([function-color : Color+sRGB                             #:= 'Blue]
     [string-color : Color+sRGB                               #:= 'Orange]
     [number-color : Color+sRGB                               #:= 'Tomato]
     [type-color : Color+sRGB                                 #:= 'Purple]
     [paran-color : Color+sRGB                                #:= 'Sienna]
     [border-color : Color+sRGB                               #:= 'Lavender]
     [background-color : Color+sRGB                           #:= "Honeydew"]
     [otherwise : (Option (Listof (Pairof Symbol CSS-Datum))) #:= #false])
    #:transparent)

  (define css-descriptor-filter : CSS-Declaration-Filter
    (lambda [suitcased-name desc-value rest]
      (values (cond [(css-font-property-filter suitcased-name desc-value rest) => values]
                    [(memq suitcased-name '(paran-color function-color string-color number-color type-color
                                                        background-color border-color))
                     => (λ [v] (css-declared-color-filter desc-value rest))]
                    [else (map css-token->datum (cons desc-value rest))])
              #false)))

  (define css-preference-filter : (CSS-Cascaded-Value-Filter Bitmap-Test-Preference)
    (lambda [declared-values initial-values inherit-values]
      (make-bmp #:function-color (css-color->color% (css-ref declared-values inherit-values 'function-color))
                #:string-color (css-color->color% (css-ref declared-values inherit-values 'string-color))
                #:number-color (css-color->color% (css-ref declared-values inherit-values 'number-color))
                #:type-color (css-color->color% (css-ref declared-values inherit-values 'type-color))
                #:paran-color (css-color->color% (css-ref declared-values inherit-values 'paran-color))
                #:border-color (css-color->color% (css-ref declared-values inherit-values 'border-color))
                #:background-color (css-color->color% (css-ref declared-values inherit-values 'background-color))
                #:otherwise (for/list : (Listof (Pairof Symbol CSS-Datum)) ([desc-name (in-hash-keys declared-values)])
                              (cons desc-name
                                    (css-ref declared-values #false desc-name
                                             (λ [[desc-name : Symbol] [desc-value : CSS-Datum]]
                                               desc-value)))))))
  
  tamer-bitmap-test
  (define btp : Bitmap-Test-Preference
    (time-run (let-values ([(preference for-children)
                            (css-cascade (list tamer-sheet)
                                         tamer-bitmap-test
                                         css-descriptor-filter
                                         css-preference-filter
                                         (make-bmp)
                                         #false)])
                preference)))

  btp
  (apply bitmap-vl-append
         (for/fold ([bitmap-descs : (Listof Bitmap) null])
                   ([width (in-list (list 32 16  2   2             32  32               32               256 256))]
                    [words (in-list (list "" " " "!" "bitmap-desc" "!" "Hello, Racket!" "Hello,\nWorld!"
                                          (string-join (list "That last testcase is not a mysterious phenomena:"
                                                             "It seemed to do some strange optimizing so that"
                                                             "no single letter would be printed in a single line."
                                                             "\n\nBug Fixed!"))
                                          "最后一个例子不是谜之现象，之前误以为它会自己优化而不会出现一行只有一个字母的情况。此缺陷已修正！"))])
           (define desc (bitmap-desc words width #:background-color (bmp-background-color btp) #:combine? #false))
           (define combined-desc (bitmap-desc words width #:background-color (bmp-background-color btp) #:combine? #true))
           (define-values (normal-width combined-width) (values (send desc get-width) (send combined-desc get-width)))
           (define-values (height combined-height) (values (send desc get-height) (send combined-desc get-height)))
           (append bitmap-descs
                   (list (bitmap-pin 1 1/2 0 1/2
                                     (bitmap-text "> ")
                                     (bitmap-text "(" #:color (bmp-paran-color btp))
                                     (bitmap-hc-append #:gapsize 7
                                                       (bitmap-text "bitmap-desc" #:color (bmp-function-color btp))
                                                       (bitmap-text (~s words) #:color (bmp-string-color btp))
                                                       (bitmap-text (~a width) #:color (bmp-number-color btp)))
                                     (bitmap-text ")" #:color (bmp-paran-color btp)))
                         (bitmap-text (format "- : (Bitmap ~a ~a)" normal-width height) #:color (bmp-type-color btp))
                         (bitmap-pin 0 0 0 0
                                     (bitmap-frame desc #:margin 1 #:border-style 'transparent #:style 'transparent)
                                     (bitmap-frame (bitmap-blank width height) #:border-color (bmp-border-color btp)))
                         (bitmap-text (format "- : (Bitmap ~a ~a #:combined)" combined-width combined-height) #:color 'Purple)
                         (bitmap-lt-superimpose (bitmap-frame combined-desc #:margin 1 #:border-style 'transparent #:style 'transparent)
                                                (bitmap-frame #:border-color (bmp-border-color btp)
                                                              (bitmap-blank width combined-height))))))))