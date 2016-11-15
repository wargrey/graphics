#lang at-exp typed/racket

(provide (all-defined-out) Color+sRGB Color Bitmap Font)
(provide (all-from-out "colorspace.rkt"))
(provide (all-from-out typed/racket/draw images/flomap typed/images/logos typed/images/icons))

@require{css.rkt}
@require{colorspace.rkt}

(require racket/fixnum)
(require racket/flonum)

(require typed/racket/draw)

(require (except-in images/flomap flomap->bitmap bitmap->flomap))
(require typed/images/logos)
(require typed/images/icons)

(define-type Flomap flomap)

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
(define make-font+ : (->* () (Font #:size Real #:face (Option String) #:size-in-pixels? (U Boolean Symbol) #:family (Option Font-Family)
                                   #:style (Option Font-Style) #:weight (Option Font-Weight) #:hinting (Option Font-Hinting)
                                   #:underlined? (U Boolean Symbol) #:smoothing (Option Font-Smoothing)) Font)
  (lambda [[basefont (current-css-default-font)] #:size [size +nan.0] #:face [face #false] #:family [family #false]
           #:style [style #false] #:weight [weight #false] #:hinting [hinting #false] #:smoothing [smoothing #false]
           #:underlined? [underlined 'default] #:size-in-pixels? [size-in-pixels? 'default]]
    (define ?face : (Option String) (or face (send basefont get-face)))
    (define underlined? : Boolean (if (boolean? underlined) underlined (send basefont get-underlined)))
    (define pixels? : Boolean (if (boolean? size-in-pixels?) size-in-pixels? (send basefont get-size-in-pixels)))
    (define fontsize : Real (min 1024.0 (cond [(positive? size) size]
                                              [(or (zero? size) (nan? size)) (smart-font-size basefont)]
                                              [else (* (- size) (smart-font-size basefont))])))
    (if (string? ?face)
        (send the-font-list find-or-create-font fontsize ?face
              (or family (send basefont get-family)) (or style (send basefont get-style))
              (or weight (send basefont get-weight)) underlined?
              (or smoothing (send basefont get-smoothing)) pixels?
              (or hinting (send basefont get-hinting)))
        (send the-font-list find-or-create-font fontsize
              (or family (send basefont get-family)) (or style (send basefont get-style))
              (or weight (send basefont get-weight)) underlined?
              (or smoothing (send basefont get-smoothing)) pixels?
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

(define bitmap-text : (->* (String) (Font #:combine? Boolean #:color (Option Color+sRGB)
                                          #:background-color (Option Color+sRGB)) Bitmap)
  (lambda [content [font (current-css-default-font)] #:combine? [combine? #true] #:color [fgcolor #false]
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

(define bitmap-desc : (->* (String Nonnegative-Real)
                           (Font #:combine? Boolean #:color (Option Color+sRGB)
                                 #:background-color (Option Color+sRGB)) Bitmap)
  (lambda [description max-width.0 [font (current-css-default-font)] #:combine? [combine? #true] #:color [fgcolor #false]
           #:background-color [bgcolor #false]]
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank)))
    (define max-width : Nonnegative-Flonum (real->double-flonum max-width.0))
    (define desc-extent : (-> String Integer Integer (Values String Nonnegative-Flonum Nonnegative-Flonum))
      (lambda [desc start end]
        (define subdesc : String (substring desc start end))
        (define-values (width height descent ascent) (send dc get-text-extent subdesc font combine?))
        (values subdesc (real->double-flonum width) (real->double-flonum height))))

    (define-values (_ char-width phantom-height) (desc-extent " " 0 1))
    (define-values (descs ys width height)
      (for/fold ([descs : (Listof String) null] [ys : (Listof Flonum) null] [width : Nonnegative-Flonum 0.0] [height : Flonum 0.0])
                ([desc : String (in-list (string-split description (string #\newline)))])
        (define terminal : Index (string-length desc))
        (let desc-row ([idx0 : Nonnegative-Fixnum 0] [descs : (Listof String) descs] [ys : (Listof Flonum) ys]
                                                     [Widthn : Nonnegative-Flonum width] [yn : Flonum height])
          (define-values (descn widthn heightn idx)
            (let desc-col-expt : (Values String Nonnegative-Flonum Flonum Nonnegative-Fixnum)
              ([interval : Nonnegative-Fixnum 1] [open : Nonnegative-Fixnum idx0] [close : Nonnegative-Fixnum terminal]
                                                 [backtracking : String ""] ; char-width is bad for forecasting the next width 
                                                 [back-width : Nonnegative-Flonum max-width]
                                                 [back-height : Nonnegative-Flonum phantom-height])
              (define idx : Nonnegative-Fixnum (fxmin close (fx+ open interval)))
              (define next-open : Nonnegative-Fixnum (fx+ open (fxquotient interval 2)))
              (define-values (line width height) (desc-extent desc idx0 idx))
              (define found? : Boolean (and (fl> width max-width) (fx= interval 1) (fl<= back-width max-width)))
              (cond [found? (values backtracking back-width back-height (if (zero? open) terminal (max 0 (sub1 idx))))]
                    [(fl> width max-width) (desc-col-expt 1 next-open idx backtracking back-width back-height)]
                    [(fx= close idx) (values line width height idx)]
                    [else (desc-col-expt (fxlshift interval 1) open close line width height)])))
          (cond [(fx= idx terminal) (values (cons descn descs) (cons yn ys) (max widthn Widthn) (fl+ yn heightn))]
                [else (desc-row idx (cons descn descs) (cons yn ys) (max widthn Widthn) (fl+ yn heightn))]))))
    
    (send dc set-bitmap (bitmap-blank (max 1 width) (max 1 phantom-height height)))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-rgba-color fgcolor)))
    (unless (false? bgcolor)
      (define color : Color (select-rgba-color bgcolor))
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
  ;;; https://drafts.csswg.org/css-text-decor                                                   ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require "css.rkt")
  (require "colorspace.rkt")

  (require racket/fixnum)

  (require typed/racket/draw)

  (require (for-syntax syntax/parse))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-type Color (Instance Color%))
  (define-type Color+sRGB (U Index Symbol String Color))
  (define-type CSS-Color-Datum (U Color+sRGB CSS-Color))

  (define-css-value css-color #:as CSS-Color ())
  (define-css-value hexa #:as HEXA #:=> css-color ([hex : Index] [a : Nonnegative-Flonum]))
  (define-css-value rgba #:as RGBA #:=> css-color ([r : Byte] [g : Byte] [b : Byte] [a : Nonnegative-Flonum]))
  (define-css-value hsba #:as HSBA #:=> css-color ([>rgb : HSB->RGB] [h : Real] [s : Real] [b : Real] [a : Nonnegative-Flonum]))
  
  (define the-color-pool : (HashTable Fixnum Color) (make-hasheq))
  
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

  (define-css-atomic-filter <css#color> #:-> (U Index HEXA) #:with [[color-value : css:hash?]]
    (define-values (?rgb alpha) (css-hex-color->rgba (css:hash-datum color-value)))
    (cond [(false? ?rgb) (make-exn:css:range color-value)]
          [(fl= alpha 1.0) ?rgb]
          [else (hexa ?rgb alpha)])
    #:where
    [(define (css-hex-color->rgba [hash-color : Keyword]) : (Values (Option Index) Nonnegative-Flonum)
       ;;; https://drafts.csswg.org/css-color/#numeric-rgb
       (define color : String (keyword->string hash-color))
       (define digits : Index (string-length color))
       (define ?hexcolor : (Option Number)
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
       (cond [(or (fx= digits 3) (fx= digits 6)) (values (and (index? ?hexcolor) ?hexcolor) 1.0)]
             [(not (exact-integer? ?hexcolor)) (values #false 1.0)]
             [else (let ([hex-rgb (arithmetic-shift ?hexcolor -8)])
                     (values (and (index? hex-rgb) hex-rgb)
                             (flabs (fl/ (fx->fl (fxand ?hexcolor #xFF)) 255.0))))]))])

  (define-css-function-filter <css-color-notation> #:-> CSS-Color
    ;;; https://drafts.csswg.org/css-color/#rgb-functions
    ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
    ;;; https://drafts.csswg.org/css-color/#the-hwb-notation
    [(rgba rgb) #:=> [(rgba [r ? byte?] [g ? byte?] [b ? byte?] [alpha ? nonnegative-flonum?])]
     (make-parser <:rgb:> <:rgb:>)]
    [(hsla hsl) #:=> [(hsba hsl->rgb [h ? real?] [s ? single-flonum?] [l ? single-flonum?] [alpha ? nonnegative-flonum?])]
     (make-parser <:hue:> (CSS<^> (<css:percentage>)))]
    [(hsva hsv) #:=> [(hsba hsv->rgb [h ? real?] [s ? single-flonum?] [v ? single-flonum?] [alpha ? nonnegative-flonum?])]
     (make-parser <:hue:> (CSS<^> (<css:percentage>)))]
    [(hsia hsi) #:=> [(hsba hsi->rgb [h ? real?] [s ? single-flonum?] [i ? single-flonum?] [alpha ? nonnegative-flonum?])]
     (make-parser <:hue:> (CSS<^> (<css:percentage>)))]
    [(hwba hwb) #:=> [(hsba hwb->rgb [h ? real?] [w ? single-flonum?] [b ? single-flonum?] [alpha ? nonnegative-flonum?])]
     (make-parser <:hue:> (CSS<^> (<css:percentage>)))]
    #:where
    [(define-css-disjoined-filter <rgb-byte> #:-> Integer
       (<css:integer> byte?)
       (CSS:<~> (<css:percentage> 0.0f0 <= 1.0f0) (λ [[% : Single-Flonum]] (exact-round (* % 255.0))))
       (CSS:<~> (<css:flonum> 0.0 fl<= 255.0) exact-round))

     (define make-alpha-parser : (-> (-> (CSS:Filter Char)) (CSS-Parser (Listof CSS-Datum)))
       (lambda [<delimiter>]
         (CSS<$> (CSS<?> [(<delimiter>) (CSS<^> (<css-%flunit>))]) 1.0)))
     
     (define make-parser : (-> (CSS-Parser (Listof CSS-Datum)) (CSS-Parser (Listof CSS-Datum)) (CSS-Parser (Listof CSS-Datum)))
       ;;; https://github.com/w3c/csswg-drafts/issues/266
       (lambda [c1 c2]
         (CSS<&> c1 (CSS<?> [(<css-comma>) (CSS<#> c2 '(2)) (make-alpha-parser <css-comma>)]
                            [else          (CSS<*> c2 '(2)) (make-alpha-parser <css-slash>)]))))

     (define <:rgb:> (CSS<^> (<rgb-byte>)))
     (define <:hue:> (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>) (<css:angle>))))])

  (define-css-atomic-filter <racket-colorbase> #:-> Color
    #:with [[color-value : css:string?] [px : Regexp #px"(?i:grey)$"] [to : String "gray"]]
    (define name : String (string-replace (css:string-datum color-value) px to))
    (cond [(send the-color-database find-color name) => values]
          [else (make-exn:css:range color-value)]))
  
  (define-css-racket-value-filter <racket-color> #:with ?color #:as (U String Symbol Index Color)
    [(is-a? ?color color%) (cast ?color Color)]
    [(index? ?color) ?color]
    [(and (symbol? ?color)
          (or (and (hash-has-key? css-named-colors ?color) ?color)
              (let ([color (string->symbol (string-downcase (symbol->string ?color)))])
                (and (hash-has-key? css-named-colors color) color)))) => values]
    [(and (string? ?color) (send the-color-database find-color ?color)) ?color])

  (define-css-disjoined-filter <css-color> #:-> (U CSS-Color-Datum CSS-Wide-Keyword)
    ;;; https://drafts.csswg.org/css-color/#color-type
    ;;; https://drafts.csswg.org/css-color/#named-colors
    #:with [[hint? : Any #false]]
    (CSS:<~> (<css-keyword> (cons 'currentcolor (cons 'transparent (hash-keys css-named-colors))))
             (λ [[c : Symbol]] (cond [(not (eq? c 'currentcolor)) c]
                                     [(false? hint?) c]
                                     [else css:inherit])))
    (<css#color>)
    (<css-color-notation>)
    (<racket-colorbase>)
    (<racket-color>))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-type Bitmap (Instance Bitmap%))
  (define-type CSS-Image-Datum (U CSS-@λ CSS-Image String))

  (define-css-value css-image #:as CSS-Image ())
  (define-css-value image #:as Image #:=> css-image ([content : CSS-Image-Datum] [fallback : CSS-Color-Datum]))
  (define-css-value image-set #:as Image-Set #:=> css-image ([options : (Listof (List CSS-Image-Datum Flonum))]))

  (define the-image-pool : (HashTable (U CSS-Image String) Bitmap) (make-hash))

  (define-@λ-pool the-@icon-pool #:λnames #px"-(icon|logo)$"
    images/logos images/icons/arrow images/icons/control
    images/icons/file images/icons/misc images/icons/stickman
    images/icons/symbol images/icons/tool)

  (define-@λ-pool the-@draw-pool #:λnames [make-font make-pen make-brush] racket/draw)

  (define css-@draw-filter : CSS-@λ-Filter
    (lambda [λname ?λ:kw]
      (case (or ?λ:kw λname)
        [(#:size) (CSS:<+> (<css:integer> 0 < 1024) (<css:flonum> 0.0 fl< 1024.0))]
        [(#:face) (CSS:<+> (<css:string>) (<css:escape> false?))]
        [(#:family) (<css:ident> (make-predicate Font-Family))]
        [(#:style) (<css:ident> (make-predicate Font-Style))]
        [(#:weight) (<css:ident> (make-predicate Font-Weight))]
        [(#:underlined? #:size-in-pixels?) (<css:escape> boolean?)]
        [(#:smoothing) (<css:ident> racket-font-smoothing?)]
        [(#:hinting) (<css:ident> racket-font-hinting?)])))
  
  (define css-@icon-filter : CSS-@λ-Filter
    (lambda [λname ?λ:kw]
      (define-css-disjoined-filter <text-icon-font> #:-> (U Font CSS-@λ)
        (<css-system-font>)
        (<racket-font>)
        (<css:@λ> the-@draw-pool css-@draw-filter '(make-font)))
      (define <:clock-pointers:> : (CSS-Parser (Listof CSS-Datum))
        (CSS<$> (CSS<&> (CSS<^> (<css:integer> 0 <= 11))
                        (CSS<$> (CSS<^> (CSS:<+> (<css:integer> 0 <= 60)
                                                 (<css:flonum> 0.0 fl<= 60.0)))))))
      (case (or ?λ:kw λname)
        [(#:backing-scale) (<css+real> '#:nonzero)]
        [(#:height #:thickness #:outline) (<css+%real>)]
        [(#:material) (<css:ident> '(plastic-icon-material rubber-icon-material glass-icon-material metal-icon-material))]
        [(#:trim?) (<css:escape> boolean?)]
        [(text-icon) (CSS<&> (CSS<^> (<css:string>)) (CSS<$> (CSS<^> (<text-icon-font>))))]
        [(regular-polygon-icon) (CSS<&> (CSS<^> (<css-natural> '#:nonzero)) (CSS<$> (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>)))))]
        [(lock-icon) (CSS<$> (CSS<^> (<css:escape> boolean?)))]
        [(running-stickman-icon) (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>)))]
        [else (if ?λ:kw (<css-color>) <:clock-pointers:>)])))

  (define-css-function-filter <css-image-notation> #:-> CSS-Image
    ;;; https://drafts.csswg.org/css-images/#image-notation
    ;;; https://drafts.csswg.org/css-images/#image-set-notation
    [(image) #:=> [(image "" [fallback ? index? string? symbol? css-color?])
                   (image [content ? css-image? string? css-@λ?] [fallback ? index? string? symbol? css-color?])]
     (CSS<+> (CSS<^> (<css-color>)) ; NOTE: both color and url accept strings, however their domains are not intersective.
             (CSS<&> (CSS<^> (CSS:<+> (<css-image>) (<css:string>)))
                     (CSS<$> (CSS<?> [(<css-comma>) (CSS<^> (<css-color>))]) 'transparent)))]
    [(image-set) #:=> [(image-set [options ? css-image-sets?])]
     (CSS<!> (CSS<#> (CSS<!> (CSS<^> (list (CSS:<+> (<css:string>) (<css-image>))
                                           (λ [[token : CSS-Syntax-Any]] : (CSS-Option Flonum)
                                             (and (css:dimension? token)
                                                  (let* ([datum (css:dimension-datum token)]
                                                         [unit (css:dimension-unit token)]
                                                         [scalar (css-resolution->scalar datum unit)])
                                                    (cond [(fl> scalar 0.0) scalar]
                                                          [(fl<= datum 0.0) (make-exn:css:range token)]
                                                          [(eq? unit 'x) datum]
                                                          [else (make-exn:css:type token)])))))))))]
    #:where
    [(define-predicate css-image-datum? CSS-Image-Datum)
     (define-predicate css-image-sets? (Listof (List CSS-Image-Datum Flonum)))])

  (define-css-disjoined-filter <css-image> #:-> CSS-Image-Datum
    ;;; https://drafts.csswg.org/css-images/#image-values
    ;;; https://drafts.csswg.org/css-images/#invalid-image
    ;[(css:function=:=? image-value 'image) (css-extract-image image-value (css:function-arguments image-value))]
    (<css:@λ> the-@icon-pool css-@icon-filter)
    (<css-image-notation>)
    (CSS:<?> (<css:url> string?) #false (λ _ "")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-type Font (Instance Font%))

  (define-syntax (call-with-font stx)
    (syntax-parse stx
      [(_ font (~optional (~seq #:root? ?root?)) sexp ...)
       (with-syntax ([root? (or (attribute ?root?) #'#false)])
         ; NOTE: This operation is extremely expensive (at least 0.4s), but Racket do have its own cache strategy.
         #'(begin (unless (eq? font (unbox &font))
                    ; WARNING: 'xh' seems to be impractical, the font% size is just a nominal size
                    ;            and usually smaller than the generated text in which case the 'ex' is
                    ;            always surprisingly larger than the size, the '0w' therefore is used instead.                      
                    ;(define-values (xw xh xd xe) (send dc get-text-extent "x" font)]
                    (define-values (0w 0h 0d 0e) (send dc get-text-extent "0" font))
                    (define-values (ww wh wd we) (send dc get-text-extent "水" font))
                    (define em : Nonnegative-Flonum (smart-font-size font))
                    (when root? (set-flcss%-rem! length% em))
                    (set-flcss%-em! length% em)
                    (set-flcss%-ex! length% (real->double-flonum 0w))
                    (set-flcss%-ch! length% (real->double-flonum 0w))
                    (set-flcss%-ic! length% (real->double-flonum ww))
                    (set-box! &font font))
                  sexp ...))]))

  (define current-css-default-font : (Parameterof Font) (make-parameter (make-font)))
  (define css-font-family-names/no-variants : (Listof String) (get-face-list 'all #:all-variants? #false))
  (define dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
  (define &font : (Boxof Font) (box (current-css-default-font)))

  (define css-font-generic-families : (Listof Symbol)
    '(default decorative roman script  swiss      modern    system    symbol
       emoji  fantasy    serif cursive sans-serif monospace system-ui math fangsong))

  (define-predicate racket-font-smoothing? Font-Smoothing)
  (define-predicate racket-font-hinting? Font-Hinting)
  
  (define css-font-synthesis-options : (Listof Symbol) '(weight style small-caps))
  
  (define css-font-style-option : (Listof Symbol) '(normal italic oblique slant))
  (define css-font-kerning-option : (Listof Symbol) '(auto normal none))
  (define css-font-variant-ligatures-options : (Listof Symbol) '(normal none))
  (define css-font-position-option : (Listof Symbol) '(normal sub super))
  (define css-font-caps-option : (Listof Symbol) '(normal small-caps all-small-caps petite-caps all-petite-caps unicase titling-caps))
  (define css-font-weight-option : (Listof Symbol) '(normal bold bolder light lighter))
  (define css-font-size-option : (Listof Symbol) '(xx-small x-small small medium large x-large xx-large smaller larger))
  (define css-font-variant-options/21 : (Listof Symbol) '(normal small-caps))
  (define css-font-stretch-option : (Listof Symbol) '(normal condensed expanded ultra-condensed extra-condensed semi-condensed
                                                             ultra-expanded extra-expanded semi-expanded))
  
  (define-css-system-parameters-filter <css-system-font> #:-> Font
    [caption       (current-css-default-font)]
    [icon          (current-css-default-font)]
    [menu          (current-css-default-font)]
    [message-box   (current-css-default-font)]
    [small-caption (current-css-default-font)]
    [status-bar    (current-css-default-font)])
  
  (define-css-racket-value-filter <racket-font> #:is-a? font% #:as Font)

  (define smart-font-size : (-> Font Nonnegative-Flonum)
    (let ([macosx? (eq? (system-type 'os) 'macosx)])
      (lambda [font]
        (css-length->scalar (real->double-flonum (send font get-size))
                            (if (or macosx? (send font get-size-in-pixels))
                                'px 'pt)))))

  (define css-font->longhand-properties : (->* (Font) (CSS-Longhand-Values) CSS-Longhand-Values)
    (lambda [font [longhand css-longhand]]
      (let* ([longhand++ (hash-set longhand 'font-weight (send font get-weight))]
             [longhand++ (hash-set longhand++ 'font-style (send font get-style))]
             [longhand++ (hash-set longhand++ 'font-size (smart-font-size font))])
        (hash-set longhand++ 'font-family
                  (list (or (send font get-face)
                            (send font get-family)))))))

  (define-css-disjoined-filter <font-stretch> #:-> (U Symbol Nonnegative-Single-Flonum)
    ;;; https://drafts.csswg.org/css-fonts-4/#font-stretch-prop
    (<css-keyword> css-font-stretch-option)
    (<css:percentage> nonnegative-single-flonum?))

  (define-css-disjoined-filter <font-weight> #:-> (U Symbol Integer)
    ;;; https://drafts.csswg.org/css-fonts/#font-weight-prop
    (<css-keyword> css-font-weight-option)
    (<css:integer> 0 < 1000))
  
  (define-css-disjoined-filter <font-size> #:-> (U Symbol Nonnegative-Inexact-Real CSS:Length:Font)
    ;;; https://drafts.csswg.org/css-fonts/#font-size-prop
    (<css+length>)
    (CSS:<~> (<css+%real>) exact->inexact)
    (<css-keyword> css-font-size-option))

  (define-css-disjoined-filter <line-height> #:-> (U Symbol Nonnegative-Flonum CSS:Length:Font CSS-Wide-Keyword)
    ;;; http://www.w3.org/TR/CSS2/visudet.html#propdef-line-height
    (CSS:<~> (<css+%real>) real->double-flonum)
    (<css+length>)
    (CSS:<~> (<css-keyword> '(normal inherit)) css-wide-keywords-filter-map))

  (define <:font-family:> : (CSS-Parser (Listof CSS-Datum))
    ;;; https://drafts.csswg.org/css-fonts/#font-family-prop
    ;;; https://drafts.csswg.org/css-fonts-4/#extended-generics
    (CSS<#> (CSS<+> (CSS<^> (CSS:<+> (<css:string>) (<css-keyword> css-font-generic-families)))
                    (CSS<!> (CSS<^> (<css:ident>))))))
  
  (define <:font-shorthand:> : (Pairof CSS-Shorthand-Parser (Listof Symbol))
    ;;; https://drafts.csswg.org/css-fonts/#font-prop
    (cons (CSS<+> (CSS<^> (<css-system-font>) css-font->longhand-properties)
                  (CSS<^> (<racket-font>) css-font->longhand-properties)
                  (CSS<&> (CSS<*> (CSS<+> (CSS<_> (CSS<^> (<css-keyword> 'normal) '|Ignoring, some properties use it as defaults|))
                                          (CSS<^> (<font-weight>) 'font-weight)
                                          (CSS<^> (<css-keyword> css-font-style-option) 'font-style)
                                          (CSS<^> (<css-keyword> css-font-variant-options/21) 'font-variant)
                                          ; <font-stretch> also accepts percentage in css-fonts-4 specification,
                                          ; however it preempts the 'font-size
                                          (CSS<^> (<css-keyword> css-font-stretch-option) 'font-stretch)))
                          (CSS<^> (<font-size>) 'font-size)
                          (CSS<*> (CSS<?> [(<css-slash>) (CSS<^> (<line-height>) 'line-height)]) '?)
                          (CSS<^> <:font-family:> '(font-family))))
          '(font-style font-variant font-weight font-stretch font-size line-height font-family
                       font-size-adjust font-kerning font-language-override)))

  (define css-datum->font-family : (-> Symbol CSS-Datum (U String Font-Family))
    (lambda [_ value]
      (define (generic-family-map [family : Symbol]) : Font-Family
        (case family
          [(decorative fantasy) 'decorative]
          [(roman serif) 'roman]
          [(script cursive) 'script]
          [(swiss sans-serif) 'swiss]
          [(modern monospace) 'modern]
          [(system system-ui) 'system]
          [(symbol math) 'symbol]
          [else 'default]))
      (define (face-filter [family : String]) : (Option String)
        (for/or : (Option String) ([face (in-list css-font-family-names/no-variants)])
          (and (string-ci=? family face) family)))
      (let select ([families (if (list? value) value (list value))])
        (cond [(null? families) (let ([pfont (unbox &font)]) (or (send pfont get-face) (send pfont get-family)))]
              [else (let ([family (car families)])
                      (or (and (symbol? family) (generic-family-map family))
                          (and (string? family) (face-filter family))
                          (and (list? family) (face-filter (string-trim (~a family) #px"(^[(])|([)]$)")))
                          (select (cdr families))))]))))

  (define css-datum->font-size : (-> Symbol CSS-Datum Nonnegative-Real)
    (lambda [property value]
      (cond [(symbol? value)
             (let ([css-font-medium (smart-font-size (current-css-default-font))])
               (case value
                 [(xx-large) (* 2/1 css-font-medium)]
                 [(x-large)  (* 3/2 css-font-medium)]
                 [(large)    (* 6/5 css-font-medium)]
                 [(small)    (* 8/9 css-font-medium)]
                 [(x-small)  (* 3/4 css-font-medium)]
                 [(xx-small) (* 3/5 css-font-medium)]
                 [(smaller)  (* 5/6 (flcss%-em length%))] ; TODO: find a better function to deal with these two keywords.
                 [(larger)   (* 6/5 (flcss%-em length%))] ; http://style.cleverchimp.com/font_size_intervals/altintervals.html#bbs
                 [else       css-font-medium]))]
            [(nonnegative-flonum? value) value]
            [(nonnegative-single-flonum? value) (fl* (real->double-flonum value) (flcss%-em length%))]
            [(css+length? value) (css:length->scalar value #false)]
            [(eq? property 'min-font-size) 0.0]
            [(eq? property 'max-font-size) 1024.0]
            [else +nan.0 #| used to determine whether size-in-pixels? will be inherited, see (css-extract-font) |#])))

  (define css-datum->font-weight : (-> Symbol CSS-Datum Font-Weight)
    (lambda [_ value]
      (cond [(symbol? value)
             (case value
               [(normal light bold) value]
               [(bolder) (if (eq? (send (unbox &font) get-weight) 'light) 'normal 'bold)]
               [(lighter) (if (eq? (send (unbox &font) get-weight) 'bold) 'normal 'light)]
               [else (send (current-css-default-font) get-weight)])]
            [(and (fixnum? value) (fx<= value 300)) 'light]
            [(and (fixnum? value) (fx>= value 700)) 'bold]
            [else (send (current-css-default-font) get-weight)])))

  (define css-datum->font-style : (-> Symbol CSS-Datum Font-Style)
    (lambda [_ value]
      (case value
        [(normal italic) value]
        [(oblique slant) 'slant]
        [else (send (current-css-default-font) get-style)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-text-decor-line-options : (Listof Symbol) '(underline overline line-through blink))
  (define css-text-decor-skip-options : (Listof Symbol) '(objects spaces ink edges box-decoration))

  (define css-text-decor-style-option : (Listof Symbol) '(solid double dotted dashed wavy))

  (define css-fold-decoration-line : CSS-Longhand-Update
    (lambda [_ old-options property]
      (if (list? old-options) (cons property old-options) (list property))))
  
  (define <:text-decoration:> : (Pairof CSS-Shorthand-Parser (Listof Symbol))
    ;;; https://drafts.csswg.org/css-text-decor/#text-decoration-color-property
    (cons (CSS<*> (CSS<+> (CSS<^> (CSS:<=> (<css-keyword> 'none) null) 'text-decoration-line)
                          (CSS<^> (<css-keyword> css-text-decor-line-options) 'text-decoration-line css-fold-decoration-line)
                          (CSS<^> (<css-keyword> css-text-decor-style-option) 'text-decoration-style)
                          (CSS<^> (<css-color>) 'text-decoration-color)))
          '(text-decoration-line text-decoration-color text-decoration-style))))

(require (submod "." css))

(define select-rgba-color : (->* (Color+sRGB) (Nonnegative-Flonum) Color)
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
                   [(eq? color-name 'currentcolor) (select-rgba-color (current-css-element-color))]
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

(define current-css-element-color : (Parameterof CSS-Datum Color+sRGB)
  (make-parameter (select-rgba-color #x000000)
                  (λ [[c : CSS-Datum]]
                    (define color : (U Color CSS-Wide-Keyword 'currentcolor) (css-datum->color 'color c))
                    (if (object? color) color (current-css-element-color)))))

(provide <css-color> <css-image> <css-system-font> current-css-default-font
         current-css-caption-font current-css-small-caption-font current-css-menu-font
         current-css-icon-font current-css-status-bar-font current-css-message-box-font)

(define css-color-property-parsers : (->* (Symbol) ((U Regexp (Listof Symbol))) (Option CSS-Declaration-Parser))
  (lambda [name [px.names #px"-color$"]]
    (or (and (eq? name 'color) (CSS<^> (<css-color> '#:inherit-currentcolor)))
        (and (or (and (list? px.names) (memq name px.names))
                 (and (regexp? px.names) (regexp-match? px.names (symbol->string name))))
             (CSS<^> (<css-color>))))))

(define css-image-property-parsers : (->* (Symbol) ((U Regexp (Listof Symbol))) (Option CSS-Declaration-Parser))
  (lambda [name [px.names #px"-(image|icon|logo)$"]]
    (and (or (and (list? px.names) (memq name px.names))
             (and (regexp? px.names) (regexp-match? px.names (symbol->string name))))
         (CSS<^> (<css-image>)))))

(define css-font-property-parsers : (-> Symbol (Option CSS-Declaration-Parser))
  ;;; https://drafts.csswg.org/css-fonts/#basic-font-props
  ;;; https://drafts.csswg.org/css-fonts-4/#basic-font-props
  ;;; https://drafts.csswg.org/css-fonts-4/#expanded-font-weight-scale
  (lambda [suitcased-name]
    (case suitcased-name
      [(font) <:font-shorthand:>]
      [(font-family) <:font-family:>]
      [(font-style) (CSS<^> (<css-keyword> css-font-style-option))]
      [(font-kerning) (CSS<^> (<css-keyword> css-font-kerning-option))]
      [(font-variant-ligatures) (CSS<^> (<css-keyword> css-font-variant-ligatures-options))]
      [(font-variant-position) (CSS<^> (<css-keyword> css-font-position-option))]
      [(font-variant-caps) (CSS<^> (<css-keyword> css-font-caps-option))]
      [(line-height) (CSS<^> (<line-height>))]
      [(font-synthesis) (<:css-keywords:> css-font-synthesis-options 'none)]
      [(font-size min-font-size max-font-size) (CSS<^> (<font-size>))]
      [(font-stretch) (CSS<^> (<font-stretch>))]
      [(font-weight) (CSS<^> (<font-weight>))]
      [(font-size-adjust) (CSS<^> (CSS:<+> (<css-keyword> 'none) (<css+real>)))]
      [(-racket-font-smoothing) (CSS<^> (<css:ident-norm> racket-font-smoothing?))]
      [(-racket-font-hinting) (CSS<^> (<css:ident-norm> racket-font-hinting?))]
      [else #false])))

(define css-text-decoration-property-parsers : (-> Symbol (Option CSS-Declaration-Parser))
  ;;; https://drafts.csswg.org/css-text-decor/#line-decoration
  (lambda [suitcased-name]
    (case suitcased-name
      [(text-decoration) <:text-decoration:>]
      [(text-decoration-line) (<:css-keywords:> css-text-decor-line-options 'none)]
      [(text-decoration-style) (CSS<^> (<css-keyword> css-text-decor-style-option))]
      [(text-decoration-skip) (<:css-keywords:> css-text-decor-skip-options 'none)]
      [(text-decoration-color) (CSS<^> (<css-color>))]
      [else #false])))

(define css-extract-font : (->* (CSS-Values (Option CSS-Values)) (Font) Font)
  (lambda [declared-values inherited-values [basefont (current-css-default-font)]]
    (define ?font : CSS-Datum (and inherited-values (css-ref inherited-values #false 'font)))
    (define inherited-font : Font (if (object? ?font) (cast ?font Font) basefont))
    (define (css-datum->font-underlined [_ : Symbol] [value : CSS-Datum]) : (Listof CSS-Datum)
      (if (list? value) value (if (send inherited-font get-underlined) (list 'underline) null)))
    (call-with-font inherited-font #:root? (false? ?font)
      (define family : (U String Font-Family) (css-ref declared-values #false 'font-family css-datum->font-family))
      (define min-size : Nonnegative-Real (css-ref declared-values #false 'min-font-size css-datum->font-size))
      (define max-size : Nonnegative-Real (css-ref declared-values #false 'max-font-size css-datum->font-size))
      (define font-size : Nonnegative-Real (css-ref declared-values #false 'font-size css-datum->font-size))
      (define style : Font-Style (css-ref declared-values #false 'font-style css-datum->font-style))
      (define weight : Font-Weight (css-ref declared-values #false 'font-weight css-datum->font-weight))
      (define decorations : (Listof CSS-Datum) (css-ref declared-values #false 'text-decoration-line css-datum->font-underlined))
      (define smoothing : Font-Smoothing (css-ref declared-values #false '-racket-font-smoothing racket-font-smoothing? 'default))
      (define hinting : Font-Hinting (css-ref declared-values #false '-racket-font-hinting racket-font-hinting? 'aligned))
      (define size : Nonnegative-Real (max (min max-size font-size) min-size))
      (define font : Font
        (make-font+ #:face (and (string? family) family) #:family (and (symbol? family) family)
                    #:size size #:size-in-pixels? (implies (nan? size) #| NOTE |# 'inherited) #:hinting hinting
                    #:style style #:weight weight #:underlined? (and (memq 'underline decorations) #true) #:smoothing smoothing
                    basefont))
     (call-with-font font
       (css-set! declared-values 'font font)
       (when (nan? size) (css-set! declared-values 'font-size size))
       font))))

(define css-datum->color : (-> Symbol CSS-Datum (U Color CSS-Wide-Keyword 'currentcolor))
  (lambda [desc-name color]
    (cond [(or (index? color) (symbol? color) (string? color)) (select-rgba-color color)]
          [(hexa? color) (select-rgba-color (hexa-hex color) (hexa-a color))]
          [(rgba? color) (select-rgba-color (rgb-bytes->hex (rgba-r color) (rgba-g color) (rgba-b color)) (rgba-a color))]
          [(hsba? color) (select-rgba-color (hsb->rgb-hex (hsba->rgb color) (hsba-h color) (hsba-s color) (hsba-b color)) (hsba-a color))]
          [(and (object? color) (is-a? color color%)) (cast color Color)]
          [(eq? color 'transparent) (select-rgba-color #x000000 0.0)]
          [(eq? color 'currentcolor) color]
          [else css:initial])))

#|(define css-datum->bitmap : (-> Symbol CSS-Datum (U Bitmap CSS-Wide-Keyword))
  (lambda [desc-name color]
    (cond [(or (index? color) (symbol? color) (string? color)) (select-rgba-color color)]
          [(hexa? color) (select-rgba-color (hexa-hex color) (hexa-a color))]
          [(rgba? color) (select-rgba-color (rgb-bytes->hex (rgba-r color) (rgba-g color) (rgba-b color)) (rgba-a color))]
          [(hsba? color) (select-rgba-color (hsb->rgb-hex (hsba->rgb color) (hsba-h color) (hsba-s color) (hsba-b color)) (hsba-a color))]
          [(and (object? color) (is-a? color color%)) (cast color Color)]
          [(eq? color 'transparent) (select-rgba-color #x000000 0.0)]
          [(eq? color 'currentcolor) color]
          [else css:initial])))|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (provide (all-from-out "css.rkt"))
  (provide (all-from-out (submod "..")))
  
  (require "css.rkt")
  (require "format.rkt")
  (require (submod ".."))

  (define-syntax (time-run stx)
    (syntax-case stx []
      [(_ prefix sexp ...)
       #'(let ([momery0 : Natural (current-memory-use)])
           (define-values (result cpu real gc) (time-apply (thunk sexp ...) null))
           (printf "~a: memory: ~a cpu time: ~a real time: ~a gc time: ~a~n" prefix
                   (~size (- (current-memory-use) momery0) 'Bytes) cpu real gc)
           (car result))]))
  
  (define run-file : Path-String (find-system-path 'run-file))
  (define DrRacket? : Boolean (regexp-match? #px"DrRacket$" run-file))
  (define bitmap.css : Path-String
    (simplify-path (cond [(not DrRacket?) (build-path (find-system-path 'orig-dir) run-file 'up 'up "tamer" "bitmap.css")]
                         [else (build-path 'up "tamer" "bitmap.css")])))

  (define-values (in out) (make-pipe))
  (define css-logger (make-logger 'css #false))
  (define css (thread (thunk (let forever ([/dev/log (make-log-receiver css-logger 'debug)])
                               (match (sync/enable-break /dev/log)
                                 [(vector 'debug _ (? eof-object?) _) (close-output-port out)]
                                 [(vector level message _ 'exn:css:syntax) (fprintf out "[~a] ~a~n" level message) (forever /dev/log)]
                                 [else (forever /dev/log)])))))
  
  (current-logger css-logger)
  (css-cache-computed-object-value #false)
  (current-css-media-type 'screen)
  (current-css-media-preferences
   ((inst make-hasheq Symbol CSS-Media-Datum)
    (list (cons 'orientation 'landscape)
          (cons 'width 1440)
          (cons 'height 820))))

  (define-preference btest #:as Bitmap-TestCase #:with ([color-properties Color+sRGB])
    ([symbol-color : Color+sRGB                                #:= 'Blue]
     [string-color : Color+sRGB                                #:= 'Orange]
     [number-color : Color+sRGB                                #:= 'Tomato]
     [output-color : Color+sRGB                                #:= 'Chocolate]
     [paren-color : Color+sRGB                                 #:= 'Firebrick]
     [border-color : Color+sRGB                                #:= 'Crimson]
     [foreground-color : Color+sRGB                            #:= "Grey"]
     [background-color : Color+sRGB                            #:= "Snow"]
     [font : Font                                              #:= (current-css-default-font)]
     [width : Index                                            #:= 512]
     [combine? : Boolean                                       #:= #false]
     [desc : String                                            #:= "['desc' property is required]"]
     [descriptors : (HashTable Symbol CSS-Datum)               #:= (make-hasheq)])
    #:transparent)

  (define css-descriptor-filter : CSS-Declaration-Parsers
    (lambda [suitcased-name deprecated!]
      (or (css-font-property-parsers suitcased-name)
          (css-text-decoration-property-parsers suitcased-name)
          (css-color-property-parsers suitcased-name btest-color-properties)
          (css-image-property-parsers suitcased-name)
          (case suitcased-name
            [(desc) (CSS<*> (CSS<^> (<css:string>)) '+)]
            [(count width) (CSS<^> (<css-natural>))]
            [(combine) (CSS<^> (<css-keyword> '(combine none)))]
            [(at-exp) (CSS<^> (list (<css:λracket>) (<css:racket>) (<css:block>)))])
          #false)))

  (define css-preference-filter : (CSS-Cascaded-Value-Filter Bitmap-TestCase)
    (lambda [declared-values initial-values inherit-values]
      (define (css-datum->desc [_ : Symbol] [value : CSS-Datum]) : String
        (cond [(string? value) value]
              [(list? value) (string-join (map (λ [v] (~a v)) value))]
              [else (btest-desc initial-values)]))
      (parameterize ([current-css-element-color (css-ref declared-values inherit-values 'color)])
        (make-btest #:symbol-color (css-ref declared-values inherit-values 'symbol-color css-datum->color)
                    #:string-color (css-ref declared-values inherit-values 'string-color css-datum->color)
                    #:number-color (css-ref declared-values inherit-values 'number-color css-datum->color)
                    #:output-color (css-ref declared-values inherit-values 'output-color css-datum->color)
                    #:paren-color (css-ref declared-values inherit-values 'paren-color css-datum->color)
                    #:border-color (css-ref declared-values inherit-values 'border-color css-datum->color)
                    #:foreground-color (css-ref declared-values inherit-values 'foreground-color css-datum->color)
                    #:background-color (css-ref declared-values inherit-values 'background-color css-datum->color)
                    #:font (css-extract-font declared-values inherit-values)
                    #:width (css-ref declared-values inherit-values 'width index? css:initial)
                    #:combine? (eq? 'normal (css-ref declared-values inherit-values 'font-variant-ligatures symbol? 'normal))
                    #:desc (css-ref declared-values inherit-values 'desc css-datum->desc)
                    #:descriptors (for/hash : (HashTable Symbol CSS-Datum) ([(k fv) (in-css-values declared-values)])
                                    (values k (fv)))))))

  (define tamer-sheet : CSS-StyleSheet (read-css-stylesheet bitmap.css))
  (define tamer-main : CSS-Subject (make-css-subject #:type 'module #:id '#:root #:classes '(main)))

  (define :values : CSS-Values (make-css-values))
  (define :root : Bitmap-TestCase
    (time-run 'tamer-main
              (let-values ([(toplevel topvalues)
                            (css-cascade (list tamer-sheet) tamer-main
                                         css-descriptor-filter css-preference-filter
                                         (make-btest) #false)])
                (set! :values topvalues)
                toplevel)))

  (define-values (bitmap-descs testcases)
    (for/fold ([bitmap-descs : (Listof Bitmap) null]
               [testcases : (Listof Bitmap-TestCase) null])
              ([i (in-range (css-ref :values #false 'count index? 8))])
      (define-values (tobj _) (css-cascade (list tamer-sheet)
                                           (make-css-subject #:type 'test #:id (string->keyword (~a 'case i)))
                                           css-descriptor-filter css-preference-filter
                                           :root :values))

      (define-values (fgcolor bgcolor rcolor bdcolor)
        (values (btest-foreground-color tobj) (btest-background-color tobj)
                (btest-output-color tobj) (btest-border-color tobj)))

      (define-values (width words font combine?) (values (btest-width tobj) (btest-desc tobj) (btest-font tobj) (btest-combine? tobj)))
      (define desc (bitmap-desc words width font #:color fgcolor #:background-color bgcolor #:combine? combine?))
      (define-values (desc-width height) (bitmap-size desc))
      
      (values (append bitmap-descs
                      (list (bitmap-pin 1 1/2 0 1/2
                                        (bitmap-text "> ")
                                        (bitmap-text "(" #:color (btest-paren-color tobj))
                                        (bitmap-hc-append #:gapsize 7
                                                          (bitmap-text (~a "bitmap-case" i) #:color (btest-symbol-color tobj))
                                                          (bitmap-text (~s words) #:color (btest-string-color tobj))
                                                          (bitmap-text (~a width) #:color (btest-number-color tobj)))
                                        (bitmap-text ")" #:color (btest-paren-color tobj)))
                            (bitmap-text #:color rcolor
                                         (cond [(not combine?) (format "- : (Bitmap ~a ~a)" desc-width height)]
                                               [else (format "- : (Bitmap ~a ~a #:combined)" desc-width height)]))
                            (bitmap-lt-superimpose (bitmap-frame desc #:margin 1 #:border-style 'transparent #:style 'transparent)
                                                   (bitmap-frame (bitmap-blank width height) #:border-color bdcolor))))
              (cons tobj testcases))))

  (when DrRacket? tamer-sheet)
  (when DrRacket? :root)
  (when DrRacket? (apply bitmap-vl-append bitmap-descs))
  (when DrRacket? length%)

  (log-message css-logger 'debug "exit" eof)
  (when DrRacket? (copy-port in (current-output-port))))
