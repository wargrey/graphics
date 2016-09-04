#lang at-exp typed/racket

(provide (all-defined-out))
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
(define-type Bitmap (Instance Bitmap%))
(define-type String+Color (U String (Instance Color%)))

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
    (define backing-scale : Nonnegative-Real (send bmp get-backing-scale))
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
                                      [else (* -1 size default-size)])))
      (if (string? maybe-face)
          (send the-font-list find-or-create-font fontsize maybe-face
                (or family (send basefont get-family)) (or style (send basefont get-style))
                (or weight (send basefont get-weight)) underlined? 'smoothed pixels?
                (or hinting (send basefont get-hinting)))
          (send the-font-list find-or-create-font fontsize
                (or family (send basefont get-family)) (or style (send basefont get-style))
                (or weight (send basefont get-weight)) underlined? 'smoothed pixels?
                (or hinting (send basefont get-hinting)))))))

(define bitmap-blank : (->* () (Nonnegative-Real (Option Nonnegative-Real) #:backing-scale Nonnegative-Real) Bitmap)
  (lambda [[w 0] [h #false] #:backing-scale [backing-scale (default-icon-backing-scale)]]
    (define width : Positive-Integer (max 1 (exact-ceiling w)))
    (define height : Positive-Integer (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale backing-scale)))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-blank #:backing-scale (send bmp get-backing-scale)
                  (send bmp get-width) (send bmp get-height))))

(define bitmap-text : (->* (String) ((Instance Font%) #:combine? Boolean #:color (Option String+Color)
                                                      #:background-color (Option String+Color)) Bitmap)
  (lambda [content [font (make-font+)] #:combine? [combine? #false] #:color [fgcolor #false]
           #:background-color [bgcolor #false]]
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank)))
    (define-values (width height descent ascent) (send dc get-text-extent content font combine?))
    (send dc set-bitmap (bitmap-blank width height))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground fgcolor))
    (when bgcolor (send dc set-text-background bgcolor))
    (when bgcolor (send dc set-text-mode 'solid))
    (send dc draw-text content 0 0 combine?)
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-desc : (->* (String Positive-Real)
                           ((Instance Font%) #:combine? Boolean #:color (Option String+Color)
                                             #:background-color (Option String+Color)) Bitmap)
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
          ([idx0 : Natural 0] [descs : (Listof String) descs] [ys : (Listof Real) ys]
                              [Widthn : Natural width] [yn : Real height])
          (define-values (descn widthn heightn idx)
            (let desc-col-expt : (Values String Natural Real Natural)
              ([interval : Natural 1] [open : Natural idx0] [close : Natural terminal]
                                      [backtracking : String ""] ; char-width is bad for forecasting the next width 
                                      [back-width : Natural max-width] [back-height : Nonnegative-Real phantom-height])
              (define idx : Natural (min close (+ open interval)))
              (define next-open : Natural (+ open (quotient interval 2)))
              (define-values (line width height) (desc-extent desc idx0 idx))
              (define found? : Boolean (and (> width max-width) (= interval 1) (<= back-width max-width)))
              (cond [found? (values backtracking back-width back-height (if (zero? open) terminal (max 0 (sub1 idx))))]
                    [(> width max-width) (desc-col-expt 1 next-open idx backtracking back-width back-height)]
                    [(= close idx) (values line width height idx)]
                    [else (desc-col-expt (arithmetic-shift interval 1) open close line width height)])))
          (if (fx= idx terminal)
              (values (cons descn descs) (cons yn ys) (max widthn Widthn) (+ yn heightn))
              (desc-row idx (cons descn descs) (cons yn ys) (max widthn Widthn) (+ yn heightn))))))
    
    (send dc set-bitmap (bitmap-blank (max 1 width) (max 1 phantom-height height)))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground fgcolor))
    (unless (false? bgcolor)
      (send dc set-smoothing 'aligned)
      (send dc set-pen bgcolor 0 'transparent)
      (send dc set-brush bgcolor 'solid)
      (send dc draw-rectangle 0 0 (max 1 width) (max 1 phantom-height height)))
    (for ([desc (in-list (reverse descs))] [y (in-list (reverse ys))])
      (send dc draw-text desc 0 y combine?))
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-frame : (-> Bitmap [#:margin Index] [#:color String+Color] [#:style Brush-Style]
                           [#:border-color String+Color] [#:border-width Index] [#:border-style Pen-Style] Bitmap)
  (lambda [bmp #:margin [margin 0] #:color [brush-color (make-object color% "White")] #:style [brush-style 'transparent]
           #:border-color [pen-color (make-object color% "Black")] #:border-width [pen-size 1] #:border-style [pen-style 'solid]]
    (define width : Positive-Integer (+ margin margin (send bmp get-width) pen-size pen-size))
    (define height : Positive-Integer (+ margin margin (send bmp get-height) pen-size pen-size))
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
    (send dc set-smoothing 'aligned)
    (send dc set-pen pen-color pen-size (if (zero? pen-size) 'transparent pen-style))
    (send dc set-brush brush-color brush-style)
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
                  ;[(list base bmp)  TODO: if gapsize can be defined with bitmap-pin
                  ; (match alignment
                  ;   ['vl (bitmap-pin  0  1  0  0  base bmp)]
                  ;   ['vc (bitmap-pin 1/2 1 1/2 0  base bmp)]
                  ;   ['vr (bitmap-pin  1  1  1  0  base bmp)]
                  ;   ['ht (bitmap-pin  1  0  0  0  base bmp)]
                  ;   ['hc (bitmap-pin  1 1/2 0 1/2 base bmp)]
                  ;   ['hb (bitmap-pin  1  1  0  1  base bmp)]
                  ;   [should-not-happen base])]
                  [else (let*-values ([(base others) (values (car bitmaps) (cdr bitmaps))]
                                      [(min-width min-height) (bitmap-size base)])
                          (define-values (width0 height0)
                            (for/fold ([width : Real (send base get-width)]
                                       [height : Real (send base get-height)])
                                      ([child : Bitmap (in-list others)])
                              (define w : Positive-Integer (send child get-width))
                              (define h : Positive-Integer (send child get-height))
                              (match alignment
                                [(or 'vl 'vc 'vr) (values (max width w) (+ height h delta))]
                                [(or 'ht 'hc 'hb) (values (+ width w delta) (max height h))]
                                [should-not-happen (values (max width w) (max height h))])))
                 
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
                                  [else (values this-x-if-use this-y-if-use)]))
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
                                 [else (values 0 0)]))
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
  
  (require typed/racket/draw)
  
  (define the-colorbase : (HashTable Datum (Instance Color%)) (hasheq))

  (define css-hash-color->rgba : (-> Keyword (Option CSS-RGBA))
    (lambda [hash-color]
      (define color : String (keyword->string hash-color))
      (define digits : Index (string-length color))
      (define maybe-hexcolor : (Option Natural)
        (and (memq digits (list 3 4 6 8))
             (for/fold ([hexcolor : (Option Natural) 0])
                       ([ch : Char (in-string color)])
               (define digit : (U Integer Void)
                 (cond [(char-numeric? ch) (- (char->integer ch) #x30)]
                       [(char<=? #\A ch #\F) (- (char->integer ch) #x41 -10)]
                       [(char<=? #\a ch #\f) (- (char->integer ch) #x61 -10)]))
               (and hexcolor
                    (exact-nonnegative-integer? digit)
                    (let ([result : Natural (bitwise-ior (arithmetic-shift hexcolor 4) digit)])
                      (if (>= digits 6) result (bitwise-ior (arithmetic-shift result 4) digit)))))))
      (cond [(false? maybe-hexcolor) #false]
            [(or (= digits 3) (= digits 6)) (cons (min #xFFFFFF maybe-hexcolor) 1.0)]
            [else (let ([alpha (bitwise-and maybe-hexcolor #xFF)])
                    (cons (min #xFFFFFF (arithmetic-shift maybe-hexcolor -8))
                          (real->double-flonum (abs (/ alpha 255.0)))))])))
  
  (define css-rgb->scalar : (-> CSS-Token (U Natural CSS-Declared-Result))
    (lambda [t]
      (cond [(css:percentage? t) (css:percentage=> t fl* 255.0 exact-round)]
            [(css:natural=:=? t fx<= #xFF) (css:natural-datum t)]
            [(css:flunum=:=? t fl<= 255.0) (css:flunum=> t exact-round)]
            [(css-number? t) (vector exn:css:range t)]
            [else (vector exn:css:type t)])))

  (define css-alpha->scalar : (-> CSS-Token (U Gamut CSS-Declared-Result))
    (lambda [t]
      (cond [(css:percentage? t) (css:percentage-datum t)]
            [(css:flunum=:=? t fl<= 1.0) (css:flunum-datum t)]
            [(css:natural=:=? t fx<= 1) (css:natural=> t fx->fl)]
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
                                            (Values (Option CSS-Declared-Result) (Listof CSS-Token) Natural))
    ;;; https://github.com/w3c/csswg-drafts/issues/266
    (lambda [id args n:min [comma? #true]]
      (define n:max : Natural (add1 n:min))
      (let fold ([result : (Option CSS-Declared-Result) #false]
                 [sgra : (Listof CSS-Token) null]
                 [n : Natural 0]
                 [tokens : (Listof CSS-Token) args]
                 [i : Natural 0])
        (cond [(or (and (null? tokens) (>= n n:min)) (vector? result)) (values result (reverse sgra) n)]
              [(and (null? tokens) (< n n:min)) (values (vector exn:css:missing-value id) (reverse sgra) n)]
              [else (let-values ([(t r ni) (values (car tokens) (cdr tokens) (add1 i))])
                      (cond [(>= n n:max) (fold (vector exn:css:overconsumption tokens) sgra n r ni)]
                            [(not (css:delim? t)) (fold result (cons t sgra) (add1 n) r ni)]
                            [else (let ([d : Char (css:delim-datum t)]
                                        [slot? : Boolean (odd? i)]
                                        [following? : Boolean (pair? r)]
                                        [/? : Boolean (css:delim? result)])
                                    (cond [(and (char=? d #\,) slot? following? (or comma? /?)) (fold t sgra n r ni)]
                                          [(and (char=? d #\/) slot? following? (not /?)) (fold t sgra n r ni)]
                                          [(not following?) (fold (vector exn:css:missing-value t) sgra n r ni)]
                                          [else (fold (vector exn:css:unrecognized t) sgra n r ni)]))]))]))))
  
  (define css-apply-rgba : (-> (U CSS:Function CSS:Ident) (Listof CSS-Token) CSS-Declared-Result)
    ;;; https://drafts.csswg.org/css-color/#rgb-functions
    (lambda [frgba args]
      (define-values (result rgba-tokens argsize) (css-color-filter-delimiter frgba args 3))
      (cond [(vector? result) result]
            [else (let ([r (css-rgb->scalar (first rgba-tokens))]
                        [g (css-rgb->scalar (second rgba-tokens))]
                        [b (css-rgb->scalar (third rgba-tokens))]
                        [a (if (= argsize 4) (css-alpha->scalar (fourth rgba-tokens)) 1.0)])
                    (cond [(not (exact-nonnegative-integer? r)) r]
                          [(not (exact-nonnegative-integer? g)) g]
                          [(not (exact-nonnegative-integer? b)) b]
                          [(not (flonum? a)) a]
                          [else (css-remake-token [frgba (last args)] css:rgba
                                                  (cons (min #xFFFFFF (bitwise-ior (arithmetic-shift r 16) (arithmetic-shift g 8) b))
                                                        a))]))])))

  (define css-apply-hsba : (-> (U CSS:Function CSS:Ident) (Listof CSS-Token) HSB->RGB CSS-Declared-Result)
    ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
    (lambda [fhsba args ->rgb]
      (define-values (result hsba-tokens argsize) (css-color-filter-delimiter fhsba args 3))
      (cond [(vector? result) result]
            [else (let ([h (css-hue->scalar (first hsba-tokens))]
                        [s (second hsba-tokens)]
                        [b (third hsba-tokens)]
                        [a (if (= argsize 4) (css-alpha->scalar (fourth hsba-tokens)) 1.0)])
                    (cond [(not (flonum? h)) h]
                          [(not (css:percentage? s)) (vector exn:css:type s)]
                          [(not (css:percentage? b)) (vector exn:css:type b)]
                          [(not (flonum? a)) a]
                          [else (let-values ([(flr flg flb) (->rgb h (css:percentage-datum s) (css:percentage-datum b))])
                                  (css-remake-token [fhsba (last args)] css:rgba
                                                    (cons (min #xFFFFFF
                                                               (bitwise-ior (arithmetic-shift (gamut->byte flr) 16)
                                                                            (arithmetic-shift (gamut->byte flg) 8)
                                                                            (gamut->byte flb)))
                                                          a)))]))]))))

(require (submod "." css))

(define select-rgba-color : (->* (Datum) (Gamut) (Instance Color%))
  (lambda [color [alpha 1.0]]
    color%
    (hash-ref the-colorbase color)))

(define css-color-declaration-filter : (-> (Listof+ CSS-Token) CSS-Declared-Result)
  ;;; https://drafts.csswg.org/css-color/#color-type
  ;;; https://drafts.csswg.org/css-color/#named-colors
  ;;; https://drafts.csswg.org/css-color/#numeric-rgb
  ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
  ;;; https://drafts.csswg.org/css-color/#the-hwb-notation
  (lambda [color-values]
    (define argsize : Index (length color-values))
    (define color-value : CSS-Token (car color-values))
    (cond [(> argsize 1) (vector exn:css:overconsumption (cdr color-values))]
          [(css:ident? color-value)
           (define name : String (css:ident=> color-value symbol->string))
           (cond [(send the-color-database find-color name) color-value]
                 [(member (string-downcase name) '("transparent" "currentcolor" "rebeccapurple")) color-value]
                 [else exn:css:range])]
          [(css:hash? color-value)
           (define maybe-rgba : (Option CSS-RGBA) (css:hash=> color-value css-hash-color->rgba))
           (cond [(false? maybe-rgba) exn:css:range]
                 [else (css-remake-token color-value css:rgba maybe-rgba)])]
          [(css:function? color-value)
           (case (css:function-norm color-value)
             [(rgb rgba) (css-apply-rgba color-value (css:function-arguments color-value))]
             [(hsl hsla) (css-apply-hsba color-value (css:function-arguments color-value) hsl->rgb)]
             [(hsv hsva) (css-apply-hsba color-value (css:function-arguments color-value) hsv->rgb)]
             [(hsi hsia) (css-apply-hsba color-value (css:function-arguments color-value) hsi->rgb)]
             [(hwb hwba) (css-apply-hsba color-value (css:function-arguments color-value) hwb->rgb)]
             [else (vector exn:css:unrecognized color-value)])]
          [else exn:css:type])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test0
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
                                     (bitmap-text "(" #:color "Sienna")
                                     (bitmap-hc-append #:gapsize 7
                                                       (bitmap-text "bitmap-desc" #:color "Blue")
                                                       (bitmap-text (~s words) #:color "Orange")
                                                       (bitmap-text (~a width) #:color "Tomato"))
                                     (bitmap-text ")" #:color "Sienna"))
                         (bitmap-text (format "- : (Bitmap ~a ~a)" normal-width height) #:color "Purple")
                         (bitmap-pin 0 0 0 0
                                     (bitmap-frame desc #:margin 1 #:border-style 'transparent #:style 'transparent)
                                     (bitmap-frame (bitmap-blank width height) #:border-color "Lavender"))
                         (bitmap-text (format "- : (Bitmap ~a ~a #:combined)" combined-width combined-height) #:color "Purple")
                         (bitmap-lt-superimpose (bitmap-frame combined-desc #:margin 1 #:border-style 'transparent #:style 'transparent)
                                                (bitmap-frame (bitmap-blank width combined-height) #:border-color "Lavender")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test typed/racket
  (require "css.rkt")
  (require (submod ".."))
  (require (submod "css.rkt" test/digitama))

  (define symbol-suffix? : (-> Symbol String Boolean)
    (lambda [name suffix]
      (string-suffix? (symbol->string name) suffix)))

  (define ~rgba : (-> CSS-RGBA Datum)
    (lambda [rgba]
      (define rgb : Natural (car rgba))
      (define alpha : Gamut (cdr rgba))
      (cons (~r rgb #:base 16 #:min-width 6 #:pad-string "0")
            alpha)))
  
  (current-css-media-type 'screen)
  (current-css-media-preferences
   ((inst make-hash Symbol CSS-Media-Datum)
    (list (cons 'orientation 'landscape)
          (cons 'width 1440)
          (cons 'height 820))))
  
  (define tamer-sheet : CSS-StyleSheet (time-run (read-css-stylesheet (simplify-path (build-path 'up "tamer" "tamer.css")))))
  (define tamer-subject : CSS-Subject (make-css-subject #:type 'body #:id '#:header))

  (define-configuration descriptors #:as Descriptors
    ([color : Datum]
     [background-color : Datum]
     [border-color : Datum]
     [otherwise : (Option (HashTable Symbol Datum))])
    #:transparent)

  (define token->color : (-> Symbol CSS-Cascaded-Value Datum)
    (lambda [desc-name maybe-value]
      (cond [(css:rgba? maybe-value) (css:rgba=> maybe-value ~rgba)]
            [(css:ident? maybe-value) (css:ident-datum maybe-value)]
            [else #false])))
  
  (define css-descriptor-filter : CSS-Declared-Value-Filter
    (lambda [suitcased-name desc-values]
      (values (case suitcased-name
                [(color background-color border-color) (css-color-declaration-filter desc-values)]
                [else desc-values])
              #false)))

  (define css-filter : (CSS-Cascaded-Value-Filter (Option Descriptors))
    (lambda [declared-values all default-values inherit-values]
      (make-descriptors #:color (css-ref declared-values 'color token->color)
                        #:background-color (css-ref declared-values 'background-color token->color)
                        #:border-color (css-ref declared-values 'border-color token->color)
                        #:otherwise (for/hash : (HashTable Symbol Datum) ([desc-name (in-hash-keys declared-values)]
                                                                          #:when (not (symbol-suffix? desc-name "color")))
                                      (values desc-name
                                              (css-ref declared-values desc-name
                                                       (λ [[desc-name : Symbol] [maybe-value : CSS-Cascaded-Value]]
                                                         (cond [(token->color desc-name maybe-value) => values]
                                                               [(css-token? maybe-value) (css-token->datum maybe-value)]
                                                               [else (and maybe-value (map css-token->datum maybe-value))]))))))))
  
  tamer-subject
  (time-run (css-cascade (list tamer-sheet)
                         tamer-subject
                         css-descriptor-filter
                         css-filter
                         #false
                         #false)))
