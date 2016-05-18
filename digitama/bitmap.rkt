#lang at-exp typed/racket

(provide (all-defined-out))
(provide (all-from-out typed/racket/draw images/flomap typed/images/logos typed/images/icons))

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
  (let ([fl->byte : (-> Flonum Byte)
         (λ [x] (let ([v (max 0 (fl->fx (flround (fl* x 255.0))))])
                  (if (byte? v) v 255)))])
    (lambda [fmp]
      (match-define (flomap vs 4 w h) fmp)
      (define bs : Bytes (make-bytes (* 4 w h)))
      (for ([ia (in-range 0 (* 4 w h) 4)])
        (define-values (ir ig ib) (values (fx+ ia 1) (fx+ ia 2) (fx+ ia 3)))
        (bytes-set! bs ia (fl->byte (flvector-ref vs ia)))
        (bytes-set! bs ir (fl->byte (flvector-ref vs ir)))
        (bytes-set! bs ig (fl->byte (flvector-ref vs ig)))
        (bytes-set! bs ib (fl->byte (flvector-ref vs ib))))

      (define bmp : Bitmap
        (make-bitmap (max 1 (exact-ceiling (/ w (default-icon-backing-scale))))
                     (max 1 (exact-ceiling (/ h (default-icon-backing-scale))))
                     #:backing-scale (default-icon-backing-scale)))
      (send bmp set-argb-pixels 0 0 (max 0 w) (max 0 h) bs #true #true #:unscaled? #true)
      (send bmp set-argb-pixels 0 0 (max 0 w) (max 0 h) bs #false #true #:unscaled? #true)
      bmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-font+ : (->* () ((Instance Font%)
                              #:size (Option Real) #:face (Option String) #:family (Option Font-Family)
                              #:style (Option Font-Style) #:weight (Option Font-Weight) #:hinting (Option Font-Hinting)
                              #:underlined? (U 'default Boolean) #:size-in-pixels? (U 'default Boolean)) (Instance Font%))
  (let ([default-font : (Instance Font%) (make-font)])
    (lambda [[basefont default-font] #:size [size #false] #:face [face #false] #:family [family #false]
             #:style [style #false] #:weight [weight #false] #:hinting [hinting #false]
             #:underlined? [underlined 'default] #:size-in-pixels? [size-in-pixels? 'default]]
      (define maybe-face : (Option String) (or face (send basefont get-face)))
      (define underlined? : Boolean (if (boolean? underlined) underlined (send basefont get-underlined)))
      (define pixels? : Boolean (if (boolean? size-in-pixels?) size-in-pixels? (send basefont get-size-in-pixels)))
      (if (string? maybe-face)
          (send the-font-list find-or-create-font (or size (send basefont get-size)) maybe-face
                (or family (send basefont get-family)) (or style (send basefont get-style))
                (or weight (send basefont get-weight)) underlined? 'smoothed pixels?
                (or hinting (send basefont get-hinting)))
          (send the-font-list find-or-create-font (or size (send basefont get-size))
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

    (match-define-values (_ char-width phantom-height) (desc-extent " " 0 1))
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
     (if (real? refer)
         (bitmap-scale bmp (/ refer (min (send bmp get-width) (send bmp get-height))))
         (bitmap-resize bmp (send refer get-width) (send refer get-height)))]
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
               (match order
                 ['over  (void (send dc draw-bitmap bmp1 dx1 dy1)
                               (send dc draw-bitmap bmp2 dx2 dy2))]
                 ['under (void (send dc draw-bitmap bmp2 dx2 dy2)
                               (send dc draw-bitmap bmp1 dx1 dy1))])
               (or (send dc get-bitmap) (bitmap-blank))))
           bitmap-pin)])
    (values (make-pin 'over) (make-pin 'under))))

(define bitmap-pin : (-> Real Real Real Real Bitmap Bitmap * Bitmap)
  (lambda [x1-frac y1-frac x2-frac y2-frac bmp0 . bmps]
    (match-define-values (bmp _ _)
      (for/fold: ([bmp : Bitmap  bmp0]
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
            (match bitmaps
              [(list) (bitmap-blank)]
              [(list base) base]
              ;[(list base bmp)  TODO: if gapsize can be defined with bitmap-pin
              ; (match alignment
              ;   ['vl (bitmap-pin  0  1  0  0  base bmp)]
              ;   ['vc (bitmap-pin 1/2 1 1/2 0  base bmp)]
              ;   ['vr (bitmap-pin  1  1  1  0  base bmp)]
              ;   ['ht (bitmap-pin  1  0  0  0  base bmp)]
              ;   ['hc (bitmap-pin  1 1/2 0 1/2 base bmp)]
              ;   ['hb (bitmap-pin  1  1  0  1  base bmp)]
              ;   [should-not-happen base])]
              [(list-rest base others)
               (let ([min-width : Positive-Integer (send base get-width)]
                     [min-height : Positive-Integer (send base get-height)])
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
                 (define dc : (Instance Bitmap-DC%)
                   (make-object bitmap-dc% (bitmap-blank width height)))
                 (send dc set-smoothing 'aligned)
                 
                 (let append-bitmap : Void ([bmps : (Listof Bitmap) (cons base others)]
                                            [xoff : Real (- delta)] [yoff : Real (- delta)])
                   (unless (null? bmps)
                     (define bmp : Bitmap (car bmps))
                     (define w : Positive-Integer (send bmp get-width))
                     (define h : Positive-Integer (send bmp get-height))
                     (define this-x-if-use : Real (+ xoff delta))
                     (define this-y-if-use : Real (+ yoff delta))
                     (define-values (x y)
                       (match alignment
                         ['vl (values 0                 this-y-if-use)]
                         ['vc (values (/ (- width w) 2) this-y-if-use)]
                         ['vr (values (- width w)       this-y-if-use)]
                         ['ht (values this-x-if-use     0)]
                         ['hc (values this-x-if-use     (/ (- height h) 2))]
                         ['hb (values this-x-if-use     (- height h))]
                         [should-not-happen (values this-x-if-use this-y-if-use)]))
                     (send dc draw-bitmap bmp x y)
                     (append-bitmap (cdr bmps) (+ this-x-if-use w) (+ this-y-if-use h))))
                 (or (send dc get-bitmap) (bitmap-blank)))])))]
        [make-superimpose
         (lambda [alignment] : (-> Bitmap * Bitmap)
           (match-lambda*
             [(list) (bitmap-blank)]
             [(list base) base]
             [(list base bmp)
              (match alignment
                ['lt (bitmap-pin  0   0   0   0  base bmp)]
                ['lc (bitmap-pin  0  1/2  0  1/2 base bmp)]
                ['lb (bitmap-pin  0   1   0   1  base bmp)]
                ['ct (bitmap-pin 1/2  0  1/2  0  base bmp)]
                ['cc (bitmap-pin 1/2 1/2 1/2 1/2 base bmp)]
                ['cb (bitmap-pin 1/2  1  1/2  1  base bmp)]
                ['rt (bitmap-pin  1   0   1   0  base bmp)]
                ['rc (bitmap-pin  1  1/2  1  1/2 base bmp)]
                ['rb (bitmap-pin  1   1   1   1  base bmp)]
                [should-not-happen base])]
             [(list-rest bitmaps)
              (let-values ([(width height) (for/fold ([width : Positive-Integer 1]
                                                      [height : Positive-Integer 1])
                                                     ([bmp : Bitmap (in-list bitmaps)])
                                             (values (max width (send bmp get-width))
                                                     (max height (send bmp get-height))))])
                (define dc : (Instance Bitmap-DC%)
                  (make-object bitmap-dc% (bitmap-blank width height)))
                (send dc set-smoothing 'aligned)
                
                (for ([bmp : Bitmap (in-list bitmaps)])
                  (define w : Positive-Integer (send bmp get-width))
                  (define h : Positive-Integer (send bmp get-height))
                  (define-values (rx by) (values (- width w) (- height h)))
                  (define-values (cx cy) (values (/ rx 2) (/ by 2)))
                  (define-values (x y)
                    (match alignment
                      ['lt (values  0 0)] ['lc (values  0 cy)] ['lb (values  0 by)]
                      ['ct (values cx 0)] ['cc (values cx cy)] ['cb (values cx by)]
                      ['rt (values rx 0)] ['rc (values rx cy)] ['rb (values rx by)]
                      [should-not-happen (values 0 0)]))
                  (send dc draw-bitmap bmp x y))
                (or (send dc get-bitmap) (bitmap-blank)))]))])
    (values (make-append 'vl) (make-append 'vc) (make-append 'vr)
            (make-append 'ht) (make-append 'hc) (make-append 'hb)
            (make-superimpose 'lt) (make-superimpose 'lc) (make-superimpose 'lb)
            (make-superimpose 'ct) (make-superimpose 'cc) (make-superimpose 'cb)
            (make-superimpose 'rt) (make-superimpose 'rc) (make-superimpose 'rb))))



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
