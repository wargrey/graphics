#lang typed/racket

(provide (all-defined-out) select-rgba-color)
(provide (except-out (all-from-out css/digitama/bitmap) require-image))

(require css/digitama/bitmap)
(require css/digitama/misc)
(require css/color)

(define bitmap : (->* ((U Path-String Input-Port)) (Positive-Real) Bitmap)
  ;;; https://drafts.csswg.org/css-images/#image-fragments
  (lambda [src [density (default-icon-backing-scale)]]
    (define (image-section [raw : Bitmap] [xywh : String] [hint : Symbol]) : Bitmap
      (cond [(not (send raw ok?)) the-invalid-image]
            [else (match (regexp-match #px"^(\\d+),(\\d+),(\\d+),(\\d+)$" xywh)
                    [(list _ (? string? (app string->number (? index? x))) (? string? (app string->number (? index? y)))
                           (? string? (app string->number (? index? w))) (? string? (app string->number (? index? h))))
                     (bitmap-section raw x y w h)]
                    [_ (raise-user-error hint "malformed fragment")])]))
    (with-handlers ([exn? (λ [[e : exn]] (css-log-read-error e src) the-invalid-image)])
      (cond [(input-port? src) (read-bitmap src #:backing-scale density)]
            [(not (regexp-match? #px"[?]id=" src))
             (define (read-image.bmp [src.bmp : String]) : Bitmap (read-bitmap src.bmp #:backing-scale density))
             (match (string-split (if (path? src) (path->string src) src) "#xywh=")
               [(list src.rkt xywh) (image-section (read-image.bmp src.rkt) xywh 'read-image)]
               [(list src.rkt) (read-image.bmp src.rkt)]
               [_ (raise-user-error 'read-image "too many fragment")])]
            [else #| path?id=binding#xywh=x,y,w,h |#
             (define (read-image.rkt [src.rkt : String] [id : Symbol]) : Bitmap
               (define raw (require-image src.rkt id density))
               (cond [(bitmap%? raw) raw]
                     [else (error 'require-image "contract violation: received ~s" raw)]))
             (match (string-split (if (path? src) (path->string src) src) #px"([?]id=)|(#xywh=)")
               [(list src.rkt id xywh) (image-section (read-image.rkt src.rkt (string->symbol id)) xywh 'require-image)]
               [(list src.rkt id) (read-image.rkt src.rkt (string->symbol id))]
               [_ (raise-user-error 'require-image "too many fragment")])]))))

(define sprite : (->* ((U Path-String Input-Port)) (Positive-Real) (Listof Bitmap))
  (lambda [src [density (default-icon-backing-scale)]]
    (define (image-disassemble [raw : Bitmap] [grid : String] [hint : Symbol]) : (Listof Bitmap)
      (cond [(not (send raw ok?)) null]
            [else (match (regexp-match #px"^(\\d+),(\\d+)$" grid)
                    [(list _ (? string? (app string->number (? exact-positive-integer? cols)))
                           (? string? (app string->number (? exact-positive-integer? rows))))
                     (bitmap->sprite raw cols rows)]
                    [_ (raise-user-error hint "malformed fragments")])]))
    (with-handlers ([exn? (λ [[e : exn]] (css-log-read-error e src) null)])
      (cond [(input-port? src) (list (read-bitmap src #:backing-scale density))]
            [(not (regexp-match? #px"[?]id=" src))
             (define (read-sprite.bmp [src.bmp : String]) : Bitmap (read-bitmap src.bmp #:backing-scale density))
             (match (string-split (if (path? src) (path->string src) src) "#grids=")
               [(list src.rkt xywh) (image-disassemble (read-sprite.bmp src.rkt) xywh 'read-sprite)]
               [(list src.rkt) (list (read-sprite.bmp src.rkt))]
               [_ (raise-user-error 'read-sprite "too many fragments")])]
            [else #| path?id=binding |#
             (define (read-sprite.rkt [src.rkt : String] [id : Symbol] [grid : (Option String)]) : (Listof Bitmap)
               (define raw (require-image src.rkt id density))
               (cond [(list? raw) (filter-map (λ [v] (and (bitmap%? v) v)) raw)]
                     [(not (bitmap%? raw)) (error 'require-sprite "contract violation: received ~s" raw)]
                     [(string? grid) (image-disassemble raw grid 'require-sprite)]
                     [else (list raw)]))
             (match (string-split (if (path? src) (path->string src) src) #px"([?]id=)|(#grids=)")
               [(list src.rkt id grid) (read-sprite.rkt src.rkt (string->symbol id) grid)]
               [(list src.rkt id) (read-sprite.rkt src.rkt (string->symbol id) #false)]
               [_ (raise-user-error 'require-sprite "too many queries")])]))))

(define bitmap-size : (case-> [Bitmap -> (Values Positive-Integer Positive-Integer)]
                              [Bitmap Nonnegative-Real -> (Values Nonnegative-Real Nonnegative-Real)]
                              [Bitmap Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Real Nonnegative-Real)])
  (case-lambda [(bmp) (values (send bmp get-width) (send bmp get-height))]
               [(bmp ratio) (values (* (send bmp get-width) ratio) (* (send bmp get-height) ratio))]
               [(bmp w-ratio h-ratio) (values (* (send bmp get-width) w-ratio) (* (send bmp get-height) h-ratio))]))

(define bitmap-size+density : (Bitmap -> (Values Positive-Integer Positive-Integer Positive-Real))
  (lambda [bmp]
    (values (send bmp get-width)
            (send bmp get-height)
            (send bmp get-backing-scale))))

(define bitmap-intrinsic-size : (-> Bitmap (Values Positive-Integer Positive-Integer))
  (lambda [bmp]
    (define density : Positive-Real (send bmp get-backing-scale))
    (values (max (exact-ceiling (* (send bmp get-width) density)) 1)
            (max (exact-ceiling (* (send bmp get-height) density)) 1))))

(define bitmap->sprite : (->* (Bitmap) (Positive-Integer Positive-Integer) (Listof Bitmap))
  (lambda [bmp [cols 1] [rows 1]]
    (define-values (src-width src-height) (bitmap-size bmp))
    (define-values (width height) (values (/ src-width cols) (/ src-height rows)))
    (reverse (for*/fold ([sprite : (Listof Bitmap) null])
                        ([y (in-range rows)] [x (in-range cols)])
               (cons (bitmap-copy bmp (* x width) (* y height) width height) sprite)))))

(define bitmap-alter-density : (->* (Bitmap) (Positive-Real) Bitmap)
  (lambda [raw [density (default-icon-backing-scale)]]
    (define ratio : Nonnegative-Real (/ (send raw get-backing-scale) density))
    (cond [(= ratio 1.0) raw]
          [else (let ([bmp (bitmap-blank (* (send raw get-width) ratio) (* (send raw get-height) ratio) density)])
                  ; This algorithm is much faster than the (get/set-argb-pixels) one
                  (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
                  (send dc set-smoothing 'aligned)
                  (send dc set-scale ratio ratio)
                  (send dc draw-bitmap raw 0 0)
                  bmp)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-font+ : (->* () (Font #:size Real #:face (Option String) #:size-in-pixels? (U Boolean Symbol) #:family (Option Font-Family)
                                   #:style (Option Font-Style) #:weight (Option Font-Weight) #:hinting (Option Font-Hinting)
                                   #:underlined? (U Boolean Symbol) #:smoothing (Option Font-Smoothing)) Font)
  (lambda [[basefont (default-css-font)] #:size [size +nan.0] #:face [face #false] #:family [family #false]
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

(define bitmap-blank : (->* () (Nonnegative-Real (Option Nonnegative-Real) Positive-Real) Bitmap)
  (lambda [[w 0] [h #false] [density (default-icon-backing-scale)]]
    (define width : Positive-Integer (max 1 (exact-ceiling w)))
    (define height : Positive-Integer (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale density)))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-blank (send bmp get-width)
                  (send bmp get-height)
                  (send bmp get-backing-scale))))

(define bitmap-solid : (->* () (Color+sRGB Nonnegative-Real Positive-Real) Bitmap)
  (lambda [[color 'transparent] [size 1] [density (default-icon-backing-scale)]]
    (define solid : Bitmap (bitmap-blank size size density))
    (define dc : (Instance Bitmap-DC%) (send solid make-dc))
    (send dc set-background (select-rgba-color color))
    (send dc clear)
    solid))

(define bitmap-text : (->* (String) (Font #:combine? Boolean #:color (Option Color+sRGB)
                                          #:background-color (Option Color+sRGB)) Bitmap)
  (lambda [content [font (default-css-font)] #:combine? [combine? #true] #:color [fgcolor #false]
           #:background-color [bgcolor #false]]
    (define-values (width height descent ascent) (send the-dc get-text-extent content font combine?))
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-rgba-color fgcolor)))
    (when bgcolor (send dc set-text-background (select-rgba-color bgcolor)))
    (when bgcolor (send dc set-text-mode 'solid))
    (send dc draw-text content 0 0 combine?)
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-desc : (->* (String Nonnegative-Real)
                           (Font #:combine? Boolean #:color (Option Color+sRGB)
                                 #:background-color (Option Color+sRGB)) Bitmap)
  (lambda [description max-width.0 [font (default-css-font)]
           #:combine? [combine? #true] #:color [fgcolor #false] #:background-color [bgcolor #false]]
    (define max-width : Nonnegative-Flonum (real->double-flonum max-width.0))
    (define desc-extent : (-> String Integer Integer (Values String Nonnegative-Flonum Nonnegative-Flonum))
      (lambda [desc start end]
        (define subdesc : String (substring desc start end))
        (define-values (width height descent ascent) (send the-dc get-text-extent subdesc font combine?))
        (values subdesc (real->double-flonum width) (real->double-flonum height))))

    (define-values (_ _w phantom-height) (desc-extent " " 0 1))
    (define-values (descs ys width height)
      (for/fold ([descs : (Listof String) null] [ys : (Listof Flonum) null] [width : Nonnegative-Flonum 0.0] [height : Flonum 0.0])
                ([desc : String (in-list (string-split description (string #\newline)))])
        (define terminal : Index (string-length desc))
        (let desc-row ([idx0 : Nonnegative-Fixnum 0] [descs : (Listof String) descs] [ys : (Listof Flonum) ys]
                                                     [Widthn : Nonnegative-Flonum width] [yn : Flonum height])
          (define-values (descn widthn heightn idx)
            (let desc-col-expt : (Values String Nonnegative-Flonum Flonum Nonnegative-Fixnum)
              ([interval : Nonnegative-Fixnum 1] [open : Nonnegative-Fixnum idx0] [close : Nonnegative-Fixnum terminal]
                                                 [backtracking : String ""] [back-width : Nonnegative-Flonum max-width]
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

    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width (max phantom-height height))))
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
    (define frame : Bitmap (bitmap-blank width height (send bmp get-backing-scale)))
    (define dc : (Instance Bitmap-DC%) (send frame make-dc))
    (send dc set-smoothing 'aligned)
    (send dc set-pen (select-rgba-color pen-color) pen-size (if (zero? pen-size) 'transparent pen-style))
    (send dc set-brush (select-rgba-color brush-color) brush-style)
    (send dc draw-rectangle 0 0 width height)
    (send dc draw-bitmap bmp margin margin)
    frame))

(define bitmap-copy : (case-> [Bitmap -> Bitmap]
                              [Bitmap Real Real Nonnegative-Real Nonnegative-Real -> Bitmap])
  (case-lambda
    [(bmp) (bitmap-copy bmp 0 0 (send bmp get-width) (send bmp get-height))]
    [(bmp left top width height)
     (define clone : Bitmap (bitmap-blank width height (send bmp get-backing-scale)))
     (define dc : (Instance Bitmap-DC%) (send clone make-dc))
     (send dc set-smoothing 'aligned)
     (send dc draw-bitmap-section bmp 0 0 left top width height)
     clone]))

(define bitmap-section : (-> Bitmap Real Real Nonnegative-Real Nonnegative-Real Bitmap)
  (lambda [bmp left top width height]
    (define-values (src-width src-height) (bitmap-size bmp))
    (cond [(and (zero? left) (zero? top) (= width src-width) (= height src-height)) bmp]
          [else (bitmap-copy bmp left top width height)])))

(define bitmap-section/dot : (-> Bitmap Real Real Real Real Bitmap)
  (lambda [bmp left top right bottom]
    (bitmap-section bmp left top (max (- right left) 0) (max (- bottom top) 0))))

(define bitmap-inset : (case-> [Bitmap Real -> Bitmap]
                               [Bitmap Real Real -> Bitmap]
                               [Bitmap Real Real Real Real -> Bitmap])
  (case-lambda
    [(bmp inset) (bitmap-inset bmp inset inset inset inset)]
    [(bmp horizontal vertical) (bitmap-inset bmp horizontal vertical horizontal vertical)]
    [(bmp left top right bottom)
     (bitmap-section/dot bmp (- left) (- top) (+ (send bmp get-width) right) (+ (send bmp get-height) bottom))]))

(define bitmap-crop : (-> Bitmap Positive-Real Positive-Real Real Real Bitmap)
  (lambda [bmp width height left% top%]
    (define-values (w h) (bitmap-size bmp))
    (define-values (x y) (values (* (- width w) left%) (* (- height h) top%)))
    (bitmap-inset bmp x y (- width w x) (- height h y))))

(define bitmap-lt-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0 0)))
(define bitmap-lc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0 1/2)))
(define bitmap-lb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 0 1)))
(define bitmap-ct-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1/2 0)))
(define bitmap-cc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1/2 1/2)))
(define bitmap-cb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1/2 1)))
(define bitmap-rt-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1 0)))
(define bitmap-rc-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1 1/2)))
(define bitmap-rb-crop : (-> Bitmap Positive-Real Positive-Real Bitmap) (λ [bmp w h] (bitmap-crop bmp w h 1 1)))

(define bitmap-scale : (->* (Bitmap Real) (Real) Bitmap)
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (let ([width : Nonnegative-Real (* (send bmp get-width) (abs scale-x))]
                       [height : Nonnegative-Real (* (send bmp get-height) (abs scale-y))])
                   (define scaled : Bitmap (bitmap-blank width height (send bmp get-backing-scale)))
                   (define dc : (Instance Bitmap-DC%) (send scaled make-dc))
                   (send dc set-smoothing 'aligned)
                   (send dc set-scale scale-x scale-y)
                   (send dc draw-bitmap bmp 0 0)
                   scaled)])]))

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
  (lambda [x1% y1% x2% y2% bmp0 . bmps]
    (define-values (bmp _who _cares)
      (for/fold ([bmp : Bitmap  bmp0]
                 [x : Exact-Rational  0]
                 [y : Exact-Rational  0])
                ([bmp1 (in-list (cons bmp0 bmps))]
                 [bmp2 (in-list bmps)])
        (define-values (w1 h1) (bitmap-size bmp1))
        (define-values (w2 h2) (bitmap-size bmp2))
        (define x1 (+ x (- (inexact->exact (* x1% w1)) (inexact->exact (* x2% w2)))))
        (define y1 (+ y (- (inexact->exact (* y1% h1)) (inexact->exact (* y2% h2)))))
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
