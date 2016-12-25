#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitmap.rkt")
(require "digitama/color.rkt")

(require racket/math)
(require typed/images/icons)

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
