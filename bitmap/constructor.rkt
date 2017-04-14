#lang typed/racket

(provide (all-defined-out))
(provide (rename-out [bitmap-ellipse bitmap-circle]
                     [bitmap-ellipse bitmap-disk]))

(require "digitama/digicore.rkt")
(require "background.rkt")
(require "color.rkt")
(require "font.rkt")

(require racket/math)
(require typed/images/icons)

(define bitmap-blank : (->* () (Real (Option Real) Positive-Real) Bitmap)
  (lambda [[w 0] [h #false] [density (default-icon-backing-scale)]]
    (define width : Positive-Integer (max 1 (exact-ceiling w)))
    (define height : Positive-Integer (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale density)))

(define bitmap-dc : (->* ((-> (Instance Bitmap-DC%) Nonnegative-Flonum Nonnegative-Flonum Any))
                         (Nonnegative-Real (Option Nonnegative-Real) Positive-Real)
                         Bitmap)
  (lambda [draw-with [w 0] [h #false] [density (default-icon-backing-scale)]]
    (define bmp : Bitmap (bitmap-blank w h density))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    ;;; let the device deal with non-integer size
    (draw-with dc (real->double-flonum w) (real->double-flonum (or h w)))
    bmp))

(define bitmap-ghost : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-blank (send bmp get-width)
                  (send bmp get-height)
                  (send bmp get-backing-scale))))

(define bitmap-solid : (->* () (Color+sRGB Real Positive-Real) Bitmap)
  (lambda [[color 'transparent] [size 1] [density (default-icon-backing-scale)]]
    (define solid : Bitmap (bitmap-blank size size density))
    (define dc : (Instance Bitmap-DC%) (send solid make-dc))
    (send dc set-background (select-color color))
    (send dc clear)
    solid))

(define bitmap-text : (->* (String) ((Instance Font%) #:combine? (U Boolean Symbol) #:color (Option Color+sRGB)
                                                      #:background-color (Option Color+sRGB) #:baseline (Option Pen+Color)
                                                      #:capline (Option Pen+Color) #:meanline (Option Pen+Color)
                                                      #:ascent (Option Pen+Color) #:descent (Option Pen+Color)) Bitmap)
  (lambda [content [font (default-css-font)] #:combine? [?combine? 'auto] #:color [fgcolor #false] #:background-color [bgcolor #false]
                   #:ascent [alcolor #false] #:descent [dlcolor #false] #:capline [clcolor #false] #:meanline [mlcolor #false]
                   #:baseline [blcolor #false]]
    (define combine? : Boolean (if (boolean? ?combine?) ?combine? (implies (css-font%? font) (send font get-combine?))))
    (define-values (width height _ zero) (send the-dc get-text-extent content font combine?))
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-color fgcolor)))
    (when bgcolor (send dc set-text-background (select-color bgcolor)))
    (when bgcolor (send dc set-text-mode 'solid))
    (send dc draw-text content 0 0 combine?)
    (when (or alcolor clcolor mlcolor blcolor dlcolor)
      (define-values (ascent capline meanline baseline descent) (get-font-metrics-lines font content))
      (unless (false? alcolor)
        (send dc set-pen (select-pen alcolor))
        (send dc draw-line 0 ascent width ascent))
      (unless (false? clcolor)
        (send dc set-pen (select-pen clcolor))
        (send dc draw-line 0 capline width capline))
      (unless (false? mlcolor)
        (send dc set-pen (select-pen mlcolor))
        (send dc draw-line 0 meanline width meanline))
      (unless (false? blcolor)
        (send dc set-pen (select-pen blcolor))
        (send dc draw-line 0 baseline width baseline))
      (unless (false? dlcolor)
        (send dc set-pen (select-pen dlcolor))
        (send dc draw-line 0 descent width descent)))
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-desc : (->* (String Real)
                           ((Instance Font%) #:color (Option Color+sRGB) #:background (Option Brush+Color)
                                             #:max-height Real #:combine? (U Boolean Symbol))
                           Bitmap)
  (lambda [description max-width.0 [font (default-css-font)] #:max-height [max-height.0 +inf.0]
           #:combine? [?combine? 'auto] #:color [fgcolor #false] #:background [bgcolor #false]]
    (define combine? : Boolean (if (boolean? ?combine?) ?combine? (implies (css-font%? font) (send font get-combine?))))
    (define-values (max-width max-height) (values (real->double-flonum max-width.0) (real->double-flonum max-height.0)))
    (define desc-extent : (-> String Integer Integer (Values String Nonnegative-Flonum Nonnegative-Flonum))
      (lambda [desc start end]
        (define subdesc : String (substring desc start end))
        (define-values (width height descent ascent) (send the-dc get-text-extent subdesc font combine?))
        (values subdesc (real->double-flonum width) (real->double-flonum height))))

    (define-values (_ _w phantom-height) (desc-extent " " 0 1))
    (define-values (descs ys width height)
      (for/fold ([descs : (Listof String) null] [ys : (Listof Flonum) null] [width : Flonum 0.0] [height : Flonum 0.0])
                ([desc : String (in-list (string-split description (string #\newline)))])
        (define terminal : Index (string-length desc))
        (let desc-row ([idx0 : Fixnum 0] [descs : (Listof String) descs] [ys : (Listof Flonum) ys]
                                         [Widthn : Flonum width] [yn : Flonum height])
          (define-values (descn widthn heightn idx)
            (let desc-col-expt : (Values String Flonum Flonum Fixnum)
              ([interval : Nonnegative-Fixnum 1] [open : Fixnum idx0] [close : Fixnum terminal]
                                                 [backtracking : String ""] [back-width : Flonum max-width]
                                                 [back-height : Flonum phantom-height])
              (define idx : Fixnum (fxmin close (fx+ open interval)))
              (define next-open : Fixnum (fx+ open (fxquotient interval 2)))
              (define-values (line width height) (desc-extent desc idx0 idx))
              (define found? : Boolean (and (fl> width max-width) (fx= interval 1) (fl<= back-width max-width)))
              (cond [found? (values backtracking back-width back-height (if (zero? open) terminal (fx- idx 1)))]
                    [(fl> width max-width) (desc-col-expt 1 next-open idx backtracking back-width back-height)]
                    [(fx= close idx) (values line width height idx)]
                    [else (desc-col-expt (fxlshift interval 1) open close line width height)])))
          (cond [(fl> (fl+ yn heightn) max-height) (values descs ys (max widthn Widthn) yn)]
                [(fx= idx terminal) (values (cons descn descs) (cons yn ys) (max widthn Widthn) (fl+ yn heightn))]
                [else (desc-row idx (cons descn descs) (cons yn ys) (max widthn Widthn) (fl+ yn heightn))]))))

    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width (max phantom-height height))))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground (select-color fgcolor)))
    (unless (false? bgcolor)
      (send dc set-smoothing 'aligned)
      (send dc set-pen (select-pen 'transparent))
      (send dc set-brush (select-brush bgcolor))
      (send dc draw-rectangle 0 0 (max 1 width) (max 1 phantom-height height)))
    (for ([desc (in-list (reverse descs))] [y (in-list (reverse ys))])
      (send dc draw-text desc 0 y combine?))
    (or (send dc get-bitmap) (bitmap-blank))))

(define bitmap-frame : (-> Bitmap [#:margin Nonnegative-Real] [#:border Nonnegative-Real] [#:inset Nonnegative-Real]
                           [#:color Pen+Color] [#:background Brush+Color]
                           Bitmap)
  (lambda [bmp #:margin [margin 0] #:border [border 1] #:inset [inset 0] #:color [pen-hint 'black] #:background [brush-hint 'transparent]]
    ;;; TODO:
    ; In this aligned bitmap context, border width 0 behaves the same as 1.
    ; It seems that, Racket also use the "as thin as possible" algorithm to deal with zero width line. 
    (define offset : Nonnegative-Real (+ (max border 1) inset))
    (define width : Positive-Real (+ (send bmp get-width) offset offset))
    (define height : Positive-Real (+ (send bmp get-height) offset offset))
    (define full-width : Positive-Real (+ width margin margin))
    (define full-height : Positive-Real (+ height margin margin))
    (define frame : Bitmap (bitmap-blank full-width full-height (send bmp get-backing-scale)))
    (define dc : (Instance Bitmap-DC%) (send frame make-dc))
    (send dc set-smoothing 'aligned)
    (send dc set-pen (select-pen pen-hint))
    (send dc set-brush (select-brush brush-hint))
    (send dc draw-rectangle margin margin width height)
    (send dc draw-bitmap bmp (+ offset margin) (+ offset margin))
    frame))

(define bitmap-rectangle : (->* (Real) (Real (Option Real) #:color Brush+Color #:border Pen+Color) Bitmap)
  (lambda [w [h #false] [radius #false] #:color [color #x000000] #:border [pen-color 'transparent]]
    (define width : Nonnegative-Real (max w 0.0))
    (define height : Nonnegative-Real (max (or h w) 0.0))
    (define bmp : Bitmap (bitmap-blank width height))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    (send dc set-pen (select-pen pen-color))
    (send dc set-brush (select-brush color))
    (cond [(false? radius) (send dc draw-rectangle 0 0 width height)]
          [else (send dc draw-rounded-rectangle 0 0 width height radius)])
    bmp))

(define bitmap-ellipse : (->* (Real) (Real #:color Brush+Color #:border Pen+Color) Bitmap)
  (lambda [w [h #false] #:color [color #x000000] #:border [pen-color 'transparent]]
    (define width : Nonnegative-Real (max w 0.0))
    (define height : Nonnegative-Real (max (or h w) 0.0))
    (define bmp : Bitmap (bitmap-blank width height))
    (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
    (send dc set-smoothing 'aligned)
    (send dc set-pen (select-pen pen-color))
    (send dc set-brush (select-brush color))
    (send dc draw-ellipse 0 0 width height)
    bmp))
