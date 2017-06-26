#lang typed/racket

(require "../digitama/draw.rkt")
(require "../constructor.rkt")
(require "../composite.rkt")
(require "../font.rkt")
(require "../color.rkt")

(define examples : (Listof Bitmap)
  (list (bitmap-text "Field Name: ")    (bitmap-text "Testcase for (bitmap-table)")
        (bitmap-text "Specification: ") (bitmap-paragraph "* Shown in a table of 2 rows;\n* This Field has no examples." #:max-width 300)
        (bitmap-text "Examples: ")))

(define monospace : Font (desc-font #:family 'monospace))
(define bitmaps : (Listof Bitmap)
  (for/list : (Listof Bitmap) ([x (in-range 1000)])
    (define rc : Integer (random #xFFFFFF))
    (bitmap-text (string-upcase (format "~x" rc)) monospace #:color (rgb* rc))))

(collect-garbage)
(time (bitmap-table 2 examples '(rc lc) '(ct) '(8) '(8)))

(collect-garbage) ; 35±3ms
(time (void (bitmap-vl-append* bitmaps)))

(collect-garbage) ; 39±3ms
(time (bitmap-table 26 bitmaps '(rc) '(cc) '(10) '(10)))
