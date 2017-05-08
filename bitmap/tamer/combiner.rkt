#lang typed/racket

(require "../digitama/digicore.rkt")
(require "../constructor.rkt")
(require "../combiner.rkt")
(require "../font.rkt")

(define examples : (Listof Bitmap)
  (list (bitmap-text "Field Name: ")    (bitmap-text "Testcase for (bitmap-table)")
        (bitmap-text "Specification: ") (bitmap-paragraph "* Shown in a table of 2 rows;\n* This Field has no examples." 300)
        (bitmap-text "Examples: ")))

(define modern : Font (make-css-font #:family 'modern))
(define bitmaps : (Listof Bitmap)
  (for/list : (Listof Bitmap) ([x (in-range 1000)])
    (define rc : Integer (random #xFFFFFF))
    (bitmap-text (string-upcase (format "~x" rc)) modern #:color rc)))

(collect-garbage)
(time (bitmap-table 2 examples '(rc lc) '(ct) '(8) '(8)))

(collect-garbage)
(time (void (bitmap-vl-append* bitmaps)))

(collect-garbage)
(time (bitmap-table 26 bitmaps '(rc) '(cc) '(10) '(10)))
