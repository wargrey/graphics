#lang typed/racket/base

(require "../digitama/digicore.rkt")
(require "../constructor.rkt")
(require "../combiner.rkt")
(require "../font.rkt")

(bitmap-table 2
              (list (bitmap-text "Field Name: ")    (bitmap-text "Testcase for (bitmap-table)")
                    (bitmap-text "Specification: ") (bitmap-desc "* Shown in a table of 2 rows;\n* This Field has no examples." 300)
                    (bitmap-text "Examples: "))
               '(rc lc) '(ct) '(8) '(8))

(define modern : Font (make-css-font #:family 'modern))
(define bitmaps : (Listof Bitmap)
  (for/list : (Listof Bitmap) ([x (in-range 1024)])
    (define rc : Integer (random #xFFFFFF))
    (bitmap-text (string-upcase (format "~x" rc)) modern #:color rc)))

(time (bitmap-table 26 bitmaps '(rc) '(cc) '(10) '(10)))
