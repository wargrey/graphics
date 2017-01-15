#lang typed/racket/base

(require "../constructor.rkt")
(require "../combiner.rkt")

(bitmap-table 2 '(rc lc) '(ct) '(8) '(8)
              (bitmap-text "Field Name: ")    (bitmap-text "Testcase for (bitmap-table)")
              (bitmap-text "Specification: ") (bitmap-desc "* Shown in a table of 2 rows;\n* This Field has no examples." 300)
              (bitmap-text "Examples: "))
