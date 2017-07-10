#lang typed/racket

(require "../../draw.rkt")
(require "../../constructor.rkt")
(require "../../composite.rkt")
(require "../../font.rkt")
(require "../../color.rkt")

(define examples : (Listof Bitmap)
  (list (bitmap-text "Field Name: ")    (bitmap-text "Testcase for (bitmap-table)")
        (bitmap-text "Specification: ") (bitmap-paragraph "* Shown in a table of 2 rows;\n* This Field has no examples." #:max-width 300)
        (bitmap-text "Examples: ")))

(define monospace : Font (desc-font #:family 'monospace))
(define bitmaps : (Listof Bitmap)
  (time (build-list 1350 (λ _ (let ([rc (random #xFFFFFF)])
                                (bitmap-text #:color (rgb* rc)
                                             (string-upcase (format "~x" rc))
                                             monospace))))))

(collect-garbage) ; 6-9ms
(time (bitmap-table 2 examples '(rc lc) '(ct) '(8) '(8)))

(collect-garbage) ; 23-28ms
(time (void (bitmap-vl-append* bitmaps)))

(collect-garbage) ; 25-30ms
(time (bitmap-table 26 bitmaps '(rc) '(cc) '(10) '(10)))
