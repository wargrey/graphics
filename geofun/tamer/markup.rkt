#lang typed/racket

;;; https://docs.gtk.org/Pango/pango_markup.html

(require geofun/vector)
(require geofun/bitmap)

(require geofun/digitama/markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define font (desc-font #:family 'math #:size 'xx-large))

(define pango-markup-xexprs : (Listof PExpr)
  (list #;'(span)
        '(markup (lt "plain" 169 "plteen.fun" gt))
        '(span ([letter_spacing . 1.0])
               ("x" (sub ("n+" (span ([style . normal]
                                      [size . small])
                                     ("1"))))
                    #\space "=" #\space #\space "x" (sub ("n")) #\space
                    (span ([style . normal]
                           [bgcolor . #:CCCCCC]
                           [bgalpha . 50]) ("+ 1"))))))

(define raw-markups : (Listof Geo)
  (list (geo-markup #:color 'ForestGreen
                    (string-append "<markup><span foreground=\"blue\" size=\"xx-large\">Blue text</span> is <i>cool</i>!</markup>"))
        (geo-bitmap (bitmap-markup #:background 'WhiteSmoke #:lines '(line-through)
                                   (string-append "<span foreground=\"purple\">ا</span>"
                                                  "<span foreground=\"red\">َ</span>ل<span foreground=\"blue\">ْ</span>"
                                                  "ع<span foreground=\"red\">َ</span>ر<span foreground=\"red\">َ</span>"
                                                  "ب<span foreground=\"red\">ِ</span>ي<span foreground=\"green\">ّ</span>"
                                                  "<span foreground=\"red\">َ</span>ة<span foreground=\"blue\">ُ</span>")
                                   font))
        (geo-markup #:background 'GhostWhite #:alignment 'center #:lines '(undercurl)
                    (string-append "x<sub>n</sub> = x<sub>n<span style=\"normal\">-<span size=\"smaller\">1</span></span></sub><span style=\"normal\"> + 1</span>\n"
                                   "e<sup> i π</sup><span style=\"normal\"> + 1 = 0</span>")
                    font)
        (geo-markup "Plain!" font #:lines '(underdouble))
        (geo-markup "<span>哈哈</span><span" #:error-color 'GhostWhite #:error-background 'Crimson)))

(define pexprs : (Listof Geo)
  (for/list : (Listof Geo) ([pexpr (in-list pango-markup-xexprs)])
    (displayln (dc-markup-datum->text pexpr))
    (geo-markup #:background 'Azure
                pexpr font)))

(module+ main
  (geo-vl-append* #:gapsize 4.0 raw-markups)
  (geo-vl-append* #:gapsize 4.0 pexprs))
