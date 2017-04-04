#lang racket

(require "../main.rkt")

(define bitmap-text*
  (lambda [text font]
    (bitmap-frame #:color 'gray
                  (bitmap-text #:ascent-color 'Magenta #:descent-color 'Blue
                               #:capline-color 'Orange #:meanline-color 'Green #:baseline-color 'Red
                               text (make-css-font (make-css-font font #:size 'xx-large) #:size 2.f0)))))

#;(for/list ([face (in-list (get-face-list))])
    (cons face (bitmap-vl-append (bitmap-text* "Sphinx[+]" (make-css-font #:family face #:ligature 'normal))
                                 (bitmap-text* "Sphinx[-]" (make-css-font #:family face #:ligature 'none)))))

(bitmap-vl-append* #:gapsize 16
 (for/list ([face (in-list (list "Qomolangma-Uchen Sarchung" "Microsoft Himalaya" "Kailasa"))])
   (bitmap-vl-append (bitmap-text* (string-append face ": བོད་ཡིག[+]") (make-css-font #:family face #:ligature 'normal))
                     (bitmap-text* (string-append face ": བོད་ཡིག[-]") (make-css-font #:family face #:ligature 'none)))))

(bitmap-vl-append* #:gapsize 16
 (for/list ([family (in-list (list 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))])
   (define font (make-css-font #:family family #:ligature 'normal))
   (bitmap-text* (format "~a[~a]: norm -> Sphinx" (send font get-family) (send font get-face)) font)))
