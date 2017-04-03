#lang racket

(require "../main.rkt")

(define bitmap-text*
  (lambda [text font]
    (bitmap-frame #:color 'gray
                  (bitmap-text #:ascent-color 'Magenta #:descent-color 'Blue
                               #:capline-color 'Orange #:meanline-color 'Green #:baseline-color 'Red
                               text (make-css-font (make-css-font font #:size 'xx-large) #:size 'larger)))))

#;(for/list ([face (in-list (get-face-list))])
    (cons face (bitmap-vl-append (bitmap-text* "Sphinx[#t]" (make-css-font #:family face #:combine? #true))
                                 (bitmap-text* "Sphinx[#f]" (make-css-font #:family face #:combine? #false)))))

(bitmap-vl-append* #:gapsize 16
 (for/list ([face (in-list (list "Qomolangma-Uchen Sarchung" "Microsoft Himalaya" "Kailasa"))])
   (bitmap-vl-append (bitmap-text* (string-append face ": བོད་ཡིག[#t]") (make-css-font #:family face #:combine? #true))
                     (bitmap-text* (string-append face ": བོད་ཡིག[#f]") (make-css-font #:family face #:combine? #false)))))

(bitmap-vl-append* #:gapsize 16
 (for/list ([family (in-list (list 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))])
   (define font (make-css-font #:family family #:combine? #true))
   (bitmap-vl-append (bitmap-text* (format "~a[~a]: #t -> Sphinx" (send font get-family) (send font get-face)) font)
                     (bitmap-text* (format "~a[~a]: #f -> Sphinx" (send font get-family) (send font get-face))
                                   (make-css-font font #:combine? #false)))))
