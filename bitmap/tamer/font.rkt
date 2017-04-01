#lang racket

(require bitmap)

(define bitmap-text*
  (lambda [text font]
    (bitmap-frame #:color 'gray
                  (bitmap-text #:baseline-color 'green #:ascentline-color 'red
                               text (make-font+ font #:size 24)))))

(for/list ([face (in-list (get-face-list))])
    (cons face (bitmap-vl-append (bitmap-text* "Sphinx[#t]" (make-font+ #:family face #:combine? #true))
                                 (bitmap-text* "Sphinx[#f]" (make-font+ #:family face #:combine? #false)))))

(bitmap-vl-append* #:gapsize 16
 (for/list ([face (in-list (list "Qomolangma-Uchen Sarchung" "Microsoft Himalaya" "Kailasa"))])
   (bitmap-vl-append (bitmap-text* (string-append face ": བོད་ཡིག[#t]") (make-font+ #:family face #:combine? #true))
                     (bitmap-text* (string-append face ": བོད་ཡིག[#f]") (make-font+ #:family face #:combine? #false)))))

(bitmap-vl-append* #:gapsize 16
 (for/list ([family (in-list (list 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))])
   (define font (make-font+ #:family family #:combine? #true))
   (bitmap-vl-append (bitmap-text* (format "~a[~a]: #t -> Sphinx" (send font get-family) (send font get-face)) font)
                     (bitmap-text* (format "~a[~a]: #f -> Sphinx" (send font get-family) (send font get-face))
                                   (make-font+ font #:combine? #false)))))
