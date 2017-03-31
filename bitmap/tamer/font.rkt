#lang racket

(require bitmap)

(define bitmap-text*
  (lambda [text font]
    (bitmap-frame #:color 'gray
                  (bitmap-text #:baseline-color 'green #:ascentline-color 'red
                               text (make-font+ font #:size 24)))))

(for/list ([face (in-list (list "Qomolangma-Uchen Sarchung" "Microsoft Himalaya" "Kailasa"))])
  (cons (bitmap-text* (string-append face ": བོད་ཡིག[#t]")
                      (make-font+ #:family face #:combine? #true))
        (bitmap-text* (string-append face ": བོད་ཡིག[#f]")
                      (make-font+ #:family face #:combine? #false))))

(for/list ([family (in-list (list 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system))])
  (cons (bitmap-text* (format "~a: Sphinx[#t]" (font-family->font-face family))
                      (make-font+ #:family family #:combine? #true))
        (bitmap-text* (format "~a: Sphinx[#f]" (font-family->font-face family))
                      (make-font+ #:family family #:combine? #false))))
