#lang typed/racket

(provide (all-defined-out))

(require "../constructor.rkt")
(require "../composite.rkt")
(require "../constants.rkt")
(require "../paint.rkt")
(require "../font.rkt")

(require "../digitama/unsafe/convert.rkt")
(require "../digitama/font.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(default-border (desc-stroke long-dash #:color 'gray #:width 1))

(define bitmap-text* : (->* (String Font) (Real) Bitmap)
  (lambda [text font [size -2.0]]
    (define content (bitmap-text #:ascent magenta #:descent blue #:capline orange #:meanline green #:baseline red
                                 text (desc-font (desc-font font #:size 'xx-large) #:size size)))
    (bitmap-frame content)))


(module+ main
  (for/list : (Listof (Pairof String Bitmap)) ([face (in-list (list-font-faces))])
    (cons face (bitmap-text* (format "~a: Sphinx 0123456789" face) (desc-font #:family face))))
  
  (bitmap-vr-append* #:gapsize 16.0
                     (for/list : (Listof Bitmap) ([family (in-list css-font-generic-families)])
                       (define font (desc-font #:family family #:variant 'small-caps))
                       (bitmap-text* (format "~a[~a]: Sphinx 0123456789" (font-face->family (font-face font)) (font-face font)) font))))
