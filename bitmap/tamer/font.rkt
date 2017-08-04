#lang typed/racket

(require "../draw.rkt")
(require "../constructor.rkt")
(require "../composite.rkt")
(require "../constants.rkt")
(require "../paint.rkt")
(require "../font.rkt")

(require "../digitama/font.rkt")

(default-border (desc-stroke long-dash #:color 'gray #:width 1))

(define bitmap-text* : (-> String Font Bitmap)
  (lambda [text font]
    (bitmap-frame (bitmap-text #:ascent magenta #:descent blue #:capline orange #:meanline green #:baseline red
                               text (desc-font (desc-font font #:size 'xx-large) #:size 2f0)))))

(for/list : (Listof (Pairof String Bitmap)) ([face (in-list (get-face-list))])
  (cons face (bitmap-text* (format "~a: Sphinx" face) (desc-font #:family face))))

(bitmap-vr-append* #:gapsize 16.0
                   (for/list : (Listof Bitmap) ([family (in-list css-font-generic-families)])
                     (define font (desc-font #:family family))
                     (bitmap-text* (format "~a[~a]: Sphinx" (font-face->family (Font-face font)) (Font-face font)) font)))
