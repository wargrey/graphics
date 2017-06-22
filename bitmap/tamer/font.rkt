#lang typed/racket

(require "../constructor.rkt")
(require "../constants.rkt")
(require "../paint.rkt")
(require "../font.rkt")
(require "../color.rkt")

(require "../digitama/draw.rkt")
(require "../digitama/font.rkt")

(default-stroke (desc-stroke #:color 'gray #:width 1 #:dash 'solid))

(define bitmap-text* : (-> String Font Bitmap)
  (lambda [text font]
    (bitmap-frame (bitmap-text #:ascent magenta #:descent blue #:capline orange #:meanline green #:baseline red
                               text (desc-font (desc-font font #:size 'xx-large) #:size 2f0)))))

(for/list : (Listof (Pairof String Bitmap)) ([face (in-list (get-face-list))])
  (cons face (bitmap-text* (format "~a: Sphinx" face) (desc-font #:family face))))

;(bitmap-vl-append* #:gapsize 16
 (for/list : (Listof Bitmap) ([family (in-list css-font-generic-families)])
   (define font (desc-font #:family family))
   (bitmap-text* (format "~a[~a]: Sphinx" (font-face->family (font-face font)) (font-face font)) font));)
