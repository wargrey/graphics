#lang typed/racket

(provide (all-defined-out))

(require bitmap)
(require geofun/digitama/font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(default-border (desc-stroke #:color 'gray #:width 1 #:dash 'long-dash))

(define bitmap-text* : (->* (String Font) (Real) Bitmap)
  (lambda [text font [size -2.0]]
    (define content (bitmap-text #:ascent 'magenta #:descent 'blue #:capline 'orange #:meanline 'green #:baseline 'red
                                 text (desc-font (desc-font font #:size 'xx-large) #:size size)))
    (bitmap-frame content)))


(module+ main
  (for/list : (Listof (Pairof Bitmap String)) ([face (in-list (list-font-faces))])
    (cons (bitmap-text* (format "λ[~a]: Sphinx 0123456789" face) (desc-font #:family face))
          face))
  
  (bitmap-vr-append* #:gapsize 16.0
                     (for/list : (Listof Bitmap) ([family (in-list css-font-generic-families)])
                       (define font (desc-font #:family family #:variant 'small-caps))
                       (bitmap-text* (format "~a[~a]: λ Sphinx 0123456789" (font-face->family (font-face font)) (font-face font)) font))))
