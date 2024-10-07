#lang typed/racket

(provide (all-defined-out))

(require geofun/vector)
(require geofun/digitama/font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(default-border (desc-stroke #:color 'gray #:width 1 #:dash 'long-dash))

(define geo-text* : (->* (String Font) (Real) Geo)
  (lambda [text font [size -2.0]]
    (define content (geo-text #:ascent 'magenta #:descent 'blue #:capline 'orange #:meanline 'green #:baseline 'red
                              text (desc-font (desc-font font #:size 'xx-large) #:size size)))
    (geo-frame content)))


(module+ main
  (for/list : (Listof (Pairof Geo String)) ([face (in-list (list-font-faces))])
    (cons (geo-text* (format "λ[~a]: Sphinx 0123456789 汉字测试" face) (desc-font #:family face))
          face))
  
  (geo-vr-append* #:gapsize 16.0
                  (for/list : (Listof Geo) ([family (in-list css-font-generic-families)])
                    (define font (desc-font #:family family #:variant 'small-caps))
                    (geo-text* (format "~a[~a]: λ Sphinx 0123456789 汉字测试" (font-face->family (font-face font)) (font-face font)) font)))

  (geo-vr-append* #:gapsize 16.0
                  (for/list : (Listof Geo) ([monospace (in-list (list-monospace-font-families))])
                    (define font (desc-font #:family monospace))
                    (geo-text* (format "monospace[~a]: λ Sphinx 0123456789 汉字测试" (font-face font)) font))))
