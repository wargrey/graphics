#lang typed/racket

(provide (all-defined-out))

(require geofun/vector)
(require geofun/digitama/font)

(require "shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-vr-append* #:gapsize 16.0
                  (for/list : (Listof Geo) ([family (in-list css-font-generic-families)])
                    (define font (desc-font #:family family #:variant 'small-caps))
                    (geo-text* (format "~a[~a]: ΣλπΦ Sphinx +0123456789 汉字测试" (font-face->family (font-face font)) (font-face font)) font)))

  (geo-vr-append* #:gapsize 16.0
                  (for/list : (Listof Geo) ([monospace (in-list (list-monospace-font-families))])
                    (define font (desc-font #:family monospace))
                    (geo-text* (format "monospace[~a]: ΣλπΦ Sphinx +0123456789 汉字测试" (font-face font)) font))))
