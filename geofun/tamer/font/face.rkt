#lang typed/racket

(provide (all-defined-out))

(require geofun/vector)
(require geofun/digitama/font)

(require "shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (for/list : (Listof (Pairof Geo String)) ([face (in-list (list-font-faces))])
    (cons (geo-text* (format "λ[~a]: ΣλπΦ Sphinx +0123456789 汉字测试" face) (desc-font #:family face))
          face)))
