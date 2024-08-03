#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/convert.rkt")

(require racket/format)

(require pangocairo/font)
(require pangocairo/source)
(require pangocairo/digitama/unsafe/dc/text)
(require pangocairo/constants)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define geo-text : (->* (Any) ((Option Font) #:color Fill-Paint #:background (Option Fill-Paint) #:lines (Listof Symbol)) Geo)
  (lambda [text [font #false] #:color [fgsource 'black] #:background [bgsource #false] #:lines [lines null]]
    (struct geo:text geo
      ([content : String]
       [font : (Option Font)]
       [lines : (Listof Symbol)]))
    
    (geo:text (~a text) font lines)))
