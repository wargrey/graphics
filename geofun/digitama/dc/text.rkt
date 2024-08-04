#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

(require "../convert.rkt")
(require "../vector.rkt")
(require "../source.rkt")
(require "../color.rkt")

(require "../unsafe/font.rkt")
(require "../unsafe/source.rkt")
(require "../unsafe/dc/text.rkt")

(require "../../font.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:text geo
  ([content : String]
   [font : (Option Font-Description)]
   [lines : (Listof Symbol)]
   [fgcolor : (Option Fill-Source)]
   [bgcolor : (Option Fill-Source)])
  #:type-name Geo:Text
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-text : (->* (Any) ((Option Font) #:id Symbol #:color (Option Fill-Paint) #:background (Option Fill-Paint) #:lines (Listof Symbol)) Geo:Text)
  (lambda [text [font #false] #:id [id #false] #:color [fgsource #false] #:background [bgsource #false] #:lines [lines null]]
    
    (create-geometry-object geo:text #:with geo-text-surface #:id (or id (gensym 'text))
                            (~a text) (and font (font-description font)) lines
                            (fill-paint->source* fgsource) (fill-paint->source* bgsource))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-text-surface : Geo-Surface-Create
  (lambda [self [paint (default-stroke)] [fill #false] [fstyle 'winding]]
    (with-asserts ([self geo:text?])
      (dc_text create-abstract-surface
               (geo:text-content self) (or (geo:text-font self) (font-description (default-font))) (geo:text-lines self)
               (or (geo:text-fgcolor self) (rgb* ((default-make-currentcolor)))) (geo:text-fgcolor self)
               #false #false #false #false #false 1.0))))
