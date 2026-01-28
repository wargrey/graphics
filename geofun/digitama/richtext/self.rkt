#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Text-Alignment rich-datum<%>? Rich-Datum<%>)

(require "markup.rkt")

(require "../self.rkt")
(require "../unsafe/visual.rkt")
(require "../unsafe/dc/text-layout.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Rich-Text (U String Complex Rich-Datum<%> Geo-Markup-Datum))
(define-type Geo-Option-Rich-Text (Option Geo-Rich-Text))
(define-type Geo-Maybe-Rich-Text (U Void Geo-Option-Rich-Text))

(struct geo:rich:dimension rich-datum<%>
  ([value : Complex]
   [unit : (U Geo String Symbol Geo-Markup-Datum)])
  #:type-name Geo:Rich:Dimension
  #:constructor-name grt:dim
  #:transparent)

(struct geo:rich:point rich-datum<%>
  ([coordinates : (U Complex (List* Real Real (Listof Real)))])
  #:type-name Geo:Rich:Point
  #:constructor-name grt:pt
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rich-text? : (-> Any Boolean : Geo-Rich-Text)
  (lambda [v]
    (or (string? v)
        (complex? v)
        (rich-datum<%>? v)
        (geo-markup-datum? v))))

(define geo-option-rich-text? : (-> Any Boolean : Geo-Option-Rich-Text)
  (lambda [v]
    (or (geo-rich-text? v)
        (not v))))
