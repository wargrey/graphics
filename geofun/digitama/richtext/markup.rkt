#lang typed/racket/base

(provide (all-defined-out))

(require "pexpr.rkt")
(require "../unsafe/dc/text-layout.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Markup-Datum (U PExpr-Element Bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-markup-datum->text : (-> Geo-Markup-Datum Bytes)
  (lambda [src]
    (cond [(bytes? src) src]
          [else (pexpr->bytes src)])))

(define geo-markup-datum->option-text : (-> (Option Geo-Markup-Datum) (Option Bytes))
  (lambda [src]
    (and src
         (geo-markup-datum->text src))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-markup-datum? : (-> Any Boolean : Geo-Markup-Datum)
  (lambda [info]
    (or (pexpr-element? info)
        (bytes? info))))

(define geo-markup-option-datum? : (-> Any Boolean : (Option Geo-Markup-Datum))
  (lambda [info]
    (or (geo-markup-datum? info)
        (not info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-markup-extract-plain-text : (-> Geo-Markup-Datum (Option String))
  (lambda [src]
    (dc_markup_plain_text (geo-markup-datum->text src))))
