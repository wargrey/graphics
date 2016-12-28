#lang typed/racket

(provide (all-defined-out) <css-image>)

(require "digitama/digicore.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/image.rkt")
(require "recognizer.rkt")

;;; TODO: meanwhile <css+resolution> accepts 0
;;; TODO: deal with the default resolution

(define css-image-property-parsers : (->* (Symbol) ((U Regexp (Listof Symbol))) (Option CSS-Declaration-Parser))
  ;;; https://drafts.csswg.org/css-images/#image-processing
  (lambda [name [px.names #px"-(image|icon|logo)$"]]
    (case name
      [(image-resolution) (CSS<*> (CSS<^> (CSS:<+> (<css-keyword> '(from-image snap)) (<css+resolution>))) '+)]
      [(image-rendering) (CSS<^> (<css-keyword> css-image-rendering-option))]
      [else (and (or (and (list? px.names) (memq name px.names))
                     (and (regexp? px.names) (regexp-match? px.names (symbol->string name))))
                 (CSS<^> (<css-image>)))])))

(define make-css->bitmap : (case-> [(U Positive-Real (-> Bitmap Bitmap)) -> (-> Symbol CSS-Datum (U Bitmap CSS-Wide-Keyword))]
                                   [Positive-Real (U Positive-Real (-> Bitmap)) -> (-> Symbol CSS-Datum (U Bitmap CSS-Wide-Keyword))]
                                   [Positive-Real Positive-Real (-> Bitmap) -> (-> Symbol CSS-Datum (U Bitmap CSS-Wide-Keyword))])
  (case-lambda
    [(height density/image)
     (cond [(real? density/image) (make-css->bitmap (make-image-normalizer height density/image default-css-invalid-image))]
           [else (make-css->bitmap (make-image-normalizer height (default-icon-backing-scale) density/image))])]
    [(height density mk-image) (make-css->bitmap (make-image-normalizer height density mk-image))]
    [(height/normalize)
     (define normalize : (-> Bitmap Bitmap)
       (cond [(not (real? height/normalize)) height/normalize]
             [else (make-image-normalizer height/normalize (default-icon-backing-scale) default-css-invalid-image)]))
     (λ [_ image]
       (cond [(bitmap%? image) (normalize image)]
             [(css-image-datum? image) (normalize (image->bitmap image))]
             [else css:initial]))]))

(define css->bitmap (make-css->bitmap values))
