#lang typed/racket

(provide (all-defined-out) <css-image>)

(require bitmap/constructor)

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

(define make-css->bitmap : (All (racket) (case-> [(-> (CSS-Maybe Bitmap) racket) -> (CSS->Racket racket)]
                                                 [Positive-Real (-> Bitmap) -> (CSS->Racket Bitmap)]
                                                 [Positive-Real Positive-Real (-> Bitmap) -> (CSS->Racket Bitmap)]
                                                 [Positive-Real -> (CSS->Racket (CSS-Maybe Bitmap))]
                                                 [Positive-Real Positive-Real -> (CSS->Racket (CSS-Maybe Bitmap))]))
  (case-lambda
    [(height density/image)
     (cond [(real? density/image) (css->normalized-image (make-image-normalizer height density/image))]
           [else (css->normalized-image (make-image-normalizer height (default-icon-backing-scale) density/image))])]
    [(height density mk-image) (css->normalized-image (make-image-normalizer height density mk-image))]
    [(height/normalize)
     (cond [(not (real? height/normalize)) (css->normalized-image height/normalize)]
           [else (css->normalized-image (make-image-normalizer height/normalize (default-icon-backing-scale)))])]))

(define-values (css->bitmap css->image)
  (values (css->normalized-image (λ [[raw : (CSS-Maybe Bitmap)]] raw))
          (css->normalized-image (λ [[raw : (CSS-Maybe Bitmap)]]
                                   (cond [(and (bitmap%? raw) (send raw ok?)) raw]
                                         [else (bitmap-solid)])))))
