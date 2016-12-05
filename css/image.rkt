#lang typed/racket

(provide (all-defined-out) <css-image>)

(require bitmap)

(require "digitama/image.rkt")
(require "digitama/misc.rkt")
(require "digitama/digicore.rkt")
(require "recognizer.rkt")
(require "racket.rkt")
(require "color.rkt")

(define css-image-property-parsers : (->* (Symbol) ((U Regexp (Listof Symbol))) (Option CSS-Declaration-Parser))
  ;;; https://drafts.csswg.org/css-images/#image-processing
  (lambda [name [px.names #px"-(image|icon|logo)$"]]
    (case name
      [(image-resolution) (CSS<*> (CSS<^> (CSS:<+> (<css-keyword> '(from-image snap)) (<css+resolution>))) '+)]
      [(image-rendering) (CSS<^> (<css-keyword> css-image-rendering-option))]
      [else (and (or (and (list? px.names) (memq name px.names))
                     (and (regexp? px.names) (regexp-match? px.names (symbol->string name))))
                 (CSS<^> (<css-image>)))])))

(define make-css->bitmap : (case-> [-> (-> Symbol CSS-Datum (U Bitmap CSS-Wide-Keyword))]
                                   [(-> Bitmap Bitmap) -> (-> Symbol CSS-Datum (U Bitmap CSS-Wide-Keyword))]
                                   [Positive-Real (U Positive-Real Bitmap) -> (-> Symbol CSS-Datum (U Bitmap CSS-Wide-Keyword))]
                                   [Positive-Real Positive-Real Bitmap -> (-> Symbol CSS-Datum (U Bitmap CSS-Wide-Keyword))])
  (case-lambda
    [() (make-css->bitmap values)]
    [(height density/alt-image)
     (cond [(real? density/alt-image) (make-css->bitmap height density/alt-image (default-css-invalid-image))]
           [else (make-css->bitmap height (default-icon-backing-scale) (default-css-invalid-image))])]
    [(height density alt-image)
     (make-css->bitmap
      (λ [[raw : Bitmap]]
        (define (normalize [bmp : Bitmap]) : Bitmap
          (bitmap-scale (bitmap-alter-density bmp density)
                        (/ height (send bmp get-height))))
        (cond [(send raw ok?) (normalize raw)]
              [(> (send alt-image get-height) height) (normalize alt-image)]
              [else (bitmap-cc-superimpose (bitmap-blank height height density)
                                           (bitmap-alter-density alt-image density))])))]
    [(normalize)
     (letrec ([image->bitmap : (->* (CSS-Image-Datum) (Positive-Real) Bitmap)
               (λ [img [the-density (default-icon-backing-scale)]]
                 (cond [(non-empty-string? img) (bitmap img the-density)]
                       [(image? img)
                        (define bmp : Bitmap (image->bitmap (image-content img)))
                        (cond [(send bmp ok?) bmp]
                              [else (let ([color (css->color '_ (image-fallback img))])
                                      (bitmap-solid (if (object? color) color 'transparent)))])]
                       [(image-set? img)
                        (define-values (src density)
                          (for/fold ([the-src : CSS-Image-Datum ""]
                                     [resolution : Nonnegative-Flonum 0.0])
                                    ([option (in-list (image-set-options img))])
                            (define this-density : Nonnegative-Flonum (cadr option))
                            ; - resoltions should not duplicate, but if so, use the first one.
                            ; - if there is no specific resolution, use the highest one.
                            (if (or (= resolution the-density) (fl<= this-density resolution))
                                (values the-src resolution)
                                (values (car option) this-density))))
                        (cond [(zero? density) the-invalid-image]
                              [else (image->bitmap src density)])]
                       [(css-@λ? img)
                        (with-handlers ([exn? (λ [[e : exn]] (css-log-eval-error e 'css->bitmap) the-invalid-image)])
                          (define sexp : (Listof Any) (css-@λ->top-level-form img (flcss%-em length%)))
                          (define icon : Any (call-with-values (thunk (eval sexp)) (λ _ (car _))))
                          (if (bitmap%? icon) icon the-invalid-image))]
                       [else the-invalid-image]))])
       (λ [_ image]
         (cond [(bitmap%? image) (normalize image)]
               [(css-image-datum? image) (normalize (image->bitmap image))]
               [else css:initial])))]))

(define css->bitmap (make-css->bitmap))
