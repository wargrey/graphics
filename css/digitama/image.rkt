#lang typed/racket

;;; https://drafts.csswg.org/css-images

(provide (all-defined-out))

(require bitmap/base)
(require bitmap/resize)
(require bitmap/misc)
(require bitmap/combiner)
(require bitmap/constructor)

(require "bitmap.rkt")
(require "digicore.rkt")
(require "font.rkt")
(require "color.rkt")
(require "misc.rkt")
(require "../recognizer.rkt")
(require "../racket.rkt")
(require "../color.rkt")

(define-type Image-Set-Option (List CSS-Image-Datum Positive-Flonum))
(define-type Image-Set-Options (Listof Image-Set-Option))
(define-type CSS-Image-Datum (U CSS-@λ CSS-Image String))

(define-predicate css-image-datum? CSS-Image-Datum)

(define-css-value css-image #:as CSS-Image ())
(define-css-value image #:as Image #:=> css-image ([content : CSS-Image-Datum] [fallback : CSS-Color-Datum]))
(define-css-value image-set #:as Image-Set #:=> css-image ([options : Image-Set-Options]))

(define-@λ-pool the-@icon-pool #:λnames #px"-(icon|logo)$"
  images/logos images/icons/arrow images/icons/control
  images/icons/file images/icons/misc images/icons/stickman
  images/icons/symbol images/icons/tool)

(define-@λ-pool the-@draw-pool #:λnames [make-font make-pen make-brush] racket/draw)

(define css-image-rendering-option : (Listof Symbol) '(auto crisp-edges pixelated))
(define css-image-fit-option : (Listof Symbol) '(fill contain cover none scale-down))

(define make-image-normalizer : (case-> [Nonnegative-Real Positive-Real (-> Bitmap) -> (-> (CSS-Maybe Bitmap) Bitmap)]
                                        [Nonnegative-Real Positive-Real -> (-> (CSS-Maybe Bitmap) (CSS-Maybe Bitmap))])
  (let ([normalize (λ [[h : Nonnegative-Real] [d : Positive-Real] [b : Bitmap]]
                     (bitmap-scale (bitmap-alter-density b d) (/ h (send b get-height))))])
    (case-lambda
      [(height density)
       (λ [[raw : (CSS-Maybe Bitmap)]]
         (if (and (bitmap%? raw) (send raw ok?)) (normalize height density raw) css:initial))]
      [(height density mk-image)
       (λ [[raw : (CSS-Maybe Bitmap)]]
         (cond [(and (bitmap%? raw) (send raw ok?)) (normalize height density raw)]
               [else (let ([alt-image : Bitmap (mk-image)])
                       (cond [(> (send alt-image get-height) height) (normalize height density alt-image)]
                             [else (bitmap-cc-superimpose (bitmap-blank height height density)
                                                          (bitmap-alter-density alt-image density))]))]))])))

(define image->bitmap : (->* (CSS-Image-Datum) (Positive-Real) Bitmap)
  (lambda [img [the-density (default-icon-backing-scale)]]
    (cond [(non-empty-string? img)
           (with-handlers ([exn? (λ [[e : exn]] (css-log-read-error e img) the-invalid-image)])
             (bitmap img the-density))]
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
             (assert (css-eval-@λ img (module->namespace 'bitmap) (flcss%-em length%)) bitmap%?))]
          [else the-invalid-image])))

(define css->normalized-image : (All (racket) (-> (-> (CSS-Maybe Bitmap) racket) (CSS->Racket racket)))
  (lambda [normalize]
    (λ [_ image]
      (cond [(bitmap%? image) (normalize image)]
            [(css-image-datum? image) (normalize (image->bitmap image))]
            [else (normalize css:initial)]))))

(define css-@draw-filter : CSS-@λ-Filter
  (lambda [λname ?λ:kw]
    (case (or ?λ:kw λname)
      [(#:size) (CSS:<+> (<css:integer> 0 < 1024) (<css:flonum> 0.0 fl< 1024.0))]
      [(#:face) (CSS:<+> (<css:string>) (<css-#false>))]
      [(#:family) (<css:ident> (make-predicate Font-Family))]
      [(#:style) (<css:ident> (make-predicate Font-Style))]
      [(#:weight) (<css:ident> (make-predicate Font-Weight))]
      [(#:underlined? #:size-in-pixels?) (<css-#boolean>)]
      [(#:smoothing) (<css:ident> racket-font-smoothing?)]
      [(#:hinting) (<css:ident> racket-font-hinting?)])))
  
(define css-@icon-filter : CSS-@λ-Filter
  (lambda [λname ?λ:kw]
    (define-css-disjoined-filter <text-icon-font> #:-> (U Font CSS-@λ)
      (<css-system-font>)
      (<racket-font>)
      (<css:@λ> the-@draw-pool css-@draw-filter '(make-font)))
    (define <:clock-pointers:> : (CSS-Parser (Listof CSS-Datum))
      (CSS<$> (CSS<&> (CSS<^> (<css:integer> 0 <= 11))
                      (CSS<$> (CSS<^> (CSS:<+> (<css:integer> 0 <= 60)
                                               (<css:flonum> 0.0 fl<= 60.0)))))))
    (case (or ?λ:kw λname)
      [(#:backing-scale) (<css+real> '#:nonzero)]
      [(#:height #:thickness #:outline) (<css+%real>)]
      [(#:material) (<css:ident> '(plastic-icon-material rubber-icon-material glass-icon-material metal-icon-material))]
      [(#:trim?) (<css-#boolean>)]
      [(text-icon) (CSS<&> (CSS<^> (<css:string>)) (CSS<$> (CSS<^> (<text-icon-font>))))]
      [(regular-polygon-icon) (CSS<&> (CSS<^> (<css-natural> '#:nonzero)) (CSS<$> (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>)))))]
      [(lock-icon) (CSS<$> (CSS<^> (<css-#boolean>)))]
      [(running-stickman-icon) (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>)))]
      [else (cond [(false? ?λ:kw) <:clock-pointers:>]
                  [else (CSS:<+> (<css:racket>) ; their is no need to evaluating racket bindings right now 
                                 (CSS:<@> (<css-color>) (λ [[c : CSS-Datum]] (css->color '_ c))))])])))

(define-css-function-filter <css-image-notation> #:-> CSS-Image
  ;;; https://drafts.csswg.org/css-images/#image-notation
  ;;; https://drafts.csswg.org/css-images/#image-set-notation
  [(image) #:=> [(image "" [fallback ? index? string? symbol? css-color?])
                 (image [content ? css-image? string? css-@λ?] [fallback ? css-basic-color-datum? css-color?])]
   (CSS<+> (CSS<^> (<css-color>)) ; NOTE: both color and url accept strings, however their domains are not intersective.
           (CSS<&> (CSS<^> (CSS:<+> (<css-image>) (<css:string>)))
                   (CSS<$> (CSS<?> [(<css-comma>) (CSS<^> (<css-color>))]) 'transparent)))]
  [(image-set) #:=> [(image-set [options ? css-image-sets?])]
   (CSS<!> (CSS<#> (CSS<!> (CSS<^> (list (CSS:<+> (<css:string>) (<css-image>)) (<css+resolution>))))))]
  #:where
  [(define-predicate css-image-sets? Image-Set-Options)])

(define-css-disjoined-filter <css-image> #:-> CSS-Image-Datum
  ;;; https://drafts.csswg.org/css-images/#image-values
  ;;; https://drafts.csswg.org/css-images/#invalid-image
  (<css:@λ> the-@icon-pool css-@icon-filter)
  (<css-image-notation>)
  (CSS:<?> (<css:url> string?) #false (λ _ "")))
