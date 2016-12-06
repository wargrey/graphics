#lang typed/racket

;;; https://drafts.csswg.org/css-images

(provide (all-defined-out))

(require "bitmap.rkt")
(require "digicore.rkt")
(require "font.rkt")
(require "color.rkt")
(require "../recognizer.rkt")
(require "../racket.rkt")

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

(define css-@draw-filter : CSS-@λ-Filter
  (lambda [λname ?λ:kw]
    (case (or ?λ:kw λname)
      [(#:size) (CSS:<+> (<css:integer> 0 < 1024) (<css:flonum> 0.0 fl< 1024.0))]
      [(#:face) (CSS:<+> (<css:string>) (<css:escape> false?))]
      [(#:family) (<css:ident> (make-predicate Font-Family))]
      [(#:style) (<css:ident> (make-predicate Font-Style))]
      [(#:weight) (<css:ident> (make-predicate Font-Weight))]
      [(#:underlined? #:size-in-pixels?) (<css:escape> boolean?)]
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
      [(#:trim?) (<css:escape> boolean?)]
      [(text-icon) (CSS<&> (CSS<^> (<css:string>)) (CSS<$> (CSS<^> (<text-icon-font>))))]
      [(regular-polygon-icon) (CSS<&> (CSS<^> (<css-natural> '#:nonzero)) (CSS<$> (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>)))))]
      [(lock-icon) (CSS<$> (CSS<^> (<css:escape> boolean?)))]
      [(running-stickman-icon) (CSS<^> (CSS:<+> (<css:integer>) (<css:flonum>)))]
      [else (if ?λ:kw (<css-color>) <:clock-pointers:>)])))

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
  ;[(css:function=:=? image-value 'image) (css-extract-image image-value (css:function-arguments image-value))]
  (<css:@λ> the-@icon-pool css-@icon-filter)
  (<css-image-notation>)
  (CSS:<?> (<css:url> string?) #false (λ _ "")))
