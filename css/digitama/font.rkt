#lang typed/racket

;;; https://drafts.csswg.org/css-fonts
;;; https://drafts.csswg.org/css-fonts-4

(provide (all-defined-out))

(require bitmap/font)
(require bitmap/digitama/font)

(require "bitmap.rkt")
(require "digicore.rkt")
(require "../recognizer.rkt")
(require "../racket.rkt")

(define &font : (Boxof CSS-Font) (box (default-css-font)))

(define css-font-generic-families : (Listof Symbol)
  '(default roman swiss      decorative modern    script  system    symbol
     emoji  serif sans-serif fantasy    monospace cursive system-ui math fangsong))
  
(define css-font-synthesis-options : (Listof Symbol) '(weight style small-caps))
  
(define css-font-style-option : (Listof Symbol) '(normal italic oblique slant))
(define css-font-kerning-option : (Listof Symbol) '(auto normal none))
(define css-font-variant-ligatures-options : (Listof Symbol) '(normal none))
(define css-font-position-option : (Listof Symbol) '(normal sub super))
(define css-font-caps-option : (Listof Symbol) '(normal small-caps all-small-caps petite-caps all-petite-caps unicase titling-caps))
(define css-font-weight-option : (Listof Symbol) '(normal bold bolder light lighter))
(define css-font-size-option : (Listof Symbol) '(xx-small x-small small medium large x-large xx-large smaller larger))
(define css-font-variant-options/21 : (Listof Symbol) '(normal small-caps))
(define css-font-stretch-option : (Listof Symbol) '(normal condensed expanded ultra-condensed extra-condensed semi-condensed
                                                           ultra-expanded extra-expanded semi-expanded))
  
(define-css-prefab-filter <css-system-font> #:-> CSS-Font #:format "default-css-~a-font"
  [caption       (default-css-font)]
  [icon          (default-css-font)]
  [menu          (default-css-font)]
  [message-box   (default-css-font)]
  [small-caption (default-css-font)]
  [status-bar    (default-css-font)])

(define-css-racket-value-filter <racket-font> #:? font%? #:as Font)

(define css-font->longhand-properties : (->* (Font) ((HashTable Symbol Any)) (HashTable Symbol Any))
  (lambda [font [longhand css-longhand]]
    (let* ([longhand++ (hash-set longhand 'font-weight (send font get-weight))]
           [longhand++ (hash-set longhand++ 'font-style (send font get-style))]
           [longhand++ (hash-set longhand++ 'font-size (smart-font-size font))])
      (hash-set longhand++ 'font-family
                (list (or (send font get-face)
                          (send font get-family)))))))

(define-css-disjoint-filter <font-stretch> #:-> (U Symbol Nonnegative-Single-Flonum)
  ;;; https://drafts.csswg.org/css-fonts-4/#font-stretch-prop
  (<css-keyword> css-font-stretch-option)
  (<css:percentage> nonnegative-single-flonum?))

(define-css-disjoint-filter <font-weight> #:-> (U Symbol Integer)
  ;;; https://drafts.csswg.org/css-fonts/#font-weight-prop
  (<css-keyword> css-font-weight-option)
  (<css:integer> 0 < 1000))
  
(define-css-disjoint-filter <font-size> #:-> (U Symbol Nonnegative-Inexact-Real CSS:Length:Font)
  ;;; https://drafts.csswg.org/css-fonts/#font-size-prop
  (<css-size>)
  (<css-keyword> css-font-size-option))

(define-css-disjoint-filter <line-height> #:-> (U Symbol Nonnegative-Flonum Single-Flonum CSS:Length:Font CSS-Wide-Keyword)
  ;;; http://www.w3.org/TR/CSS2/visudet.html#propdef-line-height
  (<css-unitless-size>)
  (CSS:<~> (<css-keyword> '(normal inherit)) css-wide-keywords-filter-map))

(define <:font-family:> : (CSS-Parser (Listof Any))
  ;;; https://drafts.csswg.org/css-fonts/#font-family-prop
  ;;; https://drafts.csswg.org/css-fonts-4/#extended-generics
  (CSS<#> (CSS<+> (CSS<^> (CSS:<+> (<css:string>) (<css-keyword> css-font-generic-families)))
                  (CSS<!> (CSS<^> (<css:ident>))))))
  
(define <:font-shorthand:> : (Pairof CSS-Shorthand-Parser (Listof Symbol))
  ;;; https://drafts.csswg.org/css-fonts/#font-prop
  (cons (CSS<+> (CSS<^> (<css-system-font>) css-font->longhand-properties)
                (CSS<^> (<racket-font>) css-font->longhand-properties)
                (CSS<&> (CSS<*> (CSS<+> (CSS<_> (CSS<^> (<css-keyword> 'normal) '|Ignoring, some properties use it as defaults|))
                                        (CSS<^> (<font-weight>) 'font-weight)
                                        (CSS<^> (<css-keyword> css-font-style-option) 'font-style)
                                        (CSS<^> (<css-keyword> css-font-variant-options/21) 'font-variant)
                                        ; <font-stretch> also accepts percentage in css-fonts-4 specification,
                                        ; however it preempts the 'font-size
                                        (CSS<^> (<css-keyword> css-font-stretch-option) 'font-stretch)))
                        (CSS<^> (<font-size>) 'font-size)
                        (CSS<*> (CSS<?> [(<css-slash>) (CSS<^> (<line-height>) 'line-height)]) '?)
                        (CSS<^> <:font-family:> '(font-family))))
        '(font-style font-variant font-weight font-stretch font-size line-height font-family
                     font-size-adjust font-kerning font-language-override)))

(define css->font-family : (CSS->Racket (U String Symbol))
  (lambda [_ value]
    (let select ([families (if (list? value) value (list value))])
      (cond [(null? families) (let ([pfont (unbox &font)]) (or (send pfont get-face) (send pfont get-family)))]
            [else (let ([family (car families)])
                    (or (and (symbol? family) family)
                        (and (string? family) (face-filter family))
                        (and (list? family) (face-filter (string-trim (~a family) #px"(^[(])|([)]$)")))
                        (select (cdr families))))]))))

(define css->font-size : (CSS->Racket Nonnegative-Real)
  (lambda [property value]
    (cond [(symbol? value) (generic-font-size-map value (unbox &font) (default-css-font))]
          [(nonnegative-flonum? value) value]
          [(nonnegative-single-flonum? value) (fl* (real->double-flonum value) (css-em))]
          [(css+length? value) (css:length->scalar value #false)]
          [(eq? property 'min-font-size) 0.0]
          [(eq? property 'max-font-size) +inf.0]
          [(eq? value css:inherit) (css-em)]
          [else (generic-font-size-map 'medium (unbox &font) (default-css-font))])))

(define css->line-height : (-> Symbol Any (U Nonnegative-Flonum Negative-Single-Flonum))
  (lambda [_ value]
    (cond [(css+length? value) (css:length->scalar value #false)]
          [(nonnegative-flonum? value) value #|computed value is the used value|#]
          [(nonnegative-single-flonum? value) (fl* (real->double-flonum value) (css-em))]
          [(#|negative-|#single-flonum? value) value #|computed value is *not* the used value|#]
          [else #|'normal|# +nan.0 #|computed value is *not* the used value|#])))
