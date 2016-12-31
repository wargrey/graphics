#lang typed/racket

(require racket/provide)
(provide (all-defined-out) <:font-shorthand:> <:font-family:> <css-system-font>)
(provide (matching-identifiers-out #px"(^default-css-|%?$)" (all-from-out "digitama/font.rkt")))

(require bitmap/misc)

(require "digitama/digicore.rkt")
(require "digitama/font.rkt")
(require "digitama/bitmap.rkt")
(require "recognizer.rkt")
(require "text-decor.rkt")
(require "color.rkt")

(define css-font-property-parsers : (-> Symbol (Option CSS-Declaration-Parser))
  ;;; https://drafts.csswg.org/css-fonts/#basic-font-props
  ;;; https://drafts.csswg.org/css-fonts-4/#basic-font-props
  ;;; https://drafts.csswg.org/css-fonts-4/#expanded-font-weight-scale
  (lambda [suitcased-name]
    (case suitcased-name
      [(font) <:font-shorthand:>]
      [(font-family) <:font-family:>]
      [(font-style) (CSS<^> (<css-keyword> css-font-style-option))]
      [(font-kerning) (CSS<^> (<css-keyword> css-font-kerning-option))]
      [(font-variant-ligatures) (CSS<^> (<css-keyword> css-font-variant-ligatures-options))]
      [(font-variant-position) (CSS<^> (<css-keyword> css-font-position-option))]
      [(font-variant-caps) (CSS<^> (<css-keyword> css-font-caps-option))]
      [(line-height) (CSS<^> (<line-height>))]
      [(font-synthesis) (<:css-keywords:> css-font-synthesis-options 'none)]
      [(font-size min-font-size max-font-size) (CSS<^> (<font-size>))]
      [(font-stretch) (CSS<^> (<font-stretch>))]
      [(font-weight) (CSS<^> (<font-weight>))]
      [(font-size-adjust) (CSS<^> (CSS:<+> (<css-keyword> 'none) (<css+real>)))]
      [(-racket-font-smoothing) (CSS<^> (<css:ident-norm> racket-font-smoothing?))]
      [(-racket-font-hinting) (CSS<^> (<css:ident-norm> racket-font-hinting?))]
      [else #false])))

(define css-extract-font : (->* (CSS-Values (Option CSS-Values)) (Font) Font)
  (lambda [declared-values inherited-values [basefont (default-css-font)]]
    (define ?font : CSS-Datum (and inherited-values (css-ref inherited-values #false 'font)))
    (define inherited-font : Font (if (object? ?font) (cast ?font Font) basefont))
    (define (css->font-underlined [_ : Symbol] [value : CSS-Datum]) : (Listof CSS-Datum)
      (if (list? value) value (if (send inherited-font get-underlined) (list 'underline) null)))
    (call-with-font inherited-font #:root? (false? ?font)
      (define family : (U String Font-Family) (css-ref declared-values #false 'font-family css->font-family))
      (define min-size : Nonnegative-Real (css-ref declared-values #false 'min-font-size css->font-size))
      (define max-size : Nonnegative-Real (css-ref declared-values #false 'max-font-size css->font-size))
      (define font-size : Nonnegative-Real (css-ref declared-values #false 'font-size css->font-size))
      (define style : Font-Style (css-ref declared-values #false 'font-style css->font-style))
      (define weight : Font-Weight (css-ref declared-values #false 'font-weight css->font-weight))
      (define decorations : (Listof CSS-Datum) (css-ref declared-values #false 'text-decoration-line css->font-underlined))
      (define smoothing : Font-Smoothing (css-ref declared-values #false '-racket-font-smoothing racket-font-smoothing? 'default))
      (define hinting : Font-Hinting (css-ref declared-values #false '-racket-font-hinting racket-font-hinting? 'aligned))
      (define size : Nonnegative-Real (max (min max-size font-size) min-size))
      (define font : Font
        (make-font+ #:face (and (string? family) family) #:family (and (symbol? family) family)
                    #:size size #:size-in-pixels? (implies (nan? size) #| NOTE |# 'inherited) #:hinting hinting
                    #:style style #:weight weight #:underlined? (and (memq 'underline decorations) #true) #:smoothing smoothing
                    basefont))
     (call-with-font font
       (css-set! declared-values 'font font)
       (when (nan? size) (css-set! declared-values 'font-size size))
       font))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define font-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-text-decoration-property-parsers suitcased-name))))

(define font-filter : (CSS-Cascaded-Value-Filter Font)
  (lambda [declared-values inherited-values]
    (css-extract-font declared-values inherited-values)))

(define font+color-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-text-decoration-property-parsers suitcased-name)
        (css-color-property-parsers suitcased-name '()))))

(define font+color-filter : (CSS-Cascaded-Value-Filter (Pairof Font Color))
  (lambda [declared-values inherited-values]
    (cons (css-extract-font declared-values inherited-values)
          (css-color-ref declared-values inherited-values))))
