#lang typed/racket

(require racket/provide)
(provide (all-defined-out) (rename-out [<line-height> <css-line-height>]) <css-system-font> css->line-height)
(provide (matching-identifiers-out #px"^default-css-[a-zA-Z0-9-]+$" (all-from-out "digitama/font.rkt")))

(require bitmap/font)

(require "digitama/digicore.rkt")
(require "digitama/font.rkt")
(require "digitama/bitmap.rkt")
(require "recognizer.rkt")
(require "text-decor.rkt")
(require "values.rkt")

(define css-normal-line-height : (Parameterof Nonnegative-Flonum) (make-parameter 1.2))

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

(define css-extract-font : (->* (CSS-Values (Option CSS-Values)) (Font) CSS-Font)
  (lambda [declared-values inherited-values [basefont (default-css-font)]]
    (define (css->font-underlined [_ : Symbol] [value : Any]) : (Listof Any)
      (if (list? value) value (if (send inherited-font get-underlined) (list 'underline) null)))
    (define ?font : Any (and inherited-values (css-ref inherited-values #false 'font)))
    (define inherited-font : Font (if (font%? ?font) ?font basefont))
    (call-with-font inherited-font
      (define family : (U String Symbol) (css-ref declared-values #false 'font-family css->font-family))
      (define min-size : Nonnegative-Real (css-ref declared-values #false 'min-font-size css->font-size))
      (define max-size : Nonnegative-Real (css-ref declared-values #false 'max-font-size css->font-size))
      (define font-size : Nonnegative-Real (css-ref declared-values #false 'font-size css->font-size))
      (define style : (Option Symbol) (css-ref declared-values #false 'font-style symbol? #false))
      (define weight : (Option Symbol) (css-ref declared-values #false 'font-weight symbol? #false))
      (define decorations : (Listof Any) (css-ref declared-values #false 'text-decoration-line css->font-underlined))
      (define smoothing : (Option Font-Smoothing) (css-ref declared-values #false '-racket-font-smoothing racket-font-smoothing? #false))
      (define hinting : (Option Font-Hinting) (css-ref declared-values #false '-racket-font-hinting racket-font-hinting? #false))
      (define size : Nonnegative-Real (max (min max-size font-size) min-size))
      (define ligatures : Symbol (css-ref declared-values inherited-values 'font-variant-ligatures symbol? 'normal))
      (define font : CSS-Font
        (make-font+ #:family family #:size size #:style style #:weight weight #:hinting hinting
                    #:underlined? (and (memq 'underline decorations) #true) #:smoothing smoothing
                    #:combine? (eq? ligatures 'normal)
                    inherited-font))
     (call-with-font font
       (css-set! declared-values 'font font)
       (when (nan? size) (css-set! declared-values 'font-size size))
       font))))

(define select-size : (->* ((U Nonnegative-Flonum Negative-Single-Flonum)) (Nonnegative-Flonum) Nonnegative-Flonum)
  (lambda [computed-value [normal (css-normal-line-height)]]
    (void 'see css->line-height)
    (cond [(nan? computed-value) (fl* normal ((css-lazy-font-scalar) 'em))]
          [(nonnegative-flonum? computed-value) computed-value]
          [else (fl* (real->double-flonum (- computed-value)) ((css-lazy-font-scalar) 'em))])))

(define css-font-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-text-decoration-property-parsers suitcased-name))))
