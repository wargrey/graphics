#lang typed/racket

(require racket/provide)
(provide (all-defined-out))
(provide (all-from-out bitmap/font))
(provide (rename-out [<line-height> <css-line-height>]) <css-system-font> css->line-height current-css-element-font)
(provide (matching-identifiers-out #px"^default-css-[a-zA-Z0-9-]+$" (all-from-out "digitama/font.rkt")))

(require bitmap/font)

(require "digitama/syntax/digicore.rkt")
(require "digitama/font.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/text-decor.rkt")
(require "recognizer.rkt")
(require "text-decor.rkt")
(require "values.rkt")

(require (for-syntax syntax/parse))

(define css-normal-line-height : (Parameterof Nonnegative-Flonum) (make-parameter 1.2))

; TODO: Is this a good design? How about using (default-css-font) directly?
(define current-css-element-font : (-> Font) (λ [] (unbox &font)))

(define css-font-property-parsers : (-> Symbol (Option CSS-Declaration-Parser))
  ;;; https://drafts.csswg.org/css-fonts/#basic-font-props
  ;;; https://drafts.csswg.org/css-fonts-4/#basic-font-props
  ;;; https://drafts.csswg.org/css-fonts-4/#expanded-font-weight-scale
  (lambda [suitcased-name]
    (case suitcased-name
      [(font) <:font-shorthand:>]
      [(font-family) <:font-family:>]
      [(font-style) (<css-keyword> css-font-style-option)]
      [(font-kerning) (<css-keyword> css-font-kerning-option)]
      [(font-variant-ligatures) (<css-keyword> css-font-variant-ligatures-options)]
      [(font-variant-position) (<css-keyword> css-font-position-option)]
      [(font-variant-caps) (<css-keyword> css-font-caps-option)]
      [(line-height) (<line-height>)]
      [(font-synthesis) (<:css-keywords:> css-font-synthesis-options 'none)]
      [(font-size min-font-size max-font-size) (<font-size>)]
      [(font-stretch) (<font-stretch>)]
      [(font-weight) (<font-weight>)]
      [(font-size-adjust) (CSS:<+> (<css-keyword> 'none) (<css+real>))]
      [else #false])))

(define css-extract-font : (->* (CSS-Values (Option CSS-Values)) (Font) Font)
  (lambda [declared-values inherited-values [basefont (default-css-font)]]
    (define ?font : Any (and inherited-values (css-ref inherited-values #false 'font)))
    (define inherited-font : Font (if (css-font%? ?font) ?font basefont))
    (define rem? : Boolean (positive? (css-rem)))

    (let call-with-inherited-font ()
      (when (or (not (eq? inherited-font (unbox &font))) (not rem? #| for initializing metrics of root element |#))
        (css-font-relative-lengths (send inherited-font get-metrics))
        (when (not rem?)
          (css-rem (css-em))
          (css-rlh (select-size +nan.0)))
        (set-box! &font inherited-font)))

    (define min-size : Nonnegative-Real (css-ref declared-values #true 'min-font-size css->font-size))
    (define max-size : Nonnegative-Real (css-ref declared-values #true 'max-font-size css->font-size))
    (define font-size : Nonnegative-Real (css-ref declared-values #true 'font-size css->font-size))
    (define lines : (Listof Symbol) (css-ref declared-values inherited-values 'text-decoration-line css->text-decor-lines))
    (define line-style : Symbol (css-ref declared-values inherited-values 'text-decoration-style symbol? 'solid))
    (define font : Font
      ;;; WARNING
      ;; Do not pass inherited-font here for properties that racket font% already set, since the inheriting is already done
      ;; as the `#true`s indicate, giving properties a chance to use their initial value if they are desired to. Also see (css-ref).
      (make-css-font #:size (max (min max-size font-size) min-size)
                     #:family (css-ref declared-values #true 'font-family css->font-family)
                     #:style (css-ref declared-values #true 'font-style symbol? 'normal (send inherited-font get-style))
                     #:weight (css-ref declared-values #true 'font-weight pango-font-weight? 'normal (send inherited-font get-weight))
                     #:stretch (css-ref declared-values inherited-values 'font-stretch symbol? 'normal)
                     #:lines (cond [(null? lines) lines]
                                   [else (case line-style
                                           [(solid) lines]
                                           [(double) (cons 'underdouble lines)]
                                           [else (cons 'undercurl lines)])])))

    (let call-with-current-element-font ()
      (css-set! declared-values 'font font)
      (set-box! &font font)
      (css-font-relative-lengths (send font get-metrics))
      (css-lh (select-size (css-ref declared-values inherited-values 'line-height css->line-height)))
      (when (false? rem?) ; this is the root element
        (css-rem (css-em))
        (css-rlh (css-lh)))
      font)))

(define select-size : (->* ((U Nonnegative-Flonum Negative-Single-Flonum)) (Nonnegative-Flonum) Nonnegative-Flonum)
  (lambda [computed-value [normal (css-normal-line-height)]]
    (void 'see css->line-height)
    (cond [(nan? computed-value) (fl* normal (css-em))]
          [(nonnegative-flonum? computed-value) computed-value]
          [else (fl* (real->double-flonum (- computed-value)) (css-em))])))

(define css-font-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-text-decoration-property-parsers suitcased-name))))
