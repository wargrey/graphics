#lang typed/racket

(require racket/provide)
(provide (except-out (all-defined-out) call-with-font))
(provide (all-from-out bitmap/font))
(provide (rename-out [<line-height> <css-line-height>]) <css-system-font> css->line-height current-css-element-font)
(provide (matching-identifiers-out #px"^default-css-[a-zA-Z0-9-]+$" (all-from-out "digitama/font.rkt")))

(require bitmap/font)

(require "digitama/digicore.rkt")
(require "digitama/font.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/text-decor.rkt")
(require "recognizer.rkt")
(require "text-decor.rkt")
(require "values.rkt")

(require (for-syntax syntax/parse))

(define css-normal-line-height : (Parameterof Nonnegative-Flonum) (make-parameter 1.2))

; TODO: Is this a good design? How about using (default-css-font) directly?
(define current-css-element-font : (-> CSS-Font) (λ [] (unbox &font)))

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

(define css-extract-font : (->* (CSS-Values (Option CSS-Values)) (CSS-Font) CSS-Font)
  (lambda [declared-values inherited-values [basefont (default-css-font)]]
    (define ?font : Any (and inherited-values (css-ref inherited-values #false 'font)))
    (define inherited-font : CSS-Font (if (css-font%? ?font) ?font basefont))
    (define rem? : Boolean (positive? (css-rem)))
    (call-with-font inherited-font #:root? (not rem?)
      (define min-size : Nonnegative-Real (css-ref declared-values #true 'min-font-size css->font-size))
      (define max-size : Nonnegative-Real (css-ref declared-values #true 'max-font-size css->font-size))
      (define font-size : Nonnegative-Real (css-ref declared-values #true 'font-size css->font-size))
      (define smoothing : Font-Smoothing (send inherited-font get-smoothing))
      (define hinting : Font-Hinting (send inherited-font get-hinting))
      (define font : CSS-Font
        ;;; WARNING
        ;; Do not pass inherited-font here, since the inheriting is already done as the `#true`s indicate,
        ;; give properties a chance to use their initial value if they are desired to.
        ;; Also see (css-ref)
        (make-css-font #:size (max (min max-size font-size) min-size)
                       #:family (css-ref declared-values #true 'font-family css->font-family)
                       #:style (css-ref declared-values #true 'font-style symbol? 'normal (send inherited-font get-style))
                       #:weight (css-ref declared-values #true 'font-weight symbol? 'normal (send inherited-font get-weight))
                       #:smoothing (css-ref declared-values #true '-racket-font-smoothing racket-font-smoothing? #false smoothing)
                       #:hinting (css-ref declared-values #true '-racket-font-hinting racket-font-hinting? #false hinting)
                       #:ligature (css-ref declared-values inherited-values 'font-variant-ligatures symbol? 'normal)
                       #:lines (css-ref declared-values inherited-values 'text-decoration-line css->text-decor-lines)))
     (call-with-font font #:root? #false
       (css-set! declared-values 'font font)
       (css-lh (select-size (css-ref declared-values inherited-values 'line-height css->line-height)))
       (when (false? rem?) ; this is the root element, alse see (call-with-font) 
         (css-rem (css-em))
         (css-rlh (css-lh)))
       font))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (call-with-font stx)
  (syntax-parse stx
    [(_ font #:root? root? sexp ...)
     #'(let ([last-font (unbox &font)])
         (when (or (not (eq? font last-font)) root? #| for initializing metrics for root element |#)
           (css-font-relative-lengths (send font get-metrics))
           (unless (false? root?)
             (css-rem (css-em))
             (css-rlh (select-size (css-lh))))
           (set-box! &font font))
         sexp ...)]))
