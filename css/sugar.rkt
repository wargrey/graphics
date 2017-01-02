#lang digimon/sugar

(provide (all-defined-out))

(require "digitama/bitmap.rkt")

(require "syntax.rkt")
(require "color.rkt")
(require "font.rkt")
(require "text-decor.rkt")

(define-syntax (define-preference* stx)
  (syntax-parse stx #:literals [:]
    [(self preference #:as Preference (fields ...) options ...)
     #'(self preference #:as Preference #:with [] (fields ...) options ...)]
    [(_ preference #:as Preference #:with extra-bindings (field-info ...) options ...)
     (with-syntax* ([(property-definitions ...)
                     (for/list ([<field-info> (in-list (syntax->list #'(field-info ...)))])
                       (syntax-parse <field-info> #:datum-literals [Color Unitless-Size]
                         [(p : Color #:= dv) #'[p : Color #:= dv #:~> Color+sRGB select-color]]
                         [(p : Unitless-Size #:= dv) #'[p : Nonnegative-Flonum #:= dv #:~> (U Nonnegative-Flonum Negative-Single-Flonum)
                                                          select-size]]
                         [(p : Unitless-Size #:= dv nv) #'[p : Nonnegative-Flonum #:= dv #:~> (U Nonnegative-Flonum Negative-Single-Flonum)
                                                             (λ [[v : (U Nonnegative-Flonum Negative-Single-Flonum)]]
                                                               (select-size v nv))]]
                         [_ <field-info>]))])
       #'(define-preference preference #:as Preference #:with extra-bindings (property-definitions ...) options ...))]))

(define-syntax (call-with-css-box stx)
  (syntax-parse stx
    [(_ declared-values inherited-values sexp ...)
     #'(call-with-css-box declared-values inherited-values #:with () sexp ...)]
    [(_ declared-values inherited-values #:with (extra-parameters ...) sexp ...)
     #'(parameterize ([current-css-element-color (css-color-ref declared-values inherited-values)]
                      [default-css-font (css-extract-font declared-values inherited-values)]
                      extra-parameters ...)
         sexp ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-font+colors-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-text-decoration-property-parsers suitcased-name)
        (css-color-property-parsers suitcased-name '(background-color)))))

(define css-font-filter : (CSS-Cascaded-Value-Filter Font)
  (lambda [declared-values inherited-values]
    (css-extract-font declared-values inherited-values)))

(define css-colors-filter : (CSS-Cascaded-Value-Filter (Pairof Color Color))
  (lambda [declared-values inherited-values]
    (cons (css-color-ref declared-values inherited-values)
          (let ([bgcolor (css-color-ref declared-values inherited-values 'background-color)])
            (if (css-wide-keyword? bgcolor) (select-color 'transparent) bgcolor)))))

(define css-font+color-filter : (CSS-Cascaded-Value-Filter (Pairof Font Color))
  (lambda [declared-values inherited-values]
    (cons (css-font-filter declared-values inherited-values)
          (css-color-ref declared-values inherited-values))))

(define css-font+colors-filter : (CSS-Cascaded-Value-Filter (Pairof Font (Pairof Color Color)))
  (lambda [declared-values inherited-values]
    (cons (css-font-filter declared-values inherited-values)
          (css-colors-filter declared-values inherited-values))))
