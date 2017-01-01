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
                       (syntax-parse <field-info> #:datum-literals [Color Used-Size]
                         [(p : Color #:= dv) #'[p : Color #:= dv #:~> Color+sRGB select-color]]
                         [(p : Used-Size #:= dv) #'[p : Nonnegative-Flounm #:= dv #:~> CSS-Unitless-Size select-size]]
                         [_ <field-info>]))])
       #'(define-preference preference #:as Preference #:with extra-bindings (property-definitions ...) options ...))]))

(define-predicate pen-style? Pen-Style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-font+colors-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-text-decoration-property-parsers suitcased-name)
        (css-color-property-parsers suitcased-name '(background-color)))))

(define css-simple-box-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-text-decoration-property-parsers suitcased-name)
        (css-color-property-parsers suitcased-name '(background-color border-color))
        (case suitcased-name
          [(width height padding margin) (CSS<^> (<css-size>))]
          [(border-style) (CSS<^> (<css:ident> pen-style?))]
          [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
