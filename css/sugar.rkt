#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitmap.rkt")

(require "syntax.rkt")
(require "font.rkt")
(require "text-decor.rkt")
(require "color.rkt")
(require "image.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-type Font+Color (Pairof Font Color))
(define-type Font+Colors (Pairof Font (Pairof Color Color)))

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
    [(_ declared-values inherited-values #:with [box-size size-ref color-ref icon-ref local-ref] sexp ...)
     (with-syntax ([___ (datum->syntax #'local-ref '...)])
       #'(parameterize ([current-css-element-color (css-color-ref declared-values inherited-values)]
                        [default-css-font (css-extract-font declared-values inherited-values)])
           (define box-size : (->* (Nonnegative-Flonum Nonnegative-Flonum) (Nonnegative-Flonum Nonnegative-Flonum)
                                   (Values Nonnegative-Flonum Nonnegative-Flonum))
             (lambda [initial-width initial-height [width% (flcss%-vw length%)] [height% (flcss%-vh length%)]]
               (css-box-size declared-values inherited-values initial-width initial-height width% height%)))
           (define size-ref : (case-> [Symbol Nonnegative-Flonum -> (CSS-Maybe Nonnegative-Flonum)]
                                      [Symbol Nonnegative-Flonum Nonnegative-Flonum -> Nonnegative-Flonum])
             (case-lambda [(property 100%) (css-size-ref declared-values inherited-values property 100%)]
                          [(property 100% defsize) (css-size-ref declared-values inherited-values property 100% defsize)]))
           (define icon-ref : (-> Symbol CSS-Make-Icon Nonnegative-Real Color+sRGB Bitmap)
             (lambda [property default-icon height color]
               (css-icon-ref declared-values inherited-values property default-icon height color)))
           (define color-ref : (-> Symbol (CSS-Maybe Color)) (lambda [property] (css-color-ref declared-values inherited-values property)))
           (define-syntax (local-ref stx) (syntax-case stx [] [(_ argl ___) #'(css-ref declared-values inherited-values argl ___)]))
           sexp ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-box-size : (->* (CSS-Values (Option CSS-Values) Nonnegative-Flonum Nonnegative-Flonum)
                            (Nonnegative-Flonum Nonnegative-Flonum)
                            (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [declares inherits initial-width initial-height [width% (flcss%-vw length%)] [height% (flcss%-vh length%)]]
    (values (css-ref declares inherits 'width (make-css->size initial-width #:100% width%))
            (css-ref declares inherits 'height (make-css->size initial-height #:100% height%)))))

(define css-size-ref : (case-> [CSS-Values (Option CSS-Values) Symbol Nonnegative-Flonum -> (CSS-Maybe Nonnegative-Flonum)]
                               [CSS-Values (Option CSS-Values) Symbol Nonnegative-Flonum Nonnegative-Flonum -> Nonnegative-Flonum])
  (lambda [declares inherits property 100% [defsize css:initial]]
    (css-ref declares inherits property (make-css->size defsize #:100% 100%))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-font+colors-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-parsers suitcased-name deprecated!)
        (css-color-property-parsers suitcased-name null))))

(define css-font-filter : (CSS-Cascaded-Value-Filter Font)
  (lambda [declared-values inherited-values]
    (css-extract-font declared-values inherited-values)))

(define css-colors-filter : (CSS-Cascaded-Value-Filter (Pairof Color Color))
  (lambda [declared-values inherited-values]
    (cons (css-color-ref declared-values inherited-values)
          (let ([bgcolor (css-color-ref declared-values inherited-values 'background-color)])
            (if (css-wide-keyword? bgcolor) (select-color 'transparent) bgcolor)))))

(define css-font+color-filter : (CSS-Cascaded-Value-Filter Font+Color)
  (lambda [declared-values inherited-values]
    (cons (css-font-filter declared-values inherited-values)
          (css-color-ref declared-values inherited-values))))

(define css-font+colors-filter : (CSS-Cascaded-Value-Filter Font+Colors)
  (lambda [declared-values inherited-values]
    (cons (css-font-filter declared-values inherited-values)
          (css-colors-filter declared-values inherited-values))))
