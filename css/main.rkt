#lang digimon/sugar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.w3.org/Style/CSS/specs.en.html                                                   ;;;
;;;                                                                                             ;;;
;;; https://drafts.csswg.org/css-syntax                                                         ;;;
;;; https://drafts.csswg.org/css-values                                                         ;;;
;;; https://drafts.csswg.org/css-cascade                                                        ;;;
;;; https://drafts.csswg.org/selectors                                                          ;;;
;;; https://drafts.csswg.org/css-namespaces                                                     ;;;
;;; https://drafts.csswg.org/css-variables                                                      ;;;
;;; https://drafts.csswg.org/css-conditional                                                    ;;;
;;; https://drafts.csswg.org/mediaqueries                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(require/provide bitmap)
(require/provide "syntax.rkt" "racket.rkt"
                 "color.rkt" "image.rkt"
                 "font.rkt" "text-decor.rkt")

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

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [css-read read]))
  (provide (rename-out [css-read-syntax read-syntax]))
  (provide (rename-out [css-info get-info]))
  
  (require css/language-info))
