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

(define all+graphics-filter : (CSS-Cascaded-Value-Filter (HashTable Symbol Any))
  (lambda [declared-values inherited-values]
    (parameterize ([current-css-element-color (css-ref declared-values #false 'color)])
      (css-extract-font declared-values #false)
      (for/hash : (HashTable Symbol Any) ([desc-name (in-hash-keys (css-values-descriptors declared-values))])
        (values desc-name (css-ref declared-values inherited-values desc-name))))))

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [css-read read]))
  (provide (rename-out [css-read-syntax read-syntax]))
  (provide (rename-out [css-language-info get-language-info]))
  (provide (rename-out [css-info get-info]))
  
  (require css/language-info))
