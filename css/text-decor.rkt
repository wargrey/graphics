#lang typed/racket/base

(provide (all-defined-out) <:text-decoration:>)

(require "digitama/syntax/digicore.rkt")
(require "digitama/color.rkt")
(require "digitama/text-decor.rkt")
(require "recognizer.rkt")

(define css-text-decoration-property-parsers : (-> Symbol (Option CSS-Declaration-Parser))
  ;;; https://drafts.csswg.org/css-text-decor/#line-decoration
  (lambda [suitcased-name]
    (case suitcased-name
      [(text-decoration) <:text-decoration:>]
      [(text-decoration-line) (<:css-keywords:> css-text-decor-line-options 'none)]
      [(text-decoration-style) (<css-keyword> css-text-decor-style-option)]
      [(text-decoration-skip) (<:css-keywords:> css-text-decor-skip-options 'none)]
      [(text-decoration-color) (<css-color>)]
      [else #false])))
