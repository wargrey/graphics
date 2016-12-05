#lang typed/racket

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

(require digimon/digitama/sugar)

(require/provide bitmap)
(require/provide "syntax.rkt" "racket.rkt"
                 "color.rkt" "image.rkt"
                 "font.rkt" "text-decor.rkt")
