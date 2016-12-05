#lang typed/racket

;;; https://drafts.csswg.org/css-cascade
;;; https://drafts.csswg.org/css-values

(provide (all-defined-out))

(require "misc.rkt")
(require "digicore.rkt")
(require "../recognizer.rkt")

(require (for-syntax racket/string))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

