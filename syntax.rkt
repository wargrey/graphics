#lang typed/racket/base

;;; https://drafts.csswg.org/css-syntax

(provide (except-out (all-from-out "digitama/digicore.rkt") css-log-syntax-error))
(provide (except-out (all-from-out "digitama/grammar.rkt") css-stylesheet-placeholder))

(provide css-parse-stylesheet
         css-parse-rule
         css-parse-rules
         css-parse-declaration
         css-parse-declarations
         css-parse-component-value
         css-parse-component-values
         css-parse-component-valueses
         css-parse-media-queries
         css-parse-feature-query
         css-parse-selectors)

(provide define-css-parser-entry
         css-consume-stylesheet
         css-query-support?
         css-components->declaration)

(require "digitama/digicore.rkt")
(require "digitama/parser.rkt")
(require "digitama/grammar.rkt")
