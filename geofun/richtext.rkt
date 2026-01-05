#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/richtext/self.rkt"))
(provide geo-rich-text-realize geo-rich-text-realize*)
(provide geo-rich-text->plain-text geo-rich-text-match?)

(require "digitama/richtext/self.rkt")
(require "digitama/richtext/markup.rkt")
(require "digitama/richtext/realize.rkt")
(require "digitama/richtext/plain.rkt")
