#lang at-exp racket

(require scribble/base)
(require setup/getinfo)

(provide (all-defined-out))
(provide (all-from-out racket))

(define info-ref (get-info/full (vector-ref (current-command-line-arguments) 0)))

(define smart-radiobox
  {lambda [selected stage . desc]
    @item{@(if (string=? stage selected) "" "[ ]") @bold{@|stage|}: @|desc|}})

(define smart-checkbox
  {lambda [checked target . desc]
    @item{@"["@(if (member target checked) "X" " ")@"]" @bold{@|target|}: @|desc|}})
