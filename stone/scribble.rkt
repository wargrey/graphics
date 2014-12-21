#lang at-exp racket/base

(require scribble/base)
(require setup/getinfo)

(provide (all-defined-out))

(define info-ref (get-info/full (current-directory)))

(define smart-radiobox
  {lambda [selected stage . desc]
    @item{@(if (string=? stage selected) "" "[ ]") @bold{@|stage|}: @|desc|}})

(define smart-checkbox
  {lambda [checked target . desc]
    @item{@"["@(if (member target checked) "X" " ")@"]" @bold{@|target|}: @|desc|}})
