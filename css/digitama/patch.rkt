#lang typed/racket/base

(provide (all-defined-out))

(require/typed/provide
 racket/base
 [port-counts-lines? (-> (U Input-Port Output-Port) Boolean)]
 [set-port-next-location! (-> (U Input-Port Output-Port) (Option Positive-Integer) (Option Natural) (Option Positive-Integer) Void)])
