#lang scribble/base

@(require "../digitama/runtime.rkt")
@(require setup/getinfo)
@(define info-ref (get-info/full (digimon-zone)))

@title{@(info-ref 'collection)}
@(info-ref 'pkg-desc).
