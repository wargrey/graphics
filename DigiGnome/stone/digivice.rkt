#lang scribble/text

@(require "../digitama/digicore.rkt")

#!/bin/sh

#|
# `raco setup` makes it hard to set other options,
# I have to keep the launch command simple.
exec racket --name "`basename $0 .rkt`" --require "$0" -- ${1+"$@|#\@|"}
|#

#lang typed/racket

(require "../digitama/digicore.rkt")

(define digivice : Symbol (#%module))

(provide main)

(define show-help-and-exit : {[#:erract (Option String)] -> Void}
  {lambda [#:erract [error-action #false]]
    (define printf0 : {String Any * -> Void} (if error-action eprintf printf))
    (define acts : (Listof String) (#{filter-map @|#\@| String Path}
                                    {λ [act] (and (regexp-match? #px"\\.rkt$" act) (path->string act))}
                                    (directory-list (symbol->string digivice))))
    (define width : Natural (string-length (argmax string-length acts)))
    (printf0 "Usage: ~a <action> [<option> ...] [<arg> ...]~n~nwhere <action> is one of~n" digivice)
    (for ([act : String (in-list acts)])
      (printf0 "  ~a ~a~n" (~a (regexp-replace #px"^(.+).rkt$" act "\\1") #:min-width width)
               (car (#{call-with-values @|#\@| (Listof Any)}
                     {λ _ (dynamic-require `(file ,(format "~a/~a" digivice act)) 'desc {λ _ "[Missing Description]"})}
                     list))))
    (when (string? error-action)
      (printf0 "~n")
      (raise-user-error digivice "Unrecognized action: ~a" error-action))})

(define main : Racket-Main
  {lambda arglist
    (call-as-normal-termination
     {λ _ (parameterize* ([current-digimon "@(current-digimon)"]
                          [current-directory (digimon-digivice)])
            (cond [(or (null? arglist) (string=? "help" (car arglist))) (show-help-and-exit)]
                  [else (parameterize ([current-command-line-arguments (list->vector (cdr arglist))]
                                       [current-namespace (make-base-namespace)])
                          (define act.rkt : Path-String (format "~a/~a.rkt" digivice (car arglist)))
                          (if (file-exists? act.rkt)
                              (call-with-values {λ _ (eval `(require (submod (file ,act.rkt) ,digivice)))} void)
                              (show-help-and-exit #:erract (car arglist))))]))})})

;;; `raco setup` makes it hard to set --main option when making launcher
(apply main (vector->list (current-command-line-arguments)))
