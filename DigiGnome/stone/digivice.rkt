#lang scribble/text

@(require "../digitama/digicore.rkt")

@(define digivice (getenv "current-digivice"))

#lang typed/racket

(require "@(path->string (path-replace-suffix (getenv "digicore.rkt") ".typed.rkt"))")

(define show-help-and-exit : {[#:erract (Option String)] -> Void}
  {lambda [#:erract [error-action #false]]
    (define printf0 : {String Any * -> Void} (if error-action eprintf printf))
    (define acts : (Listof String) (#{filter-map @|#\@| String Path}
                                    {λ [act] (and (regexp-match? #px"\\.rkt$" act) (path->string act))}
                                    (directory-list "@|digivice|")))
    (define width : Natural (string-length (argmax string-length acts)))
    (printf0 "Usage: @|digivice| <action> [<option> ...] [<arg> ...]~n~nwhere <action> is one of~n")
    (for ([act : String (in-list acts)])
      (printf0 "  ~a ~a~n" (~a (regexp-replace #px"^(.+).rkt$" act "\\1") #:min-width width)
               (car (#{call-with-values @|#\@| (Listof Any)}
                     {λ _ (dynamic-require `(file ,(format "@|digivice|/~a" act)) 'desc {λ _ "[Missing Description]"})}
                     list))))
    (when (string? error-action)
      (printf0 "~n")
      (raise-user-error '@|digivice| "Unrecognized action: ~a" error-action))})

(define main : Racket-Main
  {lambda arglist
    (call-as-normal-termination
     {λ _ (parameterize* ([current-digimon "@(current-digimon)"]
                          [current-directory (digimon-digivice)])
            (cond [(or (null? arglist) (string=? "help" (car arglist))) (show-help-and-exit)]
                  [else (parameterize ([current-command-line-arguments (list->vector (cdr arglist))]
                                       [current-namespace (make-base-namespace)])
                          (define act.rkt : Path-String (format "@|digivice|/~a.rkt" (car arglist)))
                          (if (file-exists? act.rkt)
                              (call-with-values {λ _ (eval `(require (submod (file ,act.rkt) @|digivice|)))} void)
                              (show-help-and-exit #:erract (car arglist))))]))})})

;;; `raco setup` makes it hard to set --main option when making launcher
(apply main (vector->list (current-command-line-arguments)))
