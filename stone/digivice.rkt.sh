#lang scribble/text

@(define digivice (vector-ref (current-command-line-arguments) 0))

#!/bin/sh

#|
D=`dirname "$0"`
F=`basename "$0"`
cd "$D"
while test -L "$F"; do
  P=`readlink "$F"`
  D=`dirname "$P"`
  F=`basename "$P"`
  cd "$D"
done

exec racket ${F} ${1+"$@|#\@|"}
|#

#lang racket/base

(require racket/format)
(require racket/path)
(require racket/list)
(require racket/vector)

(define show-help-and-exit
  {lambda [#:exitcode [exitcode 0] #:errcmd [error-command #false]]
    (define printf0 (if error-command eprintf printf))
    (define cmds (for/list ([cmd.rkt (in-directory "@|digivice|-ark" {lambda [whocares] #false})]
                            #:when (regexp-match? #px".rkt$" cmd.rkt))
                   (cons (path->string (file-name-from-path (path-replace-suffix cmd.rkt #"")))
                         (dynamic-require cmd.rkt 'desc {lambda [] "[Mission Description]"}))))
    (define width (+ (string-length (car (argmax (compose1 string-length car) cmds))) 3))
    (printf0 "Usage: ~a <command> [<option> ...] [<arg> ...]~n~nAll available commands:~n" (find-system-path 'run-file))
    (for ([help (in-list cmds)])
      (printf0 "  ~a ~a~n" (~a (car help) #:min-width width) (cdr help)))
    (when error-command (printf0 "~n~a: Unrecognized command: ~a~n"
                                 (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)) error-command))
    (unless (zero? exitcode) (exit exitcode))})

(cond [(zero? (vector-length (current-command-line-arguments))) (show-help-and-exit)]
      [(string=? (vector-ref (current-command-line-arguments) 0) "help") (show-help-and-exit)]
      [else (let ([cmd (vector-ref (current-command-line-arguments) 0)])
              (parameterize ([current-command-line-arguments (vector-drop (current-command-line-arguments) 1)]
                             [current-namespace (make-base-namespace)])
                (define cmd.rkt (path-add-suffix (build-path "@|digivice|-ark" cmd) #".rkt"))
                (cond [(file-exists? cmd.rkt) (eval `(require (submod ,cmd.rkt @|digivice|)))]
                      [else (show-help-and-exit #:exitcode 1 #:errcmd cmd)])))])
