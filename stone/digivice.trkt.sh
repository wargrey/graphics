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

#lang typed/racket

(define show-help-and-exit : {[#:exitcode Integer] [#:errcmd (Option String)] -> Void}
  {lambda [#:exitcode [exitcode 0] #:errcmd [error-command #false]]
    (define printf0 : {String Any * -> Void} (if error-command eprintf printf))
    (define cmds : (Listof String) (map path->string (filter ((inst curry Regexp Boolean Path) regexp-match? #px".rkt$")
                                                             (directory-list "@|digivice|-tark"))))
    (define width : Natural (string-length (argmax string-length cmds)))
    (printf0 "Usage: ~a <command> [<option> ...] [<arg> ...]~n~nAll available commands:~n" (find-system-path 'run-file))
    (for ([cmd : String (in-list cmds)])
      (printf0 "  ~a ~a~n" (~a (regexp-replace #px"^(.+).rkt$" cmd "\\1") #:min-width width)
               (car ((inst call-with-values (Listof Any))
                     {lambda [] (dynamic-require (format "@|digivice|-tark/~a" cmd) (cast 'desc Symbol)
                                                 {lambda [] : Any "[Mission Description]"})} list))))
    (when error-command
      (printf0 "~n~a: Unrecognized command: ~a~n"
               (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)) error-command))
    (void (unless (zero? exitcode)
            (exit exitcode)))})

(cond [(zero? (vector-length (current-command-line-arguments))) (show-help-and-exit)]
      [(string=? (vector-ref (current-command-line-arguments) 0) "help") (show-help-and-exit)]
      [else (let ([cmd (vector-ref (current-command-line-arguments) 0)])
              (parameterize ([current-command-line-arguments (vector-drop (current-command-line-arguments) 1)]
                             [current-namespace (make-base-namespace)])
                (define cmd.rkt (path-add-suffix (build-path "@|digivice|-tark" cmd) #".rkt"))
                (cond [(file-exists? cmd.rkt) (eval `(require (submod ,cmd.rkt sakuyamon)))]
                      [else (show-help-and-exit #:exitcode 1 #:errcmd cmd)])))])
