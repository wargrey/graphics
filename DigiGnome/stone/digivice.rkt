#lang scribble/text

@(require "../digitama/runtime.rkt")

@(define digivice (getenv "current-digivice"))

#lang typed/racket

(require "@(path->string (path-replace-suffix (getenv "runtime-path") ".typed.rkt"))")

(current-digimon "@(current-digimon)")
(current-directory (digimon-digivice))

(define show-help-and-exit : {[#:exitcode Integer] [#:errcmd (Option String)] -> Void}
  {lambda [#:exitcode [exitcode 0] #:errcmd [error-command #false]]
    (define printf0 : {String Any * -> Void} (if error-command eprintf printf))
    (define cmds : (Listof String) ((inst filter-map String Path) {lambda [cmd] (cond [(regexp-match? #px"\\.rkt$" cmd) (path->string cmd)]
                                                                                      [else #false])}
                                                                  (directory-list "@|digivice|")))
    (define width : Natural (string-length (argmax string-length cmds)))
    (printf0 "Usage: @|digivice| <command> [<option> ...] [<arg> ...]~n~nAll available commands:~n")
    (for ([cmd : String (in-list cmds)])
      (printf0 "  ~a ~a~n" (~a (regexp-replace #px"^(.+).rkt$" cmd "\\1") #:min-width width)
               (car ((inst call-with-values (Listof Any))
                     {lambda [] (dynamic-require `(file ,(format "@|digivice|/~a" cmd))
                                                 (cast 'desc Symbol)
                                                 {lambda [] : Any "[Mission Description]"})}
                     list))))
    (when (string? error-command)
      (printf0 "~n@|digivice|: Unrecognized command: ~a~n" error-command))
    (void (unless (zero? exitcode)
            (exit exitcode)))})

(cond [(zero? (vector-length (current-command-line-arguments))) (show-help-and-exit)]
      [(string=? (vector-ref (current-command-line-arguments) 0) "help") (show-help-and-exit)]
      [else (let ([cmd (vector-ref (current-command-line-arguments) 0)])
              (parameterize ([current-command-line-arguments (vector-drop (current-command-line-arguments) 1)]
                             [current-namespace (make-base-namespace)])
                (define cmd.rkt (format "@|digivice|/~a.rkt" cmd))
                (cond [(file-exists? cmd.rkt) (eval `(require (submod (file ,cmd.rkt) @|digivice|)))]
                      [else (show-help-and-exit #:exitcode 1 #:errcmd cmd)])))])
