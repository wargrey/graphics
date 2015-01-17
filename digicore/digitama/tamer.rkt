#lang racket

(require rackunit)
(require rackunit/text-ui)

(require racket/sandbox)

(require scribble/eval)
(require scribble/manual)

(require "runtime-path.rkt")

(provide (all-defined-out))

(provide (all-from-out racket "runtime-path.rkt"))
(provide (all-from-out rackunit rackunit/text-ui))
(provide (all-from-out scribble/manual scribble/eval))

(define current-tamer-story (make-parameter #false))
(define current-tamer-zone (make-parameter #false))

(define /dev/null (open-output-nowhere '/dev/null #true))

(define-syntax {tamer-action stx}
  (syntax-case stx []
    [{_ s-exps ...} (syntax/loc stx (interaction #:eval (current-tamer-zone) s-exps ...))]))

(define-syntax {tamer-eval stx}
  (syntax-case stx []
    [{_ bindname} #'(dynamic-require/expose (current-tamer-story) bindname)]
    [_ #'(values curry dynamic-require/expose (current-tamer-story))]))

(define tamer-story->libpath
  {lambda [story-path]
    (path->digimon-libpath (build-path (getenv "digimon-tamer") story-path) #:submodule 'story)})

(define tamer-partner->libpath
  {lambda [partner-path]
    (path->digimon-libpath (build-path (getenv "digimon-zone") partner-path))})

(define tamer-partner->filepath
  {lambda [partner-path #:subtamer [subtamer 'same]]
    `(file ,(path->string (find-relative-path (simplify-path (build-path (getenv "digimon-tamer") subtamer))
                                              (simplify-path (build-path (getenv "digimon-zone") partner-path)))))})

(define tamer-zone
  {lambda [#:submodule [submod #false]]
    (define tamer.rkt (path->string (build-path (getenv "digimon-tamer") "tamer.rkt")))
    (define tamer-submod (if submod submod (string->symbol (regexp-replace #px".+/(.+?).rkt" (cadadr (current-tamer-story)) "\\1"))))
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string])
      ((make-eval-factory (list `(file ,tamer.rkt)
                                `(submod (file ,tamer.rkt) ,tamer-submod)
                                (current-tamer-story)))))})

(define tamer-note
  {lambda suites
    (parameterize ([current-output-port /dev/null]
                   [current-error-port /dev/null])
      (define briefs (for/fold ([briefs null]) ([suite (in-list suites)])
                       (define status (with-handlers ([exn? (const 'undef)])
                                        (run-tests (tamer-eval suite) 'quiet)))
                       (append briefs (list (racketkeywordfont (format "~a: " suite))
                                            (cond [(symbol? status) (racketcommentfont (symbol->string status))]
                                                  [(zero? status) (racketvalfont (format "~a" #true))]
                                                  [else (racketerror (format "~a" #false))])
                                            (linebreak)))))
      (apply margin-note (bold "Testsuite Briefs") (linebreak) briefs))})

(define-binary-check {check-$? routine expected}
  (let ([status +NaN.0])
    (parameterize ([exit-handler {lambda [retcode] (set! status retcode)}]
                   [current-output-port /dev/null]
                   [current-error-port /dev/null])
      (routine))
    (check-eq? status expected (cond [(nan? status) "routine does not exit!"]
                                     [else ""]))))
