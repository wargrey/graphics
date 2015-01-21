#lang racket

(require rackunit)

(require racket/sandbox)

(require scribble/eval)
(require scribble/manual)

(require "runtime-path.rkt")

(provide (all-defined-out))

(provide (all-from-out racket "runtime-path.rkt" rackunit))
(provide (all-from-out scribble/manual scribble/eval))

(define current-tamer-story (make-parameter #false))
(define current-tamer-zone (make-parameter #false))

(define /dev/null (open-output-nowhere '/dev/null #true))
(define handbook (make-hash))

(define indents (make-hash))
(define ~indent
  {lambda [count]
    (unless (hash-has-key? indents count)
      (hash-set! indents count (make-string (* count 2) #\space)))
    (hash-ref indents count)})

(define-syntax {tamer-action stx}
  (syntax-case stx []
    [{_ s-exps ...} (syntax/loc stx (interaction #:eval (current-tamer-zone) s-exps ...))]))

(define-syntax {tamer-require stx}
  (syntax-case stx []
    [{_ binding} #'(dynamic-require/expose (current-tamer-story) binding)]
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

(define tamer-record-handbook
  {lambda [name action]
    (unless (hash-has-key? handbook name)
      (define restored #false)
      (define-values {results cpu real gc}
        (time-apply {thunk (parameterize ([exit-handler {lambda [status] (let* ([errmsg (format "Racket exits with status ~a!" status)])
                                                                           (set! restored (run-test-case name {thunk (fail errmsg)})))}]
                                          [current-output-port /dev/null]
                                          [current-error-port /dev/null])
                             (run-test-case name action))} null))
      (hash-set! handbook name (cond [restored (list restored 0 0 0)]
                                     [else (list (car results) cpu real gc)])))
    (hash-ref handbook name)})

(define tamer-script
  {lambda suites
    (for ([suite (in-list suites)])
      (define ~suite (~a #:width 62 #:pad-string "." #:limit-marker "..." suite))
      (define results (reverse (fold-test-results #:run tamer-record-handbook
                                                  {lambda [results seed] (cons (car results) seed)}
                                                  null
                                                  (tamer-require suite))))
      (cond [(andmap test-success? results) (printf "~a~a" ~suite #true)]
            [else (for ([result (filter-not test-success? results)])
                    (define name (test-result-test-case-name result))
                    (cond [(test-failure? result) (let ([infos (exn:test:check-stack (test-failure-result result))])
                                                    (eprintf "~a~a~n" ~suite #false)
                                                    (eprintf "» FAILURE: ~a~n" name)
                                                    (for ([info (in-list infos)])
                                                      (eprintf "»» ~a: ~s~n"
                                                               (check-info-name info)
                                                               (if (symbol=? 'location (check-info-name info))
                                                                   (cons (find-relative-path (getenv "digimon-world")
                                                                                             (car (check-info-value info)))
                                                                         (cdr (check-info-value info)))
                                                                   (check-info-value info)))))]
                          [else (let ([err (test-error-result result)])
                                  (printf "~a~a~n" ~suite "#e")
                                  (eprintf "» ERROR: ~a~n" name)
                                  (eprintf "»» ~a: ~a~n"
                                           (object-name err)
                                           (exn-message err)))]))]))})

(define tamer-spec
  {lambda []
    (dynamic-require (current-tamer-story) #false)
    (parameterize ([current-namespace (module->namespace (current-tamer-story))])
      (void (fold-test-results #:run tamer-record-handbook
                               #:fdown {lambda [name seed] (printf "~a~a~n" (~indent seed) name) (add1 seed)}
                               #:fup {lambda [name seed] (sub1 seed)}
                               {lambda [results seed]
                                 (define result (car results))
                                 (define name (test-result-test-case-name result))
                                 (cond [(test-success? result) (let-values ([{cpu real gc} (apply values (cdr results))])
                                                                 (printf "~a~a [~a wallclock ms, ~a task + ~a gc = ~a CPU]~n" (~indent seed)
                                                                         name real (- cpu gc) gc cpu))]
                                       [(test-failure? result) (let ([infos (exn:test:check-stack (test-failure-result result))])
                                                                 (printf "~a~a~n" (~indent seed) name)
                                                                 (eprintf "~a» FAILURE~n" (~indent seed))
                                                                 (for ([info (in-list infos)])
                                                                   (eprintf (string-append (~indent seed) "»» ~a: ~s~n")
                                                                            (check-info-name info)
                                                                            (if (symbol=? 'location (check-info-name info))
                                                                                (cons (find-relative-path (getenv "digimon-world")
                                                                                                          (car (check-info-value info)))
                                                                                      (cdr (check-info-value info)))
                                                                                (check-info-value info)))))]
                                       [else (let ([err (test-error-result result)])
                                               (printf "~a~a~n" (~indent seed) name)
                                               (eprintf "~a» ERROR~n" (~indent seed))
                                               (eprintf "~a»» ~a: ~a~n" (~indent seed)
                                                        (object-name err)
                                                        (exn-message err)))])
                                 seed}
                               0
                               (make-test-suite (path->string (build-path (cadadr (current-tamer-story))))
                                                (filter test-suite?
                                                        (filter-map {lambda [var] (namespace-variable-value var #false {lambda [] #false})}
                                                                    (namespace-mapped-symbols)))))))})

(define tamer-note
  {lambda suites
    (apply margin-note
           (for/fold ([briefs null]) ([suite (in-list suites)])
             (append briefs (with-handlers ([exn? {lambda [e] (list (racketerror (exn-message e)))}])
                              (car (fold-test-results #:run tamer-record-handbook
                                                      {lambda [results seed]
                                                        (define result (car results))
                                                        (define-values {cpu real gc} (apply values (cdr results)))
                                                        (define name (test-result-test-case-name result))
                                                        (define count (cdr seed))
                                                        (cons (append (car seed)
                                                                      (cond [(test-success? result) (list (racketvalfont (format "~a " #true))
                                                                                                          (racketvarfont (format "~a " count))
                                                                                                          (racketcommentfont (format "[~ams, ~ams]" cpu real)))]
                                                                            [(test-failure? result) (list (racketerror (format "~a " #false))
                                                                                                          (racketvarfont (format "~a " count))
                                                                                                          (racketcommentfont name))]
                                                                            [else (let ([err (test-error-result result)])
                                                                                    (list (racketerror (format "#e "))
                                                                                          (racketvarfont (format "~a " count))
                                                                                          (racketcommentfont (exn-message err))))])
                                                                      (list (linebreak)))
                                                              (add1 count))}
                                                      (cons (list (racketidfont (format "~a" suite)) (linebreak)) 1)
                                                      (tamer-require suite)))))))})

(define stdpipe->cons
  {lambda [routine]
    (let ([stdout (open-output-string 'stdout)]
          [stderr (open-output-string 'stderr)])
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [exit-handler void])
        (routine))
      (cons (get-output-string stdout)
            (get-output-string stderr)))})

(define-binary-check {check-$? routine expected}
  (let ([status +NaN.0])
    (parameterize ([exit-handler {lambda [retcode] (set! status retcode)}]
                   [current-output-port /dev/null]
                   [current-error-port /dev/null])
      (routine))
    (check-eq? status expected (cond [(nan? status) "routine does not exit!"]
                                     [else (format "routine exit with ~a rather than ~a!"
                                                   status expected)]))))

(define test-$?
  {lambda [name routine expected]
    (test-case name (check-$? routine expected))})
