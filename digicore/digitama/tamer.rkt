#lang racket

(require rackunit)

(require racket/sandbox)

(require scribble/eval)
(require scribble/manual)

(require "runtime.rkt")
(require "tamer/prove.rkt")

(provide /dev/null)
(provide (all-defined-out))

(provide (all-from-out racket "runtime.rkt" rackunit))
(provide (all-from-out scribble/manual scribble/eval))

(define current-tamer-story (make-parameter #false))
(define current-tamer-zone (make-parameter #false))

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

(define tamer-run-suite
  {lambda [suite]
    (prove-harness (cond [(symbol? suite) (with-handlers ([exn? exn->test-suite]) (tamer-require suite))]
                         [(ormap (curryr apply suite null) (list test-suite? test-case?)) suite]
                         [else (with-handlers ([exn? exn->test-suite]) (run-test suite))]))})

(define tamer-prove
  {lambda suites
    (define smry (foldl summary** initial-summary (map tamer-run-suite suites)))
    (define-values {success failure error cpu real gc}
      (apply values (summary-success smry) (summary-failure smry) (summary-error smry)
             (map (curryr / 1000.0) (list (summary-cpu smry) (summary-real smry) (summary-gc smry)))))
    (define population (+ success failure error))
    (cond [(zero? population) (printf "~nNo testcase, do not try to fool me!~n")]
          [else (printf "~nTestsuite~a = ~a, Testcase~a = ~a, Failure~a = ~a, Error~a = ~a, ~a% Okay.~n~a wallclock seconds (~a task + ~a gc = ~a CPU).~n"
                        (plural-suffix (length suites)) (length suites) (plural-suffix population) population
                        (plural-suffix (+ failure error)) (+ failure error) (plural-suffix error) error
                        (~r (/ (* success 100) population) #:precision '(= 2)) real (- cpu gc) gc cpu)])
    (+ failure error)})

(define tamer-harness
  {lambda []
    (dynamic-require (current-tamer-story) #false)
    (parameterize ([current-namespace (module->namespace (current-tamer-story))])
      (apply tamer-prove (filter {lambda [val] (ormap (curryr apply val null) (list test-suite? test-case?))}
                                 (filter-map (curryr namespace-variable-value #false (const #false))
                                             (namespace-mapped-symbols)))))})

(define tamer-spec
  {lambda []
    (dynamic-require (current-tamer-story) #false)
    (parameterize ([current-namespace (module->namespace (current-tamer-story))])
      (define smry (prove-spec (make-test-suite (path->string (build-path (cadadr (current-tamer-story))))
                                                (filter test-suite? (filter-map (curryr namespace-variable-value #false (const #false))
                                                                                (namespace-mapped-symbols))))))
      (define-values {success failure error cpu real gc}
        (apply values (summary-success smry) (summary-failure smry) (summary-error smry)
               (map (curryr / 1000.0) (list (summary-cpu smry) (summary-real smry) (summary-gc smry)))))
      (define population (+ success failure error))
      (cond [(zero? population) (printf "~nNo example, do not try to fool me!~n")]
            [else (printf "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)~n~a example~a, ~a failure~a, ~a error~a, ~a% Okay.~n"
                          real (- cpu gc) gc cpu population (plural-suffix population) failure (plural-suffix failure) error (plural-suffix error)
                          (~r (/ (* success 100) population) #:precision '(= 2)))])
      (+ failure error))})

(define tamer-note
  {lambda suites
    (apply margin-note
           (for/fold ([briefs null]) ([suite (in-list suites)])
             (append briefs (cdr (foldts-test-suite default-fdown
                                                    default-fup
                                                    {lambda [testcase name action count.briefs]
                                                      (define validated (tamer-record-handbook testcase))
                                                      (define result (validation-result validated))
                                                      (define-values {cpu real gc}
                                                        (values (validation-cpu validated) (validation-real validated) (validation-gc validated)))
                                                      (define count (car count.briefs))
                                                      (cons (add1 count)
                                                            (append (cdr count.briefs)
                                                                    (cond [(test-success? result) (list (racketvalfont (format "~a " (~result result)))
                                                                                                        (racketvarfont (format "~a " count))
                                                                                                        (racketcommentfont (format "~a wallclock ms" real)))]
                                                                          [(test-failure? result) (list (racketerror (format "~a " (~result result)))
                                                                                                        (racketvarfont (format "~a " count))
                                                                                                        (racketcommentfont name))]
                                                                          [else (let ([err (test-error-result result)])
                                                                                  (list (racketerror (format "~a " (~result result)))
                                                                                        (racketvarfont (format "~a " count))
                                                                                        (racketcommentfont (with-input-from-string (exn-message err) {thunk (read-line)}))))])
                                                                    (list (linebreak))))}
                                                    (cons 1 (list (racketidfont (format "~a" suite)) (linebreak)))
                                                    (with-handlers ([exn? exn->test-suite])
                                                      (tamer-require suite)))))))})
