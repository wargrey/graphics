#lang racket

(require rackunit)

(provide (all-defined-out))

(struct validation {result cpu real gc})

(define /dev/null (open-output-nowhere '/dev/null #true))
(define handbook (make-hash))

(define indents (make-hash))
(define ~indent
  {lambda [count]
    (unless (hash-has-key? indents count)
      (hash-set! indents count (make-string (* count 2) #\space)))
    (hash-ref indents count)})

(define tamer-record-handbook
  {lambda [name action]
    (unless (hash-has-key? handbook name)
      (define restored #false)
      (define stderr (open-output-bytes 'rescue))
      (define atexit {lambda [status] (let* ([errsize (file-position stderr)]
                                             [errmsg (cond [(positive? errsize) (get-output-string stderr)]
                                                           [else (format "Racket exits with status ~a!" status)])])
                                        (set! restored (run-test-case name {thunk (fail errmsg)})))})
      (define-values {results cpu real gc}
        (time-apply {thunk (parameterize ([current-output-port /dev/null]
                                          [current-error-port stderr]
                                          [exit-handler atexit])
                             (run-test-case name action))} null))
      (hash-set! handbook name (cond [restored (validation restored 0 0 0)]
                                     [else (validation (car results) cpu real gc)]))
      (close-output-port stderr))
    (hash-ref handbook name)})

(define ~time
  {lambda [cpu real gc]
    (format "[~a wallclock ms, ~a task + ~a gc = ~a CPU]" real (- cpu gc) gc cpu)})

(define display-failure
  {lambda [result #:indent [leader-space ""]]
    (eprintf "~a» FAILURE » ~a~n" leader-space (test-result-test-case-name result))
    (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
      (eprintf "~a»» ~a: ~s~n"
               leader-space
               (check-info-name info)
               (if (symbol=? 'location (check-info-name info))
                   (cons (find-relative-path (getenv "digimon-world")
                                             (car (check-info-value info)))
                         (cdr (check-info-value info)))
                   (check-info-value info))))})

(define display-error
  {lambda [result #:indent [leader-space ""]]
    (define errobj (test-error-result result))
    (eprintf "~a» ERROR » ~a~n" leader-space (test-result-test-case-name result))
    (eprintf "~a»» ~a: ~a~n" leader-space (object-name errobj) (exn-message errobj))})

(define prove-spec
  {lambda [suite]
    (void (foldts-test-suite {lambda [testsuite name pre-action post-action seed]
                               (pre-action)
                               (printf "~a~a~n" (~indent seed) name)
                               (add1 seed)}
                             {lambda [testsuite name pre-action post-action seed children-seed]
                               (post-action)
                               (sub1 children-seed)}
                             {lambda [testcase name action seed]
                               (define validated (tamer-record-handbook name action))
                               (define result (validation-result validated))
                               (define leader-space (~indent seed))
                               (cond [(test-success? result) (printf "~a~a ~a~n" leader-space (test-result-test-case-name result)
                                                                     (~time (validation-cpu validated) (validation-real validated) (validation-gc validated)))]
                                     [(test-failure? result) (display-failure result #:indent leader-space)]
                                     [else (display-error result #:indent leader-space)])
                               seed}
                             0
                             suite))})

(define prove-basic
  {lambda [suite]
    (void (foldts-test-suite {lambda [testsuite name pre-action post-action seed]
                               (pre-action)
                               seed}
                             {lambda [testsuite name pre-action post-action seed children-seed]
                               (post-action)
                               children-seed}
                             {lambda [testcase name action count]
                               (define validated (tamer-record-handbook name action))
                               (define result (validation-result validated))
                               (printf "~a ~a - ~a~n" (if (test-error? result) "#e" (test-success? result)) count (test-result-test-case-name result))
                               (cond [(test-success? result) (printf "~a~n" (~time (validation-cpu validated) (validation-real validated) (validation-gc validated)))]
                                     [(test-failure? result) (display-failure result)]
                                     [else (display-error result)])
                               (add1 count)}
                             1
                             suite))})

(define prove-harness
  {lambda [suite]
    (define name.results (foldts-test-suite {lambda [testsuite name pre-action post-action name.results]
                                              (pre-action)
                                              (cons (or (car name.results) name) (cdr name.results))}
                                            {lambda [testsuite name pre-action post-action seed name.results]
                                              (post-action)
                                              name.results}
                                            {lambda [testcase name action name.results]
                                              (define validated (tamer-record-handbook name action))
                                              (cons (car name.results) (cons (validation-result validated) (cdr name.results)))}
                                            (cons #false null)
                                            suite))
    (define ~suite (~a #:width 62 #:pad-string "." #:limit-marker "..." (car name.results)))
    (define results (reverse (cdr name.results)))
    (cond [(andmap test-success? results) (printf "~a~a~n" ~suite #true)]
          [else (for ([result (filter-not test-success? results)])
                  (eprintf "~a~a~n" ~suite (if (test-failure? result) "#f" "#e"))
                  (cond [(test-failure? result) (display-failure result)]
                        [else (display-error result)]))])})
