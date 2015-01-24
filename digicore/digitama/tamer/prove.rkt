#lang racket

(require rackunit)

(require "../runtime.rkt")

(provide (all-defined-out))

(struct validation {result cpu real gc})
(struct summary {success failure error cpu real gc})
(define initial-summary (summary 0 0 0 0 0 0))
(define summary++
  {lambda [summary0 record]
    (define result (validation-result record))
    (summary (+ (summary-success summary0) (if (test-success? result) 1 0))
             (+ (summary-failure summary0) (if (test-failure? result) 1 0))
             (+ (summary-error summary0) (if (test-error? result) 1 0))
             (+ (summary-cpu summary0) (validation-cpu record))
             (+ (summary-real summary0) (validation-real record))
             (+ (summary-gc summary0) (validation-gc record)))})
(define summary**
  {lambda [summary1 summary2]
    (summary (+ (summary-success summary1) (summary-success summary2))
             (+ (summary-failure summary1) (summary-failure summary2))
             (+ (summary-error summary1) (summary-error summary2))
             (+ (summary-cpu summary1) (summary-cpu summary2))
             (+ (summary-real summary1) (summary-real summary2))
             (+ (summary-gc summary1) (summary-gc summary2)))})


(define handbook (make-hash))
(define tamer-record-handbook
  {lambda [testcase]
    (unless (hash-has-key? handbook testcase)
      (define restored #false)
      (parameterize ([current-output-port /dev/null]
                     [current-error-port (open-output-string 'stderr)]
                     [exit-handler {lambda [status]
                                     (let* ([errsize (file-position (current-error-port))]
                                            [errmsg (cond [(positive? errsize) (string-trim (get-output-string (current-error-port)))]
                                                          [else (format "Racket exits with status ~a!" status)])])
                                       (set! restored (run-test-case (rackunit-test-case-name testcase)
                                                                     {thunk (fail errmsg)})))}])
        (define-values {results cpu real gc}
          (time-apply {thunk (car (run-test testcase))} null))
        (hash-set! handbook testcase (validation (or restored (car results)) cpu real gc))))
    (hash-ref handbook testcase)})

(define indents (make-hash))
(define ~indent
  {lambda [count #:times [times 2]]
    (unless (hash-has-key? indents count)
      (hash-set! indents count (make-string (* count times) #\space)))
    (hash-ref indents count)})

(define plural-suffix
  {lambda [count]
    (if (> count 1) "s" "")})

(define ~time
  {lambda times
    (define-values {cpu real gc} (apply values (map (curryr / 1000.0) times)))
    (format "[~a wallclock seconds (~a task + ~a gc = ~a CPU)]" real (- cpu gc) gc cpu)})

(define ~result
  {lambda [result]
    (cond [(test-error? result) "#?"]
          [else (format "~a" (test-success? result))])})

(define exn->test-suite
  {lambda [e]
    (test-suite (format "»Maybe ~a«" (object-name e))
                (test-case (symbol->string (object-name e))
                           (raise-user-error (exn-message e))))})

(define display-failure
  {lambda [result #:indent [leader-space ""]]
    (eprintf "~a» FAILURE » ~a~n" leader-space (test-result-test-case-name result))
    (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
      (eprintf "~a»» ~a: ~s~n"
               leader-space
               (check-info-name info)
               (if (symbol=? 'location (check-info-name info))
                   (cons (build-path "/" (find-relative-path (getenv "digimon-world")
                                                             (car (check-info-value info))))
                         (cdr (check-info-value info)))
                   (check-info-value info))))})

(define display-error
  {lambda [result #:indent [leader-space0 ""]]
    (define errobj (test-error-result result))
    (define headline (format " ~a: " (object-name errobj)))
    (define leader-space (~indent (string-length headline) #:times 1))
    (define messages (call-with-input-string (string-replace (exn-message errobj) (getenv "digimon-world") "") port->lines))
    (eprintf "~a» FATAL » ~a~n" leader-space0 (test-result-test-case-name result))
    (unless (null? messages)
      (eprintf "~a»»~a~a~n" leader-space0 headline (car messages))
      (for-each (curry eprintf "~a»»~a~a~n" leader-space0 leader-space) (cdr messages)))})

(define default-fdown
  {lambda [testsuite name pre-action post-action seed]
    (pre-action)
    seed})
  
(define default-fup
  {lambda [testsuite name pre-action post-action seed children-seed]
    (post-action)
    children-seed})

(define prove-spec
  {lambda [suite]
    (define smry (cdr (foldts-test-suite {lambda [testsuite name pre-action post-action seed]
                                           (pre-action)
                                           (printf "~a~a~n" (~indent (length (car seed))) name)
                                           (cons (cons 1 (car seed)) (cdr seed))}
                                         {lambda [testsuite name pre-action post-action seed children-seed]
                                           (post-action)
                                           (cons (cdar children-seed) (cdr children-seed))}
                                         {lambda [testcase name action seed]
                                           (define record (tamer-record-handbook testcase))
                                           (define result (validation-result record))
                                           (define headline (format "~a~a ~a - " (~indent (length (car seed))) (~result result) (caar seed)))
                                           (define leader-space (~indent (string-length headline) #:times 1))
                                           (printf "~a~a~n" headline (test-result-test-case-name result))
                                           (cond [(test-success? result) (void)]
                                                 [(test-failure? result) (display-failure result #:indent leader-space)]
                                                 [(test-error? result) (display-error result #:indent leader-space)]
                                                 [else (error "RackUnit has new test result type added!")])
                                           (cons (cons (add1 (caar seed)) (cdar seed))
                                                 (summary++ (cdr seed) record))}
                                         (cons null initial-summary)
                                         suite)))
    (printf "~a~n" (~time (summary-cpu smry) (summary-real smry) (summary-gc smry)))
    smry})

(define prove-harness
  {lambda [suite]
    (define-values {$?=0 $?≠0 smry}
      (let*-values ([{smry} (foldts-test-suite default-fdown
                                               default-fup
                                               {lambda [testcase name action seed]
                                                 (define record (tamer-record-handbook testcase))
                                                 (define result (validation-result record))
                                                 (cons (cons result (car seed))
                                                       (summary++ (cdr seed) record))}
                                               (cons null initial-summary)
                                               suite)]
                    [{$0 $?} (partition test-success? (car smry))])
        (values $0 $? (cdr smry))))
    (printf "~a~a~n" (~a #:width 62 #:pad-string "." #:limit-marker "..." (rackunit-test-suite-name suite))
            (cond [(false? (null? $?≠0)) (~result (with-handlers ([exn:fail:contract? (const (car $?≠0))]) (car (filter test-error? $?≠0))))]
                  [(false? (null? $?=0)) (~result (car $?=0))]
                  [else #true]))
    (for ([result (in-list (reverse $?≠0))])
      (cond [(test-failure? result) (display-failure result)]
            [(test-error? result) (display-error result)]
            [else (error "RackUnit has new test result type added!")]))
    (printf "~a~n" (~time (summary-cpu smry) (summary-real smry) (summary-gc smry)))
    smry})
