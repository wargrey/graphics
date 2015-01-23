#lang racket

(require rackunit)

(require "../runtime.rkt")

(provide (all-defined-out))

(struct validation {result cpu real gc})
(struct summary {success failure error cpu real gc}
  #:methods gen:custom-write
  [(define write-proc
     {lambda [smry port mode]
       (define plural-suffix {lambda [count] (if (> count 1) "s" "")})
       (define-values {success failure error cpu real gc}
         (apply values (summary-success smry) (summary-failure smry) (summary-error smry)
                (map (curryr / 1000.0) (list (summary-cpu smry) (summary-real smry) (summary-gc smry)))))
       (define population (+ success failure error))
       (cond [(zero? population) (printf "No example, do not try to fool me!")]
             [else (fprintf port "Finished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)~n~a example~a, ~a failure~a, ~a error~a, ~a% Okay."
                            real (- cpu gc) gc cpu population (plural-suffix population) failure (plural-suffix failure) error (plural-suffix error)
                            (~r (/ (* success 100) population) #:precision '(= 2)))])})])
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
                                            [errmsg (cond [(positive? errsize) (get-output-string (current-error-port))]
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

(define ~time
  {lambda times
    (define-values {cpu real gc} (apply values (map (curryr / 1000.0) times)))
    (format "[~a wallclock ms (~a task + ~a gc = ~a CPU)]" real (- cpu gc) gc cpu)})

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
    (define initial-summary (summary 0 0 0 0 0 0))
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
                                           (define leader-headline (format "~a~a ~a - " (~indent (length (car seed))) (test-success? result) (caar seed)))
                                           (define leader-space (~indent (string-length leader-headline) #:times 1))
                                           (printf "~a~a~n" leader-headline (test-result-test-case-name result))
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
    (define summary≠0 (foldts-test-suite default-fdown
                                         default-fup
                                         {lambda [testcase name action seed]
                                           (define record (tamer-record-handbook testcase))
                                           (define result (validation-result record))
                                           (cons (if (test-success? result) (car seed) (cons result (car seed)))
                                                 (summary++ (cdr seed) record))}
                                         (cons null (summary 0 0 0 0 0 0))
                                         suite))
    (printf "~a~a~n" (~a #:width 62 #:pad-string "." #:limit-marker "..." (rackunit-test-suite-name suite))
            (zero? (+ (summary-failure (cdr summary≠0)) (summary-error (cdr summary≠0)))))
    (for ([result (in-list (reverse (car summary≠0)))])
      (cond [(test-failure? result) (display-failure result)]
            [(test-error? result) (display-error result)]
            [else (error "RackUnit has new test result type added!")]))
    (cdr summary≠0)})
