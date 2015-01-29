#lang racket

(require rackunit)

(require "../runtime.rkt")

(provide (all-defined-out))

(struct tamer-seed {datum brief name-path})
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
  {lambda [name:case«suites action]
    (define short-name (car name:case«suites))
    (define long-name (string-join name:case«suites " « "))
    (unless (hash-has-key? handbook long-name)
      (define-values {results cpu real gc}
        (time-apply {λ _ (call-with-escape-continuation
                          {λ [return] (parameterize ([current-output-port /dev/null]
                                                     [current-error-port (open-output-string 'stderr)]
                                                     [exit-handler {λ [status] ;;; Todo: `raco test` doesnt need too much time
                                                                     (let* ([errsize (file-position (current-error-port))]
                                                                            [errmsg (cond [(positive? errsize) (string-trim (get-output-string (current-error-port)))]
                                                                                          [else (format "Racket exits with status ~a!" status)])])
                                                                       (return (run-test-case short-name {λ _ (fail errmsg)})))}])
                                        (return (run-test-case short-name action)))})} null))
        (hash-set! handbook long-name (validation (car results) cpu real gc)))
    (hash-ref handbook long-name)})

(define indents (make-hash))
(define ~indent
  {lambda [count #:times [times 2] #:padchar [padding #\space]]
    (define key (format "~a~a" count padding))
    (unless (hash-has-key? indents key)
      (hash-set! indents key (make-string (* count times) padding)))
    (hash-ref indents key)})

(define plural-suffix
  {lambda [count]
    (if (> count 1) "s" "")})

(define ~time
  {lambda times
    (define-values {cpu real gc} (apply values (map (curryr / 1000.0) times)))
    (format "[~a wallclock seconds (~a task + ~a gc = ~a CPU)]" real (- cpu gc) gc cpu)})

(define ~result
  {lambda [result]
    (define indicators (hash (object-name struct:test-error) "#?"
                             (object-name struct:test-success) "#t"
                             (object-name struct:test-failure) "#f"))
    (hash-ref indicators (object-name result))})

(define exn->test-suite
  {lambda [e]
    (test-suite (format "Maybe (⧴ ~a)" (object-name e))
                (test-case (symbol->string (object-name e))
                           ;;; We just avoid `thunk` to make a test-error object.
                           (raise-user-error (exn-message e))))})

(define display-failure
  {lambda [result #:indent [headspace ""]]
    (eechof #:fgcolor 'red "~a⧴ FAILURE » ~a~n" headspace (test-result-test-case-name result))
    (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
      (eechof #:fgcolor 'red "~a»» ~a: ~s~n" headspace (check-info-name info)
              (if (symbol=? 'location (check-info-name info))
                  (srcloc->string (apply srcloc (check-info-value info)))
                  (check-info-value info))))})

(define display-error
  {lambda [result #:indent [headspace0 ""]]
    (define errobj (test-error-result result))
    (define messages (call-with-input-string (string-replace (exn-message errobj) (getenv "digimon-world") "") port->lines))
    (eechof #:fgcolor 'red #:attributes '{inverse} "~a⧴ FATAL » ~a~n" headspace0 (test-result-test-case-name result))
    (eechof #:fgcolor 'red #:attributes '{inverse} "~a»» name: ~a~n" headspace0 (object-name errobj))
    (unless (null? messages)
      (define msghead " message: ")
      (define msgspace (~indent (string-length msghead) #:times 1))
      (eechof #:fgcolor 'red #:attributes '{inverse} "~a»»~a~a~n" headspace0 msghead (car messages))
      (for-each (curry eechof #:fgcolor 'red #:attributes '{inverse} "~a»»~a~a~n" headspace0 msgspace) (cdr messages)))
    (for ([stack (in-list (continuation-mark-set->context (exn-continuation-marks errobj)))])
      (when (cdr stack)
        (define srcinfo (srcloc->string (cdr stack)))
        (unless (or (false? srcinfo) (absolute-path? srcinfo))
          (eechof #:fgcolor 245 "~a»»»» ~a: ~a~n" headspace0
                  (string-replace srcinfo (getenv "digimon-world") "")
                  (or (car stack) 'λ)))))})

(define default-fseed
  {lambda last-is-seed
    (last last-is-seed)})
  
(define fold-test-suite
  {lambda [seed:datum testsuite #:fdown [fdown default-fseed] #:fup [fup default-fseed] #:fhere [fhere default-fseed]]
    (define-values {seed-box cpu real gc}
      (parameterize ([current-custodian (make-custodian)])
        (time-apply foldts-test-suite (list {λ [testsuite name pre-action post-action seed]
                                              (pre-action)
                                              (tamer-seed (fdown name (tamer-seed-datum seed))
                                                          (tamer-seed-brief seed )
                                                          (cons name (tamer-seed-name-path seed)))}
                                            {λ [testsuite name pre-action post-action seed children-seed]
                                              (post-action)
                                              (tamer-seed (fup name (tamer-seed-datum seed) (tamer-seed-datum children-seed))
                                                          (tamer-seed-brief children-seed)
                                                          (tamer-seed-name-path seed))}
                                            {λ [testcase name action0 seed]
                                              (define-values {name-path action}
                                                (if (false? name)
                                                    (values (cons (format "Maybe (⧴ ~a)" (object-name exn:fail:user))
                                                                  (tamer-seed-name-path seed))
                                                            {λ _ (raise-user-error "Test case must have a name")})
                                                    (values (cons name (tamer-seed-name-path seed)) action0)))
                                              (define record (tamer-record-handbook name-path action))
                                              (tamer-seed (fhere record (tamer-seed-datum seed))
                                                          (summary++ (tamer-seed-brief seed) record)
                                                          name-path)}
                                            (tamer-seed seed:datum initial-summary null)
                                            testsuite))))
    (values (tamer-seed-datum (car seed-box))
            (tamer-seed-brief (car seed-box))
            cpu real gc)})

(define prove-spec
  {lambda [suite]
    (define-values {whocares brief cpu real gc}
      (fold-test-suite #:fdown {λ [name seed:ordered]
                                 (if (null? seed:ordered)
                                     (echof #:fgcolor 202 #:attributes '{underline} "λ ~a~a~n" (~indent (length seed:ordered)) name)
                                     (echof "~aλ~a ~a~n" (~indent (length seed:ordered)) (string-join (map number->string (reverse seed:ordered)) ".") name))
                                 (cons 1 seed:ordered)}
                       #:fup {λ [name seed:ordered children:ordered]
                               (cond [(null? seed:ordered) null]
                                     [else (cons (add1 (car seed:ordered))
                                                 (cdr seed:ordered))])}
                       #:fhere {λ [record seed:ordered]
                                 (define result (validation-result record))
                                 (define headline (format "~a~a ~a - " (~indent (length seed:ordered)) (~result result) (car seed:ordered)))
                                 (define headspace (~indent (string-length headline) #:times 1))
                                 (cond [(test-success? result) (void (echof #:fgcolor 'green "~a~a" headline (test-result-test-case-name result))
                                                                     (echof #:fgcolor 245 " [~ams]~n" (validation-real record)))]
                                       [(test-failure? result) (void (echof #:fgcolor 'lightred "~a~a~n" headline (test-result-test-case-name result))
                                                                     (display-failure result #:indent headspace))]
                                       [(test-error? result) (void (echof #:fgcolor 'red "~a~a~n" headline (test-result-test-case-name result))
                                                                   (display-error result #:indent headspace))]
                                       [else (error "RackUnit has new test result type added!")])
                                 (cons (add1 (car seed:ordered)) (cdr seed:ordered))}
                       null
                       suite))
    (values brief cpu real gc)})

(define prove-harness
  {lambda [suite]
    (define-values {$?≠0 brief cpu real gc}
      (fold-test-suite #:fhere {λ [record seed:$?≠0]
                                 (define result (validation-result record))
                                 (cond [(test-success? result) seed:$?≠0]
                                       [else (cons result seed:$?≠0)])}
                       null
                       suite))
    (echof "~a" (~a #:width 64 #:pad-string "." #:limit-marker "..." (rackunit-test-suite-name suite)))
    (cond [(positive? (summary-error brief)) (echof #:fgcolor 'red "~a~n" (~result struct:test-error))]
          [(positive? (summary-failure brief)) (echof #:fgcolor 'lightred "~a~n" (~result struct:test-failure))]
          [(positive? (summary-success brief)) (echof #:fgcolor 'green "~a~n" (~result struct:test-success))]
          [else #| No testcase |# (echof #:fgcolor 'lightgreen "~a~n" #true)])
    (for ([result (in-list (reverse $?≠0))])
      (cond [(test-failure? result) (display-failure result)]
            [(test-error? result) (display-error result)]
            [else (error "RackUnit has new test result type added!")]))
    (when (null? $?≠0) (echof #:fgcolor 245 "~a~n" (~time (summary-cpu brief) (summary-real brief) (summary-gc brief))))
    (values brief cpu real gc)})
