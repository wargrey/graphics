#lang racket

(require rackunit)

(require racket/sandbox)

(require scribble/core)
(require scribble/eval)
(require scribble/manual)

(require "runtime.rkt")
(require "tamer/prove.rkt")

(provide /dev/null)
(provide (all-defined-out))

(provide (all-from-out racket "runtime.rkt" rackunit))
(provide (all-from-out scribble/core scribble/manual scribble/eval))

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
    (define-values {brief cpu real gc}
      (prove-harness (cond [(symbol? suite) (with-handlers ([exn? exn->test-suite]) (tamer-require suite))]
                           [(ormap (curryr apply suite null) (list test-suite? test-case?)) suite]
                           [else (with-handlers ([exn? exn->test-suite]) (run-test suite))])))
    brief})

(define tamer-prove
  {lambda suite-vars
    (define-values {brief-box cpu0 real0 gc0}
      (time-apply {λ _ (foldl summary** initial-summary (map tamer-run-suite suite-vars))} null))
    (define-values {success failure error cpu real gc}
      (apply values (summary-success (car brief-box)) (summary-failure (car brief-box)) (summary-error (car brief-box))
             (map (curryr / 1000.0) (list cpu0 real0 gc0))))
    (define population (+ success failure error))
    (cond [(zero? population) (printf "~nNo testcase, do not try to fool me!~n")]
          [else (printf "~n~a% tests successful.~nTestsuite~a = ~a, Testcase~a = ~a, Failure~a = ~a, Error~a = ~a.~n~a wallclock seconds (~a task + ~a gc = ~a CPU).~n"
                        (~r (/ (* success 100) population) #:precision '(= 2))
                        (plural-suffix (length suite-vars)) (length suite-vars) (plural-suffix population) population
                        (plural-suffix failure) failure (plural-suffix error) error real (- cpu gc) gc cpu)])
    (+ failure error)})

(define tamer-spec
  {lambda []
    (dynamic-require (current-tamer-story) #false)
    (parameterize ([current-namespace (module->namespace (current-tamer-story))])
      (define-values {brief cpu0 real0 gc0}
        (prove-spec (make-test-suite (path->string (build-path (cadadr (current-tamer-story))))
                                     (filter test-suite? (filter-map (curryr namespace-variable-value #false {λ _ #false})
                                                                     (namespace-mapped-symbols))))))
      (define-values {success failure error cpu real gc}
        (apply values (summary-success brief) (summary-failure brief) (summary-error brief)
               (map (curryr / 1000.0) (list cpu0 real0 gc0))))
      (define population (+ success failure error))
      (cond [(zero? population) (printf "~nNo example, do not try to fool me!~n")]
            [else (printf "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)~n~a example~a, ~a failure~a, ~a error~a, ~a% Okay.~n"
                          real (- cpu gc) gc cpu population (plural-suffix population) failure (plural-suffix failure) error (plural-suffix error)
                          (~r (/ (* success 100) population) #:precision '{= 2}))])
      (+ failure error))})

(define tamer-summary
  {lambda []
    (dynamic-require (current-tamer-story) #false)
    (define-values {harness-in harness-out} (make-pipe #false 'hspec-in 'hspec-out))
    (define tamer-spec (thread {λ _ (parameterize ([current-output-port harness-out]
                                                   [current-error-port harness-out]
                                                   [current-namespace (module->namespace (current-tamer-story))])
                                      (apply tamer-prove (filter {λ [val] (ormap (curryr apply val null) (list test-suite? test-case?))}
                                                                 (filter-map (curryr namespace-variable-value #false {λ _ #false})
                                                                             (namespace-mapped-symbols))))
                                      (close-output-port harness-out))}))
    (nested #:style (make-style "boxed" null)
            (apply filebox (italic (format "~a" (cadadr (current-tamer-story))))
                   (let awk ([summary? #false]) ;;; status: =0 => sucess; >0 => failure; <0 => error
                     (define line (read-line harness-in))
                     (cond [(eof-object? line) null]
                           [(regexp-match #px"^(.+?\\.{3,})(.+)$" line)
                            => {λ [pieces]
                                 (list* (racketkeywordfont (literal (list-ref pieces 1)))
                                        (let ([status (list-ref pieces 2)])
                                          ((if (string=? status (~result struct:test-success)) racketvalfont racketerror) status))
                                        (linebreak) (awk #false))}]
                           [(regexp-match #px"^⧴ (FAILURE|FATAL) » .+?\\s*$" line)
                            => {λ [whocares]
                                 (list* (racketcommentfont line) (linebreak) (awk #false))}]
                           [(regexp-match #px"^\\s*$" line)
                            => {λ [whocares]
                                 (list* (linebreak) (awk #true))}]
                           [else (cond [(false? summary?) (awk summary?)]
                                       [else (list* (racketoutput line)
                                                    (linebreak) (awk summary?))])]))))})

(define tamer-note
  {lambda suite-vars
    (define-values {hspec-in hspec-out} (make-pipe #false 'hspec-in 'hspec-out))
    (define tamer-spec (thread {λ _ (parameterize ([current-output-port hspec-out]
                                                   [current-error-port hspec-out])
                                      (for ([suite (in-list suite-vars)])
                                        (define-values {brief cpu real gc}
                                          (prove-spec (with-handlers ([exn? exn->test-suite])
                                                        (tamer-require suite))))
                                        (define-values {success failure error}
                                          (values (summary-success brief) (summary-failure brief) (summary-error brief)))
                                        (unless (zero? (+ failure error))
                                          (printf "~n~a failure~a ~a error~a~n"
                                                  failure (plural-suffix failure)
                                                  error (plural-suffix error))))
                                      (close-output-port hspec-out))}))
    (apply margin-note
           (let awk ([status 0]) ;;; status: =0 => sucess; >0 => failure; <0 => error; NaN => summary comes
             (define line (read-line hspec-in))
             (cond [(eof-object? line) null]
                   [(regexp-match #px"^(λ)\\s+(.+)" line)
                    => {λ [pieces]
                         (echof #:fgcolor 202 #:attributes '{underline} "~a~n" line)
                         (list* (racketmetafont (list-ref pieces 1) ~ (literal (list-ref pieces 2)))
                                (linebreak) (awk 0))}]
                   [(regexp-match #px"^\\s+λ(\\d+(.\\d)*)\\s+(.+?)\\s*$" line)
                    => {λ [pieces]
                         (echof "~a~n" line)
                         (list* (racketparenfont (list-ref pieces 1)) (racketcommentfont ~ (literal (list-ref pieces 3)))
                                (linebreak) (awk 0))}]
                   [(regexp-match #px"^\\s+(.+?) (\\d+) - (.+?)( \\[(.+?)\\])?\\s*$" line)
                    => {λ [pieces]
                         (echof #:fgcolor 'green "~a~n" line)
                         (define-values {stts indx tm}
                           (values (list-ref pieces 1) (list-ref pieces 2)
                                   (let ([tm (list-ref pieces 5)]) (if tm tm "-"))))
                         (list* ((if (string=? stts (~result struct:test-success)) racketvalfont racketerror) stts)
                                (racketvarfont ~ indx) (racketresultfont ~ tm) (racketcommentfont ~ (literal (list-ref pieces 3)))
                                (linebreak) (awk 0))}]
                   [(regexp-match #px"^\\s+⧴ (FAILURE|FATAL) » .+?\\s*$" line)
                    => {λ [pieces]
                         (case (list-ref pieces 1)
                           [{"FAILURE"} (eechof #:fgcolor 'red "~a~n" line) (awk +inf.0)]
                           [{"FATAL"} (eechof #:fgcolor 'red #:attributes '{inverse} "~a~n" line) (awk -inf.0)])}]
                   [(regexp-match #px"^\\s+»» (.+?)?:?\\s+\"?(.+?)\"?\\s*$" line)
                    => {λ [pieces]
                         (eechof #:fgcolor 'red #:attributes (if (< status 0) '{inverse} null) "~a~n" line)
                         (append (if (equal? (list-ref pieces 1) "message")
                                     (list (racketcommentfont (italic "»»" ~ (literal (list-ref pieces 2)))) (linebreak))
                                     null)
                                 (awk status))}]
                   [(regexp-match #px"^\\s+»»»» .+$" line)
                    => {λ [whocares]
                         (eechof #:fgcolor 245 "~a~n" line)
                         (awk status)}]
                   [(regexp-match #px"^$" line)
                    => {λ [whocares]
                         (awk +NaN.0)}]
                   [else (eechof "~a~n" line)
                         (append (if (nan? status) (list (racketoutput line) (linebreak)) null)
                                 (awk status))])))})
