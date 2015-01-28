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
          [else (printf "~nTestsuite~a = ~a, Testcase~a = ~a, Failure~a = ~a, Error~a = ~a, ~a% Okay.~n~a wallclock seconds (~a task + ~a gc = ~a CPU).~n"
                        (plural-suffix (length suite-vars)) (length suite-vars) (plural-suffix population) population
                        (plural-suffix failure) failure (plural-suffix error) error
                        (~r (/ (* success 100) population) #:precision '(= 2)) real (- cpu gc) gc cpu)])
    (+ failure error)})

(define tamer-harness
  {lambda []
    (dynamic-require (current-tamer-story) #false)
    (parameterize ([current-namespace (module->namespace (current-tamer-story))])
      (apply tamer-prove (filter {λ [val] (ormap (curryr apply val null) (list test-suite? test-case?))}
                                 (filter-map (curryr namespace-variable-value #false {λ _ #false})
                                             (namespace-mapped-symbols)))))})

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

(define tamer-note
  {lambda suite-vars
    (define note-width 30)
    (define ~desc {λ [fmt #:max [max-width note-width] . argv] (nonbreaking (~a (apply format fmt argv) #:max-width max-width #:limit-marker "..."))})
    (define-values {hspec-in hspec-out} (make-pipe #false 'hspec-in 'hspec-out))
    (define tamer-spec (thread {λ _ (parameterize ([current-output-port hspec-out]
                                                   [current-error-port hspec-out])
                                      (for ([suite (in-list suite-vars)])
                                        (prove-spec (with-handlers ([exn? exn->test-suite])
                                                      (tamer-require suite))))
                                      (close-output-port hspec-out))}))
    (define-values {px.success px.failure px.error}
      (apply values (map (compose1 pregexp (curry format "^\\s+(~a) (\\d+) - (.+?)( \\[(.+?)\\])?\\s*$") regexp-quote ~result)
                         (list struct:test-success struct:test-failure struct:test-error))))
    (apply margin-note
           (let awk ([No. 0] [status 0] [shrunk 0]) ;;; status: =0 => sucess; >0 => failure; <0 => error
             (define line (read-line hspec-in))
             (cond [(eof-object? line) null]
                   [(regexp-match? #px"^λ\\s.+" line)
                    => {λ [whocares]
                         (echof #:fgcolor 202 #:attributes '{underline} "~a~n" line)
                         (list* (racketidfont (~desc (format "> ~a" (list-ref suite-vars No.)))) (linebreak)
                                (racketparenfont (~desc "~a" line))
                                (linebreak) (awk (add1 No.) 0 0))}]
                   [(regexp-match #px"^\\s+λ(\\d+(.\\d)*)\\s+(.+?)\\s*$" line)
                    => {λ [pieces]
                         (echof "~a~n" line)
                         (list* (racketparenfont (~desc "~a ~a" (list-ref pieces 1) (list-ref pieces 3)))
                                (linebreak) (awk No. 0 0))}]
                   [(regexp-match px.success line)
                    => {λ [pieces]
                         (echof #:fgcolor 'green "~a~n" line)
                         (define-values {stts indx tm nm}
                           (values (list-ref pieces 1) (list-ref pieces 2) (list-ref pieces 5) (list-ref pieces 3)))
                         (list* (racketvalfont stts) (racketvarfont ~ indx) (racketresultfont ~ tm)
                                (racketcommentfont ~ (~desc #:max (- note-width (string-length (format "~a ~a ~a " stts indx tm))) nm))
                                (linebreak) (awk No. 0 0))}]
                   [(or (regexp-match px.failure line) (regexp-match px.error line))
                    => {λ [pieces]
                         (echof #:fgcolor (if (string=? (list-ref pieces 1) (~result struct:test-failure)) 'lightred 'red) "~a~n" line)
                         (define-values {stts indx} (values (list-ref pieces 1) (list-ref pieces 2)))
                         (list* (racketerror stts) (racketvarfont ~ indx)
                                (awk No. 0 (string-length (format "~a ~a" stts indx))))}]
                   [(regexp-match #px"^\\s+⧴ (FAILURE|FATAL) » .+?\\s*$" line)
                    => {λ [pieces]
                         (case (list-ref pieces 1)
                           [{"FAILURE"} (eechof #:fgcolor 'red "~a~n" line) (awk No. +inf.0 shrunk)]
                           [{"FATAL"} (eechof #:fgcolor 'red #:attributes '{inverse} "~a~n" line) (awk No. -inf.0 shrunk)])}]
                   [(regexp-match #px"^\\s+»» (.+?)?:?\\s+\"?(.+?)\"?\\s*$" line)
                    => {λ [pieces]
                         (eechof #:fgcolor 'red #:attributes (if (< status 0) '{inverse} null) "~a~n" line)
                         (append (if (equal? (list-ref pieces 1) "message")
                                     (list (racketcommentfont (~desc #:max (- note-width shrunk) " - ~a" (list-ref pieces 2))) (linebreak))
                                     null)
                                 (awk No. status shrunk))}]
                   [(regexp-match #px"^\\s+»»»» .+$" line)
                    => {λ [whocares]
                         (eechof #:fgcolor 245 "~a~n" line)
                         (awk No. status shrunk)}]
                   [else (eechof "~a~n" line)
                         (awk No. status shrunk)])))})
