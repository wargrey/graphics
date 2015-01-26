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
    (define brief (foldl summary** initial-summary (map tamer-run-suite suites)))
    (define-values {success failure error cpu real gc}
      (apply values (summary-success brief) (summary-failure brief) (summary-error brief)
             (map (curryr / 1000.0) (list (summary-cpu brief) (summary-real brief) (summary-gc brief)))))
    (define population (+ success failure error))
    (cond [(zero? population) (printf "~nNo testcase, do not try to fool me!~n")]
          [else (printf "~nTestsuite~a = ~a, Testcase~a = ~a, Failure~a = ~a, Error~a = ~a, ~a% Okay.~n~a wallclock seconds (~a task + ~a gc = ~a CPU).~n"
                        (plural-suffix (length suites)) (length suites) (plural-suffix population) population
                        (plural-suffix failure) error (plural-suffix error) error
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
      (define brief (prove-spec (make-test-suite (path->string (build-path (cadadr (current-tamer-story))))
                                                 (filter test-suite? (filter-map (curryr namespace-variable-value #false (const #false))
                                                                                 (namespace-mapped-symbols))))))
      (define-values {success failure error cpu real gc}
        (apply values (summary-success brief) (summary-failure brief) (summary-error brief)
               (map (curryr / 1000.0) (list (summary-cpu brief) (summary-real brief) (summary-gc brief)))))
      (define population (+ success failure error))
      (cond [(zero? population) (printf "~nNo example, do not try to fool me!~n")]
            [else (printf "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)~n~a example~a, ~a failure~a, ~a error~a, ~a% Okay.~n"
                          real (- cpu gc) gc cpu population (plural-suffix population) failure (plural-suffix failure) error (plural-suffix error)
                          (~r (/ (* success 100) population) #:precision '{= 2}))])
      (+ failure error))})

(define tamer-note
  {lambda suites
    (define note-width 24)
    (define ~desc {lambda [fmt #:max [max-width note-width] . vals] (~a (apply format fmt vals) #:max-width max-width #:limit-marker "...")})
    (define-values {hspec-in hspec-out} (make-pipe #false 'hspec-in 'hspec-out))
    (define tamer-spec (thread {thunk (parameterize ([current-output-port hspec-out]
                                                     [current-error-port hspec-out])
                                        (for ([suite (in-list suites)])
                                          (prove-spec (with-handlers ([exn? exn->test-suite])
                                                        (tamer-require suite))))
                                        (close-output-port hspec-out))}))
    (apply margin-note
           (let awk ([No. 0])
             (define line (read-line hspec-in))
             (cond [(eof-object? line) null]
                   [(regexp-match? #px"^\\S.+" line)
                    => {lambda [whocares]
                         (echof #:fgcolor 202 #:attributes '{underline} "~a~n" line)
                         (list* (racketidfont (~desc (format "> ~a" (list-ref suites No.)))) (linebreak)
                                (racketparenfont (~desc "λ ~a" line #:max (+ note-width 4)))
                                (linebreak) (awk (add1 No.)))}]
                   [(regexp-match #px"^\\s+(λ\\d+(.\\d)*)\\s+(.+?)\\s*$" line)
                    => {lambda [pieces]
                         (echof #:fgcolor 202 "~a~n" line)
                         (list* (racketparenfont (~desc "~a ~a" (string-replace (list-ref pieces 1) #px"\\d+(\\.|$)" "λ") (list-ref pieces 3) #:max (+ note-width 4)))
                                (linebreak) (awk No.))}]
                   [(regexp-match #px"^\\s+(.+?) (\\d+) - (.+?) [(.+?)]\\s*$" line)
                    => {lambda [pieces]
                         (echof #:fgcolor 'green "~a~n" line)
                         (list* (racketvalfont (list-ref pieces 1)) (racketvarfont (list-ref pieces 2)) (racketresultfont (list-ref pieces 4))
                                (racketcommentfont (~desc #:max (- note-width (string-length (list-ref pieces 4)) 1) ((list-ref pieces 3))))
                                (linebreak) (awk No.))}]
                   [else (displayln line) (awk No.)])))
    (apply tamer-note0 suites)})

(define tamer-note0
  {lambda suites
    (define note-width 24)
    (define ~desc {lambda [fmt #:max [max-width note-width] . vals] (~a (apply format fmt vals) #:max-width max-width #:limit-marker "...")})
    (define note-fdown {lambda [name seed]
                         (cons (cons (add1 (caar seed)) (cdar seed))
                               (append (cdr seed)
                                       (list (racketparenfont (~desc "~a ~a" (~indent (caar seed) #:times 1 #:padchar #\λ) name #:max (+ note-width 4)))
                                             (linebreak))))})
    (define note-fup {lambda [name seed children-seed]
                       (cons (cons (caar seed) (cdar children-seed))
                             (cdr children-seed))})
    (define note-fhere {lambda [validated seed]
                         (define result (validation-result validated))
                         (define-values {cpu real gc}
                           (values (validation-cpu validated) (validation-real validated) (validation-gc validated)))
                         (define count (cdar seed))
                         (cons (cons (caar seed) (add1 count))
                               (append (cdr seed)
                                       (cond [(test-success? result) (list* (racketvalfont (format "~a " (~result result)))
                                                                            (racketvarfont (format "~a " count))
                                                                            (let ([tms (format "~ams" real)])
                                                                              (list (racketresultfont (format "~a " tms))
                                                                                    (racketcommentfont (~desc #:max (- note-width (string-length tms) 1)
                                                                                                              (test-result-test-case-name result))))))]
                                             [(test-failure? result) (list (racketerror (format "~a " (~result result)))
                                                                           (racketvarfont (format "~a " count))
                                                                           (racketcommentfont (~desc (test-result-test-case-name result))))]
                                             [else (let ([err (test-error-result result)])
                                                     (list (racketerror (format "~a " (~result result)))
                                                           (racketvarfont (format "~a " count))
                                                           (racketcommentfont (~desc (with-input-from-string (exn-message err) {thunk (read-line)})))))])
                                       (list (linebreak))))})
    (apply margin-note
           (for/fold ([briefs null]) ([suite (in-list suites)])
             (define-values {seed whocares}
               (fold-test-suite #:fdown note-fdown #:fup note-fup #:fhere note-fhere
                                (cons (cons 1 1) (list (racketidfont (~desc "> ~a" suite)) (linebreak)))
                                (with-handlers ([exn? exn->test-suite]) (tamer-require suite))))
             (append briefs (cdr seed))))})
