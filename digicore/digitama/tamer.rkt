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

(define tamer-script
  {lambda suites
    ;(define smry (foldl summary** (summary 0 0 0 0 0 0)
     ;                   (map prove-harness suites)
      ;                  (define smry0 (prove-harness (tamer-require suite))))
    (void)})

(define tamer-harness
  {lambda []
    (dynamic-require (current-tamer-story) #false)
    (parameterize ([current-namespace (module->namespace (current-tamer-story))])
      (printf "~n~a~n" (foldl summary** (summary 0 0 0 0 0 0)
                            (map prove-harness (filter test-suite? (filter-map (curryr namespace-variable-value #false (const #false))
                                                                               (namespace-mapped-symbols)))))))})

(define tamer-spec
  {lambda []
    (dynamic-require (current-tamer-story) #false)
    (parameterize ([current-namespace (module->namespace (current-tamer-story))])
      (printf "~a~n" (prove-spec (make-test-suite (path->string (build-path (cadadr (current-tamer-story))))
                                                  (filter test-suite? (filter-map (curryr namespace-variable-value #false (const #false))
                                                                                  (namespace-mapped-symbols)))))))})

(define tamer-note
  {lambda suites
    (apply margin-note
           (for/fold ([briefs null]) ([suite (in-list suites)])
             (append briefs (with-handlers ([exn? {lambda [e] (list (racketerror (exn-message e)))}])
                              (cdr (foldts-test-suite default-fdown
                                                      default-fup
                                                      {lambda [testcase name action count.briefs]
                                                        (define validated (tamer-record-handbook name action))
                                                        (define result (validation-result validated))
                                                        (define-values {cpu real gc}
                                                          (values (validation-cpu validated) (validation-real validated) (validation-gc validated)))
                                                        (define count (car count.briefs))
                                                        (cons (add1 count)
                                                              (append (cdr count.briefs)
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
                                                                      (list (linebreak))))}
                                                      (cons 1 (list (racketidfont (format "~a" suite)) (linebreak)))
                                                      (tamer-require suite)))))))})
