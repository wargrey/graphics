#lang at-exp racket

(require rackunit)

(require racket/sandbox)

(require scribble/core)
(require scribble/eval)
(require scribble/manual)

(require "runtime.rkt")

(provide /dev/null tamer-story)
(provide (all-defined-out))

(provide (all-from-out racket "runtime.rkt" rackunit))
(provide (all-from-out scribble/manual scribble/eval))

(define tamer-zone (make-parameter #false))

(define-syntax {tamer-action stx}
  (syntax-case stx []
    [{_ s-exps ...} (syntax/loc stx (interaction #:eval (tamer-zone) s-exps ...))]))

(define-syntax {define-tamer-suite stx}
  (syntax-case stx []
    [{_ varid name #:before setup #:after teardown units ...} #'(tamer-record-story 'varid (test-suite name #:before setup #:after teardown units ...))]
    [{_ varid name #:before setup units ...} (syntax/loc stx (define-tamer-suite varid name #:before setup #:after void units ...))]
    [{_ varid name #:after teardown units ...} (syntax/loc stx (define-tamer-suite varid name #:before void #:after teardown units ...))]
    [{_ varid name units ...} (syntax/loc stx (define-tamer-suite varid name #:before void #:after void units ...))]))

(define-syntax {define-tamer-case stx}
  (syntax-case stx []
    [{_ varid name #:before setup #:after teardown checks ...} #'(tamer-record-story 'varid (delay-test (test-spec name #:before setup #:after teardown checks ...)))]
    [{_ varid name #:before setup checks ...} (syntax/loc stx (define-tamer-case varid name #:before setup #:after void checks ...))]
    [{_ varid name #:after teardown checks ...} (syntax/loc stx (define-tamer-case varid name #:before void #:after teardown checks ...))]
    [{_ varid name checks ...} (syntax/loc stx (define-tamer-case varid name #:before void #:after void checks ...))]))

(define-syntax {test-spec stx}
  (syntax-case stx []
    [{_ name #:before setup #:after teardown checks ...} (syntax/loc stx (test-case name (around (setup) checks ... (teardown))))]
    [{_ name #:before setup checks ...} (syntax/loc stx (test-spec name #:before setup #:after void checks ...))]
    [{_ name #:after teardown checks ...} (syntax/loc stx (test-spec name #:before void #:after teardown checks ...))]
    [{_ name checks ...} (syntax/loc stx (test-spec name #:before void #:after void checks ...))]))

(define tamer-require
  {lambda [name]
    (define htag (tamer-story->tag (tamer-story)))
    (define unit (hash-ref handbook-stories htag null))
    (with-handlers ([exn? {λ [e] (let ([story (exn->test-case 'tamer-require e)])
                                   (hash-set! handbook-stories htag (cons (cons name story) unit))
                                   story)}])
      (with-handlers ([exn:fail:contract? {λ [efc] (raise (make-exn:fail:contract:variable (format "'~a has not yet defined!" name)
                                                                                           (exn-continuation-marks efc) name))}])
        (cdr (assoc name unit))))})

(define tamer-story->libpath
  {lambda [story-path]
    (path->digimon-libpath (if (absolute-path? story-path) story-path (build-path (digimon-tamer) story-path)) 'story)})

(define tamer-partner->libpath
  {lambda [partner-path]
    (path->digimon-libpath (if (absolute-path? partner-path) partner-path (build-path (digimon-zone) partner-path)))})

(define handbook-smart-table
  {lambda []
    (make-traverse-block
     {λ [get set]
       (define readme? (member 'markdown (get 'scribble:current-render-mode '{html})))
       (cond [(false? readme?) (table-of-contents)]
             [else (para (hyperlink (format "http://~a.gyoudmon.org" (string-downcase (current-digimon)))
                                    (format "~a<sub>~a~a~a</sub>" :cat: :paw: :paw: :paw:)))])})})

(define handbook-title
  {lambda pre-contents
    (title (if (null? pre-contents) (list (literal "Tamer's Handbook:") ~ (info-ref 'collection)) pre-contents)
           #:version (format "~a[~a]" (version) (info-ref 'version)))})

(define handbook-story
  {lambda [#:style [style #false] . pre-contents]
    (section #:tag (tamer-story->tag (tamer-story)) #:style style
             "Story: " pre-contents)})

(define handbook-scenario
  {lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (subsection #:tag tag #:style style
                "Scenario: " pre-contents)})

(define handbook-rule
  {lambda [id . pre-flow]
    (itemlist (item (bold (format "Rule ~a " id)) pre-flow))})

(define make-tamer-zone
  {lambda []
    (define tamer.rkt (path->string (build-path (digimon-tamer) "tamer.rkt")))
    (parameterize ([sandbox-namespace-specs (append (sandbox-namespace-specs) `{(file ,tamer.rkt)})]
                   [sandbox-output 'string]
                   [sandbox-error-output 'string])
      ((make-eval-factory (list `(file ,tamer.rkt) (tamer-story)))))})

(define tamer-spec
  {lambda [] ;;; Design for testing via racket.
    (dynamic-require (tamer-story) #false)
    (define htag (tamer-story->tag (tamer-story)))
    (cond [(false? (hash-has-key? handbook-stories htag)) ({λ _ 0} (echof #:fgcolor 'yellow "~nNo particular example!~n"))]
          [else (let ([suite (make-test-suite htag (reverse (map cdr (hash-ref handbook-stories htag))))])
                  (define-values {brief-box cpu0 real0 gc0} (time-apply prove-spec (list suite)))
                  (define-values {success failure error real cpu-gc gc cpu}
                    (apply values (summary-success (car brief-box)) (summary-failure (car brief-box)) (summary-error (car brief-box))
                           (map {λ [t] (~r (/ t 1000.0) #:precision '{= 3})} (list real0 (- cpu0 gc0) gc0 cpu0))))
                  (define population (+ success failure error))
                  (printf "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)~n~a, ~a, ~a, ~a% Okay.~n"
                          real cpu-gc gc cpu @~n_w[population]{example} @~n_w[failure]{failure} @~n_w[error]{error}
                          (~r (/ (* success 100) population) #:precision '{= 2}))
                  (+ failure error))])})

(define tamer-harness
  {lambda [] ;;; Design for testing with documentation
    (define suites (cond [(false? (tamer-story)) (hash-map handbook-stories {λ [harness stories] (make-test-suite harness (reverse (map cdr stories)))})]
                         [(module-path? (tamer-story)) (reverse (map cdr (hash-ref handbook-stories (tamer-story->tag (tamer-story)) null)))]))
    (cond [(null? suites) ({λ _ 0} (printf "~nNo particular testcase!~n"))]
          [else (let ([prove {λ _ (for/fold ([brief initial-summary] [count 0]) ([suite (in-list suites)])
                                    (values (summary** brief (prove-harness suite)) (add1 count)))}])
                  (define-values {brief-box cpu0 real0 gc0} (time-apply prove null))
                  (define-values {reals gcs cpus}
                    (apply values (map (if (tamer-story) (curryr hash-ref (tamer-story) 0) (compose1 (curry foldl + 0) hash-values))
                                       (list story-reals story-gcs story-cpus))))
                  (define-values {success failure error real cpu-gc gc cpu}
                    (apply values (summary-success (car brief-box)) (summary-failure (car brief-box)) (summary-error (car brief-box))
                           (map {λ [t0 ts] (~r (/ (+ t0 ts) 1000.0) #:precision '{= 3})}
                                (list real0 (- cpu0 gc0) gc0 cpu0) (list reals (- cpus gcs) gcs cpus))))
                  (define population (+ success failure error))
                  (printf "~n~a% tests successful.~n~a, ~a, ~a, ~a.~n~a wallclock seconds (~a task + ~a gc = ~a CPU).~n"
                          (~r (/ (* success 100) population) #:precision '(= 2))
                          @~w=n[(cadr brief-box)]{Testunit} @~w=n[population]{Testcase} @~w=n[failure]{Failure} @~w=n[error]{Error}
                          real cpu-gc gc cpu)
                  (+ failure error))])})

(define tamer-smart-summary
  {lambda []
    (define story-snapshot (tamer-story))
    (make-delayed-block
     {λ [render% pthis _]
       (define-values {harness-in harness-out} (make-pipe #false 'hspec-in 'hspec-out))
       (parameterize ([tamer-story story-snapshot]
                      [current-input-port harness-in]
                      [current-error-port harness-out]
                      [current-output-port harness-out])
         (define summary? (make-parameter #false))
         (define readme? (member 'markdown (send render% current-render-mode)))
         (define ->block (cond [readme? {λ [blocks] (margin-note (para (literal "---")) (italic (string :book:)) ~ (bold "Behaviors and Features")
                                                                 "<br>" (add-between (filter-not void? blocks) "<br>"))}]
                               [else {λ [blocks] (filebox (italic (string :book:) ~
                                                                  (cond [(false? (tamer-story)) (format "Behaviors of ~a" (info-ref 'collection))]
                                                                        [(module-path? (tamer-story)) (format "Behaviors of ~a" (cadadr (tamer-story)))]))
                                                          (add-between (filter-not void? blocks) (linebreak)))}]))
         (thread {λ _ (dynamic-wind {λ _ (collect-garbage)}
                                    {λ _ (tamer-harness)}
                                    {λ _ (close-output-port harness-out)})})
         (nested #:style (make-style "boxed" null)
                 (->block (for/list ([line (in-port read-line)])
                            (cond [(regexp-match #px"^(.+?)(\\.{6,})(.+)$" line)
                                   => {λ [pieces] (match-let ([{list _ story padding status} pieces])
                                                    (define remote-url (format "http://~a.gyoudmon.org/~a" (string-downcase (current-digimon)) story))
                                                    (define result (string (cond [(string=? status (~result struct:test-success)) :heart:]
                                                                                 [(string=? status (~result struct:test-failure)) :broken-heart:]
                                                                                 [else :collision:])))
                                                    (define label (racketkeywordfont (literal story padding)))
                                                    (cond [readme? (elem result ~ (hyperlink remote-url story))]
                                                          [(false? (tamer-story)) (seclink story label result)]
                                                          [(module-path? (tamer-story)) (elem label result)]))}]
                                  [(regexp-match? #px"^⧴ (FAILURE|FATAL) » .+?\\s*$" line)
                                   => {λ _ (unless readme? (racketcommentfont line))}]
                                  [(regexp-match? #px"^\\s*$" line)
                                   => {λ _ (and (summary? #true) ~)}]
                                  [(and (summary?) readme?) (unless (regexp-match? #px"wallclock" line) (italic (literal line)))]
                                  [(summary?) (racketoutput line)])))))})})

(define tamer-note
  {lambda suite-vars
    (define story-snapshot (tamer-story))
    (make-traverse-block
     {λ [get set]
       (define-values {hspec-in hspec-out} (make-pipe #false 'hspec-in 'hspec-out))
       (parameterize ([tamer-story story-snapshot]
                      [current-input-port hspec-in])
         (define status (make-parameter 0)) ;;; status: =0 => success; >0 => failure; <0 => error; NaN => summary comes
         (thread {λ _ (dynamic-wind {λ _ (collect-garbage)}
                                    {λ _ (parameterize ([current-error-port hspec-out]
                                                        [current-output-port hspec-out])
                                           (for ([suite (in-list suite-vars)])
                                             (define-values {brief cpu real gc} (time-apply prove-spec (list (tamer-require suite))))
                                             (define-values {failure error} (values (summary-failure (car brief)) (summary-error (car brief))))
                                             (hash-set! story-cpus (tamer-story) (+ cpu (hash-ref story-cpus (tamer-story) 0)))
                                             (hash-set! story-reals (tamer-story) (+ real (hash-ref story-reals (tamer-story) 0)))
                                             (hash-set! story-gcs (tamer-story) (+ gc (hash-ref story-gcs (tamer-story) 0)))
                                             (cond [(zero? (+ failure error)) (printf "~n~a wallclock seconds~n" (~r (/ real 1000.0) #:precision '{= 3}))]
                                                   [else (printf "~n~a ~a~n" @~n_w[failure]{failure} @~n_w[error]{error})])))}
                                    {λ _ (close-output-port hspec-out)})})
         (margin-note (let ([bs (for/list ([line (in-port read-line)])
                                  (cond [(regexp-match #px"^(λ)\\s+(.+)" line)
                                         => {λ [pieces] (and (status 0)
                                                             (echof #:fgcolor 202 #:attributes '{underline} "~a~n" line)
                                                             (racketmetafont (italic (string :book:)) ~ (literal (list-ref pieces 2))))}]
                                        [(regexp-match #px"^\\s+λ(\\d+(.\\d)*)\\s+(.+?)\\s*$" line)
                                         => {λ [pieces] (and (status 0)
                                                             (echof "~a~n" line)
                                                             (racketparenfont (italic (string :bookmark:)) ~ (literal (list-ref pieces 3))))}]
                                        [(regexp-match #px"^\\s*(.+?) (\\d+) - (.+?)\\s*$" line)
                                         => {λ [pieces] (let*-values ([{color :stts:} (if (string=? (list-ref pieces 1) (~result struct:test-success))
                                                                                        (values 'green :heart:) (values 'lightred :broken-heart:))])
                                                          (status 0)
                                                          (echof #:fgcolor color "~a~n" line)
                                                          (elem (string :stts:) (racketvarfont ~ (list-ref pieces 2))
                                                                (racketcommentfont ~ (literal (list-ref pieces 3)))))}]
                                        [(regexp-match #px"^\\s*⧴ (FAILURE|FATAL) » .+?\\s*$" line)
                                         => {λ [pieces] (case (list-ref pieces 1)
                                                          [{"FAILURE"} (eechof #:fgcolor 'red "~a~n" line) (status +inf.0)]
                                                          [{"FATAL"} (eechof #:fgcolor 'red #:attributes '{inverse} "~a~n" line) (status -inf.0)])}]
                                        [(regexp-match #px"^\\s*»» (.+?)?:?\\s+\"?(.+?)\"?\\s*$" line)
                                         => {λ [pieces] (and (eechof #:fgcolor 'red #:attributes (if (< (status) 0) '{inverse} null) "~a~n" line)
                                                             (when (regexp-match? #px"message$" (list-ref pieces 1))
                                                               (elem (string :backhand:) ~ (racketcommentfont (italic (literal (list-ref pieces 2)))))))}]
                                        [(regexp-match #px"^\\s*»»»» .+$" line)
                                         => {λ _ (eechof #:fgcolor 245 "~a~n" line)}]
                                        [(regexp-match #px"^$" line)
                                         => {λ _ (status +NaN.0)}]
                                        [else (eechof #:fgcolor 'yellow "~a~n" line)
                                              (when (nan? (status)) (elem (string :pin:) ~ (racketoutput line)))]))])
                        (add-between (filter-not void? bs) (linebreak)))))})})

{module digitama racket
  (require rackunit)
  
  (require setup/getinfo)

  (require "runtime.rkt")
  
  (provide (all-defined-out))
  
  (define info-ref (get-info/full (digimon-zone)))

  (define tamer-story (make-parameter #false))

  (define tamer-story->tag
    {lambda [story]
      (with-handlers ([exn? {λ [e] (exn-message e)}])
        (path->string (find-relative-path (digimon-tamer) (build-path (digimon-world) (cadadr story)))))})
  
  (struct tamer-seed {datum brief name-path})
  (struct summary {success failure error})
  (define initial-summary (summary 0 0 0))
  (define summary++
    {lambda [summary0 result]
      (summary (+ (summary-success summary0) (if (test-success? result) 1 0))
               (+ (summary-failure summary0) (if (test-failure? result) 1 0))
               (+ (summary-error summary0) (if (test-error? result) 1 0)))})
  (define summary**
    {lambda [summary1 summary2]
      (summary (+ (summary-success summary1) (summary-success summary2))
               (+ (summary-failure summary1) (summary-failure summary2))
               (+ (summary-error summary1) (summary-error summary2)))})

  (define-values {handbook-stories handbook-records story-cpus story-reals story-gcs}
    (values (make-hash) (make-hash) (make-hash) (make-hash) (make-hash)))
  (define tamer-record-story
    {lambda [name unit]
      (define htag (tamer-story->tag (tamer-story)))
      (define units (hash-ref handbook-stories htag null))
      (unless (assoc name units)
        (hash-set! handbook-stories htag (cons (cons name unit) units)))})
  (define tamer-record-handbook
    {lambda [name:case«suites action]
      (define short-name (car name:case«suites))
      (define long-name (let ([name (string-join name:case«suites " « ")])
                          (cond [(false? (tamer-story)) name]
                                [(module-path? (tamer-story)) (string-append name " « " (tamer-story->tag (tamer-story)))])))
      (unless (hash-has-key? handbook-records long-name)
        (hash-set! handbook-records long-name
                   (call-with-escape-continuation
                    {λ [return] (parameterize ([current-output-port /dev/null]
                                               [current-error-port (open-output-string 'stderr)]
                                               [exit-handler {λ [status] (let ([errmsg (string-trim (get-output-string (current-error-port)))])
                                                                           (return (run-test-case short-name (with-check-info {{'exitcode status}}
                                                                                                                              {λ _ (fail errmsg)}))))}])
                                  (return (run-test-case short-name action)))})))
      (hash-ref handbook-records long-name)})
  
  (define indents (make-hash))
  (define ~indent
    {lambda [count #:times [times 2] #:padchar [padding #\space]]
      (define key (format "~a~a" count padding))
      (unless (hash-has-key? indents key)
        (hash-set! indents key (make-string (* count times) padding)))
      (hash-ref indents key)})
    
  (define ~result
    {lambda [result]
      (define indicators (hash (object-name struct:test-error) "#?"
                               (object-name struct:test-success) "#t"
                               (object-name struct:test-failure) "#f"))
      (hash-ref indicators (object-name result))})
  
  (define exn->test-case
    {lambda [name e]
      (delay-test (test-case (format "(~a ⧴ ~a)" name (object-name e))
                             #| no thunk, make test-error |# (raise e)))})

  (define tr-d (curryr string-replace (digimon-world) ""))
  
  (define display-failure
    {lambda [result #:indent [headspace ""]]
      (eechof #:fgcolor 'red "~a⧴ FAILURE » ~a~n" headspace (test-result-test-case-name result))
      (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
        (eechof #:fgcolor 'red "~a»» ~a: ~s~n" headspace (check-info-name info)
                (case (check-info-name info)
                  [{location} (tr-d (srcloc->string (apply srcloc (check-info-value info))))]
                  [{exception-message} (tr-d (check-info-value info))]
                  [{exception} (object-name (check-info-value info))]
                  [else (check-info-value info)])))})
  
  (define display-error
    {lambda [result #:indent [headspace0 ""]]
      (define errobj (test-error-result result))
      (define messages (call-with-input-string (tr-d (exn-message errobj)) port->lines))
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
                    (tr-d srcinfo) (or (car stack) 'λ)))))})
  
  (define default-fseed
    {lambda last-is-seed
      (last last-is-seed)})
  
  (define fold-test-suite
    {lambda [seed:datum testsuite #:fdown [fdown default-fseed] #:fup [fup default-fseed] #:fhere [fhere default-fseed]]
      (define seed (parameterize ([current-custodian (make-custodian)])
                     (define $exn (make-parameter #false))
                     (foldts-test-suite {λ [testsuite name pre-action post-action seed]
                                          (with-handlers ([exn? $exn])
                                            (pre-action))
                                          (tamer-seed (fdown name (tamer-seed-datum seed))
                                                      (tamer-seed-brief seed )
                                                      (cons name (tamer-seed-name-path seed)))}
                                        {λ [testsuite name pre-action post-action seed children-seed]
                                          (with-handlers ([exn? (compose1 display-error (curry make-test-error (format "Postaction[~a]" name)))])
                                            (post-action))
                                          ($exn #false)
                                          (tamer-seed (fup name (tamer-seed-datum seed) (tamer-seed-datum children-seed))
                                                      (tamer-seed-brief children-seed)
                                                      (tamer-seed-name-path seed))}
                                        {λ [testcase name action0 seed]
                                          (define-values {name-path action}
                                            (cond [(exn? ($exn)) (values (cons (format "Preaction[~a]" name)
                                                                               (tamer-seed-name-path seed))
                                                                         {λ _ (raise ($exn))})]
                                                  [(false? name) (values (cons (format "(⧴ ~a)" (object-name exn:fail:user))
                                                                               (tamer-seed-name-path seed))
                                                                         {λ _ (raise-user-error "Testcase should have a name")})]
                                                  [else (values (cons name (tamer-seed-name-path seed)) action0)]))
                                          (define record (tamer-record-handbook name-path action))
                                          (tamer-seed (fhere record (tamer-seed-datum seed))
                                                      (summary++ (tamer-seed-brief seed) record)
                                                      name-path)}
                                        (tamer-seed seed:datum initial-summary null)
                                        testsuite)))
      (values (tamer-seed-datum seed) (tamer-seed-brief seed))})
  
  (define prove-spec
    {lambda [unit]
      (define-values {whocares brief}
        (fold-test-suite #:fdown {λ [name seed:ordered]
                                   (cond [(null? seed:ordered) (echof #:fgcolor 202 #:attributes '{underline} "λ ~a~a~n" (~indent 0) name)]
                                         [else (echof "~aλ~a ~a~n" (~indent (length seed:ordered)) (string-join (map number->string (reverse seed:ordered)) ".") name)])
                                   (cons 1 seed:ordered)}
                         #:fup {λ [name seed:ordered children:ordered]
                                 (cond [(null? seed:ordered) null]
                                       [else (cons (add1 (car seed:ordered))
                                                   (cdr seed:ordered))])}
                         #:fhere {λ [result seed:ordered]
                                   (define headline (format "~a~a ~a - " (~indent (length seed:ordered)) (~result result)
                                                            (if (null? seed:ordered) 1 (car seed:ordered))))
                                   (define headspace (~indent (string-length headline) #:times 1))
                                   (cond [(test-success? result) (void (echof #:fgcolor 'green "~a~a~n" headline (test-result-test-case-name result)))]
                                         [(test-failure? result) (void (echof #:fgcolor 'lightred "~a~a~n" headline (test-result-test-case-name result))
                                                                       (display-failure result #:indent headspace))]
                                         [(test-error? result) (void (echof #:fgcolor 'red "~a~a~n" headline (test-result-test-case-name result))
                                                                     (display-error result #:indent headspace))]
                                         [else (error "RackUnit has new test result type added!")])
                                   (if (null? seed:ordered) null (cons (add1 (car seed:ordered)) (cdr seed:ordered)))}
                         null
                         unit))
      (values brief)})
  
  (define prove-harness
    {lambda [unit]
      (define-values {$?≠0 brief}
        (fold-test-suite null unit #:fhere {λ [result seed:$?≠0] (cond [(test-success? result) seed:$?≠0]
                                                                        [else (cons result seed:$?≠0)])}))
      (echof "~a" (~a #:width 64 #:pad-string "." #:limit-marker "......" (cond [(test-suite? unit) (rackunit-test-suite-name unit)]
                                                                                [(test-case? unit) (rackunit-test-case-name unit)])))
      (cond [(positive? (summary-error brief)) (echof #:fgcolor 'red "~a~n" (~result struct:test-error))]
            [(positive? (summary-failure brief)) (echof #:fgcolor 'lightred "~a~n" (~result struct:test-failure))]
            [(positive? (summary-success brief)) (echof #:fgcolor 'green "~a~n" (~result struct:test-success))]
            [else #| No testcase |# (echof #:fgcolor 'lightgreen "~a~n" #true)])
      (for ([result (in-list (reverse $?≠0))])
        (cond [(test-failure? result) (display-failure result)]
              [(test-error? result) (display-error result)]
              [else (error "RackUnit has new test result type added!")]))
      (values brief)})}

(require (submod "." digitama))
