#lang at-exp racket

(require rackunit)

(require racket/sandbox)

(require scribble/core)
(require scribble/eval)
(require scribble/manual)

(require "digicore.rkt")

(provide (all-defined-out) tamer-story)

(provide (all-from-out racket "digicore.rkt" rackunit))
(provide (all-from-out scribble/manual scribble/eval))

(define tamer-zone (make-parameter #false))

(define $out (open-output-bytes '/dev/tamer/stdout))
(define $err (open-output-bytes '/dev/tamer/stderr))
(define $? (make-parameter +NaN.0))

(define call-with-fresh-$
  {lambda [routine . arglist]
    (get-output-bytes $out #true)
    (get-output-bytes $err #true)
    ($? +NaN.0)
    (parameterize ([current-output-port $out]
                   [current-error-port $err]
                   [exit-handler $?])
      (apply routine arglist))})

(define make-tamer-zone
  {lambda []
    (define tamer.rkt (path->string (build-path (digimon-tamer) "tamer.rkt")))
    (parameterize ([sandbox-namespace-specs (append (sandbox-namespace-specs) `{(file ,tamer.rkt)})]
                   [sandbox-output 'string]
                   [sandbox-error-output 'string])
      ((make-eval-factory (list `(file ,tamer.rkt) (tamer-story)))))})

(define-syntax {tamer-taming-start stx}
  #'(let ([modpath (quote-module-path)])
      (cond [(path? modpath) (tamer-story (tamer-story->libpath modpath))]
            [else (and (tamer-story (tamer-story->libpath (cadr modpath)))
                       (tamer-zone (make-tamer-zone)))])))

(define-syntax {handbook-story stx}
  (syntax-case stx []
    [{_ #:style style contents ...} #'(list (tamer-taming-start) (title #:tag (tamer-story->tag (tamer-story)) #:style style "Story: " contents ...))]
    [{_ pre-contents ...} (syntax/loc stx (handbook-story #:style #false pre-contents ...))]))

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
    [{_ name #:before setup #:after teardown checks ...} #'(test-case name (around (setup) checks ... (teardown)))]
    [{_ name #:before setup checks ...} (syntax/loc stx (test-spec name #:before setup #:after void checks ...))]
    [{_ name #:after teardown checks ...} (syntax/loc stx (test-spec name #:before void #:after teardown checks ...))]
    [{_ name checks ...} (syntax/loc stx (test-spec name #:before void #:after void checks ...))]))

(define-syntax {tamer-action stx}
  (syntax-case stx []
    [{_ s-exps ...} #'(let ([story-snapshot (tamer-story)]
                            [zone-snapshot (tamer-zone)])
                        (make-traverse-block {λ _ (parameterize ([tamer-story story-snapshot]
                                                                 [tamer-zone zone-snapshot])
                                                    (interaction #:eval (tamer-zone) s-exps ...))}))]))

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

(define handbook-title
  {lambda pre-contents
    (define info-ref (get-info/full (digimon-zone)))
    (title (append (cond [(false? (symbol=? (object-name (current-input-port)) 'stupid-markdown)) null]
                         [else (list (hyperlink (~url (current-digimon)) (format "~a<sub>~a</sub>" house-garden# cat#)))])
                   (if (null? pre-contents) (list (literal "Tamer's Handbook:") ~ (info-ref 'collection {λ _ (current-digimon)})) pre-contents))
           #:version (format "~a[~a]" (version) (info-ref 'version {λ _ "Baby"}))
           #:tag "tamerbook")})

(define handbook-scenario
  {lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (section #:tag tag #:style style
             "Scenario: " pre-contents)})

(define handbook-appendix
  {lambda [#:style [style #false] . pre-contents]
    (list (section #:style style
                   (string-titlecase (format "Appendix: ~a Auxiliaries" (path-replace-suffix (tamer-story->tag (tamer-story)) "")))
                   pre-contents)
          (para (elem #:style (make-style #false (list (make-color-property (list 128 128 128))))
                      @italic{In order to avoid polluting your eyes,
                              any less important things are moved here.
                              You can simply ignore them as you wish.}))
          (tamer-story #false))})

(define handbook-rule
  {lambda [id . pre-flow]
    (define tag (format "rule ~a" id))
    (itemlist (item (elemtag tag (bold (format "~a " (string-titlecase tag)))) pre-flow))})

(define itech
  {lambda pre-contents
    (tech (italic pre-contents))})

(define tamer-prove
  {lambda []
    (define suite (cond [(false? (tamer-story)) (or (zero? (hash-count handbook-stories)) ; no story ==> no :books:
                                                    (let ([href (curry hash-ref handbook-stories)])
                                                      (make-test-suite "Behaviors and Features"
                                                                       (map {λ [unit] (make-test-suite unit (reverse (map cdr (href unit))))}
                                                                            (reverse (href books#))))))]
                        [(module-path? (tamer-story)) (let ([htag (tamer-story->tag (tamer-story))])
                                                        (and (unless (module-declared? (tamer-story)) (dynamic-require (tamer-story) #false))
                                                             (hash-has-key? handbook-stories htag)
                                                             (make-test-suite htag (reverse (map cdr (hash-ref handbook-stories htag))))))]))
    (cond [(false? (test-suite? suite)) ({λ _ 0} (echof #:fgcolor 'yellow "~nNo particular example!~n"))]
          [else (let-values ([{brief-box cpu0 real0 gc0} (time-apply prove (list suite))])
                  (define-values {success failure error real cpu-gc gc cpu}
                    (apply values (summary-success (car brief-box)) (summary-failure (car brief-box)) (summary-error (car brief-box))
                           (map {λ [t] (~r (/ t 1000.0) #:precision '{= 3})} (list real0 (- cpu0 gc0) gc0 cpu0))))
                  (define population (+ success failure error))
                  (echof #:fgcolor 'lightcyan "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU).~n~a, ~a, ~a, ~a% Okay.~n"
                         real cpu-gc gc cpu @~n_w[population]{example} @~n_w[failure]{failure} @~n_w[error]{error}
                         (~r (/ (* success 100) population) #:precision '{= 2}))
                  (+ failure error))])})

(define tamer-smart-summary
  {lambda []
    (define story-snapshot (tamer-story))
    (make-traverse-block
     {λ [get set]
       (cond [(member 'markdown (get 'scribble:current-render-mode '{html})) (para (literal "---"))]
             [else (make-delayed-block
                    {λ [render% pthis infobase]
                      (define get (curry hash-ref (collect-info-fp (resolve-info-ci infobase))))
                      (define info-ref (get-info/full (digimon-zone)))
                      (define story-ref {λ [htag] (filter-map {λ [scnr] (hash-ref (hash-ref (get 'scenario {λ _ (make-hash)}) htag {λ _ (make-hash)}) (cdr scnr) #false)}
                                                              (reverse (hash-ref handbook-stories htag null)))})
                      (nested #:style (make-style "boxed" null)
                              (filebox (cond [(false? story-snapshot) (italic (string books#) ~ (format "Behaviors of ~a" (info-ref 'collection {λ _ (current-digimon)})))]
                                             [(module-path? story-snapshot) (italic (seclink "tamerbook" (string open-book#))
                                                                                    ~ (format "Behaviors in ~a" (cadadr story-snapshot)))])
                                       (let ([base (cond [(false? story-snapshot) (for/list ([story (in-list (reverse (hash-ref handbook-stories books# null)))])
                                                                                    (cons story (with-handlers ([exn:fail:contract? {λ _ null}])
                                                                                                  (apply append (map cdr (story-ref story))))))]
                                                         [(module-path? story-snapshot) (story-ref (tamer-story->tag story-snapshot))])])
                                         (define items (for/list ([spec (in-list base)])
                                                         ;;; see (tamer-note) to check type conventions
                                                         (define-values {local# desc} (cond [(string? (car spec)) (values bookmark# (car spec))]
                                                                                             [else (values page# (unbox (car spec)))]))
                                                         (define-values {status color status#}
                                                           (cond [(null? (cdr spec)) (values (~result struct:test-success) 'lightgreen heart#)]
                                                                 [(ormap box? (cdr spec)) (values (~result struct:test-error) 'red bomb#)]
                                                                 [else (values (~result struct:test-failure) 'lightred broken-heart#)]))
                                                         (define item (~a #:width 61 #:pad-string "." #:limit-marker "......" desc))
                                                         (define label (racketkeywordfont (literal item) (string status#)))
                                                         (when (false? story-snapshot)
                                                           (echof #:fgcolor 'lightyellow item)
                                                           (echof #:fgcolor color "~a~n" status)
                                                           (for ([msg (in-list (cdr spec))])
                                                             (cond [(box? msg) (eechof #:fgcolor 'red #:attributes '{inverse} "~a~n" (unbox msg))]
                                                                   [(regexp-match #px"»»" msg) (eechof #:fgcolor 245 "~a~n" msg)]
                                                                   [(string? (car spec)) (eechof #:fgcolor 'lightred "~a~n" msg)])))
                                                         (cond [(false? story-snapshot) (seclink (car spec) (italic (string book#)) ~ label)]
                                                               [(module-path? story-snapshot) (elemref desc (italic (string local#)) ~ label)])))
                                         (match-define {list success failure error reals gcs cpus}
                                           (for/list ([meta (in-list (list 'success 'failure 'error 'real 'gc 'cpu))])
                                             (define pool (get meta {λ _ (make-hash)}))
                                             (if story-snapshot (hash-ref pool story-snapshot 0) (foldl + 0 (hash-values pool)))))
                                         (match-define {list real cpu-gc gc cpu}
                                           (map {λ [ts] (~r (/ ts 1000.0) #:precision '{= 3})} (list reals (- cpus gcs) gcs cpus)))
                                         (define summaries (let ([population (+ success failure error)])
                                                             (cond [(zero? population) (list "No particular test!")]
                                                                   [else (list (format "~a% tests successful." (~r (/ (* success 100) population) #:precision '(= 2)))
                                                                               (format "~a, ~a, ~a, ~a."
                                                                                       @~w=n[(length base) (if (false? story-snapshot) "Story" "Scenario")]
                                                                                       @~w=n[population]{Test} @~w=n[failure]{Failure} @~w=n[error]{Error})
                                                                               (format "~a wallclock seconds (~a task + ~a gc = ~a CPU)." real cpu-gc gc cpu))])))
                                         (when (false? story-snapshot)
                                           (newline)
                                           (for-each {λ [summary] (echof #:fgcolor 'lightcyan "~a~n" summary)} summaries))
                                         (let ([lb (list (linebreak))])
                                           (append (if (null? items) null (add-between #:after-last lb #:splice? #true items lb))
                                                   (add-between #:before-first lb #:splice? #true (map racketoutput summaries) lb))))))})])})})

(define handbook-smart-table
  {lambda []
    (make-traverse-block
     {λ [get set]
       (cond [(false? (member 'markdown (get 'scribble:current-render-mode '{html}))) (table-of-contents)]
             [else (make-delayed-block
                    {λ [render% pthis _]
                      (define-values {/dev/tamer/stdin /dev/tamer/stdout} (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
                      (parameterize ([current-input-port /dev/tamer/stdin]
                                     [current-error-port /dev/tamer/stdout]
                                     [current-output-port /dev/tamer/stdout]
                                     [tamer-story #false])
                        (define summary? (make-parameter #false))
                        (thread {λ _ (dynamic-wind {λ _ (collect-garbage)}
                                                   {λ _ (tamer-prove)}
                                                   {λ _ (close-output-port /dev/tamer/stdout)})})
                        (para (filter-map {λ [line] (and (not (void? line)) (map (compose1 literal ~line) (if (list? line) line (list line))))}
                                          (for/list ([line (in-port read-line)])
                                            (cond [(regexp-match #px"^λ\\s+(.+)" line)
                                                   => {λ [pieces] (format "> + ~a~a" books# (list-ref pieces 1))}]
                                                  [(regexp-match #px"^(\\s+)λ\\d+\\s+(.+?.rkt)\\s*$" line)
                                                   => {λ [pieces] (match-let ([{list _ indt ctxt} pieces]) ; (markdown list needs at least 1 char after "+ "
                                                                    (list (format ">   ~a+ ~a" indt open-book#)  ; before breaking line if "[~a](~a)" is
                                                                          (format "[~a](http://gyoudmon.org/~~~a/.~a/~a)" ctxt ; longer then 72 chars.)
                                                                                  (getenv "USER") (string-downcase (current-digimon)) ctxt)))}]
                                                  [(regexp-match #px"^(\\s+)λ\\d+(.\\d)*\\s+(.+?)\\s*$" line)
                                                   => {λ [pieces] (format ">   ~a+ ~a~a" (list-ref pieces 1) bookmark# (list-ref pieces 3))}]
                                                  [(regexp-match #px"^(\\s*)(.+?) (\\d+) - (.+?)\\s*$" line)
                                                   => {λ [pieces] (format ">   ~a- ~a ~a - ~a" (list-ref pieces 1)
                                                                          (cond [(string=? (list-ref pieces 2) (~result struct:test-success)) heart#]
                                                                                [(string=? (list-ref pieces 2) (~result struct:test-failure)) broken-heart#]
                                                                                [else bomb#])
                                                                          (list-ref pieces 3) (list-ref pieces 4))}]
                                                  [(regexp-match #px"^$" line) (summary? #true)]
                                                  [(regexp-match #px"wallclock" line) "> "]
                                                  [(summary?) (list (format "> ~a~a" pin# line) "> "
                                                                    (format "> [~a<sub>~a</sub>](http://gyoudmon.org/~~~a/.~a)"
                                                                            cat# (make-string (quotient (string-length line) 2) paw#)
                                                                            (getenv "USER") (string-downcase (current-digimon))))])))))})])})})

(define tamer-note
  {lambda unit-vars
    (define story-snapshot (tamer-story))
    (make-traverse-block
     {λ [get set]
       (define htag (tamer-story->tag story-snapshot))
       (unless (get 'scenario #false) (set 'scenario (make-hash)))
       (define scenarios (hash-ref! (get 'scenario {λ _ (make-hash)}) htag {λ _ (make-hash)}))
       (margin-note (for/list ([unit-var (in-list unit-vars)])
                      (define-values {/dev/tamer/stdin /dev/tamer/stdout} (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
                      (parameterize ([tamer-story story-snapshot]
                                     [current-input-port /dev/tamer/stdin])
                        (define unit (tamer-require unit-var))
                        (define unit-spec (make-parameter null))
                        (thread {λ _ (dynamic-wind {λ _ (collect-garbage)}
                                                   {λ _ (parameterize ([current-error-port /dev/tamer/stdout]
                                                                       [current-output-port /dev/tamer/stdout])
                                                          (define-values {brief cpu real gc} (time-apply prove (list unit)))
                                                          (define-values {success failure error}
                                                            (values (summary-success (car brief)) (summary-failure (car brief)) (summary-error (car brief))))
                                                          (for ([meta (in-list (list 'cpu 'real 'gc 'success 'failure 'error))]
                                                                [delta (in-list (list cpu real gc success failure error))])
                                                            (unless (get meta #false) (set meta (make-hash)))
                                                            (define pool (get meta {λ _ (make-hash)}))
                                                            (hash-set! pool (tamer-story) (+ (hash-ref pool (tamer-story) 0) delta)))
                                                          (cond [(zero? (+ failure error)) (printf "~n~a wall seconds~n" (~r (/ real 1000.0) #:precision '{= 3}))]
                                                                [else (printf "~n~a ~a~n" @~n_w[failure]{failure} @~n_w[error]{error})]))}
                                                   {λ _ (close-output-port /dev/tamer/stdout)})})
                        ((compose1 (curryr add-between (linebreak)) (curry filter-not void?))
                         (for/list ([line (in-port read-line)])
                           (cond [(regexp-match #px"^λ\\s+(.+)" line)
                                  => {λ [pieces] (let ([ctxt (list-ref pieces 1)])
                                                   (unit-spec (list ctxt)) ; Testsuite
                                                   (racketmetafont (italic (string open-book#)) ~ (elemtag ctxt (literal ctxt))))}]
                                 [(regexp-match #px"^\\s+λ(\\d+(.\\d)*)\\s+(.+?)\\s*$" line)
                                  => {λ [pieces] (racketparenfont (italic (string bookmark#)) ~ (literal (list-ref pieces 3)))}]
                                 [(regexp-match #px"^(\\s*)(.+?) (\\d+) - (.+?)\\s*$" line)
                                  => {λ [pieces] (match-let ([{list _ spc stts idx ctxt} pieces])
                                                   (when (string=? spc "") (unit-spec (list (box ctxt)))) ; Toplevel testcase
                                                   (define-values {stts# ftype} (cond [(string=? stts (~result struct:test-success)) (values heart# #false)]
                                                   #| follows error type conventions|# [(string=? stts (~result struct:test-error)) (values bomb# box)]
                                                                                       [else (values broken-heart# values)]))
                                                   (when (procedure? ftype) (unit-spec (cons (ftype ctxt) (unit-spec))))
                                                   ((if (string=? spc "") (curry elemtag ctxt) elem)
                                                    (string stts#) (racketvarfont ~ idx) (racketcommentfont ~ (literal ctxt))))}]
                                 [(regexp-match #px"^\\s*»» (.+?)?:?\\s+\"?(.+?)\"?\\s*$" line)
                                  => {λ [pieces] (let ([key (list-ref pieces 1)])
                                                   (unit-spec (cons (string-trim line) (unit-spec)))
                                                   (define type# (cond [(regexp-match? #px"message$" key) backhand#]
                                                                        [(regexp-match? #px"param:\\d" key) crystal-ball#]))
                                                   (unless (void? type#)
                                                     (elem #:style (make-style #false (list (make-color-property (list 128 128 128))))
                                                           (string type#) ~ (italic (literal (list-ref pieces 2))))))}]
                                 [(regexp-match #px"^$" line) (hash-set! scenarios unit (reverse (unit-spec)))]
                                 [else (when (hash-has-key? scenarios unit) (elem (string pin#) ~ (racketoutput line)
                                                                                  ~ (seclink (tamer-story->tag (tamer-story))
                                                                                             ~ (string house-garden#) (smaller (string cat#)))))]))))))})})

(define tamer-racketbox
  {lambda [path]
    (define story-snapshot (tamer-story))
    (make-traverse-block
     {λ _ (parameterize ([tamer-story story-snapshot])
            (define /path/file (simplify-path (if (symbol? path) (dynamic-require/expose (tamer-story) path) path)))
            (nested #:style (make-style "boxed" null)
                    (filebox (hyperlink /path/file (italic (string memo#) ~ (path->string (tr-if-path /path/file))))
                             (codeblock #:line-numbers 0 #:keep-lang-line? #false
                                        (file->string /path/file)))))})})

{module digitama racket
  (require rackunit)
  (require racket/undefined)
  (require syntax/location)

  (require "digicore.rkt")
  
  (provide (all-defined-out) quote-module-path)
  
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

  (define-values {handbook-stories handbook-records} (values (make-hash) (make-hash)))

  (define tamer-record-story
    {lambda [name unit]
      (define htag (tamer-story->tag (tamer-story)))
      (define units (hash-ref handbook-stories htag null))
      (unless (assoc name units)
        (hash-set! handbook-stories htag
                   (cons (cons name unit) units)))
      (let ([books (hash-ref handbook-stories books# null)])  ;;; Readme.md needs it stay here
        (unless (member htag books) (hash-set! handbook-stories books# (cons htag books))))})

  (define tamer-record-handbook
    {lambda [name:case«suites action]
      (define case-name (car name:case«suites))
      (hash-ref! (hash-ref! handbook-records (tamer-story) (make-hash))
                 (string-join name:case«suites " « ")
                 {λ _ (call-with-escape-continuation
                       {λ [return] (parameterize ([current-error-port (open-output-string '/dev/case/stderr)]
                                                  [exit-handler {λ [v] (let* ([errmsg (string-trim (get-output-string (current-error-port)))]
                                                                              [routine {λ _ (with-check-info {{'exitcode v}} (fail errmsg))}])
                                                                         (return (run-test-case case-name routine)))}])
                                     (return (run-test-case case-name action)))})})})
  
  (define indents (make-hash))
  (define ~indent
    {lambda [count #:times [times 2] #:padchar [padding #\space]]
      (define key (format "~a~a" count padding))
      (hash-ref! indents key {λ _ (make-string (* count times) padding)})})
    
  (define ~result
    {lambda [result]
      (define indicators (hash (object-name struct:test-error) "#?"
                               (object-name struct:test-success) "#t"
                               (object-name struct:test-failure) "#f"))
      (hash-ref indicators (object-name result))})

  (define ~line
    {lambda [line]
      (format "~a~a" line (make-string (- 72 (remainder (string-length line) 72)) #\space))})

  (define ~url
    {lambda [projname]
      (format "http://gyoudmon.org/~~~a/.~a" (getenv "USER") (string-downcase projname))})
  
  (define exn->test-case
    {lambda [name e]
      (delay-test (test-case (format "(~a ⧴ ~a)" name (object-name e))
                             (raise e) #| no thunk, make test-error |#))})

  (define tr-d (curryr string-replace (digimon-world) ""))
  (define tr-if-path {λ [p] (if (path? p) (build-path (tr-d (format "~a" p))) p)})
  
  (define display-failure
    {lambda [result #:indent [headspace ""]]
      (define echo {λ [key val] (eechof #:fgcolor 'red "~a»» ~a: ~s~n" headspace key val)})
      (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
        (cond [(symbol=? (check-info-name info) 'params) (for ([param (in-list (map tr-if-path (check-info-value info)))]
                                                               [index (in-naturals 1)])
                                                           (echo (format "param:~a" index) param))]
              [else (echo (check-info-name info) (case (check-info-name info)
                                                   [{location} (tr-d (srcloc->string (apply srcloc (check-info-value info))))]
                                                   [{exception-message} (tr-d (check-info-value info))]
                                                   [{exception} (object-name (check-info-value info))]
                                                   [else ((if (string? (check-info-value info)) tr-d tr-if-path) (check-info-value info))]))]))})
  
  (define display-error
    {lambda [result #:indent [headspace0 ""]]
      (define errobj (test-error-result result))
      (define messages (call-with-input-string (tr-d (exn-message errobj)) port->lines))
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
      (define seed (parameterize ([current-custodian (make-custodian)]) ;;; Prevent test routines happen to shutdown the custodian by creating am empty subone.
                     (define $exn (make-parameter undefined))
                     (foldts-test-suite {λ [testsuite name pre-action post-action seed]
                                          (with-handlers ([exn? $exn])
                                            (pre-action))
                                          (tamer-seed (fdown name (tamer-seed-datum seed))
                                                      (tamer-seed-brief seed )
                                                      (cons name (tamer-seed-name-path seed)))}
                                        {λ [testsuite name pre-action post-action seed children-seed]
                                          (with-handlers ([exn? (compose1 display-error (curry make-test-error (format "Postaction[~a]" name)))])
                                            (post-action))
                                          ($exn undefined)
                                          (tamer-seed (fup name (tamer-seed-datum seed) (tamer-seed-datum children-seed))
                                                      (tamer-seed-brief children-seed)
                                                      (tamer-seed-name-path seed))}
                                        {λ [testcase name action seed]
                                          (define-values {fixed-name fixed-action}
                                            (cond [(false? (eq? ($exn) undefined)) (values (format "Preaction[~a]" name) {λ _ (raise ($exn))})]
                                                  [(false? name) (values (format "(⧴ ~a)" (object-name struct:exn:fail:user))
                                                                         {λ _ (raise-user-error "Testcase must have a name!")})]
                                                  [else (values name action)]))
                                          (define fixed-namepath (cons fixed-name (tamer-seed-name-path seed)))
                                          (define record (tamer-record-handbook fixed-namepath fixed-action))
                                          (tamer-seed (fhere record (tamer-seed-datum seed))
                                                      (summary++ (tamer-seed-brief seed) record)
                                                      fixed-namepath)}
                                        (tamer-seed seed:datum initial-summary null)
                                        testsuite)))
      (values (tamer-seed-datum seed) (tamer-seed-brief seed))})
  
  (define prove
    {lambda [unit]
      (define-values {whocares brief}
        (fold-test-suite #:fdown {λ [name seed:ordered]
                                   (cond [(null? seed:ordered) (echof #:fgcolor 202 #:attributes '{underline} "λ ~a~a~n" (~indent 0) (tr-d name))]
                                         [else (echof "~aλ~a ~a~n" (~indent (length seed:ordered))
                                                      (string-join (map number->string (reverse seed:ordered)) ".") (tr-d name))])
                                   (cons 1 seed:ordered)}
                         #:fup {λ [name seed:ordered children:ordered]
                                 (cond [(null? seed:ordered) null]
                                       [else (cons (add1 (car seed:ordered))
                                                   (cdr seed:ordered))])}
                         #:fhere {λ [result seed:ordered]
                                   (define headline (format "~a~a ~a - " (~indent (length seed:ordered)) (~result result)
                                                            (if (null? seed:ordered) 1 (car seed:ordered))))
                                   (define headspace (~indent (string-length headline) #:times 1))
                                   (cond [(test-success? result) (void (echof #:fgcolor 'lightgreen "~a~a~n" headline (tr-d (test-result-test-case-name result))))]
                                         [(test-failure? result) (void (echof #:fgcolor 'lightred "~a~a~n" headline (tr-d (test-result-test-case-name result)))
                                                                       (display-failure result #:indent headspace))]
                                         [(test-error? result) (void (echof #:fgcolor 'red "~a~a~n" headline (tr-d (test-result-test-case-name result)))
                                                                     (display-error result #:indent headspace))]
                                         [else (error "RackUnit has new test result type added!")])
                                   (if (null? seed:ordered) null (cons (add1 (car seed:ordered)) (cdr seed:ordered)))}
                         null
                         unit))
      (values brief)})}

(require (submod "." digitama))
