#lang at-exp racket

(require rackunit)

(require racket/sandbox)

(require scribble/core)
(require scribble/eval)
(require scribble/manual)
(require scribble/html-properties)

(require (for-syntax syntax/parse))

@require{digicore.rkt}

(provide (all-defined-out) tamer-story)

(provide (all-from-out racket "digicore.rkt" rackunit))
(provide (all-from-out scribble/manual scribble/eval scribble/html-properties))

(define tamer-zone (make-parameter #false))

(define $out (open-output-bytes '/dev/tamer/stdout))
(define $err (open-output-bytes '/dev/tamer/stderr))
(define $? (make-parameter +NaN.0))

(define call-with-fresh-$
  (lambda [routine . arglist]
    (get-output-bytes $out #true)
    (get-output-bytes $err #true)
    ($? +NaN.0)
    (parameterize ([current-output-port $out]
                   [current-error-port $err]
                   [exit-handler $?])
      (apply routine arglist))))

(define make-tamer-zone
  (lambda []
    (define tamer.rkt (build-path (digimon-tamer) "tamer.rkt"))
    (dynamic-require (tamer-story) #false)
    (define tamer-namespace (module->namespace (tamer-story)))
    (parameterize ([sandbox-namespace-specs (cons (thunk tamer-namespace) null)])
      (make-base-eval #:pretty-print? #true))))

(define-syntax {tamer-taming-start stx}
  #'(let ([modpath (quote-module-path)])
      (cond [(path? modpath) (tamer-story (tamer-story->modpath modpath))]
            [else (and (tamer-story (tamer-story->modpath (cadr modpath)))
                       (tamer-zone (make-tamer-zone)))])))

(define-syntax {handbook-story stx}
  (syntax-parse stx #:literals []
    [{_ (~optional (~seq #:style s:expr)) contents ...}
     #`(list (tamer-taming-start)
             (title #:tag (tamer-story->tag (tamer-story))
                    #:style #,(attribute s)
                    "Story: " contents ...))]))

(define-syntax {define-tamer-suite stx}
  (syntax-parse stx
    [{_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) units ...}
     #`(tamer-record-story 'varid (test-suite name
                                              #:before #,(or (attribute setup) #'void)
                                              #:after #,(or (attribute teardown) #'void)
                                              units ...))]))

(define-syntax {define-tamer-case stx}
  (syntax-parse stx
    [{_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...}
     #'(tamer-record-story 'varid (delay-test (test-spec name
                                                         #:before setup
                                                         checks ...
                                                         #:after teardown)))]))

(define-syntax {test-spec stx}
  (syntax-parse stx
    [{_ name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...}
     #`(test-case name (around (#,(or (attribute setup) #'void))
                               checks ...
                               (#,(or (attribute teardown) #'void))))]))

(define-syntax {tamer-action stx}
  (syntax-case stx []
    [{_ s-exps ...}
     #'(let ([story-snapshot (tamer-story)]
             [zone-snapshot (tamer-zone)])
         (make-traverse-block
          (thunk* (parameterize ([tamer-story story-snapshot]
                                 [tamer-zone zone-snapshot])
                    (interaction #:eval (tamer-zone) s-exps ...)))))]))

(define tamer-require
  (lambda [name]
    (define htag (tamer-story->tag (tamer-story)))
    (define units (hash-ref handbook-stories htag null))
    (with-handlers ([exn? (λ [e] (let ([story (exn->test-case 'tamer-require e)])
                                   (hash-set! handbook-stories htag (cons (cons name story) units))
                                   story))])
      (dict-ref units name (thunk (raise (make-exn:fail:contract:variable (format "'~a has not yet defined!" name)
                                                                          (current-continuation-marks) name)))))))

(define tamer-story->modpath
  (lambda [story-path]
    `(submod ,story-path story)))

(define tamer-partner->modpath
  (lambda [partner-path]
    (path->digimon-modpath (if (absolute-path? partner-path) partner-path (build-path (digimon-zone) partner-path)))))

(define handbook-title
  (lambda pre-contents
    (define info-ref (get-info/full (digimon-zone)))
    (define gnome-stone (parameterize ([current-digimon (digimon-gnome)]) (digimon-stone)))
    (title #:style (let* ([stones (remove-duplicates (list (digimon-stone) gnome-stone))]
                          [css (filter file-exists? (map (curryr build-path "tamer.css") stones))])
                     (make-style #false (map make-css-addition css)))
           #:tag "tamerbook" #:version (format "~a[~a]" (version) (info-ref 'version (const "Baby")))
           (if (symbol=? (object-name (current-input-port)) '/dev/null)
               (list (hyperlink (~url (current-digimon)) (string house-garden#))
                     (hyperlink (~url (digimon-gnome)) (format "<sub>~a</sub>" cat#)))
               (list (hyperlink (~github (current-digimon)) (string house-garden#))
                     (hyperlink (~github (current-tamer)) (subscript (string cat#)))))
           (cond [(false? (null? pre-contents)) pre-contents]
                 [else (list (literal "Tamer's Handbook:") ~ (info-ref 'collection (const (current-digimon))))]))))

(define handbook-scenario
  (lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (section #:tag tag #:style style
             "Scenario: " pre-contents)))

(define handbook-appendix
  (lambda [#:style [style #false] . pre-contents]
    (list (section #:style style
                   (cond [(false? (null? pre-contents)) (cons "Appendix: " pre-contents)]
                         [else (string-titlecase (format "Appendix: ~a Auxiliaries"
                                                         (path-replace-suffix (tamer-story->tag (tamer-story)) "")))]))
          (let ([zone-snapshot (tamer-zone)])
            (make-traverse-block (thunk* ((curry dynamic-wind void)
                                          (thunk (para #:style "GYDMComment"
                                                       "In order to avoid polluting your eyes, "
                                                       "any less important things are moved here. "
                                                       "(also see "
                                                       (hyperlink (~a (digimon-tamer) "/tamer.rkt") "tamer.rkt")
                                                       ")"))
                                          (thunk (close-eval zone-snapshot))))))
          (tamer-story #false))))

(define handbook-rule
  (lambda pre-flow
    (itemlist (item (bold (deftech (format "Rule #~a" (rule-index)))) ~ pre-flow))))

(define itech
  (lambda [#:key [key #false] . pre-contents]
    (tech (italic pre-contents) #:key key)))

(define tamer-prove
  (lambda []
    (define suite (if (module-path? (tamer-story))
                      (let ([htag (tamer-story->tag (tamer-story))])
                        (and (dynamic-require (tamer-story) #false)
                             (hash-has-key? handbook-stories htag)
                             (make-test-suite htag (reverse (map cdr (hash-ref handbook-stories htag))))))
                      (or (zero? (hash-count handbook-stories)) ; no story ==> no :books:
                          (let ([href (curry hash-ref handbook-stories)])
                            (make-test-suite "Behaviors and Features"
                                             (for/list ([unit (in-list (reverse (href books#)))])
                                               (make-test-suite unit (reverse (map cdr (href unit))))))))))
    (if (false? (test-suite? suite))
        (and (echof #:fgcolor 'yellow "~nNo particular example!~n") 0)
        (let-values ([{brief-box cpu0 real0 gc0} (time-apply prove (list suite))])
          (define-values {success failure error real cpu-gc gc cpu}
            (apply values (list* (summary-success (car brief-box))
                                 (summary-failure (car brief-box))
                                 (summary-error (car brief-box))
                                 (map (compose1 (curry ~r #:precision '{= 3}) (curry * 0.001))
                                      (list real0 (- cpu0 gc0) gc0 cpu0)))))
          (define population (+ success failure error))
          (define echo (curry echof #:fgcolor 'lightcyan))
          (echo "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)." real cpu-gc gc cpu)
          (echo "~n~a, ~a, ~a, ~a% Okay.~n"
                @~n_w[population]{example} @~n_w[failure]{failure} @~n_w[error]{error}
                (~r (/ (* success 100) population) #:precision '{= 2}))
          (+ failure error)))))

(define tamer-smart-summary
  (lambda []
    (define story-snapshot (tamer-story))
    (make-traverse-block
     (λ [get set]
       (if (member 'markdown (get 'scribble:current-render-mode '{html}))
           (para (literal "---"))
           (make-delayed-block
            (λ [render% pthis infobase]
              (define get (curry hash-ref (collect-info-fp (resolve-info-ci infobase))))
              (define info-ref (get-info/full (digimon-zone)))
              (define {story-ref htag}
                (filter-map (λ [scnr] (hash-ref (hash-ref (get 'scenario make-hash) htag make-hash) (cdr scnr) #false))
                            (reverse (hash-ref handbook-stories htag null))))
              (nested #:style (make-style "boxed" null)
                      (filebox (if (module-path? story-snapshot)
                                   (italic (seclink "tamerbook" (string open-book#)) ~
                                           (~a "Behaviors in " (tamer-story->tag story-snapshot)))
                                   (italic (string books#) ~
                                           (~a "Behaviors of " (info-ref 'collection (const (current-digimon))))))
                               (let ([base (if (module-path? story-snapshot)
                                               (story-ref (tamer-story->tag story-snapshot))
                                               (for/list ([story (in-list (reverse (hash-ref handbook-stories books# null)))])
                                                 (cons story (with-handlers ([exn:fail:contract? (const null)])
                                                               (apply append (map cdr (story-ref story)))))))])
                                 (define items (for/list ([spec (in-list base)])
                                                 ;;; see (tamer-note) to check type conventions
                                                 (define-values {local# desc}
                                                   (cond [(string? (car spec)) (values bookmark# (car spec))]
                                                         [else (values page# (unbox (car spec)))]))
                                                 (define-values {status color}
                                                   (cond [(null? (cdr spec)) (values (~result struct:test-success) 'lightgreen)]
                                                         [(ormap box? (cdr spec)) (values (~result struct:test-error) 'red)]
                                                         [else (values (~result struct:test-failure) 'lightred)]))
                                                 (define item (~a #:width 61 #:pad-string "." #:limit-marker "......" desc))
                                                 (define label (racketkeywordfont (literal item) status))
                                                 (if (module-path? story-snapshot)
                                                     (elemref desc (italic (string local#)) ~ label)
                                                     (and (echof #:fgcolor 'lightyellow item)
                                                          (echof #:fgcolor color "~a~n" status)
                                                          (for ([msg (in-list (cdr spec))])
                                                            (cond [(and (box? msg) (unbox msg))
                                                                   => (curry eechof #:fgcolor 'red #:attributes '{inverse} "~a~n")]
                                                                  [(and (regexp-match? #px"»»" msg) msg)
                                                                   => (curry eechof #:fgcolor 245 "~a~n")]
                                                                  [(and (string? (car spec)) msg)
                                                                   => (curry eechof #:fgcolor 'lightred "~a~n")]))
                                                          (seclink (car spec) (italic (string book#)) ~ label)))))
                                 (match-define {list success failure error reals gcs cpus}
                                   (for/list ([meta (in-list (list 'success 'failure 'error 'real 'gc 'cpu))])
                                     (define pool (get meta make-hash))
                                     (if (module-path? story-snapshot)
                                         (hash-ref pool story-snapshot 0)
                                         (foldl + 0 (hash-values pool)))))
                                 (match-define {list real cpu-gc gc cpu}
                                   (map (compose1 (curry ~r #:precision '{= 3}) (curry * 0.001))
                                        (list reals (- cpus gcs) gcs cpus)))
                                 (define briefs
                                   (let ([population (+ success failure error)])
                                     (if (zero? population)
                                         (list "No particular test!")
                                         (list (format "~a% tests successful."
                                                       (~r #:precision '{= 2} (/ (* success 100) population)))
                                               (format "~a, ~a, ~a, ~a."
                                                       @~w=n[(length base) (if story-snapshot "Scenario" "Story")]
                                                       @~w=n[population]{Test} @~w=n[failure]{Failure} @~w=n[error]{Error})
                                               (format "~a wallclock seconds (~a task + ~a gc = ~a CPU)."
                                                       real cpu-gc gc cpu)))))
                                 (unless (module-path? story-snapshot)
                                   (newline)
                                   (for ([brief (in-list briefs)])
                                     (echof #:fgcolor 'lightcyan "~a~n" brief)))
                                 (let ([lb (list (linebreak))])
                                   (append (cond [(null? items) null]
                                                 [else (add-between #:after-last lb #:splice? #true items lb)])
                                           (add-between #:before-first lb #:splice? #true
                                                        (map racketoutput briefs) lb)))))))))))))

(define handbook-smart-table
  (lambda []
    (make-traverse-block
     (λ [get set]
       (if (false? (member 'markdown (get 'scribble:current-render-mode '{html})))
           (table-of-contents)
           (make-delayed-block
            (λ [render% pthis _]
              (define-values {/dev/tamer/stdin /dev/tamer/stdout} (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
              (parameterize ([current-input-port /dev/tamer/stdin]
                             [current-error-port /dev/tamer/stdout]
                             [current-output-port /dev/tamer/stdout]
                             [tamer-story #false])
                (define summary? (make-parameter #false))
                (thread (thunk (dynamic-wind collect-garbage
                                             tamer-prove
                                             (thunk (close-output-port /dev/tamer/stdout)))))
                (para (filter-map (λ [line] (and (not (void? line)) (map ~markdown (if (list? line) line (list line)))))
                                  (for/list ([line (in-port read-line)])
                                    (cond [(regexp-match #px"^λ\\s+(.+)" line)
                                           => (λ [pieces] (format "> + ~a~a" books# (list-ref pieces 1)))]
                                          [(regexp-match #px"^(\\s+)λ\\d+\\s+(.+?.rktl?)\\s*$" line)
                                           => (λ [pieces] (match-let ([{list _ indt ctxt} pieces]) ; (markdown listitem needs at least 1 char after "+ "
                                                            (list (format ">   ~a+ ~a" indt open-book#)  ; before breaking line if "[~a](~a)" is longer
                                                                  (hyperlink (format "~a/~a" (~url (current-digimon)) ctxt) ctxt))))] ; then 72 chars.)
                                          [(regexp-match #px"^(\\s+)λ\\d+(.\\d)*\\s+(.+?)\\s*$" line)
                                           => (λ [pieces] (format ">   ~a+ ~a~a" (list-ref pieces 1) bookmark# (list-ref pieces 3)))]
                                          [(regexp-match #px"^(\\s*)(.+?) (\\d+) - (.+?)\\s*$" line)
                                           => (λ [pieces] (apply format ">   ~a- ~a ~a - ~a" (cdr pieces)))]
                                          [(regexp-match #px"^$" line) (summary? #true)]
                                          [(regexp-match #px"wallclock" line) "> "]
                                          [(summary?) (format "> ~a~a" pin# line)]))))))))))))

(define tamer-note
  (lambda unit-vars
    (define story-snapshot (tamer-story))
    (make-traverse-block
     (λ [get set]
       (define htag (tamer-story->tag story-snapshot))
       (unless (get 'scenario #false) (set 'scenario (make-hash)))
       (define scenarios (hash-ref! (get 'scenario make-hash) htag make-hash))
       (margin-note (for/list ([unit-var (in-list unit-vars)])
                      (define-values {/dev/tamer/stdin /dev/tamer/stdout} (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
                      (parameterize ([tamer-story story-snapshot]
                                     [current-readtable readwrotten]
                                     [current-input-port /dev/tamer/stdin])
                        (define unit (tamer-require unit-var))
                        (define unit-spec (make-parameter null))
                        (thread (λ _ (dynamic-wind (thunk (collect-garbage))
                                                   (thunk (parameterize ([current-error-port /dev/tamer/stdout]
                                                                         [current-output-port /dev/tamer/stdout])
                                                            (define-values {brief cpu real gc} (time-apply prove (list unit)))
                                                            (define-values {success failure error}
                                                              (values (summary-success (car brief))
                                                                      (summary-failure (car brief))
                                                                      (summary-error (car brief))))
                                                            (for ([meta (in-list (list 'cpu 'real 'gc 'success 'failure 'error))]
                                                                  [delta (in-list (list cpu real gc success failure error))])
                                                              (unless (get meta #false) (set meta (make-hash)))
                                                              (define pool (get meta make-hash))
                                                              (hash-set! pool (tamer-story) (+ (hash-ref pool (tamer-story) 0) delta)))
                                                            (if (zero? (+ failure error))
                                                                (printf "~n~a wall seconds~n" (~r (/ real 1000.0) #:precision '{= 3}))
                                                                (printf "~n~a ~a~n" @~n_w[failure]{failure} @~n_w[error]{error}))))
                                                   (thunk (close-output-port /dev/tamer/stdout)))))
                        ((compose1 (curryr add-between (linebreak)) (curry filter-not void?))
                         (for/list ([line (in-port read-line)])
                           (cond [(regexp-match #px"^λ\\s+(.+)" line)
                                  => (λ [pieces] (let ([ctxt (list-ref pieces 1)])
                                                   (unit-spec (list ctxt)) ; Testsuite
                                                   (racketmetafont (italic (string open-book#)) ~ (elemtag ctxt (literal ctxt)))))]
                                 [(regexp-match #px"^\\s+λ(\\d+(.\\d)*)\\s+(.+?)\\s*$" line)
                                  => (λ [pieces] (racketoutput (italic (string bookmark#)) ~ (literal (list-ref pieces 3))))]
                                 [(regexp-match #px"^(\\s*)(.+?) (\\d+) - (.+?)\\s*$" line)
                                  => (λ [pieces] (match-let ([{list _ spc stts idx ctxt} pieces])
                                                   (when (string=? spc "") (unit-spec (list (box ctxt)))) ; Toplevel testcase
                                                   (define ftype (cond [(string=? stts (~result struct:test-success)) #false]
                                                                       [(string=? stts (~result struct:test-error)) box]
                                                                       [else values]))
                                                   (when (procedure? ftype) (unit-spec (cons (ftype ctxt) (unit-spec))))
                                                   ((if (string=? spc "") (curry elemtag ctxt) elem)
                                                    stts (racketkeywordfont ~ (italic idx)) (racketcommentfont ~ (literal ctxt)))))]
                                 [(regexp-match #px"^\\s*»» (.+?)?:?\\s+(.+?)\\s*$" line)
                                  => (λ [pieces] (match-let ([{list _ key val} pieces])
                                                   (unit-spec (cons (string-trim line) (unit-spec)))
                                                   (cond [(string=? "message" key)
                                                          (elem #:style (make-style #false (list (make-color-property (list 128 128 128))))
                                                                (string backhand#) ~ (italic (literal val)))]
                                                         [(member key '{"exn" "exception"})
                                                          (elem (racketvalfont (string macroscope#)) ~
                                                                (racket #,(let ([ev (fix (read (open-input-string val)))]
                                                                                [tr (curryr call-with-input-string read-line)])
                                                                            (vector-set! ev 1 (tr (vector-ref ev 1))) ev)))]
                                                         [(regexp-match? #px"param:\\d+" key)
                                                          (elem (racketvalfont (string crystal-ball#)) ~
                                                                (racket #,(fix (read (open-input-string val)))))])))]
                                 [(regexp-match #px"^\\s*»»» \\s+(expected|given|received):\\s+(.+?)\\s*$" line)
                                  ; only for errors those have multilined messages.
                                  => (λ [pieces] (and (unit-spec (cons (string-trim line) (unit-spec)))
                                                      (elem (racketvalfont (string paw#)) ~
                                                            (let ([message (list-ref pieces 2)])
                                                              (racket #,(fix (with-handlers ([exn? (λ _ message)])
                                                                               (read (open-input-string message)))))))))]
                                 [(regexp-match #px"^$" line) (hash-set! scenarios unit (reverse (unit-spec)))]
                                 [(hash-has-key? scenarios unit)
                                  (elem (string pin#) ~ ((if (regexp-match? #px"error" line) racketerror racketresultfont) line) ~
                                        (seclink (tamer-story->tag (tamer-story)) ~
                                                 (string house-garden#)
                                                 (smaller (string cat#))))]))))))))))

(define tamer-racketbox
  (lambda [path #:line-start-with [line0 0]]
    (define story-snapshot (tamer-story))
    (make-traverse-block
     (thunk* (parameterize ([tamer-story story-snapshot])
               (define /path/file (simplify-path (if (symbol? path) (dynamic-require/expose (tamer-story) path) path)))
               (nested #:style (make-style "boxed" null)
                       (filebox (hyperlink /path/file (italic (string memo#) ~ (path->string (tr-if-path /path/file))))
                                (codeblock #:line-numbers line0 #:keep-lang-line? (false? (zero? line0))
                                           (file->string /path/file)))))))))

{module digitama racket
  (require rackunit)
  (require racket/generator)
  (require racket/undefined)
  (require syntax/location)
  (require setup/xref)
  (require scribble/core)
  (require scribble/xref)
  (require scribble/manual)

  (require "digicore.rkt")
  
  (provide (all-defined-out) quote-module-path)

  (define tamer-story (make-parameter #false))
  
  (define tamer-story->tag
    (lambda [story]
      (with-handlers ([exn? exn-message])
        (path->string (find-relative-path (digimon-tamer) (cadr story))))))
  
  (struct tamer-seed {datum brief name-path})
  (struct summary {success failure error})
  (define initial-summary (summary 0 0 0))
  
  (define summary++
    (lambda [summary0 result]
      (summary (+ (summary-success summary0) (if (test-success? result) 1 0))
               (+ (summary-failure summary0) (if (test-failure? result) 1 0))
               (+ (summary-error summary0) (if (test-error? result) 1 0)))))

  (define summary**
    (lambda [summary1 summary2]
      (summary (+ (summary-success summary1) (summary-success summary2))
               (+ (summary-failure summary1) (summary-failure summary2))
               (+ (summary-error summary1) (summary-error summary2)))))

  (define-values {handbook-stories handbook-records} (values (make-hash) (make-hash)))

  (define tamer-record-story
    (lambda [name unit]
      (define htag (tamer-story->tag (tamer-story)))
      (define units (hash-ref handbook-stories htag null))
      (unless (dict-has-key? units name)
        (hash-set! handbook-stories htag
                   (cons (cons name unit) units)))
      (let ([books (hash-ref handbook-stories books# null)])  ;;; Readme.md needs it stay here
        (unless (member htag books) (hash-set! handbook-stories books# (cons htag books))))))

  (define tamer-record-handbook
    (lambda [name:case«suites action]
      (define case-name (car name:case«suites))
      (hash-ref! (hash-ref! handbook-records (tamer-story) make-hash)
                 (string-join name:case«suites " « ")
                 (thunk (let/ec return
                          (parameterize ([current-error-port (open-output-string '/dev/case/stderr)]
                                         [exit-handler (λ [v] (let* ([errmsg (string-trim (get-output-string (current-error-port)))]
                                                                     [routine (thunk (with-check-info {{'exitcode v}} (fail errmsg)))])
                                                                (return (run-test-case case-name routine))))])
                            (return (run-test-case case-name action))))))))

  (define rule-index
    (generator []
      (let loop ([id (in-naturals 1)])
        (yield (stream-first id))
        (loop (stream-rest id)))))
    
  (define ~result
    (lambda [result]
      (define indicators (hash (object-name struct:test-error) (string bomb#)
                               (object-name struct:test-success) (string heart#)
                               (object-name struct:test-failure) (string broken-heart#)))
      (hash-ref indicators (object-name result))))

  (define ~markdown
    (lambda [line]
      (define padding (λ [line] (make-string (- 72 (remainder (string-length (format "~a" line)) 72)) #\space)))
      (cond [(string? line) (literal (format "~a~a" line (padding line)))]
            [else (list line (literal (padding (car (element-content line)))))])))

  (define ~url
    (lambda [digimon]
      (format "http://gyoudmon.org/~~~a:~a" (current-tamer) digimon)))

  (define ~github
    (lambda [projname]
      (format "https://github.com/digital-world/~a" projname)))
  
  (define exn->test-case
    (lambda [name e]
      (delay-test (test-case (format "(~a ⧴ ~a)" name (object-name e))
                             (raise e) #| no thunk, make test-error |#))))

  (define tr-d (curryr string-replace (digimon-world) ""))
  (define tr-if-path (λ [p] (if (path? p) (build-path (tr-d (format "~a" p))) p)))

  (define readwrotten
    (make-readtable (current-readtable)
                    #\< 'dispatch-macro
                    (λ [< port [src #false] [line #false] [col #false] [pos #false]]
                      (fix (match (regexp-match #px"<?(procedure|path)?(:)?(.+?)?>" port)
                             [{list _ #"path" _ fname} (string->path (bytes->string/utf-8 fname))]
                             [{list _ _ #false #false} '{lambda _ ...}]
                             [{list _ #false #false <something-type/value>} (string->symbol (format "~a?" <something-type/value>))]
                             [{list _ _ _ {pregexp #px"function\\.rkt"}} (cons negate 'λ)]
                             [{list _ _ _ #"composed"} '{compose λ ...}]
                             [{list _ _ _ #"curried"} '{curry λ ...}]
                             [{list _ _ _ name} (with-handlers ([exn? (λ [ev] (list procedure-rename 'λ (exn:fail:contract:variable-id ev)))])
                                                  (eval (string->symbol (bytes->string/utf-8 name))
                                                        (let ([mod (build-path (digimon-tamer) "tamer.rkt")])
                                                          (dynamic-require mod #false)
                                                          (module->namespace mod))))])))))

  (define fix
    (lambda [val]
      (cond [(or (procedure? val) (symbol? val))
             (let*-values ([{modpath} (build-path (digimon-tamer) "tamer.rkt")]
                           [{export} (or (object-name val) val)]
                           [{xref} (load-collections-xref)]
                           [{tag} (xref-binding->definition-tag xref (list modpath export) #false)]
                           [{path anchor} (with-handlers ([exn? (λ _ (values #false #false))])
                                            (xref-tag->path+anchor xref tag #:external-root-url #false))])
               (or (and path anchor (racketvalfont (hyperlink (format "~a#~a" path anchor) (symbol->string export)))) val))]
            [(vector? val) #| also for (struct? val) |#
             (vector-map fix val)]
            [(pair? val) #| also for lists |#
             (cons (fix (car val)) (fix (cdr val)))]
            [else val])))

  (define display-failure
    (lambda [result #:indent [headspace ""]]
      (define echo (curry eechof #:fgcolor 'red "~a»» ~a: ~s~n" headspace))
      (define recho (curry eechof #:fgcolor 'red "~a»»» ~a~a~n" headspace))
      (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
        (case (check-info-name info)
          [{params} (for ([param (in-list (map tr-if-path (check-info-value info)))]
                          [index (in-naturals 1)])
                      (echo (format "param:~a" index) param))]
          [{message} (let ([messages (call-with-input-string (tr-d (check-info-value info)) port->lines)])
                       (echo "message" (car messages))
                       (for-each (curry recho (~a #:min-width 8)) (cdr messages)))]
          [else (echo (check-info-name info)
                      (case (check-info-name info)
                        [{location} (tr-d (srcloc->string (apply srcloc (check-info-value info))))]
                        [{exception-message} (tr-d (check-info-value info))]
                        [else ((if (string? (check-info-value info)) tr-d tr-if-path) (check-info-value info))]))]))))
  
  (define display-error
    (lambda [result #:indent [headspace0 ""]]
      (define errobj (test-error-result result))
      (define messages (call-with-input-string (tr-d (exn-message errobj)) port->lines))
      (eechof #:fgcolor 'red #:attributes '{inverse} "~a»» name: ~a~n" headspace0 (object-name errobj))
      (unless (null? messages)
        (define msghead " message: ")
        (define msgspace (~a #:min-width (sub1 (string-length msghead))))
        (eechof #:fgcolor 'red #:attributes '{inverse} "~a»»~a~a~n" headspace0 msghead (car messages))
        (for-each (curry eechof #:fgcolor 'red #:attributes '{inverse} "~a»»»~a~a~n" headspace0 msgspace) (cdr messages)))
      (for ([stack (in-list (continuation-mark-set->context (exn-continuation-marks errobj)))])
        (when (cdr stack)
          (define srcinfo (srcloc->string (cdr stack)))
          (unless (or (false? srcinfo) (regexp-match? #px"^/" srcinfo))
            (eechof #:fgcolor 245 "~a»»»» ~a: ~a~n" headspace0
                    (tr-d srcinfo) (or (car stack) 'λ)))))))
  
  (define default-fseed
    (lambda last-is-seed
      (last last-is-seed)))
  
  (define fold-test-suite
    (lambda [seed:datum testsuite #:fdown [fdown default-fseed] #:fup [fup default-fseed] #:fhere [fhere default-fseed]]
      (define seed (parameterize ([current-custodian (make-custodian)]) ;;; Prevent test routines happen to shutdown the custodian by creating am empty subone.
                     (define $exn (make-parameter undefined))
                     (foldts-test-suite (λ [testsuite name pre-action post-action seed]
                                          (with-handlers ([exn? $exn])
                                            (call-with-values pre-action void))
                                          (tamer-seed (fdown name (tamer-seed-datum seed))
                                                      (tamer-seed-brief seed )
                                                      (cons name (tamer-seed-name-path seed))))
                                        (λ [testsuite name pre-action post-action seed children-seed]
                                          (with-handlers ([exn? (compose1 display-error (curry make-test-error (format "Postaction[~a]" name)))])
                                            (call-with-values post-action void))
                                          ($exn undefined)
                                          (tamer-seed (fup name (tamer-seed-datum seed) (tamer-seed-datum children-seed))
                                                      (tamer-seed-brief children-seed)
                                                      (tamer-seed-name-path seed)))
                                        (λ [testcase name action seed]
                                          (define-values {fixed-name fixed-action}
                                            (cond [(false? (eq? ($exn) undefined)) (values (format "#:before ~a" name) (λ _ (raise ($exn))))]
                                                  [(false? name) (values (format "(⧴ ~a)" (object-name struct:exn:fail:user))
                                                                         (λ _ (raise-user-error "Testcase must have a name!")))]
                                                  [else (values name action)]))
                                          (define fixed-namepath (cons fixed-name (tamer-seed-name-path seed)))
                                          (define record (tamer-record-handbook fixed-namepath fixed-action))
                                          (tamer-seed (fhere record (tamer-seed-datum seed))
                                                      (summary++ (tamer-seed-brief seed) record)
                                                      fixed-namepath))
                                        (tamer-seed seed:datum initial-summary null)
                                        testsuite)))
      (values (tamer-seed-datum seed) (tamer-seed-brief seed))))
  
  (define prove
    (lambda [unit]
      (define-values {whocares brief}
        (fold-test-suite #:fdown (λ [name seed:ordered]
                                   (cond [(null? seed:ordered) (echof #:fgcolor 202 #:attributes '{underline} "λ ~a~n" (tr-d name))]
                                         [else (echof "~aλ~a ~a~n" (~a #:min-width (* (length seed:ordered) 2))
                                                      (string-join (map number->string (reverse seed:ordered)) ".") (tr-d name))])
                                   (cons 1 seed:ordered))
                         #:fup (λ [name seed:ordered children:ordered]
                                 (cond [(null? seed:ordered) null]
                                       [else (cons (add1 (car seed:ordered))
                                                   (cdr seed:ordered))]))
                         #:fhere (λ [result seed:ordered]
                                   (define headline (format "~a~a ~a - " (~a #:min-width (* (length seed:ordered) 2))
                                                            (~result result) (if (null? seed:ordered) 1 (car seed:ordered))))
                                   (define headspace (~a #:min-width (string-length headline)))
                                   (cond [(test-success? result) (void (echof #:fgcolor 'lightgreen "~a~a~n" headline (tr-d (test-result-test-case-name result))))]
                                         [(test-failure? result) (void (echof #:fgcolor 'lightred "~a~a~n" headline (tr-d (test-result-test-case-name result)))
                                                                       (display-failure result #:indent headspace))]
                                         [(test-error? result) (void (echof #:fgcolor 'red "~a~a~n" headline (tr-d (test-result-test-case-name result)))
                                                                     (display-error result #:indent headspace))]
                                         [else (error "RackUnit has new test result type added!")])
                                   (if (null? seed:ordered) null (cons (add1 (car seed:ordered)) (cdr seed:ordered))))
                         null
                         unit))
      (values brief)))}

(require (submod "." digitama))
