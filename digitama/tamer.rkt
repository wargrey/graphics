#lang at-exp racket

(provide (all-defined-out) (all-from-out (submod "." typed/digitama)) skip todo the-color-database)
(provide (all-from-out racket "digicore.rkt" "emoji.rkt" "i18n.rkt" rackunit pict pict/code))
(provide (all-from-out scribble/core scribble/manual scriblib/autobib scribble/example scribble/html-properties))

(require (only-in racket/draw the-color-database))
(require racket/sandbox)
(require rackunit)

(require pict)
(require (prefix-in pict: pict/code))

(require (except-in scribble/core table))
(require scribble/example)
(require scribble/manual)
(require scriblib/autobib)
(require scribble/html-properties)

(require (for-syntax syntax/parse))

@require{digicore.rkt}
@require{emoji.rkt}
@require{i18n.rkt}

(define #%info (get-info/full (digimon-zone)))
(define #%digimon (let ([name (#%info 'collection)]) (if (symbol? name) (current-digimon) name)))
(define #%handbook (seclink "tamer-book" (italic "Handbook")))

(define $out (open-output-bytes '/dev/tamer/stdout))
(define $err (open-output-bytes '/dev/tamer/stderr))
(define $? (make-parameter +NaN.0))

(define $shell
  (lambda [routine . arglist]
    (get-output-bytes $out #true)
    (get-output-bytes $err #true)
    ($? +NaN.0)
    (parameterize ([current-output-port $out]
                   [current-error-port $err]
                   [exit-handler $?])
      (apply routine arglist))))

(define make-tamer-zone
  (lambda [zone]
    (define tamer-module (if (module-declared? zone #true) zone (build-path (digimon-tamer) "tamer.rkt")))
    (dynamic-require tamer-module #false)
    (parameterize ([sandbox-namespace-specs (cons (thunk (module->namespace tamer-module)) null)])
      (make-base-eval #:pretty-print? #true))))

(define-syntax (tamer-taming-start stx)
  (syntax-case stx [scribble +]
    [(_ scribble)
     #'(let ([modpath (quote-module-path)])
         (cond [(path? modpath) (tamer-story (tamer-story->modpath modpath))]
               [else (let ([story (tamer-story->modpath (cadr modpath))])
                       (tamer-story story)
                       (tamer-zone (make-tamer-zone story)))]))]
    [(_)
     #'(begin (tamer-taming-start scribble)
              (module+ main (call-as-normal-termination tamer-prove)))]))

(define-syntax (define-bib stx)
  (syntax-parse stx #:literals []
    [(_ id bib-args ...)
     #'(define id (in-bib (make-bib bib-args ...) (format ":~a" 'id)))]))

(define ~cite
  (lambda [bib #:same-author? [same? #false] . bibs]
    (if (false? same?)
        (apply (tamer-cites) bib bibs)
        (apply (tamer-cite) bib bibs))))
  
(define handbook-title
  (lambda pre-contents
    (define gnome-stone (parameterize ([current-digimon (digimon-gnome)]) (digimon-stone)))
    (list (title #:style (let* ([stones (remove-duplicates (list (digimon-stone) gnome-stone))]
                                [css (filter file-exists? (map (curryr build-path "tamer.css") stones))])
                           (make-style #false (map make-css-addition css)))
                 #:tag "tamer-book" #:version (format "~a[~a]" (version) (#%info 'version (const "Baby")))
                 (if (symbol=? (object-name (current-input-port)) '/dev/null)
                     (list (hyperlink (~url (current-digimon)) (string house-garden#))
                           (hyperlink (~url (digimon-gnome)) (format "<sub>~a</sub>" cat#)))
                     (list (hyperlink (~github (current-digimon)) (string house-garden#))
                           (hyperlink (~github (digimon-gnome)) (subscript (string cat#)))))
                 (cond [(false? (null? pre-contents)) pre-contents]
                       [else (list (literal (speak 'tamer-handbook) ":") ~ #%digimon)]))
          (apply author (#%info 'pkg-authors (const (list (pkg-idun))))))))

(define-syntax (handbook-story stx)
  (syntax-parse stx #:literals []
    [(_ (~optional (~seq #:style s:expr)) contents ...)
     #`(begin (tamer-taming-start scribble)
              (define-cite ~cite ~cites ~reference #:style number-style)
              (tamer-reference ~reference)
              (tamer-cites ~cites)
              (tamer-cite ~cite)
              (title #:tag (tamer-story->tag (tamer-story))
                     #:style #,(attribute s)
                     (literal (speak 'handbook-story) ":") ~ contents ...))]))

(define handbook-scenario
  (lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (section #:tag tag #:style style
             (literal (speak 'handbook-scenario) ":") ~ pre-contents)))

(define handbook-reference
  (lambda []
    (list ((tamer-reference) #:tag (format "~a-reference" (path-replace-suffix (tamer-story->tag (tamer-story)) ""))
                                #:sec-title (speak 'handbook-reference))
          (let ([zone-snapshots (filter-not false? (list (tamer-zone)))])
            (make-traverse-block (thunk* (for-each close-eval zone-snapshots))))
          (tamer-story #false))))

(define handbook-appendix
  (let ([digimons (itemlist #:style "HBdigimon"
                            (for/list ([digimon (in-list all-digimons)])
                              (parameterize ([current-directory (build-path (digimon-world) digimon)]
                                             [current-namespace (make-base-namespace)])
                                (define info-ref (get-info/full (current-directory) #:bootstrap? #true #:namespace (current-namespace)))
                                (define titlelem (if (string=? digimon (digimon-gnome)) bold elem))
                                (define diginame (let ([c (info-ref 'collection)]) (if (symbol? c) digimon c)))
                                (define devs (content->string (element-content (apply authors (info-ref 'pkg-authors (const `(,(pkg-idun))))))))
                                (define altext (let ([org (pkg-institution)]) (if org (~a org #\( devs #\)) devs)))
                                (item (deftech #:key diginame (string paw#) ~
                                        (hyperlink #:style (make-style #false (list (hover-property (~a "by " altext))))
                                                   (~url digimon) (titlelem diginame ":" ~ (info-ref 'pkg-desc))))))))]
        [entries (list (bib-entry #:key      "Racket"
                                  #:title    "Reference: Racket"
                                  #:author   (authors "Matthew Flatt" "PLT")
                                  #:date     "2010"
                                  #:location (techrpt-location #:institution "PLT Design Inc." #:number "PLT-TR-2010-1")
                                  #:url      "http://racket-lang.org/tr1")
                       (bib-entry #:key      "Scribble"
                                  #:title    "The Racket Documentation Tool"
                                  #:author   (authors "Matthew Flatt" "Eli Barzilay")
                                  #:url      "http://docs.racket-lang.org/scribble/index.html")
                       (bib-entry #:key      "Rackunit"
                                  #:title    "Rackunit: Unit Testing"
                                  #:author   (authors "Noel Welsh" "Ryan Culpepper")
                                  #:url      "http://docs.racket-lang.org/rackunit/index.html")
                       (bib-entry #:key      "LP:WEB"
                                  #:title    "Literate Programming"
                                  #:author   (authors "Donald E. Knuth")
                                  #:date     "1984"
                                  #:location (journal-location "The Computer Journal"  #:pages '(1 15) #:number "10.1093/comjnl/27.2.97")
                                  #:url      "http://www.literateprogramming.com/knuthweb.pdf")
                       (bib-entry #:key      "LP:Issues"
                                  #:title    "Literate Programming - Issues and Problems"
                                  #:author   (authors "Kurt Nørmark")
                                  #:date     "1998"
                                  #:location (dissertation-location #:institution "Department of Computer Science Aalborg University" #:degree "Lektor")
                                  #:url      "http://people.cs.aau.dk/~normark/litpro/issues-and-problems.html"))])
    (lambda [#:index? [index? #true] . bibentries]
      (define appendix-style (make-style 'index '(grouper)))
      ((curry filter-not void?)
       (list (part #false '((part "handbook-appendix")) (list (speak 'handbook-appendix)) (make-style 'index '(unnumbered reverl)) null null
                   (list (part #f '((part "handbook-digimon")) (list (speak 'handbook-digimon)) appendix-style null (list digimons) null)
                         (struct-copy part (apply bibliography #:tag "handbook-bibliography" (append entries bibentries))
                                      [style appendix-style]
                                      [title-content (list (speak 'handbook-bibliography))])))
             (unless (false? index?)
               (struct-copy part (index-section #:tag "handbook-index")
                            [title-content (list (speak 'handbook-index))])))))))

(define handbook-smart-table
  (lambda []
    (make-traverse-block
     (λ [get set]
       (if (false? (member 'markdown (get 'scribble:current-render-mode '(html))))
           (table-of-contents)
           (make-delayed-block
            (λ [render% pthis _]
              (define-values (/dev/tamer/stdin /dev/tamer/stdout) (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
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
                                           => (λ [pieces] (match-let ([(list _ indt ctxt) pieces]) ; (markdown listitem needs at least 1 char after "+ "
                                                            (list (format ">   ~a+ ~a" indt open-book#)  ; before breaking line if "[~a](~a)" is longer
                                                                  (hyperlink (format "~a/~a" (~url (current-digimon)) ctxt) ctxt))))] ; then 72 chars.)
                                          [(regexp-match #px"^(\\s+)λ\\d+(.\\d)*\\s+(.+?)\\s*$" line)
                                           => (λ [pieces] (format ">   ~a+ ~a~a" (list-ref pieces 1) bookmark# (list-ref pieces 3)))]
                                          [(regexp-match #px"^$" line) (summary? #true)]
                                          [(summary?) (parameterize ([current-output-port /dev/stdout])
                                                        (echof "~a~n" line
                                                               #:fgcolor (match line
                                                                           [(regexp #px" 100.00% Okay") 'lightgreen]
                                                                           [(regexp #px" [^0] error") 'darkred]
                                                                           [(regexp #px" [^0] failure") 'lightred]
                                                                           [(regexp #px" [^0] TODO") 'lightmagenta]
                                                                           [(regexp #px" [^0] skip") 'lightblue]
                                                                           [_ 'lightcyan])))]))))))))))))

(define-syntax (define-tamer-suite stx)
  (syntax-parse stx
    [(_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) units ...)
     #`(tamer-record-story 'varid (test-suite name
                                              #:before #,(or (attribute setup) #'void)
                                              #:after #,(or (attribute teardown) #'void)
                                              units ...))]))

(define-syntax (define-tamer-case stx)
  (syntax-parse stx
    [(_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...)
     #'(tamer-record-story 'varid (delay-test (test-spec name
                                                         #:before setup
                                                         checks ...
                                                         #:after teardown)))]))

(define-syntax (test-spec stx)
  (syntax-parse stx
    [(_ name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...)
     #`(test-case name (around (#,(or (attribute setup) #'void))
                               checks ...
                               (#,(or (attribute teardown) #'void))))]))

(define-syntax (tamer-action stx)
  (syntax-case stx []
    [(_ s-exps ...)
     #'(let ([story-snapshot (tamer-story)]
             [zone-snapshot (tamer-zone)])
         (make-traverse-block
          (thunk* (parameterize ([tamer-story story-snapshot]
                                 [tamer-zone zone-snapshot])
                    (examples #:label #false
                              #:eval (tamer-zone)
                              s-exps ...)))))]))

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
    `(submod ,story-path tamer story)))

(define tamer-partner->modpath
  (lambda [partner-path [submodule #false]]
    (path->digimon-modpath (if (absolute-path? partner-path) partner-path (build-path (digimon-zone) partner-path))
                           submodule)))

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
    (or (and (test-suite? suite)
             (let-values ([(brief-box cpu0 real0 gc0) (time-apply prove (list suite))])
               (define-values (success failure error skip todo real cpu-gc gc cpu)
                 (apply values (list* (summary-success (car brief-box))
                                      (summary-failure (car brief-box))
                                      (summary-error (car brief-box))
                                      (summary-skip (car brief-box))
                                      (summary-todo (car brief-box))
                                      (map (compose1 (curry ~r #:precision '(= 3)) (curry * 0.001))
                                           (list real0 (- cpu0 gc0) gc0 cpu0)))))
               (define population (+ success failure error skip todo))
               (and (positive? population)
                    (let ([echo (curry echof #:fgcolor 'lightcyan)])
                      (echo "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)." real cpu-gc gc cpu)
                      (echo "~n~a, ~a, ~a, ~a, ~a, ~a% Okay.~n" @~n_w[population]{example} @~n_w[failure]{failure}
                            @~n_w[error]{error} @~n_w[skip]{skip} @~n_w[todo]{TODO}
                            (~r  #:precision '(= 2) (/ (* (+ success skip) 100) population)))
                      (+ failure error)))))
        (and (echof #:fgcolor 'darkcyan "~nNo particular example!~n") 0))))

(define tamer-smart-summary
  (lambda []
    (define story-snapshot (tamer-story))
    (make-traverse-block
     (λ [get set]
       (if (member 'markdown (get 'scribble:current-render-mode '(html)))
           (para (literal "---"))
           (make-delayed-block
            (λ [render% pthis infobase]
              (define get (curry hash-ref (collect-info-fp (resolve-info-ci infobase))))
              (define (story-ref htag)
                (filter-map (λ [scnr] (hash-ref (hash-ref (get 'scenario make-hash) htag make-hash) (cdr scnr) #false))
                            (reverse (hash-ref handbook-stories htag null))))
              (nested #:style (make-style "boxed" null)
                      (filebox (if (module-path? story-snapshot)
                                   (italic (seclink "tamer-book" (string open-book#)) ~
                                           (~a "Behaviors in " (tamer-story->tag story-snapshot)))
                                   (italic (string books#) ~
                                           (~a "Behaviors of " #%digimon)))
                               (let ([base (if (module-path? story-snapshot)
                                               (story-ref (tamer-story->tag story-snapshot))
                                               (for/list ([story (in-list (reverse (hash-ref handbook-stories books# null)))])
                                                 (cons story (with-handlers ([exn:fail:contract? (const null)])
                                                               (apply append (map cdr (story-ref story)))))))])
                                 (define statuses (map ~result (list test-error test-failure test-todo test-skip)))
                                 (define items (for/list ([spec (in-list base)])
                                                 ;;; also see (tamer-note)
                                                 (define-values (local# desc)
                                                   (cond [(string? (car spec)) (values bookmark# (car spec))] ;;; testsuite
                                                         [else (values page# (unbox (car spec)))])) ;;; toplevel testcase
                                                 (define status (car (or (ormap (curryr member (cdr spec)) statuses)
                                                                         (list (~result struct:test-success)))))
                                                 (if (module-path? story-snapshot)
                                                     (list (elem (italic (string local#)) ~
                                                                 (elemref desc (racketkeywordfont (literal desc))))
                                                           (elemref desc status #:underline? #false))
                                                     (let ([stts (make-parameter status)])
                                                       (echof #:fgcolor 'lightyellow (~a desc #:width 64 #:pad-string "." #:limit-marker "......"))
                                                       (echof #:fgcolor (~fgcolor status) "~a~n" status)
                                                       (for ([msg (in-list (cdr spec))])
                                                         (cond [(and (member msg statuses) msg)
                                                                => stts]
                                                               [(and (regexp-match? #px"»»" msg) msg)
                                                                => (curry eechof #:fgcolor 'darkgrey "~a~n")]
                                                               [(and (string? (car spec)) msg)
                                                                => (curry eechof  "~a~n"
                                                                          #:fgcolor (~fgcolor (stts))
                                                                          #:attributes (if (string=? (stts) (~result test-error)) '(inverse) null))]))
                                                       (list (elem (italic (string book#)) ~ (secref (car spec)))
                                                             (seclink (car spec) status #:underline? #false))))))
                                 (match-define (list success failure error skip todo reals gcs cpus)
                                   (for/list ([meta (in-list (list 'success 'failure 'error 'skip 'todo 'real 'gc 'cpu))])
                                     (define pool (get meta make-hash))
                                     (if (module-path? story-snapshot)
                                         (hash-ref pool story-snapshot 0)
                                         (foldl + 0 (hash-values pool)))))
                                 (match-define (list real cpu-gc gc cpu)
                                   (map (compose1 (curry ~r #:precision '(= 3)) (curry * 0.001))
                                        (list reals (- cpus gcs) gcs cpus)))
                                 (define briefs
                                   (let ([population (+ success failure error skip todo)])
                                     (if (zero? population)
                                         (list "No particular test!")
                                         (list (format "~a% tests successful."
                                                       (~r #:precision '(= 2) (/ (* (+ success skip) 100) population)))
                                               (format "~a, ~a, ~a, ~a, ~a, ~a."
                                                       @~w=n[(length base) (if story-snapshot "Scenario" "Story")]
                                                       @~w=n[population]{Test} @~w=n[failure]{Failure} @~w=n[error]{Error}
                                                       @~w=n[skip]{Skip} @~w=n[todo]{TODO})
                                               (format "~a wallclock seconds (~a task + ~a gc = ~a CPU)."
                                                       real cpu-gc gc cpu)))))
                                 (unless (module-path? story-snapshot)
                                   (for ([brief (in-list (cons "" briefs))])
                                     (echof #:fgcolor 'lightcyan "~a~n" brief)))
                                 (let ([summaries (add-between (map racketoutput briefs) (linebreak))])
                                   (cond [(null? items) summaries]
                                         [else (cons (tabular items #:style 'boxed #:column-properties '(left right))
                                                     summaries)]))))))))))))

(define tamer-note
  (lambda [unit-vars . notes]
    (define story-snapshot (tamer-story))
    (make-traverse-block
     (λ [get set]
       (define htag (tamer-story->tag story-snapshot))
       (unless (get 'scenario #false) (set 'scenario (make-hash)))
       (define scenarios (hash-ref! (get 'scenario make-hash) htag make-hash))
       (margin-note (unless (null? notes) (append notes (list (linebreak) (linebreak))))
                    (for/list ([unit-var (in-list (if (list? unit-vars) unit-vars (list unit-vars)))])
                      (define-values (/dev/tamer/stdin /dev/tamer/stdout) (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
                      (parameterize ([tamer-story story-snapshot]
                                     [current-readtable readwrotten]
                                     [current-input-port /dev/tamer/stdin])
                        (define unit (tamer-require unit-var))
                        (define unit-spec (make-parameter null))
                        (thread (λ _ (dynamic-wind (thunk (collect-garbage))
                                                   (thunk (parameterize ([current-error-port /dev/tamer/stdout]
                                                                         [current-output-port /dev/tamer/stdout])
                                                            (define-values (brief cpu real gc) (time-apply prove (list unit)))
                                                            (define-values (success failure error skip todo)
                                                              (values (summary-success (car brief))
                                                                      (summary-failure (car brief))
                                                                      (summary-error (car brief))
                                                                      (summary-skip (car brief))
                                                                      (summary-todo (car brief))))
                                                            (for ([meta (in-list (list 'cpu 'real 'gc 'success 'failure 'error 'skip 'todo))]
                                                                  [delta (in-list (list cpu real gc success failure error skip todo))])
                                                              (unless (get meta #false) (set meta (make-hash)))
                                                              (define pool (get meta make-hash))
                                                              (hash-set! pool (tamer-story) (+ (hash-ref pool (tamer-story) 0) delta)))
                                                            (if (zero? (+ failure error))
                                                                (printf "~n~a wall seconds.~n" (~r (/ real 1000.0) #:precision '(= 3)))
                                                                (printf "~n~a ~a~n" @~n_w[failure]{failure} @~n_w[error]{error}))))
                                                   (thunk (close-output-port /dev/tamer/stdout)))))
                        ((compose1 (curryr add-between (linebreak)) (curry filter-not void?))
                         (for/list ([line (in-port read-line)])
                           (cond [(regexp-match #px"^λ\\s+(.+)" line)
                                  => (λ [pieces] (let ([ctxt (list-ref pieces 1)])
                                                   (unit-spec (list ctxt)) ; Testsuite
                                                   (nonbreaking (racketmetafont (italic (string open-book#)) ~ (elemtag ctxt (literal ctxt))))))]
                                 [(regexp-match #px"^\\s+λ(\\d+(.\\d)*)\\s+(.+?)\\s*$" line)
                                  => (λ [pieces] (nonbreaking (racketoutput (italic (string bookmark#)) ~ (larger (literal (list-ref pieces 3))))))]
                                 [(regexp-match #px"^(\\s*)(.+?)\\s+(\\d+) - (.+?)\\s*$" line)
                                  => (λ [pieces] (match-let ([(list _ spc stts idx ctxt) pieces])
                                                   (when (string=? spc "") (unit-spec (list (box ctxt)))) ; Toplevel testcase
                                                   (unless (string=? stts (~result struct:test-success))
                                                     (unit-spec (cons ctxt (cons stts (unit-spec)))))
                                                   (nonbreaking ((if (string=? spc "") (curry elemtag ctxt) elem)
                                                                 stts (racketkeywordfont ~ (italic idx)) (racketcommentfont ~ (literal ctxt))))))]
                                 [(regexp-match #px"^\\s*»» (.+?)?:?\\s+(.+?)\\s*$" line)
                                  => (λ [pieces] (match-let ([(list _ key val) pieces])
                                                   (unit-spec (cons (string-trim line) (unit-spec)))
                                                   (cond [(member key '("message"))
                                                          (elem #:style (make-style #false (list (make-color-property (list 128 128 128))))
                                                                (string backhand#) ~ (italic (literal val)))]
                                                         [(member key '("SKIP" "TODO"))
                                                          (elem #:style (make-style #false (list (make-color-property (list 128 128 128))))
                                                                (string backhand#) ~ (racketparenfont key ":") ~ (italic (literal val)))]
                                                         [(member key '("exn" "exception"))
                                                          (elem (racketvalfont (string macroscope#)) ~
                                                                (racket #,(let ([ev (fix (read (open-input-string val)))]
                                                                                [tr (curryr call-with-input-string read-line)])
                                                                            (vector-set! ev 1 (tr (vector-ref ev 1))) ev)))]
                                                         [(regexp-match? #px"param:\\d+" key)
                                                          (elem (racketvalfont (string crystal-ball#)) ~
                                                                (racket #,(fix (read (open-input-string val)))))])))]
                                 [(regexp-match #px"^\\s*»»» \\s+(expected|given|received):\\s+(.+?)\\s*$" line)
                                  ; only for errors that have multilined messages.
                                  => (λ [pieces] (and (unit-spec (cons (string-trim line) (unit-spec)))
                                                      (elem (racketvalfont (string paw#)) ~
                                                            (let ([message (list-ref pieces 2)])
                                                              (racket #,(fix (with-handlers ([exn? (λ _ message)])
                                                                               (read (open-input-string message)))))))))]
                                 [(regexp-match #px"^$" line) (hash-set! scenarios unit (reverse (unit-spec)))]
                                 [(hash-has-key? scenarios unit)
                                  (nonbreaking (elem (string pin#) ~ ((if (regexp-match? #px"error" line) racketerror racketresultfont) line) ~
                                                     (seclink (tamer-story->tag (tamer-story)) ~
                                                              (string house-garden#) (smaller (string cat#)))))]))))))))))

(define tamer-racketbox
  (lambda [path #:line-start-with [line0 1]]
    (define story-snapshot (tamer-story))
    (make-traverse-block
     (thunk* (parameterize ([tamer-story story-snapshot])
               (define /path/file (simplify-path (if (symbol? path) (dynamic-require/expose (tamer-story) path) path)))
               (nested #:style (make-style "boxed" null)
                       (filebox (italic (string memo#) ~ (path->string (tr-if-path /path/file)))
                                (codeblock #:line-numbers line0 #:keep-lang-line? (> line0 0) ; make sure line number starts from 1
                                           (string-trim (file->string /path/file) #:left? #false #:right? #true)))))))))

(define tamer-racketbox/region
  (lambda [path #:pxstart [pxstart #px"\\S+"] #:pxend [pxend #false] #:greedy? [greedy? #false]]
    (define story-snapshot (tamer-story))
    (make-traverse-block
     (thunk* (parameterize ([tamer-story story-snapshot])
               (define /path/file (simplify-path (if (symbol? path) (dynamic-require/expose (tamer-story) path) path)))
               (define-values (line0 contents)
                 (call-with-input-file* /path/file
                   (lambda [in.rkt]
                     (let read-next ([lang #false] [line0 0] [contents null] [end 0])
                       (define line (read-line in.rkt))
                       (cond [(eof-object? line)
                              (if (zero? end)
                                  (values line0 (cons lang (reverse contents)))
                                  (values line0 (cons lang (take (reverse contents) end))))]
                             [(and (regexp? pxend) (false? (null? contents)) (regexp-match? pxend line))
                              ; end line itself is excluded
                              (if (false? greedy?)
                                  (values line0 (cons lang (reverse contents)))
                                  (read-next lang line0 (cons line contents) (length contents)))]
                             [(regexp-match? #px"^#lang .+$" line)
                              (read-next line (add1 line0) contents end)]
                             [(and (string? lang) (null? contents) (regexp-match pxstart line))
                              (read-next lang line0 (list line) end)]
                             [(false? (null? contents)) ; still search the end line greedily
                              (read-next lang line0 (cons line contents) end)]
                             [else ; still search for the first line
                              (read-next lang (add1 line0) contents end)])))))
               (nested #:style (make-style "boxed" null)
                       (filebox (italic (string memo#) ~ (path->string (tr-if-path /path/file)))
                                (codeblock #:line-numbers line0 #:keep-lang-line? #false
                                           (string-trim #:left? #false #:right? #true ; remove tail blank lines 
                                                        (string-join contents (string #\newline)))))))))))

(define filesystem-tree
  (let ([hint (pict-height (text ""))] [ghost (blank 0)] [text->pict (compose1 text ~a)])
    (lambda [forest #:pict [ptext text->pict] #:padding-space [gapsize 8] #:padding-x [offset 12] #:padding-box [delta 8]
                    #:dir-color [cdir 'LightSkyBlue] #:file-color [cfile 'Ghostwhite] #:line-color [cline 'Gainsboro]]
      (define yoffset (* 0.5 (+ delta hint)))
      (define xy-find (lambda [p f] (let-values ([(x y) (lt-find p f)]) (values (+ x offset) (+ y yoffset)))))
      (define (node->pict v color)
        (define content (ptext v))
        ((curryr cc-superimpose content)
         (filled-rounded-rectangle #:border-color (~a cline) #:color (~a color)
                                   (+ delta delta (pict-width content))
                                   (+ delta (pict-height content)))))
      (define (tree->pict nodes)
        (for/fold ([head (node->pict (car nodes) cdir)])
                  ([node (in-list (cdr nodes))])
          (define body (filesystem-tree node #:padding-space gapsize #:padding-x offset #:padding-box delta
                                        #:pict ptext #:dir-color cdir #:file-color cfile #:line-color cline))
          (define child (pin-line #:under? #true #:color (~a cline)
                                  (ht-append (* 4 gapsize) ghost body)
                                  ghost xy-find body xy-find))
          (pin-line #:under? #true #:color (~a cline)
                    (vl-append gapsize head child)
                    head xy-find child xy-find)))
      (cond [(null? forest) ghost]
            [(list? forest) (tree->pict forest)]
            [else (node->pict forest cfile)]))))



(module digitama racket
  (provide (all-defined-out) quote-module-path)

  (require rackunit)
  (require racket/undefined)
  (require syntax/location)
  (require setup/xref)
  (require setup/dirs)
  (require scribble/core)
  (require scribble/xref)
  (require scribble/manual)

  (require "digicore.rkt")
  (require "emoji.rkt")

  (require (rename-in rackunit/private/monad
                      [compose* >>=]
                      [sequence* >=>]))

  (define tamer-zone (make-parameter #false))

  (struct test-skip test-result (result #| : String |#))
  (struct test-todo test-result (result #| : String |#))

  (define skip
    (lambda [fmt . arglist]
      (raise (exn:fail:unsupported (apply format fmt arglist)))))

  (define todo
    (lambda [fmt . arglist]
      (raise (exn:fail:unsupported (apply format fmt arglist)))))
  
  (define tamer-story (make-parameter #false))
  (define tamer-cite (make-parameter void))
  (define tamer-cites (make-parameter void))
  (define tamer-reference (make-parameter void))
  
  (define-syntax (tamer-story->tag stx)
    (syntax-case stx []
      [(_ story-sexp)
       #'(let ([modpath (with-handlers ([exn? (const (quote-source-file))]) (cadr story-sexp))])
           (path->string (find-relative-path (digimon-tamer) modpath)))]))

  (struct summary (success failure error skip todo) #:prefab)

  (define summary++
    (lambda [summary0 result]
      (summary (+ (summary-success summary0) (if (test-success? result) 1 0))
               (+ (summary-failure summary0) (if (test-failure? result) 1 0))
               (+ (summary-error summary0) (if (test-error? result) 1 0))
               (+ (summary-skip summary0) (if (test-skip? result) 1 0))
               (+ (summary-todo summary0) (if (test-todo? result) 1 0)))))
  
  ;;; Tamer Monad
  (struct tamer-seed (datum brief namepath exns) #:mutable)

  (define make-tamer-monad
    (lambda []
      (monad (void) (tamer-seed (void) (summary 0 0 0 0 0) null null))))

  (define (monad-return value)
    (lambda [tamer-monad]
      (set-monad-value! tamer-monad value)
      tamer-monad))

  (define (monad-put seed-set! val)
    (lambda [tamer-monad]
      (seed-set! (monad-state tamer-monad) val)
      tamer-monad))

  (define (monad-get seed-ref)
    (lambda [tamer-monad]
      (let ([val (seed-ref (monad-state tamer-monad))])
        (set-monad-value! tamer-monad val)
        tamer-monad)))
  ;;; End Tamer Monad

  (define-values (handbook-stories handbook-records) (values (make-hash) (make-hash)))

  (define tamer-record-story
    (lambda [name unit]
      (define htag (tamer-story->tag (tamer-story)))
      (define units (hash-ref handbook-stories htag null))
      (unless (dict-has-key? units name)
        (hash-set! handbook-stories htag (cons (cons name unit) units)))
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
                                                                     [routine (thunk (with-check-info (('exitcode v)) (fail errmsg)))])
                                                                (return (run-test-case case-name routine))))])
                            (return (let ([result (run-test-case case-name action)])
                                      (cond [(and (test-error? result) (test-error-result result))
                                             => (λ [?] (cond [(false? (exn:fail:unsupported? ?)) result]
                                                             [(let ([stack (continuation-mark-set->context (exn-continuation-marks ?))])
                                                                (and (false? (null? stack)) (eq? (caar stack) 'todo)))
                                                              (test-todo case-name (exn-message ?))]
                                                             [else (test-skip case-name (exn-message ?))]))]
                                            [else result])))))))))

  (define ~result
    (lambda [result]
      (case (object-name result)
        [(test-error) (string bomb#)]
        [(test-success) (string green-heart#)]
        [(test-failure) (string broken-heart#)]
        [(test-skip) (string arrow-heart#)]
        [(test-todo) (string growing-heart#)])))

  (define ~fgcolor
    (lambda [result]
      (define rslt (if (string? result) result (~result result)))
      (cond [(string=? rslt (~result test-error)) 'darkred]
            [(string=? rslt (~result test-success)) 'lightgreen]
            [(string=? rslt (~result test-failure)) 'lightred]
            [(string=? rslt (~result test-skip)) 'lightblue]
            [(string=? rslt (~result test-todo)) 'lightmagenta])))

  (define ~markdown
    (lambda [line]
      (define padding (λ [line] (make-string (- 72 (remainder (string-length (format "~a" line)) 72)) #\space)))
      (cond [(string? line) (literal (format "~a~a" line (padding line)))]
            [else (list line (literal (padding (car (element-content line)))))])))

  (define ~url
    (lambda [digimon]
      (format "http://~a/~~~a:~a" (pkg-domain) (pkg-idun) digimon)))

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
                             [(list _ #"path" _ fname) (string->path (bytes->string/utf-8 fname))]
                             [(list _ _ #false #false) '(lambda _ ...)]
                             [(list _ #false #false <unprintable-value>) (string->symbol (format "~a?" <unprintable-value>))]
                             [(list _ _ _ (pregexp #px"function\\.rkt")) (cons negate 'λ)]
                             [(list _ _ _ #"composed") '(compose λ ...)]
                             [(list _ _ _ #"curried") '(curry λ ...)]
                             [(list _ _ _ name) (with-handlers ([exn? (λ [ev] (list procedure-rename 'λ (exn:fail:contract:variable-id ev)))])
                                                  (eval (string->symbol (bytes->string/utf-8 name))
                                                        (let ([mod (build-path (digimon-tamer) "tamer.rkt")])
                                                          (dynamic-require mod #false)
                                                          (module->namespace mod))))])))))

  (define fix
    (lambda [val]
      (cond [(or (procedure? val) (symbol? val))
             (let*-values ([(modpath) (build-path (digimon-tamer) "tamer.rkt")]
                           [(export) (or (object-name val) val)]
                           [(xref) (load-collections-xref)]
                           [(tag) (xref-binding->definition-tag xref (list modpath export) #false)]
                           [(path anchor) (with-handlers ([exn? (λ _ (values #false #false))])
                                            (xref-tag->path+anchor xref tag #:external-root-url #false))])
               (or (and path anchor (racketvalfont (hyperlink (format "/~~:/~a#~a" (find-relative-path (find-doc-dir) path) anchor)
                                                              (symbol->string export))))
                   val))]
            [(vector? val) #| also for (struct? val) |#
             (vector-map fix val)]
            [(pair? val) #| also for lists |#
             (cons (fix (car val)) (fix (cdr val)))]
            [else val])))

  (define display-failure
    (lambda [result [color 'darkred] #:indent [headspace ""]]
      (define echo (curry eechof #:fgcolor color "~a»» ~a: ~s~n" headspace))
      (define recho (curry eechof #:fgcolor color "~a»»» ~a~a~n" headspace))
      (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
        (case (check-info-name info)
          [(params) (for ([param (in-list (map tr-if-path (check-info-value info)))]
                          [index (in-naturals 1)])
                      (echo (format "param:~a" index) param))]
          [(message) (let ([messages (call-with-input-string (tr-d (check-info-value info)) port->lines)])
                       (echo "message" (car messages))
                       (for-each (curry recho (~a #:min-width 8)) (cdr messages)))]
          [else (echo (check-info-name info)
                      (case (check-info-name info)
                        [(location) (tr-d (srcloc->string (apply srcloc (check-info-value info))))]
                        [(exception-message) (tr-d (check-info-value info))]
                        [else ((if (string? (check-info-value info)) tr-d tr-if-path) (check-info-value info))]))]))))
  
  (define display-error
    (lambda [result [color 'darkred] #:indent [headspace0 ""]]
      (define errobj (test-error-result result))
      (define messages (call-with-input-string (tr-d (exn-message errobj)) port->lines))
      (eechof #:fgcolor color #:attributes '(inverse) "~a»» name: ~a~n" headspace0 (object-name errobj))
      (unless (null? messages)
        (define msghead " message: ")
        (define msgspace (~a #:min-width (sub1 (string-length msghead))))
        (eechof #:fgcolor color #:attributes '(inverse) "~a»»~a~a~n" headspace0 msghead (car messages))
        (for-each (curry eechof #:fgcolor color #:attributes '(inverse) "~a»»»~a~a~n" headspace0 msgspace) (cdr messages)))
      (for ([stack (in-list (continuation-mark-set->context (exn-continuation-marks errobj)))])
        (when (cdr stack)
          (define srcinfo (srcloc->string (cdr stack)))
          (unless (or (false? srcinfo) (regexp-match? #px"^/" srcinfo))
            (eechof #:fgcolor 'darkgrey "~a»»»» ~a: ~a~n" headspace0
                    (tr-d srcinfo) (or (car stack) 'λ)))))))

  (define display-skip
    (lambda [result [color 'darkblue] #:indent [headspace0 ""]]
      (define reason (test-skip-result result))
      (define messages (call-with-input-string (tr-d reason) port->lines))
      (unless (null? messages)
        (define msghead " SKIP: ")
        (define msgspace (~a #:min-width (sub1 (string-length msghead))))
        (eechof #:fgcolor color "~a»»~a~a~n" headspace0 msghead (car messages))
        (for-each (curry eechof #:fgcolor color "~a»»»~a~a~n" headspace0 msgspace) (cdr messages)))))

  (define display-todo
    (lambda [result [color 'darkmagenta] #:indent [headspace0 ""]]
      (define reason (test-todo-result result))
      (define messages (call-with-input-string (tr-d reason) port->lines))
      (unless (null? messages)
        (define msghead " TODO: ")
        (define msgspace (~a #:min-width (sub1 (string-length msghead))))
        (eechof #:fgcolor color "~a»»~a~a~n" headspace0 msghead (car messages))
        (for-each (curry eechof #:fgcolor color "~a»»»~a~a~n" headspace0 msgspace) (cdr messages)))))
  
  (define fold-test-suite
    (lambda [seed:datum testsuite #:fdown fdown #:fup fup #:fhere fhere]
      (parameterize ([current-custodian (make-custodian)]) ;;; Prevent test routines happen to shutdown the custodian.
        (monad-value ((monad-get tamer-seed-brief)
                      (foldts-test-suite (λ [testsuite name pre-action post-action seed]
                                           (define $exn (make-parameter undefined))
                                           (with-handlers ([void $exn]) ;; catch all
                                             (call-with-values pre-action void))
                                           ((>=> (>>= (monad-get tamer-seed-datum)
                                                      (λ [seed:datum] (monad-put set-tamer-seed-datum! (fdown name seed:datum))))
                                                 (>>= (monad-get tamer-seed-namepath)
                                                      (λ [seed:namepath] (monad-put set-tamer-seed-namepath! (cons name seed:namepath))))
                                                 (>>= (monad-get tamer-seed-exns)
                                                      (λ [seed:exns] (monad-put set-tamer-seed-exns! (cons ($exn) seed:exns)))))
                                            seed))
                                         (λ [testsuite name pre-action post-action seed children-seed]
                                           (with-handlers ([exn? (compose1 display-error (curry make-test-error (format "#:after ~a" name)))])
                                             (call-with-values post-action void))
                                           ((>=> (>>= (monad-get tamer-seed-datum)
                                                      (λ [children:datum] (monad-put set-tamer-seed-datum! (fup name children:datum children:datum))))
                                                 (>>= (monad-get tamer-seed-namepath)
                                                      (λ [children:namepath] (monad-put set-tamer-seed-namepath! (cdr children:namepath))))
                                                 (>>= (monad-get tamer-seed-exns)
                                                      (λ [children:exns] (monad-put set-tamer-seed-exns! (cdr children:exns)))))
                                            children-seed #| monad is a stateful structure, so seed === children-seed |#))
                                         (λ [testcase name action seed]
                                           (define-values (fixed-name fixed-action)
                                             (cond [(findf (lambda [e] (not (eq? e undefined))) (monad-value ((monad-get tamer-seed-exns) seed)))
                                                    => (lambda [e] (values (format "#:before ~a" name)
                                                                           (thunk (raise e))))]
                                                   [(false? name)
                                                    (values (format "(⧴ ~a)" (object-name struct:exn:fail:user))
                                                            (thunk (raise-user-error "Unnamed Testcase!")))]
                                                   [else (values name action)]))
                                           (define fixed-namepath (cons fixed-name (monad-value ((monad-get tamer-seed-namepath) seed))))
                                           (define record (tamer-record-handbook fixed-namepath fixed-action))
                                           ((>=> (>>= (monad-get tamer-seed-datum)
                                                      (λ [seed:datum] (monad-put set-tamer-seed-datum! (fhere record seed:datum))))
                                                 (>>= (monad-get tamer-seed-brief)
                                                      (λ [seed:summary] (monad-put set-tamer-seed-brief! (summary++ seed:summary record))))
                                                 (monad-put set-tamer-seed-namepath! fixed-namepath))
                                            seed))
                                         ((monad-put set-tamer-seed-datum! seed:datum)
                                          (make-tamer-monad))
                                         testsuite))))))
  
  (define prove
    (lambda [unit]
      (fold-test-suite #:fdown (λ [name seed:ordered]
                                 (cond [(null? seed:ordered) (echof #:fgcolor 'darkgreen #:attributes '(dim underline) "λ ~a~n" (tr-d name))]
                                       [else (echof "~aλ~a ~a~n" (~a #:min-width (* (length seed:ordered) 2))
                                                    (string-join (map number->string (reverse seed:ordered)) ".") (tr-d name))])
                                 (cons 1 seed:ordered))
                       #:fup   (λ [name maybe-children-if-monad children:ordered]
                                 (cond [(< (length children:ordered) 2) null]
                                       [else (cons (add1 (cadr children:ordered))
                                                   (cddr children:ordered))]))
                       #:fhere (λ [result seed:ordered]
                                 (define headline (format "~a~a  ~a - " (~a #:min-width (* (length seed:ordered) 2))
                                                          (~result result) (if (null? seed:ordered) 1 (car seed:ordered))))
                                 (define headspace (~a #:min-width (string-length headline)))
                                 (echof #:fgcolor (~fgcolor result) "~a~a~n" headline (tr-d (test-result-test-case-name result)))
                                 (cond [(test-success? result) (void)]
                                       [(test-failure? result) (display-failure result #:indent headspace)]
                                       [(test-error? result) (display-error result #:indent headspace)]
                                       [(test-skip? result) (display-skip result #:indent headspace)]
                                       [(test-todo? result) (display-todo result #:indent headspace)]
                                       [else (error "RackUnit has new test result type supported!")])
                                 (if (null? seed:ordered) null (cons (add1 (car seed:ordered)) (cdr seed:ordered))))
                       null ; seed:datum
                       unit))))

(module typed/digitama typed/racket
  (provide (all-defined-out))

  (require/typed file/sha1
                 [bytes->hex-string (-> Bytes String)])

  (define hexstring : (-> Any String)
    (lambda [val]
      (cond [(integer? val) (~r val #:base 16)]
            [(bytes? val) (format "~a" (regexp-match* #px".." (bytes->hex-string val)))]
            [(boolean? val) (hexstring (if val 1 0))]
            [else (hexstring (string->bytes/utf-8 (~a val)))])))

  (define symb0x->number : (-> Symbol (Option Number))
    (lambda [hex]
      (string->number (string-replace (symbol->string hex) "0x" "") 16))))

(require (submod "." digitama))
(require (submod "." typed/digitama))

(module* typed typed/racket
  (provide (all-defined-out) (all-from-out (submod ".." typed/digitama)))
  (provide (all-from-out "digicore.rkt" "emoji.rkt" "i18n.rkt" typed/rackunit))

  (require typed/rackunit)
  (require (submod ".." typed/digitama))
  
  (require (for-syntax syntax/parse))

  (require "digicore.rkt")
  (require "emoji.rkt")
  (require "i18n.rkt")

  (require/typed/provide (submod "..")
                         [#%info Info-Ref]
                         [$out Output-Port]
                         [$err Output-Port]
                         [$? (Parameterof Any)]
                         [$shell (-> (-> Any * Void) Any * Any)]
                         [tamer-story->modpath (-> Path-String (U Module-Path (List* 'submod Module-Path (Listof Symbol))))]
                         [tamer-partner->modpath (->* (Path-String) ((Option Symbol)) (U Module-Path (List 'submod Module-Path Symbol)))]
                         [tamer-prove (-> Natural)]
                         [todo (-> String Any * Nothing)]
                         [skip (-> String Any * Nothing)])

  (require/typed (submod ".." digitama)
                 [tamer-record-story (-> Symbol Test Void)])

  ;;; adapted from (submod "..")
  (define-syntax (define-tamer-suite stx)
    (syntax-parse stx
      [(_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) units ...)
       #`(tamer-record-story 'varid (test-suite name
                                                #:before #,(or (attribute setup) #'void)
                                                #:after #,(or (attribute teardown) #'void)
                                                units ...))]))
  
  (define-syntax (define-tamer-case stx)
    (syntax-parse stx
      [(_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...)
       #'(tamer-record-story 'varid (delay-test (test-spec name
                                                           #:before setup
                                                           checks ...
                                                           #:after teardown)))]))
  
  (define-syntax (test-spec stx)
    (syntax-parse stx
      [(_ name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...)
       #`(test-case name (around (#,(or (attribute setup) #'void))
                                 checks ...
                                 (#,(or (attribute teardown) #'void))))])))
