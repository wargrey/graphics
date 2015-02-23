#lang scribble/lp

@(require "tamer.rkt" (submod "tamer.rkt" makefile))

@(current-tamer-zone (tamer-zone))

@margin-note{@racket[section]: The story should have a clear, explicit title.}
@section{Story: Hello, Hacker Hero!}

@margin-note{The brief specifies: @(itemlist @item{Primary Drivers}
                                             @item{Story Effects}
                                             @item{Effects@literal{'} Value}
                                             @item{Scenarios Outline}
                                             @item{Scenarios Summary})}
Every hacker needs a @hyperlink[@(path->string (match tamer-partner
                                                 [{list 'lib lib} (build-path (digimon-world) lib)]
                                                 [{list 'file file} (simplify-path (build-path (path-only (syntax-source #'makefile)) file))]))]{@italic{makefile.rkt}}
to make life simple. However testing building routines always makes nonsense but costs high,
thus besides the simplest examples, I will check whether the subprojects satisfy the @seclink["rules"]{rules}.

@chunk[<*>
       {module story racket
         |<import tamer handbook>|
         |<tamer discipline>|
         
         |<ready? help!>|
         |<hello rules!>|}
                                                  
       |<tamer battle>|]

where @chunk[|<import tamer handbook>|
             (require "tamer.rkt")
             (require (submod "tamer.rkt" makefile))]

@tamer-summary[#:filter 'local]

@margin-note{@racket[subsection]s: Each of them describes a scenario.}
@subsection{Scenario: Ready? Let@literal{'}s have a try!}

@chunk[|<ready? help!>|
       (define spec-examples
         (test-suite "Behavioral Specification Examples"
                     |<testsuite: should pass and do pass.>|
                     |<testsuite: should pass but do fail!>|
                     |<testsuite: fatal should never happen!>|))]

Although normal @bold{Racket} script doesn@literal{'}t require the so-called @racketidfont{main} routine,
I still prefer to start with @defproc[{main [argument string?] ...} void?]

@margin-note{The structure and testsuites follow the
             @hyperlink["http://en.wikipedia.org/wiki/Hoare_logic"]{Hoare Logic}: @(itemlist @item{Initial Conditions}
                                                                                             @item{Event Triggers}
                                                                                             @item{Expected Outcome})}

@chunk[|<tamer discipline>|
       (current-directory (let ([story (cadadr (current-tamer-story))])
                            (path-only (build-path (digimon-world) story))))
       
       (define-values {make out err $? setup teardown}
         (values (dynamic-require tamer-partner 'main {λ _ #false})
                 (open-output-bytes 'stdout)
                 (open-output-bytes 'stderr)
                 (make-parameter +NaN.0)
                 {λ argv {λ _ (parameterize ([current-output-port out]
                                             [current-error-port err]
                                             [exit-handler $?])
                                (apply make argv))}}
                 {λ _ (void (get-output-bytes out #true)
                            (get-output-bytes err #true)
                            ($? +NaN.0))}))]

You may have already familiar with the @hyperlink["http://en.wikipedia.org/wiki/Make_(software)"]{GNU Make},
nonetheless you are still free to check the options first. Normal @bold{Racket} program always knows
@exec{@|-~-|h} or @exec{@|-~-|@|-~-|help} option:

@tamer-action[tamer-require
              ((tamer-require 'make) "--help")
              (code:comment @#,t{See, @racketcommentfont{@italic{makefile}} complains that @racketcommentfont{@bold{Scribble}} is killed by accident.})]

Now it@literal{'}s time to look deep into the specification examples of

@tamer-note['spec-examples]
@chunk[|<testsuite: should pass and do pass.>|
       (test-suite "make [option]"
                   (test-suite "make --silent --help"
                               #:before (setup "--silent" "--help")
                               #:after teardown
                               (test-pred "should exit normally" zero? ($?))
                               (test-pred "should have to quiet"
                                          zero? (file-position out)))
                   (test-suite "make --silent --unknown"
                               #:before (setup "--silent" "--unknown")
                               #:after teardown
                               (test-false "should abort" (zero? ($?)))
                               (test-pred "should report errors"
                                          positive? (file-position err))))]

@chunk[|<testsuite: should pass but do fail!>|
       (test-suite "make [phony goal]"
                   (test-not-exn "EXAMPLE of FAILURE" {λ _ (make "love")}))]

@chunk[|<testsuite: fatal should never happen!>|
       (test-suite "EXAMPLE DO NEED A NAME"
                   (test-begin (fail "None of my bussiness!")))]

@tamer-action[(tamer-prove 'spec-examples)]

Equipping with the powerful @bold{Scribble} that @bold{Racket} gives us,
writing the @italic{handbook} becomes fantastic. We can @racket[eval] the code while
@racket[render]ing the documents along with it. Since the two ways,
via @racket[margin-note](@racket[tamer-note]) and via @racket[interaction](@racket[tamer-action]),
are just duplicate work(with results cached), there is no need to run them in one production @italic{handbook}.

@subsection[#:tag "rules"]{Scenario: The rules serve you!}

@chunk[|<hello rules!>|
       (define digidirs (filter {λ [sub] (and (directory-exists? sub)
                                              (regexp-match? #px"/[^.][^/.]+$" sub))}
                                (directory-list (digimon-world) #:build? #true)))
       
       |<rule: info.rkt>|
       |<rule: readme.md>|]

Since @italic{Behavior Driven Development} is the evolution of @italic{Test Driven Development} which does not define
what exactly should be tested and how would the tests be performed correct. The term @italic{Architecture} is all about designing
rules, and this story is all about building system. So apart from conventions, we need a sort of rules that the @italic{makefile.rkt}
(and systems it builds) should satisfy.

@margin-note{Meanwhile @italic{parallel building} is not supported.}

@subsubsection{Rules on project organization}

@chunk[|<rule: info.rkt>|
       (require setup/getinfo)
       
       (define info-root (get-info/full (digimon-world)))
       
       (define rules:info.rkt
         (make-test-suite "Rules: info.rkt settings"
                          (cons (test-suite "with /info.rkt"
                                            |<rules: ROOT/info.rkt>|)
                                (for/list ([digidir (in-list digidirs)])
                                  (define digimon (file-name-from-path digidir))
                                  (define info-ref (get-info/full digidir))
                                  (test-suite (format "with /~a/info.rkt" digimon)
                                              |<rules: DIGIMON/info.rkt>|)))))]

@tamer-note['rules:info.rkt]
@(itemlist @item{@bold{Rule 1} The entire project is a multi-collection package,
                  non-hidden directories within it are considered as the subprojects.}
           @item{@bold{Rule 2} Each subproject should have an explicit name,
                  even if the name is the same as its directory.}
           @item{@bold{Rule 3} @racket[compile-collection-zos] and friends should never touch these files or directories:
                  @filepath{makefile.rkt}, @filepath{submake.rkt}, @filepath{info.rkt},
                  @filepath[(path->string (file-name-from-path (digimon-stone)))] and
                  @filepath[(path->string (file-name-from-path (digimon-tamer)))].}
           @item{@bold{Rule 4} @exec{raco test} should do nothing since we would do testing
                  in a more controllable way.})

@chunk[|<rules: ROOT/info.rkt>|
       (test-case "Rule 1: multi"
                  (check-not-false info-root "/info.rkt not found!")
                  (check-equal? (info-root 'collection) 'multi
                                "'collection should be 'multi")
                  (check-equal? (info-root 'compile-omit-paths) 'all
                                "'compile-omit-paths should be 'all")
                  (check-equal? (info-root 'test-omit-paths) 'all
                                "'test-omit-paths should be 'all"))
       (test-case "Subprojects should have their own info.rkt"
                  (check-pred positive? (length digidirs) "No project found!")
                  (for ([digidir (in-list digidirs)])
                    (check-pred file-exists? (build-path digidir "info.rkt")
                                (format "/~a/info.rkt not found!"
                                        (file-name-from-path digidir)))))]

@chunk[|<rules: DIGIMON/info.rkt>|
       (test-case "Rule 2: collection"
                  (with-check-info
                   {{'info.rkt (build-path digimon "info.rkt")}}
                   (check-pred string? (info-ref 'collection)
                               "'collection should be string!")))
       (test-case "Rule 3: compile-omit-paths"
                  (with-check-info
                   {{'info.rkt (build-path digimon "info.rkt")}}
                   (let ([compile-omit-paths (info-ref 'compile-omit-paths)])
                     (check-not-false compile-omit-paths
                                      "'compile-omit-paths not defined!")
                     (if (equal? compile-omit-paths 'all)
                         (check-true #true)
                         (for ([omit (in-list (list "makefile.rkt" "submake.rkt"
                                                    "info.rkt" "stone" "tamer"))])
                           (check-not-false (member omit compile-omit-paths)
                                            (format "~a should in compile-omit-paths"
                                                    omit)))))))
       (test-case "Rule 4: test-omit-paths"
                  (with-check-info
                   {{'info.rkt (build-path digimon "info.rkt")}}
                   (let ([test-omit-paths (info-ref 'test-omit-paths)])
                     (check-not-false test-omit-paths
                                      "'test-omit-paths not defined!")
                     (check-equal? test-omit-paths 'all
                                   "'test-omit-paths should be 'all!"))))]

@margin-note{Documents are deployed in @hyperlink["gyoudmon/org"]{my website} with @bold{Scribble},
                                       while sources are hosted in @hyperlink["github.com/digital-world"]{Github} with @bold{Markdown}.}
@subsubsection{Rules on project Documents}

@chunk[|<rule: readme.md>|
       (define /stone (find-relative-path (digimon-zone) (digimon-stone)))
       
       (define rules:readme.md
         (make-test-suite "Rules: readme.md readers"
                          (cons (test-suite "/index.scrbl"
                                            |<rules: ROOT/readme.md>|)
                                (for/list ([digidir (in-list digidirs)])
                                  (define digimon (file-name-from-path digidir))
                                  (define stonedir (build-path digimon /stone))
                                  (test-suite (format "with ~a" stonedir)
                                              |<rules: DIGIMON/readme.md>|)))))]

@tamer-note['rules:readme.md]
@(itemlist @item{@bold{Rule 5} The project's toplevel @italic{README.md} is designated as the @italic{main-toc} of @bold{Scribble}.}
           @item{@bold{Rule 6} Each subproject's @italic{README.md} is designated as its own content table.})

@chunk[|<rules: ROOT/readme.md>|
       (test-case (format "Rule 5: ~a/~a/index.scrbl" (digimon-gnome) /stone)
                  (check-pred file-exists?
                              (build-path (digimon-stone) "index.scrbl")
                              "index.scrbl should exists!"))]

@chunk[|<rules: DIGIMON/readme.md>|
       (test-case "Rule 6: readme.scrbl"
                  (with-check-info
                   {{'stonedir stonedir}}
                   (check-pred file-exists?
                               (build-path digidir /stone "readme.scrbl")
                               "readme.scrbl should exists!")))]

@subsection{Scenario: What if the @italic{handbook} is unavaliable?}

Furthermore, the @italic{handbook} itself is the standard test report, but it@literal{'}s still reasonable
to check the system in some more convenient ways. Hence we have @chunk[|<tamer battle>|
                                                                       {module main racket
                                                                         |<import tamer handbook>|
                                                                         
                                                                         (exit (tamer-spec))}]

Run @exec{racket «@smaller{tamer files}»} we will get @hyperlink["http://hspec.github.io"]{@italic{hspec-like}} report.

Technically speaking, @exec{raco test --submodule main} is always there,
although that way is not recommended, and is omitted by @filepath{info.rkt}.
