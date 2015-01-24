#lang scribble/lp

@(require "tamer.rkt" (submod "tamer.rkt" makefile))

@(current-tamer-zone (tamer-zone))

@margin-note{@racket[section]: The story should have a clear, explicit title.}
@section{Story: Hello, Hacker Hero!}

@margin-note{The brief specifies: @(itemlist @item{Primary Drivers}
                                             @item{Story Effects}
                                             @item{Effects@literal{'} Value}
                                             @item{Scenarios Outline})}
Every hacker needs a @italic{makefile.rkt} to make life simple, and the @italic{makefile.rkt} should always be on its best behavior.
So I@literal{'}m glad to help you to ensure that the
@hyperlink[@(path->string (match tamer-partner
                            [{list 'lib lib} (build-path rootdir lib)]
                            [{list 'file file} (simplify-path (build-path (path-only (syntax-source #'makefile)) file))]))]{makefile.rkt}
works as you wish.

@chunk[<*>
       {module story racket
         <import-tamer-handbook>
         <tamer-discipline>
         
         <ready?-help!>
         <hello-rules!>}
                                                  
       <tamer-battle-via-racket>
       <tamer-battle-via-raco>]

where @chunk[<import-tamer-handbook>
             (require "tamer.rkt")
             (require (submod "tamer.rkt" makefile))]

@margin-note{@racket[subsection]s: Each of them describes a scenario.}
@subsection{Scenario: Ready? Let@literal{'}s have a try!}

@margin-note{The structure and testsuites follow the
             @hyperlink["http://en.wikipedia.org/wiki/Hoare_logic"]{Hoare Logic}: @(itemlist @item{Initial Conditions}
                                                                                             @item{Event Triggers}
                                                                                             @item{Expected Outcome})}

@chunk[<ready?-help!>
       <testsuite:option-ready?>
       <testsuite:goal-not-ready!>]

Although normal @bold{Racket} script doesn@literal{'}t require the so-called @racketidfont{main} routine,
I still prefer to start with @defproc[{main [argument string?] ...} void?]

@chunk[<tamer-discipline>
       (current-directory (build-path (getenv "digimon-world")
                                      (cadadr (current-tamer-story)) 'up))
       
       (define-values {make out err $?}
         (values (dynamic-require tamer-partner 'main (const #false))
                 (open-output-bytes 'stdout)
                 (open-output-bytes 'stderr)
                 (make-parameter +NaN.0)))]

You may be already familiar with the @hyperlink["http://en.wikipedia.org/wiki/Make_(software)"]{GNU Make},
nonetheless you are still free to check the options first. Normal @bold{Racket} program always knows
@exec{@|-~-|h} or @exec{@|-~-|@|-~-|help} option:

@tamer-action[tamer-require
              ((tamer-require 'make) "--help")
              (code:comment @#,t{See, @racketcommentfont{@italic{makefile}} complains that @racketcommentfont{@bold{Scribble}} is killed by accident.})
              (code:comment @#,t{Meanwhile @racketcommentfont{@italic{parallel building}} is not supported.})]

Now it@literal{'}s time to look deep into the specification examples of
@itemlist{@item{a testsuite that should pass and do pass:}}

@chunk[<testsuite:option-ready?>
       (define setup
         {lambda argv
           {thunk (parameterize ([current-output-port out]
                                 [current-error-port err]
                                 [exit-handler $?])
                    (apply make argv))}})
       
       (define teardown
         {thunk (get-output-bytes out #true)
                (get-output-bytes err #true)
                ($? +NaN.0)})
       
       (define option-ready?
         (test-suite "make [option]"
                     (test-suite "make --help"
                                 #:before (setup "--help")
                                 #:after teardown
                                 (test-pred "should exit normally"
                                            zero? ($?))
                                 (test-pred "should no error"
                                            zero? (file-position err))
                                 (test-pred "should say something"
                                            positive? (file-position out)))
                     (test-pred "should flush stdout"
                                zero? (file-position out))
                     (test-suite "make --unknown"
                                 #:before (setup "--unknown")
                                 #:after teardown
                                 (test-false "should abort"
                                             (zero? ($?)))
                                 (test-pred "should say something wrong"
                                            positive? (file-position err)))
                     (test-pred "should flush stderr"
                                zero? (file-position err))
                     (test-suite "make --silent --help"
                                 #:before (setup "--silent" "--help")
                                 #:after teardown
                                 (test-pred "should say nothing"
                                            zero? (file-position out)))
                     (test-suite "make --silent --unknown"
                                 #:before (setup "--silent" "--unknown")
                                 #:after teardown
                                 (test-pred "should say nothing but errors"
                                            positive?
                                            (file-position err)))))]

@tamer-note['option-ready?]
@tamer-action[(tamer-prove 'option-ready?)]

@itemlist{@item{a testsuite that should pass but do fail:}}

@chunk[<testsuite:goal-not-ready!>
       (define goal-not-ready!
         (test-suite "make [phony target]"
                     (test-suite "make it"
                                 (test-not-exn "CONFRONT THE INTENDED FATAL"
                                               {thunk (make "it")}))))]

@tamer-note['goal-not-ready!]
@tamer-action[(tamer-prove 'goal-not-ready!)]

@itemlist{@item{a typo that should never happen:}}
@tamer-note['maybe-typo!]
@tamer-action[(tamer-prove 'maybe-typo!)]

Equipping with the powerful @bold{Scribble} that @bold{Racket} gives us,
writing the @italic{handbook} becomes fantastic. We can @racket[eval] the code while
@racket[render]ing the documents along with it. Since the two ways,
via @racket[margin-note](@racket[tamer-note]) and via @racket[interaction](@racket[tamer-action]),
are just duplicate work, there is no need to run them at the same @italic{handbook}.
I just show you the effects of the two ways, then you can choose your favor.

@subsection{Scenario: What if the @italic{handbook} is unavaliable?}

Furthermore, the @italic{handbook} itself is the standard test reporter, but it@literal{'}s still reasonable
to check the system in some more convenient ways. Thus two styles, 
@hyperlink["http://en.wikipedia.org/wiki/Test::More"]{@italic{TAP::Harness-like}} and
@hyperlink["http://hspec.github.io"]{@italic{hspec-like}},
are designated for @exec{raco test} and @exec{racket}, respectively.

@chunk[<tamer-battle-via-racket>
       {module main racket
         <import-tamer-handbook>
         
         (exit (tamer-spec))}]

@tamer-action[(tamer-spec)]

and @chunk[<tamer-battle-via-raco>
           {module test racket
             <import-tamer-handbook>
             
             (exit (tamer-harness))}]

@tamer-action[(tamer-harness)]

@subsection{Scenario: The rules serve you!}

@chunk[<hello-rules!>
       <project-hierarchy>]

Since @italic{Behavior Driven Development} is the evolution of @italic{Test Driven Development} which does not define
what exactly should be tested and how would the tests be performed correct. The term @italic{Architecture} is all about designing
rules, and this story is all about building system. So apart from conventions, we need a sort of rules that the @italic{makefile.rkt}
(and systems it builds) should satisfy.

@(itemlist @item{@bold{Rule 1}: The entire project is a @italic{multi-collection package},
                  and (nonhidden) directories within it are considered as subprojects.}
           @item{@bold{Rule 2}: The subproject should have an explicit name,
                  although the name can be the same as its directory name.})

@chunk[<project-hierarchy>
       (require setup/getinfo)
       
       (define digimons (filter {lambda [sub] (and (directory-exists? sub)
                                                   (regexp-match? #px"^[^.]" sub))}
                                (directory-list rootdir)))
       (define info-root (get-info/full rootdir))
       (define rule-1
         (test-suite "Rule 1"
                     (test-suite "Look inside the `/info.rkt`"
                                 (test-case "'collection should be 'multi"
                                            (check-not-false info-root)
                                            (check-equal? (info-root 'collection) 'multi)))))
       (define rule-2
         (make-test-suite "Rule 2"
                          (map {lambda [digimon]
                                 (define info-ref (get-info/full (build-path rootdir digimon)))
                                 (test-suite (format "Look inside the `/~a/info.rkt`" digimon)
                                             (test-case "'collection should be string"
                                                        (check-not-false info-ref)
                                                        (check-pred string? (info-ref 'collection))))}
                               digimons)))]

@tamer-note['rule-1 'rule-2]
@tamer-action[(tamer-prove 'rule-2)]
