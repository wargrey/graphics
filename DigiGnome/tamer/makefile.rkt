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

Although normal @bold{Racket} script doesn@literal{'}t require the so-called @racketidfont{main} routine,
I still prefer to start with @defproc[{main [argument string?] ...} void?]

and the story starts with @chunk[<*:outline>
                                 {module story racket
                                   <tamer-routine>
                                   
                                   <ready?-help!>
                                   <hello-rules!>}
                                                  
                                 <tamer-spec>]

where @chunk[<tamer-routine>
             (require "tamer.rkt")
             (require (submod "tamer.rkt" makefile))
           
             (define make (dynamic-require tamer-partner 'main (const #false)))]

and @chunk[<tamer-spec>
           {module test racket
             (require "tamer.rkt")
             (require (submod "tamer.rkt" makefile))
             
             (tamer-spec)}]

@margin-note{@racket[subsection]s: Each of them describes a scenario.}
@subsection{Scenario: Ready? Let@literal{'}s have a try!}

@margin-note{The structure and testsuites follow the
             @hyperlink["http://en.wikipedia.org/wiki/Hoare_logic"]{Hoare Logic}: @(itemlist @item{Initial Conditions}
                                                                                             @item{Event Triggers}
                                                                                             @item{Expected Outcome})}

@chunk[<ready?-help!>
       <testsuite:option-ready?>
       <testsuite:goal-not-ready!>]

You may be already familiar with the @hyperlink["http://en.wikipedia.org/wiki/Make_(software)"]{GNU Make},
nonetheless you are still free to check the options first. Normal @bold{Racket} program always knows
@exec{@|-~-|h} or @exec{@|-~-|@|-~-|help} option:

@tamer-action[tamer-require
              ((tamer-require 'make) "--help")
              (code:comment @#,t{See, @racketcommentfont{@italic{makefile}} complains that @racketcommentfont{@bold{Scribble}} is killed by accident.})
              (code:comment @#,t{Meanwhile parallel building is not supported.})]

Now it@literal{'}s time to look deep into the specification examples of
@itemlist{@item{a testsuite that should pass and do pass:}}
@tamer-note['option-ready?]
@chunk[<testsuite:option-ready?>
       (define option-ready?
         (test-suite "make [option]"
                     (test-suite "make --help"
                                 (test-$? "should exit normally"
                                          {thunk (make "--help")} 0))
                     (test-suite "make [unknown option]"
                                 (test-$? "should abort"
                                          {thunk (make "--unknown")} 1))
                     (let ([words (stdpipe->cons {thunk (make "-s" "--help")
                                                        (make "-s" "--say")})])
                       (test-suite "make --silent"
                                   (test-case "should say nothing but errors"
                                              (check-regexp-match
                                               #rx"^\\( \\. .+?\\)$"
                                               (format "~a" words)))))))]
@tamer-action[tamer-script
              (tamer-script 'option-ready?)]

@itemlist{@item{a testsuite that should pass but do fail:}}
@tamer-note['goal-not-ready!]
@chunk[<testsuite:goal-not-ready!>
       (define goal-not-ready!
         (test-suite "make [phony target]"
                     (test-suite "make love"
                                 (test-not-exn "should catch a fatal to regain control"
                                               {thunk (make "love")}))))]
@tamer-action[(tamer-script 'goal-not-ready!)]

@itemlist{@item{a typo that should never happen:}}
@tamer-note['maybe-typo!]
@tamer-action[(tamer-script 'maybe-typo!)]

Equipping with the powerful @bold{Scribble} that @bold{Racket} gives us,
writing the @italic{handbook} becomes fantastic. We can @racket[eval] the code while
@racket[render]ing the documents along with it. Since the two ways,
via @racket[margin-note](@racket[tamer-note]) and via @racket[interaction](@racket[tamer-action]),
are just duplicate work, there is no need to run them at the same @italic{handbook}.
I just show you the effects of the two ways, then you can choose your favor.

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

@tamer-note['rule-1 'rule-2]
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
                                             (test-case (format "'collection should be string" digimon)
                                                        (check-not-false info-ref)
                                                        (check-pred string? (info-ref 'collection))))}
                               digimons)))]

@tamer-action[(tamer-script 'rule-2)]