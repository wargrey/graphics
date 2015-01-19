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

@chunk[<*:outline>
       {module story racket
         <ready?-help!>
         <hello-rules!>}]

@margin-note{@racket[subsection]s: Each of them describes a scenario.}
@subsection{Scenario: Ready? Let@literal{'}s have a try!}

@margin-note{The structure and testsuites follow the
             @hyperlink["http://en.wikipedia.org/wiki/Hoare_logic"]{Hoare Logic}: @(itemlist @item{Initial Conditions}
                                                                                             @item{Event Triggers}
                                                                                             @item{Expected Outcome})}

@chunk[<ready?-help!>
       <tamer-routine>
       <ready?-suites>]

Although normal @bold{Racket} script doesn@literal{'}t require the so-called @racketidfont{main} routine,
I still prefer to start with the @racketidfont{main} in production. So the @tamer-action[tamer-partner] should
provide the @racketidfont{main} as its entry: @defproc[{main [argument string?] ...} void?]

To make it more meaningful
the @chunk[<tamer-routine>
           (require "tamer.rkt")
           (require (submod "tamer.rkt" makefile))
           
           (define make (dynamic-require tamer-partner 'main (const #false)))]
make the @racketidfont{make} as the alias for @racketidfont{main}.

You may be already familiar with the @hyperlink["http://en.wikipedia.org/wiki/Make_(software)"]{GNU Make},
nonetheless you are still free to check the options first. Normal @bold{Racket} program always knows
@exec{@|-~-|h} or @exec{@|-~-|@|-~-|help} option:

@tamer-action[tamer-require
              ((tamer-require 'make) "--help")
              (code:comment @#,t{See, @racketcommentfont{@italic{makefile}} complains that @racketcommentfont{@bold{Scribble}} is killed by accident.})
              (code:comment @#,t{Meanwhile parallel building is not supported.})]

Now it@literal{'}s time to interact with @chunk[<ready?-suites>
                                                <suite:option-ready?>
                                                <suite:goal-not-ready!>]
to look deep into the specification templates:

@itemlist{@item{An example of the testsuite that should pass and do pass:}}
@tamer-note['option-ready?]
@chunk[<suite:option-ready?>
       (define option-ready?
         (test-suite "makefile.rkt [maybe option]"
                     (test-case "should exit normally"
                                (check-$? {thunk (make "--help")} 0))
                     (test-case "should abort if facing unknown option"
                                (check-$? {thunk (make "--undef")} 1))
                     (test-case "should say nothing unless errors ocurr"
                                (let ([stdout (open-output-string 'stdout)]
                                      [stderr (open-output-string 'stderr)])
                                  (parameterize ([current-output-port stdout]
                                                 [current-error-port stderr]
                                                 [exit-handler void])
                                    (make "--silent" "--help")
                                    (make "--silent" "--undefined"))
                                  (let ([strout (get-output-string stdout)]
                                        [strerr (get-output-string stderr)])
                                    (check-equal? strout "")
                                    (check-not-equal? strerr ""))))))]
@tamer-action[tamer-script
              (tamer-script 'option-ready?)]

@itemlist{@item{An example of the testsuite that should pass but do fail:}}
@tamer-note['goal-not-ready!]
@chunk[<suite:goal-not-ready!>
       (define goal-not-ready!
         (test-suite "makefile.rkt [maybe target]"
                     (test-case "should abort if using default exit-handler"
                                (check-not-exn {thunk (make "love")}
                                               "Scribble is killed!"))))]
@tamer-action[(tamer-script 'goal-not-ready!)
              (code:comment @#,t{See. @racketcommentfont{@bold{Racket/Scribble}} has no idea about the broken lines.})]

@itemlist{@item{An example of the typo that should never happen:}}
@tamer-note['maybe-typo!]
@tamer-action[(tamer-script 'maybe-typo!)]

Equipping with the powerful @bold{Scribble} that @bold{Racket} gives us,
writing the @italic{handbook} becomes fantastic. We can @racket[eval] the code while
@racket[render]ing the documents along with it. Since the two ways,
via @racket[margin-note](@racket[tamer-note]) and via @racket[interaction](@racket[tamer-action]),
are just duplicate work, there is no need to run them at the same @italic{handbook}.
As examples, I just show you the effects of the two ways, then you can choose your favor.

@subsection{Scenario: The rules serve you!}

Since @italic{Behavior Driven Development} is the evolution of @italic{Test Driven Development} which doesn@literal{'}t define
what exactly should be tested and how would the tests be performed correct.
So we say @chunk[<hello-rules!>
                 (void)]
that apart from the conventions.
