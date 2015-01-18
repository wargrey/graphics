#lang scribble/lp

@(require "tamer.rkt" (submod "tamer.rkt" makefile))

@(current-tamer-zone (tamer-zone))

@margin-note{@racketid[section]: The story should have a clear, explicit title.}
@section{Story: Hello, Hacker Hero!}

@margin-note{The brief specifies: @(itemlist @item{Primary Drivers}
                                             @item{Story Effects}
                                             @item{Effects@literal{'} Value}
                                             @item{Scenarios Outline})}
Every hacker needs a @italic{makefile.rkt} to make life simple, and the @italic{makefile.rkt} should always be on its best behavior.
So I@literal{'}m glad to help you to ensure that the @hyperlink[(collection-file-path "makefile.rkt" (getenv "digimon-gnome"))]{makefile.rkt}
works as you wish.

@chunk[<*:outline>
       {module story racket
         <ready?-help!>}]

@margin-note{@racketid[subsection]s: Each of them describes a scenario.}
@subsection{Scenario: Ready? Let@literal{'}s have a try!}

@margin-note{The structure and testsuites follow the
             @hyperlink["http://en.wikipedia.org/wiki/Hoare_logic"]{Hoare Logic}: @(itemlist @item{Initial Conditions}
                                                                                             @item{Event Triggers}
                                                                                             @item{Expected Outcome})}

You may be already familiar with the @hyperlink["http://en.wikipedia.org/wiki/Make_(software)"]{GNU Make},
nonetheless you are still free to check the options first. Normal @bold{Racket} program always knows
@exec{@|-~-|h} or @exec{@|-~-|@|-~-|help} option:

@tamer-action[((tamer-eval 'main) "--help")
              (code:comment "See, makefile complains that the Scribble is killed by accident.")
              (code:comment "Meanwhile parallel building is not supported.")]

Now it@literal{'}s time to interact with @chunk[<ready?-help!>
                                                (require "tamer.rkt")
                                                (require (submod "tamer.rkt" makefile))
                                               
                                                (define make (dynamic-require tamer-partner 'main (const #false)))
                                          
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
                                                                             (check-not-equal? strerr ""))))))
                                                
                                                (define goal-not-ready!
                                                  (test-suite "makefile.rkt [maybe target]"
                                                              (test-case "should abort if using default exit-handler"
                                                                         (check-not-exn {thunk (make "love")}
                                                                                        "Scribble is killed!"))))]
to look deep into the specification templates:

@tamer-note['option-ready? 'goal-not-ready! 'Iam-invisible!]
@tamer-action[tamer-partner
              tamer-eval
              (run-tests (tamer-eval 'option-ready?))
              (run-tests (tamer-eval 'goal-not-ready!))
              (code:comment "See. Racket/Scribble has no idea about the broken lines.")
              (run-tests (tamer-eval 'I-am-invisible!))]

Equipping with the powerful @bold{Scribble} that @bold{Racket} gives us,
writing the @italic{handbook} becomes fantastic. We can @racket[eval] the code while
@racket[render]ing the documents along with it. Since the two ways,
via @racket[margin-note](@racket[tamer-note]) and via @racket[interaction](@racket[tamer-action]),
are just duplicate work, there is no need to run them at the same @italic{handbook}.
As an example, I just show you the effects of the two ways, then you can choose your favor.
