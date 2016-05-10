#lang scribble/lp2

@(require "tamer.rkt")

@(require setup/dirs)

@(require (for-label scribble/lp2))
@(require (for-label compiler/compiler))

@(define partner (tamer-partner->modpath "makefile.rkt"))

@(define-bib racket-doc
   #:title    "Racket Documentation"
   #:author   (authors "PLT")
   #:date     (seconds->date (file-or-directory-modify-seconds (find-doc-dir)))
   #:url      "http://docs.racket-lang.org"
   #:is-book? #true)

@(define-bib gnu-make
   #:title    "GNU Make: A Program for Directing Recompilation"
   #:author   (authors "Richard M. Stallman" "Roland McGrath" "Paul D. Smith")
   #:date     "2010"
   #:location (book-location #:edition "0.71" #:publisher "the Free Software Foundation")
   #:is-book? #true)

@(define-bib BDD
   #:title  "Behaviour-Driven Development"
   #:author (authors "Dan North")
   #:date   "2012"
   #:url    "http://behaviourdriven.org")

@(define-bib hspec
   #:title  "Hspec: A Testing Framework for Haskell"
   #:author (authors "Sönke Hahn" "Trystan Spangler" "Greg Weber")
   #:date   "2011"
   #:url    "http://hspec.github.io")

@(define what-a-pity
   @elem{as a part of the @tech{@bold{architecture}} should be checked per @tech{@bold{digimons}}.
 However this may affect test statistics of your clones, hence the changes in
 the generated @racketvarfont{README.md}, hence complains from @exec{git}})

@handbook-story{Hello, Hacker Hero!}

@margin-note*{See @~cite[(in-bib racket-doc ", Package Management in Racket")].}
Welcome to the @deftech{@italic{@hyperlink["https://github.com/digital-world"]{Digital World}}} where
@itemlist[
 @item{the @tech[#:key "Digital World"]{world} itself is a @italic{multi-collection package};}
 @item{@deftech[#:key "pkg-tamer"]{tamers} are you human guys, authors or developers; and}
 @nested{@deftech{digimons} are @italic{collections} with the @tech[#:key "digimon zone"]{Zone Hierarchy}
  @itemlist[
 @item{@deftech{@bold{digivice}} holds user interfaces. Namely, it works like @tt{bin}.}
 @item{@deftech{@bold{tamer}} holds the @|#%handbook|. Namely, it works like @tt{test}.}
 @item{@deftech{@bold{digitama}} is the egg of @tech{digimons}. Namely, sources within it are @bold{private} to others.}
 @nested{@deftech{@bold{stone}} stores immutable metadata files, configuration files and ancient sources to be translated.
    Yes, it@literal{'}s the @italic{Rosetta Stone} and works like @tt{share}.
    @itemlist[@item{@deftech{@bold{tongue}} stores internationalization resources.}]}]}]

This is @tech[(#%digimon)], a @tech{digimon} whose duty is helping developers construct the @tech{Digital World};
The term @deftech{@bold{Architecture}} is all about designing rules. Therefore, I am going to introduce the killer features,
conventions and @tech{rules} to you hacker heroes via checking the @deftech{building system} itself.

@margin-note{Why @cite{Racket} with @cite{Scribble} is amazing?
 We have an excellent @deftech{@bold{test report}} along with the documentation.}
@tamer-smart-summary[]

@margin-note{Also see @secref{ssh.rkt}. It is a great example of
 how @~cite[(in-bib racket-doc ", Submodules")] are used to manage different dialects of @cite{Racket}, and
 how @~cite[(in-bib racket-doc ", Macros")] can be used to make the documentation more readable.
 @linebreak[] @linebreak[] BTW, @racket[<make:*>] is a kinda auxiliary that useless for readers,
 it can be placed after @secref{readme-reference} since that unnumber section eats
 everything but its own list, hence, readers@literal{'} eyes will never be polluted.}
@chunk[|<make taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)

       (module+ tamer
         (define partner (tamer-partner->modpath "makefile.rkt"))
         (define make (dynamic-require partner 'main (const #false)))
         (module+ story |<make:*>|))]

@chunk[|<make:*>|
       (define notdir file-name-from-path)
       (define times (λ [px] (length (regexp-match* px (get-output-string $out)))))
       (define README.this (path->string (build-path (digimon-zone) "README.md")))

       |<define zone hierarchy>|
       |<make: setup>|
       |<make: teardown>|

       (define-tamer-suite make-option "Ready? It works!"
         (test-suite "make: simple options" |<testsuite: simple options>|)
         (test-suite "make: complex option" |<testcase: complex option>|))

       (define-tamer-suite birth-digimon "Birth a Baby Digimon"
         |<testsuite: birth a baby digimon>|
         |<testsuite: building the baby digimon>|)

       (define-tamer-suite digivice-option "Talk with Your Baby Digimon"
         (test-suite "digivice [action]" |<testcase: digivice action>|)
         (test-suite "digivice action [option]" |<testcase: digivice action option>|))]

@handbook-scenario[#:tag @(digimon-gnome)]{Ready? Go!}

You might have already familiar with the @~cite[gnu-make], nonetheless you are still free to check the options first.
Normal @cite{Racket} program always knows @Flag{h} or @DFlag{help} option:

@margin-note{@bold{Killer Feature} Codes are evaluated directly and results are incorporated in the documentation,
 which is a great supplement of the @cite{LP:WEB} approach.}
@tamer-action[(parameterize ([exit-handler void]) (make "--help"))]

Watch out! The buggy implementation may keep invoking another test routine endlessly in which case
this @#%handbook itself may be depended by some other files. Kill those unexpected routines crudely
is unreasonable since there might be side effects.

@chunk[|<make: setup>|
       (define ENV (current-environment-variables))
       (when (environment-variables-ref ENV #"taming")
         (error 'make "[fatal] Unexpected subroutine stops here!"))

       (define ((domake #:zone [zone babymon-name] #:check? [check? #false] . arglist))
         (define commandline (list* "++only" zone arglist))
         (dynamic-wind (thunk (environment-variables-set! ENV #"taming" #"true"))
                       (thunk (parameterize ([current-directory (digimon-world)])
                                ($shell (curry apply make commandline))
                                (define strerr (get-output-string $err))
                                (unless (or (false? check?) (zero?  (+ ($?) (file-position $err))))
                                  (error strerr))))
                       (thunk (environment-variables-set! ENV #"taming" #false))))]

So we are checking the @tech{building system} with @cite{Rackunit},
sure we should have some @racket[test-suite]s and @racket[test-case]s.

@tamer-note['make-option]{@bold{Killer Feature} We write test @racket[chunk]s,
 it reports results in the @racket[margin-note] like this and concludes like
 @tech[#:key "test report"]{@bold{that}}.}
@chunk[|<testsuite: simple options>|
       (test-suite "make --silent --help"
                   #:before (domake "--silent" "--help" #:zone (current-digimon))
                   (test-pred "should exit normally" zero? ($?))
                   (test-pred "should keep quiet" zero? (file-position $out)))
       (test-suite "make --silent love"
                   #:before (domake "--silent" "love" #:zone (current-digimon))
                   (test-pred "should exit abnormally" (negate zero?) ($?))
                   (test-check "should report errors" < 0 (file-position $err)))]

It seems that it works as expected, and so it does.
However the @racketvalfont{#:before} and @racketvalfont{#:after} routines are out of @racket[test-case] in which case
following proving on the same testsuite would lead to the duplicate and meaningless work during
the process of rendering the @#%handbook, so a @racket[test-case] with additional work sealed
inside would be better since we can cache the result by nature.

@chunk[|<testcase: complex option>|
       (test-spec "make --always-make README.md"
                  #:before (domake "--always-make" README.this #:zone (current-digimon))
                  (check-pred positive? ($?) "make should be invoked recursively!")
                  (check = (times #px"Leave Digimon Zone") 1 "has zone crossed!")
                  (check-regexp-match #px"subroutine stops here" (get-output-string $err)))]

@handbook-scenario{Birth a @tech{Baby} @tech{Digimon}}

Before we continue, we should name the @tech{baby} @tech{digimon}, say @racketidfont{babymon}.

@chunk[|<define zone hierarchy>|
       (define babymon-name "babymon")
       (define action.scrbl (build-path (digimon-stone) "action.rkt"))

       (define-values (baby-zone baby-digivice baby-digitama baby-tamer)
         (parameterize ([current-digimon babymon-name])
           (values (digimon-zone) (digimon-digivice) (digimon-digitama) (digimon-tamer))))

       (define README.md (build-path baby-zone "README.md"))
       (define info.rkt (build-path baby-zone "info.rkt"))
       (define digicore.rkt (build-path baby-digitama "digicore.rkt"))
       (define posix.rkt (build-path baby-digitama "posix.rkt"))
       (define handbook.scrbl (build-path baby-tamer "handbook.scrbl"))
       (define tamer.rkt (build-path baby-tamer "tamer.rkt"))
       (define robots.txt (build-path baby-tamer "robots.txt"))
      
       (define babymon (build-path baby-digivice babymon-name))
       (define babymon.rkt (path-add-extension babymon #".rkt"))
       (define action.rkt (build-path babymon "action.rkt"))

       (define babymon-main
         (lazy (parameterize ([exit-handler void]
                              [current-output-port /dev/null]
                              [current-error-port /dev/null])
                 (dynamic-require/expose babymon.rkt 'main))))]

Then we have its @deftech[#:key "digimon zone"]{zone hierarchy}:
@margin-note{@bold{Killer Feature} Drawing with code, then the resulting picture will be inserted into the documentation automatically.
 See @~cite[(in-bib racket-doc ", Pict")]}
@centered[(filesystem-tree '(~ (DigitalWorld #(info.rkt "[important, the identity of a digital world]" Red)
                                             (#&babymon #(info.rkt "[important, the identity of a digimon]" Red)
                                                        #(submake.rkt "[optional, if you have special files to be made]")
                                                        #(README.md "[auto generated, based on handbook.scrbl]")
                                                        (#(digivice "[optional, you can also try these templates]" Yellow)
                                                         (#(#&babymon "[name this dir after the registered digivice, defaults to the digimon's name]" Yellow)
                                                          (#&action.rkt . "[one of application entries, interface for end-users to talk with your digimon]"))
                                                         (#&babymon.rkt . "[auto generated, if registered in info.rkt and the its actions' subdir exists]"))
                                                        (digitama #(digicore.rkt "[important, you may need this all the time]" Red)
                                                                  (posix.rkt . "[optional, if you need FFI]"))
                                                        (tamer #(handbook.scrbl "[important, or you will blind to your work]" Red)
                                                               #(tamer.rkt "[important, it re-exports all you need for handbook.scrbl]" OrangeRed)
                                                               (robots.txt . "[for online handbook, leave it there even if it is empty]"))
                                                        (stone (#(tongue "[optional]")
                                                                English.rktl
                                                                Simplified-Chinese.rktl))))))]

Since all non-italic filenames and dirnames are significant, only @racketvarfont{babymon/action.rkt} is our goal here,
then hence the @racketvarfont{babymon.rkt}. @bold{Note that in this example the target @tech{digivice} is named after
the @tech{baby} @tech{digimon}.}

@tamer-racketbox[#:line-start-with 0 'action.scrbl]

This is the template of the user @deftech{action}, or @deftech{subcommand}. The most interesting things
are just a description @envvar{desc} and a @racket[module] named after the @tech{digivice}.

@tamer-note['birth-digimon]{The @tech[#:key "digimon zone"]{@bold{zone hierarchy}} @|what-a-pity|.}
@chunk[|<testsuite: birth a baby digimon>|
       (test-suite (format "make ++only ~a --dry-run --touch" babymon-name)
                   #:before (thunk ((curry with-input-from-string "Babymon")
                                    (domake "--dry-run" "--touch" #:check? #true))
                                   (make-directory* babymon)
                                   (putenv "digivice-name" babymon-name)
                                   (putenv "digivice-desc" "Digivice Demonstration")
                                   (with-output-to-file action.rkt #:exists 'replace
                                     (thunk (dynamic-require action.scrbl #false))))
                   (test-eq? "touching via making when goal not found" (times #px"Output to") 1)
                   (for ([goal (in-list (list info.rkt digicore.rkt action.rkt
                                              handbook.scrbl tamer.rkt robots.txt))])
                     (test-pred (format "~a should exist" (notdir goal)) file-exists? goal))
                   (for ([goal (in-list (list README.md babymon.rkt))])
                     (test-false (format "~a should not exist" (notdir goal)) (file-exists? goal))))]

@chunk[|<testsuite: building the baby digimon>|
       (test-suite (format "make ++only ~a" babymon-name)
                   #:before (domake #:check? #true)
                   (for ([goal (in-list (list README.md babymon.rkt))])
                     (test-pred (format "~a should exist" (notdir goal)) file-exists? goal)))]

@handbook-scenario{Talk with Your @tech{Baby} @tech{Digimon}}

Okay, everything is ready. Now you may want to know the @tech{Digital World} and @tech{digimon} in details:

@tamer-racketbox/region[(build-path (digimon-world) "info.rkt") #:pxend #px"Preferences for digimons"]

@margin-note*{Although @~cite[(in-bib racket-doc ", info.rkt")] is @bold{not} designed as a general purpose configuration file per se.
 Nonetheless, its highly constrained form makes it a great out-of-box solution. You know @cite{Racket} is homoiconic by nature,
 loading an unconstrained @racket[module] directly is dangerous.}
@itemlist[
 @nested{Notes about the @~cite[(in-bib racket-doc ", info.rkt")] of a @tech{digital world}
  @itemlist[
 @item{Those three @litchar{pkg-*}s are defined for generating @secref{handbook-digimon}. @linebreak[]
    @smaller{They also affects the URLs in the generated @racketvarfont{README.md}, in order to shut the @exec{git} up, you should keep them
     consistent among all your clones. Perhaps I need to figure out a better solution for this.}}
 @item{It may contain custom settings for @tech{digimons} since @tech{digimon} as a component can be shared by other @tech{digital world}.}]}]

@tamer-racketbox['info.rkt]

@margin-note{These @tech{@bold{rules}} @|what-a-pity|.}

@itemlist[
 @nested{@deftech{Rules} in @~cite[(in-bib racket-doc ", info.rkt")] of a @tech{digimon} in the @tech{Digital World}
  @itemlist[
 @item{@tech{digimon} should have an name explicitly, even if the name is the same as its directory.}
 @nested{@tech{digimon} should have a @deftech{stage}-like version name rather than the numeric one.
    @itemlist[@item{@bold{@deftech{Baby}}: The 1st stage of @italic{digimon evolution} hatching straightly from her @tech{digitama}.
                   Sounds like @tt{Alpha Version}.}
              @item{@bold{@deftech{Child}}: The 2nd stage of @italic{digimon evolution} evolving quickly from @tech{Baby}.
                   Sounds like @tt{Beta Version}.}
              @item{@bold{@deftech{Adult}}: The 3rd stage of @italic{digimon evolution} evolving from @tech{Child}.
                   At this stage @tech{digimons} are strong enough to live on their own.}]}
 @item{@tech{digimon} should have at least one @tech[#:key "pkg-tamer"]{tamer}.}
 @item{@racket[compile-collection-zos] and friends should never touch @tech{stone}.}
 @item{@~cite[(in-bib racket-doc ", raco test")] should do nothing since we would do testing in a more controllable way.}
 @item{@racketidfont{racket-launcher-names} and @racketidfont{racket-launcher-libraries} register the standarded @tech{digivices}.}]}]

@tamer-action[(parameterize ([exit-handler void]) ((force babymon-main)))]

So we have got the @tech{digivice}.

@tamer-note['digivice-option]
@chunk[|<testcase: digivice action>|
       (test-spec "digivice        [show help when no args]"
                  #:before (thunk ($shell (force babymon-main)))
                  (check-pred zero? ($?) (get-output-string $err))
                  (check-regexp-match #px"Usage:.+?where <action>(\\s+\\S+){3,}" (get-output-string $out)))
       (test-spec "digivice --help [a kind of mistyped action]"
                  #:before (thunk ($shell (force babymon-main) "--help"))
                  (check-pred (procedure-rename (negate zero?) 'nonzero?) ($?))
                  (check-regexp-match #px".+?<action>.+?Unrecognized" (get-output-string $err)))
       (test-spec "digivice action [mission start]"
                  #:before (thunk ($shell (force babymon-main) "action"))
                  (check-pred zero? ($?) (get-output-string $err)))]

So far it all works well. However the following tests are just based on this example,
nothing can be done to guarantee that real implementation @bold{is} taking care
the commandline arguments even though it is recommended to.

@chunk[|<testcase: digivice action option>|
       (test-spec "digivice action --help"
                  #:before (thunk ($shell (force babymon-main) "action" "--help"))
                  (check-pred zero? ($?) (get-output-string $err))
                  (check-regexp-match #px"where <option>(\\s+\\S+){3,}" (get-output-string $out)))
       (test-spec "digivice action --version"
                  #:before (thunk ($shell (force babymon-main) "action" "--version"))
                  (check-pred zero? ($?) (get-output-string $err))
                  (check-regexp-match #px"version:" (get-output-string $out)))
       (test-spec "digivice action --unknown"
                  #:before (thunk ($shell (force babymon-main) "action" "--unknown"))
                  (check-pred (procedure-rename (negate zero?) 'nonzero?) ($?))
                  (check-regexp-match #px"unknown switch" (get-output-string $err)))
       (test-spec "digivice action [job done]"
                  #:before (thunk ($shell (force babymon-main) "action" "job" "done"))
                  (check-pred zero? ($?) (get-output-string $err))
                  (check-regexp-match #px"(job done)" (get-output-string $out)))]

What a long journey, nonetheless we have a happy ending! But do not forget to restore the filesystem.

@chunk[|<make: teardown>|
       (register-handbook-finalizer
        (thunk (delete-directory/files baby-zone)))]

@handbook-scenario{Amazing! But, Here a Question, Why?}

To be honst, especially for independent developers, the @cite{LP:WEB} approach is more worth a introduce
in the certain situation. With combining other idea, say @~cite[BDD], we can easily and partially overtake
the @cite{LP:Issues}.

@margin-note{Actually if @racket[|<make: teardown>|] is made as a @racketvalfont{#:after} action of the
 @racket[test-suite], then it will do cleaning before performing the tests, hence the unexpected terminating
 of building this @bold{@|#%handbook|}. However this terrible termination will never show up with these two alternatives.}

Finally, the @#%handbook itself is also constructed by @tech[(#%digimon)], which means it may fail and
hence two more options both of which produces the @~cite[hspec] style report:

@(itemlist #:style 'compact
           @item{@exec{racket «@smaller{tamer files}»}}
           @item{@exec{makefile ++only «@smaller{digimon}» check «@smaller{tamer files}»}})

Note that @exec{raco test --submodule main} has been omitted in @tech[#:key "rule"]{info.rkt}.

@handbook-reference[]
