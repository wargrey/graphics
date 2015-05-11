#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@(define partner (tamer-partner->modpath "makefile.rkt"))

@handbook-story{Hello, Hacker Hero!}

Every hacker needs a robust developing infrastructure, more clearly meanwhile I mean @deftech{@hyperlink[@(cadr partner)]{makefile.rkt}}
to make life simple. However testing the building routines on the entire project always makes nonsense but costs high,
thus I will focus on @seclink[@(digimon-gnome)]{the meta-project @(digimon-gnome)} and @seclink["rules"]{project organization rules}.

@tamer-smart-summary[]

@chunk[|<infrastructure taming start>|
       (require "tamer.rkt")

       (tamer-taming-start)
       (define partner (tamer-partner->modpath "makefile.rkt"))
       (define make (dynamic-require partner 'main {λ _ #false}))

       |<infrastructure:*>|]

@handbook-scenario[#:tag @(digimon-gnome)]{Ready? Let@literal{'}s have a try!}

You may have already familiar with the @hyperlink["http://en.wikipedia.org/wiki/Make_(software)"]{GNU Make},
nonetheless you are still free to check the options first. Normal @bold{Racket} program always knows
@Flag{h} or @DFlag{help} option:

@tamer-action[(parameterize ([exit-handler void]) (make "--help"))]

Watch out! The buggy implementation may keep invoking another test routine endlessly in which case
this @itech{handbook} itself may be depended by some other files.
Kill those unexpected routines crudely is unreasonable since there might be side effects.

@chunk[|<setup and teardown timidly>|
       (define ENV (current-environment-variables))
       (when (environment-variables-ref ENV #"taming")
         (error 'make "[fatal] Unexpected subroutine stops here!"))

       (define {{setup . arglist}}
         (dynamic-wind {λ _ (environment-variables-set! ENV #"taming" #"true")}
                       {λ _ (parameterize ([current-directory (digimon-world)])
                              (call-with-fresh-$ (curry apply make arglist)))}
                       {λ _ (environment-variables-set! ENV #"taming" #false)}))]

Now let@literal{'}s try to make thing done:

@tamer-note['make-option]
@chunk[|<testsuite: simple options>|
       (test-suite "make --silent --help"
                   #:before (setup "--silent" "--help")
                   (test-pred "should exit normally" zero? ($?))
                   (test-pred "should keep quiet" zero? (file-position $out)))
       (test-suite "make --silent love"
                   #:before (setup "--silent" "love")
                   (test-pred "should exit abnormally" (negate zero?) ($?))
                   (test-check "should report errors" < 0 (file-position $err)))]

It seems that it works very well, and so it does. But the @italic{before} and @italic{after} routines are out of testcase
in which case the following proving of the same testsuite would lead to the duplicate and meaningless work during
the process of rendering the @itech{handbook}. So, a testcase with additional work sealed inside would be better
since we can cache the result by nature. 

@chunk[|<testcase: complex options>|
       (let* ([goal-md (build-path (digimon-world) "README.md")]
              [make-md (list "--always-make" "++only" (digimon-gnome)
                             (path->string (file-name-from-path goal-md)))])
         (test-spec (string-join (cons "make" make-md))
                    #:before (apply setup make-md)
                    |<check status and stderr>|
                    (check = (times #px"Leave Digimon Zone") 1 "has zone crossed!")
                    (check = (times #px"make: made") 1 "has too many files made!")
                    (check <= (msecs (digimon-zone)) (msecs goal-md) "goal not updated!")))
       (let ([goal-md (build-path (digimon-world) gnome "README.md")]
             [make-zone (list "--dry-run" "--touch" "++only" gnome)])
         (test-spec (string-join (cons "make" make-zone))
                    #:before {λ _ (parameterize ([current-input-port (open-input-string "Gnome")])
                                    ((apply setup make-zone)))}
                    |<check status and stderr>|
                    (check = (times #px"Leave Digimon Zone") 1 "has zone crossed!")
                    (check = (times #px"Output to") 1 "touching via making when goal not found!")
                    (check-pred (negate file-exists?) goal-md "goal should not exists!")))]

@handbook-scenario[#:tag "rules"]{The rules serve you!}

Since the term @deftech{Architecture} is all about designing rules, and this story is all about building system.
So apart from @italic{@hyperlink["https://github.com/digital-world/DigiGnome"]{conventions}},
we need a sort of rules that the @itech{makefile.rkt} (and systems it builds) should satisfy.

@subsection{Rules on project organization}

@tamer-note['rules]
@handbook-rule{The entire project is a multi-collection package, non-hidden directories within it are considered as the subprojects.}
@chunk[|<facts: multi>|
       "multi"
       (check-equal? (info-ref 'collection) 'multi "'collection should be 'multi!")
       (check < 1 (length (force digimons)) "No real project found!")]

@handbook-rule{Each subproject has a @italic{stage}-like version name rather than the numeric one.}
@chunk[|<facts: version>|
       "version"
       (check member (info-ref 'version) '{"Baby" "Child" "Adult"}
              "'version should be one of evolution stages!")]

@handbook-rule{Each subproject should have an explicit name, even if the name is the same as its directory.}
@chunk[|<facts: collection>|
       "collection"
       (check-pred string? (info-ref 'collection) "'collection should be string!")]

@handbook-rule{@racket[compile-collection-zos] and friends should never touch special paths.}
@chunk[|<facts: compile-omit-paths>|
       "compile-omit-paths"
       (for ([omit (in-list (list (path->string (file-name-from-path (digimon-stone)))))])
         (check-not-false (let ([maybe-omits (info-ref 'compile-omit-paths)])
                            (or (equal? maybe-omits 'all) (member omit maybe-omits)))
                          (format "'compile-omit-paths should contain ~a!" omit)))]

@handbook-rule{@exec{raco test} should do nothing since we would do testing in a more controllable way.}
@chunk[|<facts: test-omit-paths>|
       "test-omit-paths"
       (check-equal? (info-ref 'test-omit-paths) 'all "'test-omit-paths should be 'all!")]

@subsection{Rules on project documentation}

@handbook-rule{The project@literal{'}s toplevel @italic{README.md} is designated as the @italic{main-toc} of @bold{Scribble}.}
@chunk[|<facts: readme.scrbl>|
       (format "~a/~a" (digimon-gnome) top.scrbl)
       (check-pred file-exists? (build-path (digimon-zone) top.scrbl))]

@handbook-rule{Each subproject@literal{'}s @italic{README.md} follows its @itech{handbook}@literal{'}s index page.}
@chunk[|<facts: handbook.scrbl>|
       (format "~a/~a" digimon sub.scrbl)
       (check-pred file-exists? (build-path (digimon-world) digimon sub.scrbl))]

@handbook-scenario{What if the @itech{handbook} is unavaliable?}

Furthermore, the @itech{handbook} itself is the standard test report, but it@literal{'}s still reasonable
to check the system in some more convenient ways. Hence we have

@chunk[|<tamer battle>|
       {module+ main
         (call-as-normal-termination tamer-prove)}]

It will give us the @hyperlink["http://hspec.github.io"]{@italic{hspec-like}} report via
@(itemlist #:style 'compact
           @item{@exec{racket «@smaller{tamer files}»}}
           @item{@exec{makefile ++only «@smaller{digimon}» check «@smaller{tamer files}»}})

Technically speaking, @exec{raco test --submodule main} is always there,
although that way is not recommended, and is omitted by @filepath{info.rkt}.

@handbook-appendix[]

@chunk[|<infrastructure:*>|
       |<tamer battle>|
       {module+ story
         (define msecs file-or-directory-modify-seconds)
         (define times {λ [px] (length (regexp-match* px (get-output-string $out)))})
         (define gnome (symbol->string (gensym "gnome")))
         
         |<setup and teardown timidly>|
         (define-tamer-suite make-option "Ready? It works!"
           (test-suite "make: simple options" |<testsuite: simple options>|)
           (test-suite "make: complex options" |<testcase: complex options>|))

         (define digimons (lazy (parameterize ([current-directory (digimon-world)])
                                  (filter (curry regexp-match? #px"^[^.]")
                                          (filter directory-exists?
                                                  (directory-list))))))

         (define-tamer-suite rules "Rules serve you!"
           #:after {λ _ (delete-directory/files (build-path (digimon-world) gnome))}
           (test-suite "info.rkt settings" |<rules: info.rkt>|)
           (test-suite "README.md dependencies" |<rules: readme.md>|))}]

@chunk[|<rules: info.rkt>|
       (let ([info-ref (get-info/full (digimon-world))])
         (test-suite "/info.rkt" (test-case |<facts: multi>|)))
       (for/list ([digimon (in-list (force digimons))])
         (define info-ref (get-info/full (build-path (digimon-world) digimon)))
         (test-suite (format "/~a/info.rkt" digimon)
                     (test-case |<facts: version>|)
                     (test-case |<facts: collection>|)
                     (test-case |<facts: compile-omit-paths>|)
                     (test-case |<facts: test-omit-paths>|)))]

@chunk[|<rules: readme.md>|
       (let* ([~scrbl (compose1 (curry find-relative-path (digimon-zone)) build-path)]
              [top.scrbl (~scrbl (digimon-stone) "readme.scrbl")]
              [sub.scrbl (~scrbl (digimon-tamer) "handbook.scrbl")])
         (cons (test-suite "/README.md" (test-case |<facts: readme.scrbl>|))
               (for/list ([digimon (in-list (force digimons))])
                 (test-suite (format "/~a/readme.md" digimon)
                             (test-case |<facts: handbook.scrbl>|)))))]

@chunk[|<check status and stderr>|
       (let ([strerr (get-output-string $err)])
         (check-pred zero? ($?) strerr)
         (check-pred zero? (file-position $err) strerr))]

