#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@(tamer-story (tamer-story->libpath "infrastructure.rkt"))
@(define partner `(file ,(format "~a/makefile.rkt" (digimon-world))))
@(tamer-zone (make-tamer-zone))

@handbook-story{Hello, Hacker Hero!}

Every hacker needs a robust developing infrastructure, more clearly meanwhile I mean @deftech{@hyperlink[@(cadr partner)]{makefile.rkt}}
to make life simple. However testing the building routines on the entire project always makes nonsense but costs high,
thus I will focus on @seclink[@(digimon-gnome)]{the meta-project @(digimon-gnome)} and @seclink["rules"]{project organization rules}.

@tamer-smart-summary[]

@chunk[|<infrastructure taming start>|
       (require "tamer.rkt")
             
       (tamer-story (tamer-story->libpath "infrastructure.rkt"))
       (define partner `(file ,(format "~a/makefile.rkt" (digimon-world))))
       (define make (dynamic-require partner 'main {λ _ #false}))

       |<infrastructure:*>|]

@handbook-scenario[#:tag @(digimon-gnome)]{Ready? Let@literal{'}s have a try!}

You may have already familiar with the @hyperlink["http://en.wikipedia.org/wiki/Make_(software)"]{GNU Make},
nonetheless you are still free to check the options first. Normal @bold{Racket} program always knows
@exec{@|-~-|h} or @exec{@|-~-|@|-~-|help} option:

@tamer-action[(parameterize ([exit-handler void])
                ((dynamic-require/expose (tamer-story) 'make) "--help"))]

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
              [make-md (list "--always-make" "--touch" "++only" (digimon-gnome)
                             (path->string (file-name-from-path goal-md)))])
         (test-spec (string-join (cons "make" make-md))
                    #:before (apply setup make-md)
                    |<check status and stderr>|
                    (check = (times #px"Leave Digimon Zone") 1 "has zone crossed!")
                    (check = (times #px"make: made") 1 "has too many files made!")
                    (check <= (msecs (digimon-zone)) (msecs goal-md) "goal not updated!")
                    (check = (times #px"Output to") 0 "touching is okay!")))
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

@subsubsection{Rules on project organization}

@tamer-note['rules]
@handbook-rule[1]{The entire project is a multi-collection package, non-hidden directories within it are considered as the subprojects.}
@chunk[|<facts: rule 1>|
       "Rule 1: multi"
       (check-equal? (info-ref 'collection) 'multi "'collection should be 'multi!")
       (check < 1 (length (force digimons)) "No real project found!")]

@handbook-rule[2]{Each subproject has a @italic{stage}-like version name rather than the numeric one.}
@chunk[|<facts: rule 2>|
       "Rule 2: version"
       (check member (info-ref 'version) '{"Baby" "Child" "Adult"}
              "'version should be one of evolution stages!")]

@handbook-rule[3]{Each subproject should have an explicit name, even if the name is the same as its directory.}
@chunk[|<facts: rule 3>|
       "Rule 3: collection"
       (check-pred string? (info-ref 'collection) "'collection should be string!")]

@handbook-rule[4]{@racket[compile-collection-zos] and friends should never touch special paths.}
@chunk[|<facts: rule 4>|
       "Rule 4: compile-omit-paths"
       (for ([omit (in-list (list (path->string (file-name-from-path (digimon-stone)))))])
         (check-not-false (let ([maybe-omits (info-ref 'compile-omit-paths)])
                            (or (equal? maybe-omits 'all) (member omit maybe-omits)))
                          (format "'compile-omit-paths should contain ~a!" omit)))]

@handbook-rule[5]{@exec{raco test} should do nothing since we would do testing in a more controllable way.}
@chunk[|<facts: rule 5>|
       "Rule 5: test-omit-paths"
       (check-equal? (info-ref 'test-omit-paths) 'all "'test-omit-paths should be 'all!")]

@subsubsection{Rules on project documentation}

@handbook-rule[6]{The project@literal{'}s toplevel @italic{README.md} is designated as the @italic{main-toc} of @bold{Scribble}.}
@chunk[|<facts: rule 6>|
       (format "Rule 6: ~a/~a" (digimon-gnome) top.scrbl)
       (check-pred file-exists? (build-path (digimon-zone) top.scrbl))]

@handbook-rule[7]{Each subproject@literal{'}s @italic{README.md} follows its @itech{handbook}@literal{'}s index page.}
@chunk[|<facts: rule 7>|
       (format "Rule 7: ~a/~a" digimon sub.scrbl)
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
         (test-suite "/info.rkt" (test-case |<facts: rule 1>|)))
       (for/list ([digimon (in-list (force digimons))])
         (define info-ref (get-info/full (build-path (digimon-world) digimon)))
         (test-suite (format "/~a/info.rkt" digimon)
                     (test-case |<facts: rule 2>|)
                     (test-case |<facts: rule 3>|)
                     (test-case |<facts: rule 4>|)
                     (test-case |<facts: rule 5>|)))]

@chunk[|<rules: readme.md>|
       (let* ([~scrbl (compose1 (curry find-relative-path (digimon-zone)) build-path)]
              [top.scrbl (~scrbl (digimon-stone) "readme.scrbl")]
              [sub.scrbl (~scrbl (digimon-tamer) "handbook.scrbl")])
         (cons (test-suite "/README.md" (test-case |<facts: rule 6>|))
               (for/list ([digimon (in-list (force digimons))])
                 (test-suite (format "/~a/readme.md" digimon)
                             (test-case |<facts: rule 7>|)))))]

@chunk[|<check status and stderr>|
       (let ([strerr (get-output-string $err)])
         (check-pred zero? ($?) strerr)
         (check-pred zero? (file-position $err) strerr))]

