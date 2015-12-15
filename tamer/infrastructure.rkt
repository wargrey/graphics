#lang scribble/lp2

@(require "tamer.rkt")

@(define partner (tamer-partner->modpath "makefile.rkt"))

@handbook-story{Hello, Hacker Hero!}

Every hacker needs a robust developing infrastructure, more clearly meanwhile I mean @deftech{@hyperlink[@(cadr partner)]{makefile.rkt}}
to make life simple. However testing the building routines on the entire project always makes nonsense but costs high,
thus I will focus on @seclink[@(digimon-gnome)]{the meta-project @(digimon-gnome)}.

@tamer-smart-summary[]

@margin-note{Want a typed example? see @secref{ssh.rkt}}
@chunk[|<infrastructure taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)

       (module+ tamer
        (define partner (tamer-partner->modpath "makefile.rkt"))
        (define make (dynamic-require partner 'main (const #false)))

        (module+ story |<infrastructure:*>|))]

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

       (define ((setup . arglist))
         (dynamic-wind (thunk (environment-variables-set! ENV #"taming" #"true"))
                       (thunk (parameterize ([current-directory (digimon-world)])
                              (call-with-fresh-$ (curry apply make arglist))))
                       (thunk (environment-variables-set! ENV #"taming" #false))))]

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
                    (check-pred positive? ($?) "make should be invoked recursively!")
                    (check = (times #px"Leave Digimon Zone") 1 "has zone crossed!")
                    (check-regexp-match #px"subroutine stops here" (get-output-string $err))))
       (let ([goal-md (build-path (digimon-world) gnome "README.md")]
             [make-zone (list "--dry-run" "--touch" "++only" gnome)])
         (test-spec (string-join (cons "make" make-zone))
                    #:before (thunk (parameterize ([current-input-port (open-input-string "Gnome")])
                                      ((apply setup make-zone))))
                    (let ([strerr (get-output-string $err)])
                      (check-pred zero? ($?) strerr)
                      (check-pred zero? (file-position $err) strerr))
                    (check = (times #px"Leave Digimon Zone") 1 "has zone crossed!")
                    (check = (times #px"Output to") 1 "touching via making when goal not found!")
                    (check-pred (negate file-exists?) goal-md "goal should not exists!")))]

@handbook-scenario{What if the @itech{handbook} is unavaliable?}

Furthermore, the @itech{handbook} itself is the standard test report, but it@literal{'}s still reasonable
to check the system in some more convenient ways. Hence we have
the @hyperlink["http://hspec.github.io"]{@italic{hspec-like}} report via
@(itemlist #:style 'compact
           @item{@exec{racket «@smaller{tamer files}»}}
           @item{@exec{makefile ++only «@smaller{digimon}» check «@smaller{tamer files}»}})
Technically speaking, @exec{raco test --submodule main} is always there,
although that way is not recommended, and is omitted by @filepath{info.rkt}.

@handbook-bibliography[]

@chunk[|<infrastructure:*>|
       (define msecs file-or-directory-modify-seconds)
       (define times (λ [px] (length (regexp-match* px (get-output-string $out)))))
       (define gnome "gnome")
         
       |<setup and teardown timidly>|
       (define-tamer-suite make-option "Ready? It works!"
         #:after (thunk (delete-directory/files (build-path (digimon-world) gnome)))
         (test-suite "make: simple options" |<testsuite: simple options>|)
         (test-suite "make: complex options" |<testcase: complex options>|))]
