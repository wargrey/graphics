#!/bin/sh

#|
test "$1" = "clean" && find . -name "digicore_rkt.zo" -exec rm -fr {} ';';
exec racket --require "$0" --main -- ${1+"$@"} 
|#

#lang racket

(require make)
(require setup/getinfo)
(require compiler/compiler)
(require launcher/launcher)

(require "DigiGnome/digitama/digicore.rkt")

(provide main)

(define-namespace-anchor makefile)
(define info-ref (get-info/full (digimon-world)))

(define make-dry-run (make-parameter #false))
(define make-always-run (make-parameter #false))
(define make-just-touch (make-parameter #false))

(define current-make-collects (make-parameter null))
(define current-make-real-targets (make-parameter null))
(define current-make-phony-goal (make-parameter #false))

(make-print-dep-no-line #false)
(make-print-checking #false)
(make-print-reasons #false)

(define hack-rule
  {lambda [r]
    (define t (car r))
    (define ds (cadr r))
    (define f {λ _ (with-handlers ([symbol? void])
                     (make-parent-directory* t)
                     (call-with-atomic-output-file t {λ [whocares pseudo-t] #| pseudo-t is in the same dir|#
                                                       (close-output-port whocares)
                                                       ((caddr r) pseudo-t)
                                                       (when (make-dry-run)
                                                         (raise 'make-dry-run #true))}))})
    (list (car r) (if (make-always-run) (cons (digimon-zone) ds) ds)
          (cond [(false? (make-just-touch)) f]
                [(make-dry-run) void]
                [else {λ _ (file-or-directory-modify-seconds t (current-seconds) f)}]))})

(define compile-directory
  {lambda [cmpdir finfo]
    (define-values {/dev/make/stdin /dev/make/stdout} (make-pipe #false 'filter-checking 'verbose-message))
    (define px.inside (pregexp (if (make-print-checking) (digimon-world) (path->string (digimon-zone)))))
    (thread {λ _ (for ([line (in-port read-line /dev/make/stdin)])
                   (cond [(regexp-match? px.inside line) (printf "~a~n" line)]
                         [(regexp-match? #px":\\s+.+?\\.rkt(\\s|$)" line) 'Skip-Others-Packages]
                         [else (printf "~a~n" line)]))})
    (dynamic-wind {λ _ (void #| compiling in the main thread is good for breaking when exception occures |#)}
                  {λ _ (parameterize ([current-output-port /dev/make/stdout]
                                      [current-error-port /dev/make/stdout])
                         (with-handlers ([exn? (compose1 (curry error 'make "[error] ~a") exn-message)])
                           (compile-directory-zos cmpdir finfo #:verbose #true #:skip-doc-sources? #true)))}
                  {λ _ (close-output-port /dev/make/stdout)})})

(define make-digivice
  {lambda [template.rkt dgvc-name dgvc.rkt]
    (with-output-to-file dgvc.rkt #:exists 'replace
      {λ _ (void (putenv "current-digivice" dgvc-name)
                 (putenv "digicore.rkt" (path->string (find-relative-path (path-only dgvc.rkt) digicore.rkt)))
                 (dynamic-require template.rkt #false))})
    (let ([chmod (file-or-directory-permissions dgvc.rkt 'bits)])
      (file-or-directory-permissions dgvc.rkt (bitwise-ior chmod #o111)))})

(define smart-dependencies
    {lambda [entry [memory null]]
      (foldl {lambda [subpath memory]
               (define subsrc (simplify-path (build-path (path-only entry) (bytes->string/utf-8 subpath))))
               (cond [(member subsrc memory) memory]
                     [else (smart-dependencies subsrc memory)])}
             (append memory (list entry))
             (call-with-input-file entry (curry regexp-match* #px"(?<=@(include-section|lp-include|require)\\{).+?.(scrbl|rkt)(?=\\})")))})

(define make-implicit-rules
  {lambda []
    (define d-info (get-info/full (digimon-zone)))
    (define stone/digivice.rkt (parameterize ([current-digimon (digimon-gnome)]) (build-path (digimon-stone) "digivice.rkt")))
    (append (foldl {λ [n r] (append (filter list? n) r)} null
                   (map {λ [dgvc-name dgvc-lib]
                          (define dgvc.rkt (path->string (build-path (digimon-zone) dgvc-lib)))
                          (list (let ([t.dir (path-replace-suffix dgvc.rkt #"")]
                                      [ds (list stone/digivice.rkt (syntax-source #'makefile))])
                                  (when (directory-exists? t.dir)
                                    (list dgvc.rkt ds (curry make-digivice (car ds) dgvc-name))))
                                (let ([t (simplify-path (build-path (digimon-world) (digimon-gnome) (car (use-compiled-file-paths)) dgvc-name))]
                                      [ds (list (syntax-source #'makefile))])
                                  (list t ds (curry make-racket-launcher (list "-t-" dgvc.rkt)))))}
                        (d-info 'racket-launcher-names {λ _ null})
                        (d-info 'racket-launcher-libraries {λ _ null})))
            (map {λ [dependent.scrbl]
                   (define top? (regexp-match? #px"/readme.scrbl$" dependent.scrbl))
                   (define homepage (format "http://gyoudmon.org/~~~a/.~a" (getenv "USER") (string-downcase (current-digimon))))
                   (define-values {t ds} (cond [top? (values (build-path (digimon-world) "README.md")
                                                             (list* dependent.scrbl (syntax-source #'makefile)
                                                                    (filter file-exists? (map (curryr build-path "info.rkt")
                                                                                              (directory-list (digimon-world) #:build? #true)))))]
                                               [else (values (build-path (digimon-zone) "README.md")
                                                             (filter file-exists? (list* (syntax-source #'makefile) (build-path (digimon-zone) "info.rkt")
                                                                                         (smart-dependencies dependent.scrbl))))]))
                   (list t ds {λ [target]
                                (parameterize ([current-directory (digimon-zone)]
                                               [current-namespace (make-base-namespace)]
                                               [exit-handler {λ _ (error 'make "[fatal] /~a needs a proper `exit-handler`!"
                                                                         (find-relative-path (digimon-world) dependent.scrbl))}])
                                  (namespace-require 'scribble/core)
                                  (namespace-require 'scribble/base)
                                  (namespace-require 'scribble/render)
                                  (eval '(require (prefix-in markdown: scribble/markdown-render)))
                                  (eval `(define markdown:doc (let ([scribble:doc (dynamic-require ,dependent.scrbl 'doc)])
                                                                (struct-copy part scribble:doc
                                                                             [title-content (cons (hyperlink ,homepage ,(format "~a<sub>~a</sub>" :house-garden: :cat:))
                                                                                                  (part-title-content scribble:doc))]
                                                                             [parts (if ,top? (part-parts scribble:doc) null)]))))
                                  (eval `(render (list markdown:doc) (list ,(file-name-from-path target))
                                                 #:dest-dir ,(path-only target) #:render-mixin markdown:render-mixin #:quiet? #true))
                                  (rename-file-or-directory (path-add-suffix target #".md") target #true)
                                  (printf "  [Output to ~a]~n" target))})}
                 (filter {λ [dependent.scrbl] (and (path? dependent.scrbl) (file-exists? dependent.scrbl))}
                         (list (build-path (digimon-tamer) "handbook.scrbl")
                               (when (equal? (current-digimon) (digimon-gnome))
                                 (build-path (digimon-stone) "readme.scrbl"))))))})

(define make~all:
  {lambda []
    (define submake (build-path (digimon-zone) "submake.rkt"))

    (let ([implicit-rules (map hack-rule (make-implicit-rules))])
      (make/proc (cons (list (syntax-source #'I-am-here-just-for-fun) null void) implicit-rules)
                 (if (null? (current-make-real-targets)) (map car implicit-rules) (current-make-real-targets))))
    (compile-directory (digimon-zone) (get-info/full (digimon-zone)))
    
    (let ([modpath `(submod ,submake make:files)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (define rules (map hack-rule (foldr append null
                                              (filter {λ [val] (with-handlers ([exn? {λ _ #false}])
                                                                 (andmap {λ [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                                     (procedure-arity-includes? (third ?) 1))} val))}
                                                      (filter-map {λ [var] (namespace-variable-value var #false {λ _ #false})}
                                                                  (namespace-mapped-symbols))))))
          (make/proc (cons (list (syntax-source #'I-am-here-just-for-fun) null void) rules)
                     (if (null? (current-make-real-targets)) (map car rules) (current-make-real-targets))))))
    
    (let ([modpath `(submod ,submake make:files make)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))})

(define make~clean:
  {lambda []
    (define submake (build-path (digimon-zone) "submake.rkt"))
    (define fclean {λ [dirty] (void (cond [(file-exists? dirty) (delete-file dirty)]
                                          [(directory-exists? dirty) (delete-directory dirty)])
                                    (printf "make: deleted ~a.~n" (simplify-path dirty)))})
  
    (when (member (current-make-phony-goal) '{"distclean" "maintainer-clean"})
      (let ([clbpath `(submod ,submake make:files clobber)])
        (when (module-declared? clbpath #true)
          (dynamic-require clbpath #false))))
  
    (let ([modpath `(submod ,submake make:files)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (define px.filter (pregexp (string-join #:before-first "^(.+?:)?" #:after-last ":.+:"
                                                  (member (string-replace (current-make-phony-goal) #px"(?<!^)-?clean" "")
                                                          '{"maintainer" "dist" "clean" "mostly"}) "|")))
          (for ([var (in-list (namespace-mapped-symbols))]
                #:when (regexp-match? px.filter (symbol->string var)))
            (for-each fclean (map {λ [val] (if (list? val) (car val) val)}
                                  (namespace-variable-value var #false {λ _ null})))))))
    
    (for-each fclean (map car (make-implicit-rules)))
    (for-each fclean (find-digimon-files (curry regexp-match? (pregexp (format "/~a(?![^/])/?" (car (use-compiled-file-paths)))))
                                         (digimon-zone) #:search-compiled? #true))})

(define make~check:
  {lambda []
    (when (directory-exists? (digimon-tamer))
      (compile-directory (digimon-zone) (get-info/full (digimon-zone)))

      (for ([handbook (in-list (cond [(null? (current-make-real-targets)) (filter file-exists? (list (build-path (digimon-tamer) "handbook.scrbl")))]
                                     [else (let ([px.tamer.scrbl (pregexp (format "^~a.+?\\.(scrbl|rkt)" (digimon-tamer)))])
                                             (filter {λ [hb.scrbl] (or (regexp-match? px.tamer.scrbl hb.scrbl)
                                                                       ((negate eprintf) "make: skip non-tamer-scribble file `~a`.~n" hb.scrbl))}
                                                     (current-make-real-targets)))]))])
        (if (regexp-match? #px"\\.rkt$" handbook)
            (parameterize ([current-directory (path-only handbook)]
                           [current-namespace (make-base-namespace)]
                           [exit-handler {λ [retcode] (when (and (integer? retcode) (<= 1 retcode 255))
                                                        (error 'make "[error] /~a breaks ~a!"
                                                               (find-relative-path (digimon-world) handbook) (~n_w retcode "testcase")))}])
              (dynamic-require `(submod ,handbook main) #false))
            (parameterize ([current-directory (path-only handbook)]
                           [current-namespace (make-base-namespace)]
                           [exit-handler {λ _ (error 'make "[fatal] /~a needs a proper `exit-handler`!"
                                                     (find-relative-path (digimon-world) handbook))}])
              (namespace-require 'setup/xref)
              (namespace-require 'scribble/render)
              (eval '(require (prefix-in html: scribble/html-render)))
              (eval `(render (list ,(dynamic-require handbook 'doc)) (list ,(file-name-from-path handbook))
                             #:render-mixin {λ [%] (html:render-multi-mixin (html:render-mixin %))}
                             #:dest-dir ,(build-path (path-only handbook) (car (use-compiled-file-paths)))
                             ;#:redirect "http://docs.racket-lang.org" #:redirect-main "http://docs.racket-lang.org"
                             #:xrefs (list (load-collections-xref))
                             #:quiet? #false #:warn-undefined? #false))))))})

(define main
  {lambda argument-list
    (define fphonies (parameterize ([current-namespace (namespace-anchor->namespace makefile)])
                       (let ([px~fmake: #px"^make~(.+?):$"])
                         (for/hash ([var (in-list (namespace-mapped-symbols))]
                                    #:when (namespace-variable-value var #false {λ _ #false})
                                    #:when (regexp-match? px~fmake: (symbol->string var)))
                           (values (list-ref (regexp-match px~fmake: (symbol->string var)) 1)
                                   (namespace-variable-value var #false))))))
    (define-values {flag-table --help --unknown}
      (values `{{usage-help ,(format "Carefully options are not exactly the same as those of GNU Make.~n")} ; make "~n" work
                {once-each [{"-B" "--always-make"} ,{λ [flag] (make-always-run #true)} {"Unconditionally make all targets."}]
                           [{"-n" "--dry-run"} ,{λ [flag] (make-dry-run #true)} {"Just make without updating targets. [Except *.rkt]"}]
                           [{"-s" "--silent"} ,{λ [flag] (current-output-port /dev/null)} {"Just run commands but output nothing if no errors."}]
                           [{"-t" "--touch"} ,{λ [flag] (make-just-touch #true)} {"Touch targets instead of remaking them if it exists."}]
                           [{"-v" "--verbose"} ,{λ [flag] (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true)}
                                               {"Build with verbose messages."}]}
                {multi [{"+o" "++only"} ,{λ [++only digimon] (current-make-collects (cons digimon (current-make-collects)))}
                                        {"Only build <digimon>s." "digimon"}]}}
              {λ [-h] (foldl {λ [phony -h] (if (hash-has-key? fphonies (car phony)) (format "~a  ~a : ~a~n" -h (car phony) (cdr phony)) -h)}
                             (string-replace -h #px"  -- : .+?-h --'."
                                             (string-join #:before-first (format "~n where <phony-target> is one of~n  ") #:after-last (format "~n")
                                                          '{"all : Build the entire software without documentation. [default]"
                                                            "mostlyclean : Delete all except when remaking costs high."
                                                            "clean : Delete all except those record the configuration."
                                                            "distclean : Delete all that are excluded in the distribution."
                                                            "maintainer-clean : Delete all that can be remade. [For Maintainers]"}
                                                          (format "~n  ")))
                             (list (cons "install" "Install this software and documentation.")
                                   (cons "uninstall" "Delete all the installed files and documentation.")
                                   (cons "dist" "Create a distribution file of the source files.")
                                   (cons "check" "Validate and generate test report along with documentation.")))}
              (curry eprintf "make: I don't know what does `~a` mean!~n")))
    (define {main0 targets}
      (define-values {reals phonies} (partition filename-extension targets))
      (parameterize ([current-make-real-targets (map path->complete-path reals)])
        (for ([digimon (in-list (let ([fsetup-collects {λ _ (map path->string (directory-list (digimon-world)))}])
                                  (remove-duplicates (cond [(not (null? (current-make-collects))) (reverse (current-make-collects))]
                                                           [else (cons (digimon-gnome) (info-ref 'setup-collects fsetup-collects))]))))]
              #:when (get-info/full (build-path (digimon-world) digimon)))
          (parameterize ([current-digimon digimon])
            (dynamic-wind {λ _ (and (file-or-directory-modify-seconds (digimon-zone) (current-seconds))
                                    (printf "Enter Digimon Zone: ~a.~n" digimon))}
                          {λ _ (for ([phony (in-list (if (null? phonies) (list "all") phonies))])
                                 (parameterize ([current-make-phony-goal phony])
                                   (with-handlers ([exn? {λ [e] (exit ({λ _ 1} (eprintf "~a~n" (exn-message e))))}])
                                     (cond [(regexp-match? #px"clean$" phony) ((hash-ref fphonies "clean"))]
                                           [(hash-has-key? fphonies phony) ((hash-ref fphonies phony))]
                                           [else (error 'make "I don't know how to make `~a`!" phony)]))))}
                          {λ _ (printf "Leave Digimon Zone: ~a.~n" digimon)})))))
    (call-as-normal-termination
     {λ _ (parse-command-line (file-name-from-path (syntax-source #'program))
                              argument-list
                              flag-table
                              {λ [!voids . targets] (exit (main0 targets))}
                              '{"phony-target|file-path"}
                              (compose1 exit display --help)
                              (compose1 exit {λ _ 1} --unknown))})})
