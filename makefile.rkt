#!/bin/sh

#|
exec racket --require "$0" --main -- ${1+"$@"} 
|#

#lang racket

(require make)
(require rackunit)
(require setup/getinfo)
(require compiler/compiler)
(require launcher/launcher)

(require "DigiGnome/digitama/runtime.rkt")

(provide main)

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
                     (call-with-atomic-output-file t {λ [whocares pseudo-t]
                                                       (close-output-port whocares)
                                                       ((caddr r) pseudo-t)
                                                       (when (make-dry-run)
                                                         (raise 'make-dry-run #true))}))})
    (list (car r) (if (make-always-run) (cons (digimon-zone) ds) ds)
          (if (make-just-touch) {λ _ (file-or-directory-modify-seconds t (current-seconds) f)} f))})

(define compile-directory
  {lambda [rootdir finfo]
    (define-values {pin pout} (make-pipe #false 'filter-checking 'verbose-message))
    (define px.inside-world (pregexp (digimon-world)))
    (define verbose-awk (thread {λ _ (let awk ()
                                       (define v (read-line pin))
                                       (unless (eof-object? v)
                                         (cond [(regexp-match? px.inside-world v) (printf "~a~n" v)]
                                               [(regexp-match? #px":\\s+.+?\\.rkt(\\s|$)" v) 'Skip-Others-Packages]
                                               [else (printf "~a~n" v)])
                                         (awk)))}))
    (parameterize ([current-output-port pout])
      (compile-directory-zos rootdir finfo #:verbose #true #:skip-doc-sources? #true)
      (close-output-port pout))
    (thread-wait verbose-awk)
    (close-input-port pin)})

(define name->make~goal: {λ [phony] (string->symbol (format "make~~~a:" phony))})

(define make-implicit-rules
  {lambda []
    (define d-info (get-info/full (digimon-zone)))
    (define stone/digivice.rkt (parameterize ([current-digimon (digimon-gnome)]) (build-path (digimon-stone) "digivice.rkt")))
    (append (foldl {λ [n r] (append (filter list? n) r)} null
                   (map {λ [digivice d-ark.rkt]
                          (define d-ark (path->string (build-path (digimon-world) (current-digimon) d-ark.rkt)))
                          (list (let* ([t (build-path (digimon-zone) d-ark.rkt)]
                                       [t.dir (path-replace-suffix t #"")]
                                       [ds (list stone/digivice.rkt (syntax-source #'makefile))])
                                  (when (directory-exists? t.dir)
                                    (list t ds {λ [target] (with-output-to-file target #:exists 'replace
                                                             {λ _ (void (putenv "current-digivice" digivice)
                                                                        (putenv "runtime-path" (path->string (find-relative-path (path-only d-ark) digimon-runtime-source)))
                                                                        (dynamic-require (car ds) #false))})
                                                 (let ([chmod (file-or-directory-permissions target 'bits)])
                                                   (file-or-directory-permissions target (bitwise-ior chmod #o111)))})))
                                (let ([t (simplify-path (build-path (digimon-world) (digimon-gnome) (car (use-compiled-file-paths)) digivice))]
                                      [ds (list (syntax-source #'makefile))])
                                  (list t ds (curry make-racket-launcher (list "-t-" d-ark)))))}
                        (d-info 'racket-launcher-names {λ _ null})
                        (d-info 'racket-launcher-libraries {λ _ null})))
            (map {λ [readme.scrbl]
                   ;;; For the sake of simplicity, I do not check `readme.scrbl`s' (require ...)s.
                   ;;; Under this circumstance, (require ...)s always make nonsense.
                   ;;; See Rule 5 and Rule 6 in /DigiGnome/tamer/makefile.rkt.
                   (define-values {t ds} (if (regexp-match? #px"/index.scrbl$" readme.scrbl)
                                             (values (build-path (digimon-world) "README.md")
                                                     (list* readme.scrbl (syntax-source #'makefile)
                                                            (filter file-exists? (map (curryr build-path "info.rkt") (directory-list (digimon-world) #:build? #true)))))
                                             (values (build-path (digimon-zone) "README.md")
                                                     (list readme.scrbl (syntax-source #'makefile)))))
                   (list t ds {λ [target]
                                (parameterize ([current-directory (digimon-zone)]
                                               [current-namespace (make-base-namespace)]
                                               [exit-handler {λ [whocares] (error 'make "[error] /~a needs a proper `exit-handler`!"
                                                                                  (find-relative-path (digimon-world) readme.scrbl))}])
                                  (namespace-require 'scribble/render)
                                  (eval '(require (prefix-in markdown: scribble/markdown-render)))
                                  (eval `(render (list ,(dynamic-require readme.scrbl 'doc)) (list ,(file-name-from-path target))
                                                 #:dest-dir ,(path-only target) #:render-mixin markdown:render-mixin #:quiet? #true))
                                  (let ([tmp.md (path-add-suffix target #".md")])
                                    (display-lines-to-file (file->lines tmp.md) target #:exists 'replace)
                                    (printf "  [Output to ~a]~n" target)
                                    (delete-file tmp.md)))})}
                 (filter file-exists? (map (curry build-path (digimon-stone))
                                           (filter string? (list "readme.scrbl" (when (equal? (current-digimon) (digimon-gnome)) "index.scrbl")))))))})

(define make~all:
  {lambda []
    (define submake (build-path (digimon-zone) "submake.rkt"))
    
    (let ([implicit-rules (map hack-rule (make-implicit-rules))])
      (unless (null? implicit-rules) (make/proc implicit-rules (map car implicit-rules))))
    (compile-directory (digimon-zone) (get-info/full (digimon-zone)))
    
    (let ([modpath `(submod ,submake make:files)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (file-or-directory-modify-seconds (digimon-zone) (current-seconds))
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
    (define fclean {λ [dirty] (void (when (file-exists? dirty) (delete-file dirty))
                                    (when (directory-exists? dirty) (delete-directory dirty))
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
    
    (let ([px.exclude (pregexp (string-join #:before-first "/(\\.git|" #:after-last ")$" (map (compose1 path->string file-name-from-path) null) "|"))]
          [px.include (pregexp (format "/(~a)(?!\\.)/?" (car (use-compiled-file-paths))))])
      (for-each fclean (reverse (filter (curry regexp-match? px.include)
                                        (sequence->list (in-directory (digimon-zone) (negate (curry regexp-match? px.exclude))))))))
    
    (for-each {λ [target] (fclean target)} (make-implicit-rules))})

(define make~check:
  {lambda []
    (when (directory-exists? (digimon-tamer))
      (let ([tamer-info {λ [key fdefault] (fdefault)}])
        (compile-directory (digimon-tamer) tamer-info))
      
      (for ([handbook (in-list (cond [(null? (current-make-real-targets)) (filter file-exists? (list (build-path (digimon-tamer) "handbook.scrbl")))]
                                     [else (let ([px.tamer.scrbl (pregexp (format "^~a.+?\\.scrbl" (digimon-tamer)))])
                                             (filter {λ [hb.scrbl] (or (regexp-match? px.tamer.scrbl hb.scrbl)
                                                                       ((negate eprintf) "make: skip non-tamer-scribble file `~a`.~n" hb.scrbl))}
                                                     (current-make-real-targets)))]))])
        (parameterize ([current-directory (path-only handbook)]
                       [current-namespace (make-base-namespace)]
                       [exit-handler {λ [whocares] (error 'make "[error] /~a needs a proper `exit-handler`!" (find-relative-path (digimon-world) handbook))}])
          (namespace-require 'setup/xref)
          (namespace-require 'scribble/render)
          (eval '(require (prefix-in html: scribble/html-render)))
          (eval `(render (list ,(dynamic-require handbook 'doc)) (list ,(file-name-from-path handbook))
                         #:render-mixin {λ [%] (html:render-multi-mixin (html:render-mixin %))}
                         #:dest-dir ,(build-path (path-only handbook) (car (use-compiled-file-paths)))
                         #:xrefs (list (load-collections-xref))
                         #:quiet? #false #:warn-undefined? #false)))))})

(define main0
  {lambda [return]
    (parse-command-line (file-name-from-path (syntax-source #'program))
                        (current-command-line-arguments)
                        `{{usage-help ,(format "Carefully options are not exactly the same as those of GNU Make.~n")}
                          {once-each
                           [{"-B" "--always-make"}
                            ,{λ [flag] (make-always-run #true)}
                            {"Unconditionally make all targets."}]
                           [{"-n" "--dry-run"}
                            ,{λ [flag] (make-dry-run #true)}
                            {"Just make without updating targets. [Except *.rkt]"}]
                           [{"-s" "--silent"}
                            ,{λ [flag] (current-output-port /dev/null)}
                            {"Just run commands but output nothing if no errors."}]
                           [{"-t" "--touch"}
                            ,{λ [flag] (make-just-touch #true)}
                            {"Touch targets instead of remaking them if it exists."}]
                           [{"-v" "--verbose"}
                            ,{λ [flag] (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true)}
                            {"Build with verbose messages."}]}
                          {multi
                           [{"+o" "++only"}
                            ,{λ (++only digimon) (current-make-collects (cons digimon (current-make-collects)))}
                            {"Only build <digimon>s." "digimon"}]}}
                        {λ [!voids . targets]
                          ;;; Do not change the name of compiled file path, here we only escapes from DrRacket's convention.
                          ;;; Since compiler will check the bytecodes in the core collection which have already been compiled into <path:compiled/>.
                          (use-compiled-file-paths (list (build-path "compiled")))
                          (define-values {reals phonies} (partition {λ [t] (or (filename-extension t) (directory-exists? t))} targets))
                          (parameterize ([current-directory (digimon-world)]
                                         [current-make-real-targets (map path->complete-path reals)])
                            (for ([phony (in-list (if (null? phonies) (list "all") phonies))])
                              (parameterize ([current-make-phony-goal phony])
                                (for-each {λ [digimon] (parameterize ([current-digimon digimon])
                                                         (dynamic-wind {λ _ (printf "Enter Digimon Zone: ~a.~n" digimon)}
                                                                       {λ _ (cond [(string=? phony "all") (make~all:)]
                                                                                  [(regexp-match? #px"clean$" phony) (make~clean:)]
                                                                                  [(parameterize ([current-namespace (module->namespace (syntax-source #'makefile))])
                                                                                     (namespace-variable-value (name->make~goal: phony) #false {λ _ #false}))
                                                                                   => {λ [fmake] (fmake)}]
                                                                                  [else (and (eprintf "make: I don't know how to make `~a`!~n" phony)
                                                                                             (return 1))])}
                                                                       {λ _ (printf "Leave Digimon Zone: ~a.~n" digimon)}))}
                                          (filter get-info/full
                                                  (remove-duplicates (cond [(not (null? (current-make-collects))) (reverse (current-make-collects))]
                                                                           [else (cons (digimon-gnome) (info-ref 'setup-collects {λ _ (directory-list)}))])))))))}
                        (list "phony-target|file-path")
                        {λ [--help]
                          (display (parameterize ([current-namespace (module->namespace (syntax-source #'makefile))])
                                     (foldl {λ [phony --help] (cond [(void? (namespace-variable-value (name->make~goal: (car phony)) #false void)) --help]
                                                                    [else (format "~a  ~a : ~a~n" --help (car phony) (cdr phony))])}
                                            (string-replace --help #px"  -- : .+?-h --'."
                                                            (string-join #:before-first (format "~n where <phony-target> is one of~n  ") #:after-last (format "~n")
                                                                         '{"all : Build the entire software without documentation. [default]"
                                                                           "mostlyclean : Delete all except when remaking costs high."
                                                                           "clean : Delete all except those record the configuration."
                                                                           "distclean : Delete all that are excluded in the distribution."
                                                                           "maintainer-clean : Delete all that can be remade. [For Maintainers]"}
                                                                         (format "~n  ")))
                                            (list (cons 'install "Install this software and documentation.")
                                                  (cons 'uninstall "Delete all the installed files and documentation.")
                                                  (cons 'dist "Create a distribution file of the source files.")
                                                  (cons 'check "Validate and generate test report as well as documentation.")))))
                          (return 0)}
                        {λ [unknown]
                          (eprintf "make: I don't know what does `~a` mean!~n" unknown)
                          (return 1)})})

(define main
  {lambda arglist
    (parameterize ([current-command-line-arguments (list->vector arglist)])
      (exit (call-with-current-continuation main0)))})
