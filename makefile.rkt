#!/bin/sh

#|
exec racket --require "$0" --main -- ${1+"$@"} 
|#

#lang racket

(require make)
(require setup/getinfo)
(require compiler/compiler)
(require launcher/launcher)

(require "digicore/digitama/runtime-path.rkt")

(provide main)

(define info-ref (get-info/full digimon-world))

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
    (define f {lambda [] (with-handlers ([symbol? void])
                           (make-parent-directory* t)
                           (call-with-atomic-output-file t {lambda [whocares pseudo-t]
                                                             (close-output-port whocares)
                                                             ((caddr r) pseudo-t)
                                                             (when (make-dry-run)
                                                               (raise 'make-dry-run #true))}))})
    (list (car r) (if (make-always-run) (cons (getenv "digimon-zone") ds) ds)
          (if (make-just-touch) {lambda [] (file-or-directory-modify-seconds t (current-seconds) f)} f))})

(define compile-directory
  {lambda [rootdir finfo]
    (define-values {pin pout} (make-pipe #false 'filter-checking 'verbose-message))
    (define px.inside-world (pregexp digimon-world))
    (define verbose-awk (thread {thunk (let awk ()
                                         (define v (read-line pin))
                                         (unless (eof-object? v)
                                           (cond [(regexp-match? px.inside-world v) (printf "~a~n" v)]
                                                 [(regexp-match? #px":\\s+.+?\\.rkt(\\s|$)" v) 'Skip-Others-Packages]
                                                 [else (printf "~a~n" v)])
                                           (awk)))}))
    (parameterize ([current-output-port pout])
      (compile-directory-zos rootdir finfo #:verbose #true #:skip-doc-sources? #true))
    (close-output-port pout)
    (thread-wait verbose-awk)
    (close-input-port pin)})

(define name->make~goal: {lambda [phony] (string->symbol (format "make~~~a:" phony))})

(define make~all:
  {lambda [submake d-info]
    (let ([digivices (map hack-rule (foldl {lambda [n r] (append (filter list? n) r)} null
                                           (map {lambda [digivice d-ark.rkt]
                                                  (list (let* ([t (build-path (getenv "digimon-zone") d-ark.rkt)]
                                                               [t.dir (path-replace-suffix t #"")]
                                                               [ds (list (build-path (digimon-path "stone") "digivice.rkt"))])
                                                          (when (directory-exists? t.dir)
                                                            (list t ds {lambda [target]
                                                                         (with-output-to-file target #:exists 'replace
                                                                           {thunk (putenv "current-digivice" digivice)
                                                                                  (putenv "current-subdigivices" (path->string t.dir))
                                                                                  (dynamic-require (car ds) #false)})
                                                                         (let ([chmod (file-or-directory-permissions target 'bits)])
                                                                           (file-or-directory-permissions target (bitwise-ior chmod #o111)))})))
                                                        (let ([t (simplify-path (build-path digimon-world digimon-gnome digivice))]
                                                              [ds (list (syntax-source #'makefile))])
                                                          (list t ds {lambda [dest]
                                                                       (make-racket-launcher (list "--search" digimon-world 
                                                                                                   "--lib" (regexp-replace #px".+/(.+?)/.+$"
                                                                                                                           (path->string submake)
                                                                                                                           (format "\\1/~a" d-ark.rkt)))
                                                                                             dest)})))}
                                                (d-info 'racket-launcher-names (const null))
                                                (d-info 'racket-launcher-libraries (const null)))))])
      (unless (null? digivices) (make/proc digivices (map car digivices))))
    
    (compile-directory (getenv "digimon-zone") d-info)
    
    (let ([modpath `(submod ,submake make:files)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (file-or-directory-modify-seconds (getenv "digimon-zone") (current-seconds))
          (define rules (map hack-rule (foldr append null
                                              (filter {lambda [val]
                                                        (with-handlers ([exn? (const #false)])
                                                          (andmap {lambda [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                                   (procedure-arity-includes? (third ?) 1))} val))}
                                                      (filter-map {lambda [var] (namespace-variable-value var #false {lambda [] #false})}
                                                                  (namespace-mapped-symbols))))))
          (make/proc (cons (list (syntax-source #'I-am-here-just-for-fun) null void) rules)
                     (if (null? (current-make-real-targets)) (map car rules) (current-make-real-targets))))))
    
    (let ([modpath `(submod ,submake make:files make)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))})

(define make~clean:
  {lambda [submake d-info]
    (define fclean {lambda [dirty]
                     (when (file-exists? dirty) (delete-file dirty))
                     (when (directory-exists? dirty) (delete-directory dirty))
                     (printf "make: deleted ~a.~n" (simplify-path dirty))})
    
    (when (member (current-make-phony-goal) '{"distclean" "maintainer-clean"})
      (for-each {lambda [digivice d-ark.rkt]
                  (define t.lib (build-path (getenv "digimon-zone") d-ark.rkt))
                  (define t.bin (simplify-path (build-path digimon-world digimon-gnome digivice)))
                  (when (directory-exists? (path-replace-suffix t.lib #""))
                    (fclean t.bin))
                  (fclean t.lib)}
                (d-info 'racket-launcher-names (const null))
                (d-info 'racket-launcher-libraries (const null)))
      
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
            (for-each fclean (map {lambda [val] (if (list? val) (car val) val)}
                                  (namespace-variable-value var #false (const null))))))))
    
    (let ([px.exclude (pregexp (string-join #:before-first "/(\\.git|" #:after-last ")$" (map (compose1 path->string file-name-from-path) null) "|"))]
          [px.include #px"/(compiled)(?!\\.)/?"])
      (for-each fclean (reverse (filter (curry regexp-match? px.include)
                                        (sequence->list (in-directory (getenv "digimon-zone") (negate (curry regexp-match? px.exclude))))))))})

(define make~check:
  {lambda [submake d-info]
    (let ([tamer-info {lambda [key fdefault] (fdefault)}])
      (compile-directory (getenv "digimon-tamer") tamer-info))

    (for ([handbook (in-list (cond [(null? (current-make-real-targets)) (filter file-exists? (list (build-path (getenv "digimon-tamer") "handbook.scrbl")))]
                                   [else (filter {lambda [hb.scrbl] (unless (regexp-match? #px"\\.scrbl$" hb.scrbl)
                                                                      ((negate eprintf) "make: skip non-scribble file `~a`.~n" hb.scrbl))}
                                                 (current-make-real-targets))]))])
      (parameterize ([current-directory (path-only handbook)]
                     [current-namespace (make-base-namespace)]
                     [exit-handler {lambda [whocares] (error 'make "[error] `Scribble` needs a proper `exit-handler`!")}])
        (namespace-require 'setup/xref)
        (namespace-require 'scribble/render)
        (eval '(require (prefix-in html: scribble/html-render)))
        (eval `(render (list ,(dynamic-require handbook 'doc)) (list ,(file-name-from-path handbook))
                       #:render-mixin {lambda [%] (html:render-multi-mixin (html:render-mixin %))}
                       #:dest-dir ,(build-path (path-only handbook) (car (use-compiled-file-paths)))
                       #:xrefs (list (load-collections-xref))
                       #:quiet? #false #:warn-undefined? #false))))})

(define main0
  {lambda [return]
    (parse-command-line (file-name-from-path (syntax-source #'program))
                        (current-command-line-arguments)
                        `{{usage-help ,(format "Carefully options are not exactly the same as those of GNU Make.~n")}
                          {once-each
                           [{"-B" "--always-make"}
                            ,{lambda [flag] (make-always-run #true)}
                            {"Unconditionally make all targets."}]
                           [{"-n" "--dry-run"}
                            ,{lambda [flag] (make-dry-run #true)}
                            {"Just make without updating targets. [Except *.rkts]"}]
                           [{"-s" "--silent"}
                            ,{lambda [flag] (let ([/dev/null (open-output-nowhere '/dev/null #true)]) (current-output-port /dev/null))}
                            {"Just run commands but output nothing if no errors."}]
                           [{"-t" "--touch"}
                            ,{lambda [flag] (make-just-touch #true)}
                            {"Touch targets instead of remaking them if it exists."}]
                           [{"-v" "--verbose"}
                            ,{lambda [flag] (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true)}
                            {"Build with verbose messages."}]}
                          {multi
                           [{"+o" "++only"}
                            ,{lambda (++only digimon) (current-make-collects (cons digimon (current-make-collects)))}
                            {"Only build <digimon>s." "digimon"}]}}
                        {lambda [!voids . targets]
                          ;;; Do not change the name of compiled file path, here we only escapes from DrRacket's convention.
                          ;;; Since compiler will check the bytecodes in the core collection which have already been compiled into <path:compiled/>.
                          (use-compiled-file-paths (list (build-path "compiled")))
                          (define-values {files phonies} (partition filename-extension targets))
                          (parameterize ([current-make-real-targets (map path->complete-path files)])
                            (for ([phony (in-list (if (null? phonies) (list "all") phonies))])
                              (parameterize ([current-make-phony-goal phony])
                                (for ([digimon (in-list (remove-duplicates (cond [(not (null? (current-make-collects))) (current-make-collects)]
                                                                                 [else (append (list digimon-gnome digimon-kernel)
                                                                                               (info-ref 'setup-collects
                                                                                                         {thunk (filter {lambda [subdir] (and (directory-exists? subdir)
                                                                                                                                              (regexp-match? #px"^[^.]" subdir))}
                                                                                                                        (directory-list digimon-world))}))])))])
                                  (digimon-setenv digimon)
                                  (define digimon-info (get-info/full (getenv "digimon-zone")))
                                  (cond [(false? digimon-info) (eprintf "make: [warning] ignored digimon `~a` because `info.rkt` not found.~n" digimon)]
                                        [else (let ([submake (build-path (getenv "digimon-zone") "submake.rkt")])
                                                (dynamic-wind {thunk (printf "Enter Digimon Zone: ~a.~n" digimon)}
                                                              {thunk (putenv "makefiles" (format "~a:~a" (syntax-source #'makefile) submake))
                                                                     (cond [(string=? phony "all") (make~all: submake digimon-info)]
                                                                           [(regexp-match? #px"clean$" phony) (make~clean: submake digimon-info)]
                                                                           [else (parameterize ([current-namespace (module->namespace (syntax-source #'makefile))])
                                                                                   (define fmake (namespace-variable-value (name->make~goal: phony) #false (const #false)))
                                                                                   (when (false? fmake)
                                                                                     (eprintf "make: I don't know how to make `~a`!~n" phony)
                                                                                     (return 1))
                                                                                   (fmake submake digimon-info))])}
                                                              {thunk (printf "Leave Digimon Zone: ~a.~n" digimon)}))])))))}
                        (list "phony-target|file-path")
                        {lambda [--help]
                          (display (foldl {lambda [-h --help] (if (string? -h) (string-append --help -h) --help)}
                                          (string-replace --help #px"  -- : .+?-h --'."
                                                          (string-join #:before-first (format "~n where <phony-target> is one of~n  ") #:after-last (format "~n")
                                                                       '{"all : Build the entire software with documentation. [default]"
                                                                         "mostlyclean : Delete all except when remaking costs high."
                                                                         "clean : Delete all except those record the configuration."
                                                                         "distclean : Delete all that are excluded in the distribution."
                                                                         "maintainer-clean : Delete all that can be remade. [For Maintainers]"}
                                                                       (format "~n  ")))
                                          (parameterize ([current-namespace (module->namespace (syntax-source #'makefile))])
                                            (map {lambda [phony] (when (procedure? (namespace-variable-value (name->make~goal: (car phony)) #false void))
                                                                   (format "  ~a : ~a~n" (car phony) (cdr phony)))}
                                                 (list (cons 'install "Installing the software, then running test if testcases exist.")
                                                       (cons 'uninstall "Delete all the installed files and documentation.")
                                                       (cons 'dist "Creating a distribution file of the source files.")
                                                       (cons 'check "Performing tests on the program this makefile builds."))))))
                          (return 0)}
                        {lambda [unknown]
                          (eprintf "make: I don't know what does `~a` mean!~n" unknown)
                          (return 1)})})

(define main
  {lambda arglist
    (parameterize ([current-command-line-arguments (list->vector arglist)])
      (define status (call-with-current-continuation main0))
      (cond [(integer? status) (exit status)]
            [else (exit 0)]))})
    