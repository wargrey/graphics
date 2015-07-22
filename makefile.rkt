#!/bin/sh

#|
dir="`dirname $0`/digitama"; fn="digicore";
dzo="${dir}/compiled/${fn}_rkt.zo";
mzo="`dirname $0`/compiled/`basename $0 .rkt`_rkt.zo";
test "${dzo}" -ot "${dir}/${fn}.rkt" && rm -fr ${mzo};
if test "$1" = "clean"; then
   find "`dirname $0`/.." -name "*.zo" -exec rm -f {} ';'
   rm -fr "${dzo}" "${mzo}";
fi
exec racket --name "$0" --require "$0" --main -- ${1+"$@"} 
|#

#lang racket

(require make)

(require compiler/cm)
(require compiler/compiler)

(require dynext/compile)
(require dynext/link)

(require setup/dirs)

(require "digitama/digicore.rkt")

(provide main)

(define current-make-real-targets (make-parameter null))
(define current-make-phony-goal (make-parameter #false))

(define make-dry-run (make-parameter #false))
(define make-always-run (make-parameter #false))
(define make-just-touch (make-parameter #false))

(make-print-dep-no-line #false)
(make-print-checking #false)
(make-print-reasons #false)

(define hack-rule
  (lambda [r]
    (define t (car r))
    (define ds (cadr r))
    (define f (thunk (with-handlers ([symbol? void])
                     (make-parent-directory* t)
                     ((curry call-with-atomic-output-file t)
                      (λ [whocares pseudo-t] #| pseudo-t is in the same dir|#
                        (close-output-port whocares)
                        ((caddr r) pseudo-t)
                        (when (make-dry-run)
                          (raise 'make-dry-run #true)))))))
    (list (car r) (if (make-always-run) (cons (digimon-zone) ds) ds)
          (cond [(false? (make-just-touch)) f]
                [else (thunk (file-or-directory-modify-seconds t (current-seconds) f))]))))

(define compile-directory
  (lambda [cmpdir finfo [round 1]]
    (define again? (make-parameter #false))
    (define px.within (pregexp (if (make-print-checking) (digimon-world) (path->string (digimon-zone)))))
    (define traceln (curry printf "pass[~a]: ~a~n" round))
    (define {filter-inside info}
      (cond [(regexp-match? #px"checking:" info) (when (make-print-checking) (traceln info))]
            [(regexp-match? #px"(compil|process)ing:" info) (and (traceln info) (again? #true))]))
    (define {filter-verbose info}
      (cond [(regexp-match? #px"(newer src...|end compile|skipping|done:)" info) '|Skip Task Endline|]
            [(regexp-match? #px"newer:" info) (when (make-print-reasons) (traceln info))]
            [(regexp-match? px.within info) (filter-inside info)]
            [(regexp-match? #px":\\s+.+?\\.rkt(\\s|$)" info) '|Skip Other's Packages|]
            [else (traceln info)]))
    (with-handlers ([exn? (compose1 (curry error 'make "[error] ~a") exn-message)])
      (parameterize ([manager-trace-handler filter-verbose])
        (compile-directory-zos cmpdir finfo #:verbose #false #:skip-doc-sources? #true)))
    (when (again?) (compile-directory cmpdir finfo (add1 round)))))

(define make-digivice
  (lambda [template.rkt dgvc.rkt]
    (with-output-to-file dgvc.rkt #:exists 'replace
      (thunk (dynamic-require template.rkt #false)))
    (let ([chmod (file-or-directory-permissions dgvc.rkt 'bits)])
      (file-or-directory-permissions dgvc.rkt (bitwise-ior chmod #o111)))))

(define smart-dependencies
  (lambda [entry [memory null]]
    (foldl (λ [subpath memory]
             (define subsrc (simplify-path (build-path (path-only entry) (bytes->string/utf-8 subpath))))
             (cond [(member subsrc memory) memory]
                   [else (smart-dependencies subsrc memory)]))
           (append memory (list entry))
           (call-with-input-file entry (curry regexp-match* #px"(?<=@(include-section|require)[{[](\\(submod \")?).+?.(scrbl|rktl?)(?=[\"}])")))))

(define make-implicit-rkt-rules
  (lambda []
    (define d-info (get-info/full (digimon-zone)))
    (define stone/digivice.rkt (parameterize ([current-digimon (digimon-gnome)]) (build-path (digimon-stone) "digivice.rkt")))
    (filter-map (λ [dgvc] (let ([dgvc.rkt (path->string (build-path (digimon-zone) dgvc))])
                            (and (directory-exists? (path-replace-suffix dgvc.rkt #""))
                                 (list dgvc.rkt (list stone/digivice.rkt) (curry make-digivice stone/digivice.rkt)))))
                (d-info 'racket-launcher-libraries (const null)))))

(define make-implicit-dist-rules
  (lambda []
    (for/list ([dependent.scrbl (in-list (if (string=? (current-tamer) "root") null (list (build-path (digimon-tamer) "handbook.scrbl"))))]
               #:when (file-exists? dependent.scrbl))
      (define t (build-path (digimon-zone) "README.md"))
      (define ds (filter file-exists? (list* (build-path (digimon-zone) "info.rkt") (smart-dependencies dependent.scrbl))))
      (list t ds (λ [target]
                   (parameterize ([current-directory (digimon-zone)]
                                  [current-namespace (make-base-namespace)]
                                  [current-input-port /dev/eof] ; workaround, to tell scribble this is rendering to markdown
                                  [exit-handler (thunk* (error 'make "[fatal] /~a needs a proper `exit-handler`!"
                                                               (find-relative-path (digimon-world) dependent.scrbl)))])
                     (eval '(require (prefix-in markdown: scribble/markdown-render) scribble/core scribble/render))
                     (eval `(render (let ([scribble:doc (dynamic-require ,dependent.scrbl 'doc)])
                                      (list (struct-copy part scribble:doc [parts null])))
                                    (list ,(file-name-from-path target))
                                    #:dest-dir ,(path-only target) #:render-mixin markdown:render-mixin #:quiet? #true))
                     (rename-file-or-directory (path-add-suffix target #".md") target #true)
                     (printf "  [Output to ~a]~n" target)))))))

(define make-native-library-rules
  (lambda []
    ;;; simple implementation
    (define {include.h entry [memory null]}
      (foldl (λ [subpath memory]
               (define subsrc (simplify-path (build-path (path-only entry) (bytes->string/utf-8 subpath))))
               (cond [(member subsrc memory) memory]
                     [else (include.h subsrc memory)]))
             (append memory (list entry))
             (call-with-input-file entry (curry regexp-match* #px"(?<=#include \").+?.h(?=\")"))))
    (foldl append null
           (for/list ([c (in-list (find-digimon-files (curry regexp-match? #px"\\.c$") (digimon-zone)))])
             (define-values {tobj t}
               (values (build-path (path-only c) (car (use-compiled-file-paths))
                                   "precompiled" (system-library-subpath #false)
                                   (path-replace-suffix (file-name-from-path c) #".o"))
                       (build-path (path-only c) (car (use-compiled-file-paths))
                                   "native" (system-library-subpath #false)
                                   (path-replace-suffix (file-name-from-path c) (system-type 'so-suffix)))))
             (list (list tobj (include.h c)
                         (λ [target] (let ([cflags (current-extension-compiler-flags)])
                                       (parameterize ([current-extension-compiler-flags (cons "-m64" cflags)])
                                         (printf "cc: ~a: ~a~n" (current-extension-compiler) c)
                                         (compile-extension 'quiet c target (list (digimon-zone) "/usr/local/include"))))))
                   (list t (list tobj)
                         (λ [target] (parameterize ([current-standard-link-libraries null]
                                                    [current-extension-linker-flags (list* "-m64" "-shared" "-lssh2" "-lncurses"
                                                                                           (case (system-type)
                                                                                             [{macosx} (list "-L/usr/local/lib"
                                                                                                             (~a "-F" (find-lib-dir))
                                                                                                             "-framework" "Racket")]
                                                                                             [else null]))])
                                       (printf "ld: ~a: ~a~n" (current-extension-linker) tobj)
                                       (link-extension 'quiet (list tobj) target)))))))))

(define make~all:
  (lambda []
    (define submake (build-path (digimon-zone) "submake.rkt"))
    (define {do-make rules0}
      (unless (null? rules0)
        (define-values {imts exts} (partition (curryr assoc rules0) (current-make-real-targets)))
        (let ([rules (map hack-rule rules0)])
          (make/proc rules (if (null? (current-make-real-targets)) (map car rules) imts)))
        (current-make-real-targets exts)))

    (let ([modpath `(submod ,submake premake)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))

    (do-make (make-native-library-rules))
    (do-make (make-implicit-rkt-rules))
    (compile-directory (digimon-zone) (get-info/full (digimon-zone)))
    (do-make (make-implicit-dist-rules))
    
    (let ([modpath `(submod ,submake make:files)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (do-make (foldr append null
                          (filter (λ [val] (with-handlers ([exn? (const #false)])
                                             (andmap (λ [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                 (procedure-arity-includes? (third ?) 1))) val)))
                                  (filter-map (λ [var] (namespace-variable-value var #false (const #false)))
                                              (namespace-mapped-symbols))))))))

    (let ([modpath `(submod ,submake make:files make)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))

    (make/proc (list (list (digimon-zone) null (thunk '|I don't know how to make all these fucking files|)))
               (current-make-real-targets))

    (let ([modpath `(submod ,submake postmake)])
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))))

(define make~clean:
  (lambda []
    (define submake (build-path (digimon-zone) "submake.rkt"))
    (define {fclean dirty}
      (void (cond [(file-exists? dirty) (delete-file dirty)]
                  [(directory-exists? dirty) (delete-directory dirty)])
            (printf "make: deleted ~a.~n" (simplify-path dirty))))
  
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
            (for-each fclean (map (λ [val] (if (list? val) (car val) val))
                                  (namespace-variable-value var #false (thunk null))))))))

    (for-each fclean (map car (make-implicit-rkt-rules)))
    (for-each fclean (map car (make-implicit-dist-rules)))
    (for-each fclean (reverse (find-digimon-files (curry regexp-match? (pregexp (format "/~a(?![^/])/?" (car (use-compiled-file-paths)))))
                                                  (digimon-zone) #:search-compiled? #true)))))

(define make~check:
  (lambda []
    (when (directory-exists? (digimon-tamer))
      (let ([rules (map hack-rule (append (make-native-library-rules) (make-implicit-rkt-rules)))])
        (unless (null? rules) (make/proc rules (map car rules))))
      (compile-directory (digimon-zone) (get-info/full (digimon-zone)))
      
      (for ([handbook (in-list (if (null? (current-make-real-targets))
                                   (filter file-exists? (list (build-path (digimon-tamer) "handbook.scrbl")))
                                   (let ([px.tamer.scrbl (pregexp (format "^~a.+?\\.(scrbl|rkt)" (digimon-tamer)))])
                                     (filter (λ [hb.scrbl] (or (regexp-match? px.tamer.scrbl hb.scrbl)
                                                               ((negate eprintf) "make: skip non-tamer-scribble file `~a`.~n" hb.scrbl)))
                                             (current-make-real-targets)))))])
        (parameterize ([current-directory (path-only handbook)]
                       [current-namespace (make-base-namespace)])
          (define ./handbook (find-relative-path (digimon-world) handbook))
          (if (regexp-match? #px"\\.rkt$" handbook)
              (parameterize ([exit-handler (λ [retcode] (when (and (integer? retcode) (<= 1 retcode 255))
                                                          (error 'make "[error] /~a breaks ~a!" ./handbook (~n_w retcode "testcase"))))])
                (dynamic-require `(submod ,handbook main) #false))
              (parameterize ([exit-handler (thunk* (error 'make "[fatal] /~a needs a proper `exit-handler`!" ./handbook))])
                (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
                (eval `(render (list ,(dynamic-require handbook 'doc)) (list ,(file-name-from-path handbook))
                               #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %)))
                               #:dest-dir ,(build-path (path-only handbook) (car (use-compiled-file-paths)))
                               #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref))
                               #:quiet? #false #:warn-undefined? #false)))))))))

(define create-zone
  (lambda []
    (define gnome-stone (parameterize ([current-digimon (digimon-gnome)]) (digimon-stone)))
    (for ([prompt (in-list (list "collection" "pkg-desc"))]
          [defval (in-list (list (current-digimon) "[Missing Description]"))])
      (echof #:fgcolor 'green "info.rkt: Please input the '~a [~a]: " prompt defval)
      (putenv (format "info.~a" prompt)
              (let ([line (read-line)])
                (cond [(or (eof-object? line) (regexp-match? #px"^\\s*$" line)) defval]
                      [else (string-trim line)]))))
    (for ([src (in-list (list "info.rkt" "digicore.rkt" "handbook.scrbl" "tamer.rkt"))]
          [dest (in-list (list (digimon-zone) (digimon-digitama) (digimon-tamer) (digimon-tamer)))])
      (make-directory* dest)
      (putenv "digicore.rkt" (path->string (find-relative-path dest digicore.rkt)))
      (with-output-to-file (build-path dest src) #:exists 'error
        (thunk (dynamic-require (build-path gnome-stone src) #false))))
    (copy-file (build-path gnome-stone "robots.txt") (build-path (digimon-tamer) "robots.txt") #true)
    (unless (getenv "taming")
      (echof #:fgcolor 'green "github: Please input the repository name [~a]: " (current-digimon))
      (define remote (format "git@github.com:digital-world/~a.git"
                             (let ([line (read-line)])
                               (cond [(eof-object? line) (current-digimon)]
                                     [(regexp-match? #px"^\\s*$" line) (current-digimon)]
                                     [else (string-trim line)]))))
      (copy-file (build-path (digimon-world) ".gitignore") (build-path (digimon-zone) ".gitignore") #true)
      (and (for/and ([gitcmd (in-list (list "init"
                                            "add ."
                                            "commit -m 'Mission Start'"
                                            (format "remote add origin ~a" remote)
                                            "push -u origin master"))])
             (system (format "cd ~a && git ~a" (digimon-zone) gitcmd)))
           (system "cd ~a && git submodule add ~a ~a" (digimon-world) remote (digimon-zone))
           (system "cd ~a && git submodule" (digimon-world))))))

(define main
  (lambda argument-list
    (define current-make-collects (make-parameter null))
    (define fphonies
      (parameterize ([current-namespace (variable-reference->namespace (#%variable-reference))])
        (let ([px~fmake: #px"^make~(.+?):$"])
          (for/hash ([var (in-list (namespace-mapped-symbols))]
                     #:when (namespace-variable-value var #false (thunk #false))
                     #:when (regexp-match? px~fmake: (symbol->string var)))
            (values (list-ref (regexp-match px~fmake: (symbol->string var)) 1)
                    (namespace-variable-value var #false))))))
    (define-values {flag-table --help --unknown}
      (values `{{usage-help ,(format "Carefully options are not exactly the same as those of GNU Make.~n")} ; make "~n" work
                {once-each [{"-B" "--always-make"} ,(λ [flag] (make-always-run #true)) {"Unconditionally make all targets."}]
                           [{"-n" "--dry-run"} ,(λ [flag] (make-dry-run #true)) {"Just make without updating targets. [Except *.rkt]"}]
                           [{"-s" "--silent"} ,(λ [flag] (current-output-port /dev/null)) {"Just run commands but output nothing if no errors."}]
                           [{"-t" "--touch"} ,(λ [flag] (make-just-touch #true)) {"Touch targets instead of remaking them if it exists."}]
                           [{"-v" "--verbose"} ,(λ [flag] (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true))
                                               {"Build with verbose messages."}]}
                {multi [{"+o" "++only"} ,(λ [++only digimon] (current-make-collects (cons digimon (current-make-collects))))
                                        {{"Only build <digimon>s." "[interactively create <digimon> first if it does not exists.]"} "digimon"}]}}
              (λ [-h] (foldl (λ [phony -h] (if (hash-has-key? fphonies (car phony)) (format "~a  ~a : ~a~n" -h (car phony) (cdr phony)) -h))
                             ((curry string-replace -h #px"  -- : .+?-h --'.")
                              ((curryr string-join (format "~n  ")
                                       #:before-first (format "~n where <phony-target> is one of~n  ")
                                       #:after-last (format "~n"))
                               '{"all : Build the entire software without documentation. [default]"
                                 "mostlyclean : Delete all except when remaking costs high."
                                 "clean : Delete all except those record the configuration."
                                 "distclean : Delete all that are excluded in the distribution."
                                 "maintainer-clean : Delete all that can be remade. [For Maintainers]"}))
                             (list (cons "install" "Install this software and documentation.")
                                   (cons "uninstall" "Delete all the installed files and documentation.")
                                   (cons "dist" "Create a distribution file of the source files.")
                                   (cons "check" "Validate and generate test report along with documentation.")
                                   (cons "new" " a digimon zone that satisfy the rules."))))
              (curry eprintf "make: I don't know what does `~a` mean!~n")))
    (define {main0 targets}
      (define-values {reals phonies} (partition filename-extension targets))
      (parameterize ([current-make-real-targets (map simple-form-path reals)])
        (for ([digimon (in-list (let ([info-ref (get-info/full (digimon-world))]
                                      [fsetup-collects (thunk (map path->string (filter get-info/full (directory-list (digimon-world)))))])
                                  (remove-duplicates (cond [(not (null? (current-make-collects))) (reverse (current-make-collects))]
                                                           [(regexp-match #px"^\\w+" (find-relative-path (digimon-world) (current-directory))) => values]
                                                           [else (cons (digimon-gnome) (cond [(false? info-ref) (fsetup-collects)]
                                                                                             [else (info-ref 'setup-collects fsetup-collects)]))]))))])
          (parameterize ([current-digimon digimon])
            (dynamic-wind (thunk (printf "Enter Digimon Zone: ~a.~n" digimon))
                          (thunk (for ([phony (in-list (if (null? phonies) (list "all") phonies))])
                                 (parameterize ([current-make-phony-goal phony])
                                   (with-handlers ([exn? (compose1 exit (const 1) (curry eprintf "~a~n") exn-message)])
                                     (cond [(false? (directory-exists? (digimon-zone))) (create-zone)]
                                           [else (file-or-directory-modify-seconds (digimon-zone) (current-seconds))])
                                     (cond [(regexp-match? #px"clean$" phony) ((hash-ref fphonies "clean"))]
                                           [(hash-has-key? fphonies phony) ((hash-ref fphonies phony))]
                                           [else (error 'make "I don't know how to make `~a`!" phony)])))))
                          (thunk (printf "Leave Digimon Zone: ~a.~n" digimon)))))))
    (call-as-normal-termination
     (thunk (parse-command-line (file-name-from-path (find-system-path 'run-file))
                              argument-list
                              flag-table
                              (λ [!voids . targets] (exit (main0 targets)))
                              '{"phony-target|file-path"}
                              (compose1 exit display --help)
                              (compose1 exit (const 1) --unknown (curryr string-trim #px"[()]") (curry format "~a") values))))))
