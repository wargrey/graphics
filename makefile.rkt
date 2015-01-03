#!/usr/bin/env racket
#lang racket

{module makefile racket
  (require make)
  (require setup/getinfo)
  
  (provide (all-defined-out))
  (provide (all-from-out make))
  
  (define make-dry-run (make-parameter #false))
  (define make-always-run (make-parameter #false))
  (define make-just-touch (make-parameter #false))
  (define make-no-submakes (make-parameter #false))
  (define current-real-targets (make-parameter null))
  (define current-make-goal (make-parameter #false))
  
  (make-print-dep-no-line #false)
  (make-print-checking #false)
  (make-print-reasons #false)
  
  (define rootdir (path-only (syntax-source #'makefile))) ;;; Warning, this path is /-suffixed.
  (define dgvcdir (build-path rootdir "digivice"))
  (define dgtmdir (build-path rootdir "digitama"))
  (define vllgdir (build-path rootdir "village"))
  (define stnsdir (build-path rootdir "stone"))
  (define tmrsdir (build-path rootdir "tamer"))
  (define islndir (build-path rootdir "island"))
  
  (define info-ref (get-info/full rootdir))
  (define makefiles (filter file-exists?
                            (map (curryr build-path (file-name-from-path (syntax-source #'makefile)))
                                 (cons rootdir (with-handlers ([exn? (const null)])
                                                 (directory-list vllgdir #:build? #true))))))
  
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
      (list (car r) (if (make-always-run) (cons rootdir ds) ds)
            (if (make-just-touch) {lambda [] (file-or-directory-modify-seconds t (current-seconds) f)} f))})}

{module make:files racket
  (require (submod ".." makefile))
  (require racket/draw)
  
  (require "digitama/wikimon.rkt")
  
  (define hackdir (build-path (find-system-path 'temp-dir) (symbol->string (gensym "rktmk.hack"))))
  
  (define px.village (pregexp (format "(?<=/)~a/[^/]+" (file-name-from-path vllgdir))))
  (define px.dgvc-ark (pregexp (format "/~a/.+?-ark$" (file-name-from-path dgvcdir))))
  (define px.d-ark.rkt #px"d-ark.rkt$")
  (define digimon.rkt (build-path dgtmdir "digimon.rkt"))
  
  (define smart-dependencies
    {lambda [entry [memory null]]
      (foldl {lambda [subpath memory]
               (define subsrc (cond [(regexp-match? #px"d-ark.rkt$" subpath) digimon.rkt]
                                    [else (simplify-path (build-path (path-only entry) (bytes->string/utf-8 subpath)))]))
               (cond [(member subsrc memory) memory]
                     [else (smart-dependencies subsrc memory)])}
             (append memory (list entry))
             (call-with-input-file entry (curry regexp-match* #px"(?<=(@include-section\\{)|(\\(require \")).+?.(scrbl|rkt)(?=(\\})|(\"\\)))")))})

  (define make-markdown
    {lambda [target dentry]
      (define mdname (gensym 'readme))
      (parameterize ([current-directory (let ([village? (regexp-match px.village dentry)]) (if village? (build-path rootdir (car village?)) rootdir))]
                     [current-namespace (make-base-namespace)])
        (namespace-require 'scribble/render)
        (eval '(require (prefix-in markdown: scribble/markdown-render)))
        (eval `(render (list ,(dynamic-require dentry 'doc)) (list ,(symbol->string mdname))
                #:dest-dir ,hackdir #:render-mixin markdown:render-mixin #:quiet? #false)))
      (make-parent-directory* target)
      (with-output-to-file target #:exists 'replace
        {thunk (define awkout (current-thread))
               (define awk-format (thread {thunk (let awk ([pipen awkout])
                                                   (define in (thread-receive))
                                                   (define out (match in
                                                                 [{pregexp #px"~/"} (string-replace in "~" (format "/~a" (find-relative-path rootdir stnsdir)))]
                                                                 [{pregexp #px"^\\s*$"} (let ([next (thread-receive)])
                                                                                          (cond [(and (not (eof-object? next))
                                                                                                      (regexp-match? #px"(^\\s*\\* )|(^\\s*$)" next)) 'Skip-Empty-Line]
                                                                                                [else (thread-send pipen "")])
                                                                                          (thread-rewind-receive (list next)))]
                                                                 [_ in]))
                                                   (when (string? out) (thread-send pipen out))
                                                   (if (eof-object? out) (thread-send pipen eof) (awk pipen)))}))
               (with-input-from-file (format "~a/~a.md" hackdir mdname)
                 {thunk (let awk-readme ([pipe0 awk-format] [waitees (list (current-input-port) (thread-receive-evt))])
                          (define waiteen (if (input-port? (apply sync waitees))
                                              (match (read-line)
                                                [{? eof-object?} {begin (thread-send pipe0 eof) (list (thread-receive-evt))}]
                                                [{var line} {begin (thread-send pipe0 line) waitees}])
                                              (let ([line (thread-receive)])
                                                (unless (eof-object? line)
                                                  (displayln line)
                                                  waitees))))
                          (when (list? waiteen)
                            (awk-readme pipe0 waiteen)))})})})
  
  (define make-digimoji
    {lambda [target name]
      (define png (format (cond [(regexp-match? #px"^Test" (~a name)) "~a.png"]
                                [(member name theirfaults) "Dc~a.png"]
                                [(symbol? name) "Dc~a_w.png"]
                                [(string? name) "Dc~a_r_w.png"])
                          name))
      (make-parent-directory* target)
      (send (wikimon-image png) save-file target 'png)})
  
  (define make-digifield
    {lambda [target emblem]
      (define pxpng (pregexp (format "/images/thumb[^ ]+100px-~a_emblem.png" emblem)))
      (make-parent-directory* target)
      (send (make-object bitmap% (third (wikimon-recv! (car (regexp-match* pxpng (third (wikimon-recv! "/Field")))))) 'png/alpha)
            save-file target 'png)})
  
  (define make-image
    {lambda [target dentry]
      (define #%make:files (module->namespace (syntax-source-module #'make:files)))
      (parameterize ([current-directory (path-only dentry)]
                     [current-namespace (make-empty-namespace)])
        (namespace-attach-module #%make:files 'racket/draw)
        (namespace-require 'racket)
        (with-input-from-file dentry #:mode 'text
          {thunk (read-language) ; Do nothing
                 (let repl ([?img (void)])
                   (match (read)
                     [(? eof-object? sexp) {begin (eval `(make-parent-directory* ,target))
                                                  (eval `(send ,?img save-file ,target 'png))}]
                     [{list 'require {pregexp px.d-ark.rkt}} (repl (eval `(require (file ,(path->string digimon.rkt)))))]
                     [{var sexp} (repl (eval sexp))]))}))})
  
  (define dist:digipngs: (append (hash-map kanas {lambda [kana romaji]
                                                   (define t (build-path wikimon-dir (format "~a.png" kana)))
                                                   (list t null {thunk (make-digimoji t romaji)})})
                                 (for/list ([index (in-range (char->integer #\a) (add1 (char->integer #\z)))])
                                   (define letter (integer->char index))
                                   (define t (build-path wikimon-dir (format "~a.png" letter)))
                                   (list t null (curryr make-digimoji (~a (hash-ref alphabets letter letter)))))
                                 (hash-map fields {lambda [abbr emblem]
                                                    (define t (build-path wikimon-dir (format "~a.png" abbr)))
                                                    (list t null (curryr make-digifield emblem))})))
  
  (define mostly:readmes: (for/list ([readme.scrbl (in-list (find-files (curry regexp-match? #px"/readme.scrbl$") stnsdir))])
                            (define t (build-path rootdir (find-relative-path stnsdir (build-path (path-only readme.scrbl) "README.md"))))
                            (define ds (append (smart-dependencies readme.scrbl) (list (syntax-source #'makefile))
                                               (let* ([village? (regexp-match px.village readme.scrbl)]
                                                      [info.rkt (if village? (build-path rootdir (car village?) "info.rkt") (build-path rootdir "info.rkt"))])
                                                 (if (file-exists? info.rkt) (list info.rkt) null))))
                            (list t ds (curryr make-markdown readme.scrbl))))
  
  (define dist:images: (for/fold ([images null]) ([readme (in-list (map caadr mostly:readmes:))])
                         (define village (let ([village? (regexp-match px.village readme)]) (if village? (car village?) "")))
                         (define stone (find-relative-path rootdir stnsdir))
                         (append images (map {lambda [image]
                                               (define t (build-path rootdir village stone image))
                                               (define image.rkt (path-replace-suffix (build-path rootdir stone village image) #".rkt"))
                                               (define ds (append (smart-dependencies image.rkt) (map car dist:digipngs:)))
                                               (if (file-exists? image.rkt) (list t ds (curryr make-image image.rkt)) null)}
                                             (regexp-match* #px"(?<=~/).+?.png" (format "~a" (dynamic-require readme 'doc)))))))
  
  (define dist:digivices: (let ([px.exclude (pregexp (string-join #:before-first "/(\\.git|" #:after-last ")$"
                                                                  (remove-duplicates (map (compose1 path->string file-name-from-path)
                                                                                          (append (list stnsdir dgtmdir tmrsdir islndir)
                                                                                                  (use-compiled-file-paths)))) "|"))])
                            (foldl append null
                                   (for/list ([d-ark (in-directory vllgdir (negate (curry regexp-match? px.exclude)))]
                                              #:when (regexp-match? px.dgvc-ark d-ark))
                                     (define digivice (regexp-replace #px".+/(.+?)-ark$" (path->string d-ark) "\\1"))
                                     (define make-digivice
                                       {lambda [target dentry]
                                         (with-output-to-file target #:exists 'replace
                                           {thunk (parameterize ([current-command-line-arguments (vector digivice)])
                                                    (dynamic-require dentry #false))})
                                         (file-or-directory-permissions target (bitwise-ior (file-or-directory-permissions target 'bits) #o111))})
                                     (list (let ([t (path-add-suffix (build-path d-ark digivice) #".rkt")]
                                                 [ds (list (build-path stnsdir "digivice.rkt"))])
                                             (list t ds (curryr make-digivice (car ds))))
                                           (let ([t (simplify-path (build-path d-ark 'up digivice))]
                                                 [ds (list (build-path stnsdir "digivice.sh"))])
                                             (list t ds (curryr make-digivice (car ds)))))))))}

{module make:check: racket
  (require (submod ".." makefile))
  
  (for ([handbook (in-list (map {lambda [type] (build-path tmrsdir type "handbook.scrbl")} (list "behavior")))]
        #:when (file-exists? handbook))
    (parameterize ([current-directory (path-only handbook)]
                   [current-namespace (make-base-namespace)])
        (namespace-require 'scribble/render)
        (eval '(require (prefix-in html: scribble/html-render)))
        (eval `(render (list ,(dynamic-require handbook 'doc)) (list ,(file-name-from-path handbook))
                       #:render-mixin {lambda [%] (html:render-multi-mixin (html:render-mixin %))}
                       #:dest-dir ,(path-only handbook) #:quiet? #false))))}

{module+ main
  (require (submod ".." makefile))
  (require compiler/compiler)
  
  (define make:goal: {lambda [phony] (string->symbol (format "make:~a:" phony))})
  
  (define make-all
    {lambda []
      (compile-directory-zos rootdir info-ref #:verbose #true #:skip-doc-sources? #true)
      
      (let ([modpath `(submod ,(syntax-source #'makefile) make:files)])
        (when (module-declared? modpath)
          (dynamic-require modpath #false)
          (parameterize ([current-namespace (module->namespace modpath)])
            (file-or-directory-modify-seconds rootdir (current-seconds))
            (define rules (map hack-rule (foldr append null
                                                (filter {lambda [val]
                                                          (with-handlers ([exn? (const #false)])
                                                            (andmap {lambda [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                                     (procedure-arity-includes? (third ?) 1))} val))}
                                                        (filter-map {lambda [var] (namespace-variable-value var #false {lambda [] #false})}
                                                                    (namespace-mapped-symbols))))))
            (make/proc (cons (list (syntax-source #'I-am-here-just-for-fun) null void) rules)
                       (if (null? (current-real-targets)) (map car rules) (current-real-targets))))))
    
      (let ([modpath `(submod ,(syntax-source #'makefile) make:files make)])
        (when (module-declared? modpath)
          (dynamic-require modpath #false)))})
  
  (define make-clean
    {lambda []
      (define fclean {lambda [dirty]
                       (when (file-exists? dirty) (delete-file dirty))
                       (when (directory-exists? dirty) (delete-directory dirty))
                       (printf "make: deleted ~a.~n" (simplify-path dirty))})
      
      (let ([clbpath `(submod ,(syntax-source #'makefile) make:files clobber)])
        (when (and (member (current-make-goal) '{"distclean" "maintainer-clean"}) (module-declared? clbpath))
          (dynamic-require clbpath #false)))
      
      (let ([modpath `(submod ,(syntax-source #'makefile) make:files)])
        (when (module-declared? modpath)
          (dynamic-require modpath #false)
          (parameterize ([current-namespace (module->namespace modpath)])
            (define px.filter (pregexp (string-join #:before-first "^(.+?:)?" #:after-last ":.+:"
                                                     (member (string-replace (current-make-goal) #px"(?<!^)-?clean" "")
                                                             '{"maintainer" "dist" "clean" "mostly"}) "|")))
            (for ([var (in-list (namespace-mapped-symbols))]
                  #:when (regexp-match? px.filter (symbol->string var)))
              (for-each fclean (map {lambda [val] (if (list? val) (car val) val)}
                                    (namespace-variable-value var #false (const null))))))))
      
      (let ([px.exclude (pregexp (format "/(\\.git|~a)$" (path->string (file-name-from-path vllgdir))))]
            [px.include #px"/compiled/?"])
        (for-each fclean (reverse (filter (curry regexp-match? px.include)
                                          (sequence->list (in-directory rootdir (negate (curry regexp-match? px.exclude))))))))})
  
  (parse-command-line (file-name-from-path (syntax-source #'program))
                      (current-command-line-arguments)
                      `{{usage-help ,(format "Carefully our conventions are not exactly the same as those of GNU Make.~n")}
                        {once-each
                         [{"-B" "--always-make"}
                          ,{lambda [flag] (make-always-run #true)}
                          {"Unconditionally make all need-to-update targets."}]
                         [{"-n" "--test" "--dry-run"}
                          ,{lambda [flag] (make-dry-run #true)}
                          {"Do not actually update targets, just make. [Except Racket Sources]"}]
                         [{"-r" "--no-submakes"}
                          ,{lambda [flag] (make-no-submakes #true)}
                          {"Do not run submakes even if there is only phony targets."}]
                         [{"-s" "--silent" "--quiet"}
                          ,{lambda [flag] (current-output-port (open-output-nowhere '/dev/null #true))}
                          {"Just run commands but output nothing."}]
                         [{"-t" "--touch"}
                          ,{lambda [flag] (make-just-touch #true)}
                          {"Touch targets instead of remaking them if the target already exists."}]
                         [{"-v" "--verbose"}
                          ,{lambda [flag] (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true)}
                          {"Building with verbose messages."}]}}
                      {lambda [!voids . targets]
                        ;;; Do not change the name of compiled file path, here we only escapes from DrRacket's convention.
                        ;;; Since compiler will check the bytecodes in the core collection which have already been compiled into <path:compiled/>.
                        (use-compiled-file-paths (list (build-path "compiled")))
                        (define-values {files phonies} (partition filename-extension targets))
                        (parameterize ([current-real-targets (map {lambda [f] (if (relative-path? f) (build-path rootdir f) f)} files)])
                          (for ([phony (in-list (if (null? phonies) (list "all") phonies))])
                            (parameterize ([current-make-goal phony])
                              (cond [(string=? phony "all") (make-all)]
                                    [(regexp-match? #px"clean$" phony) (make-clean)]
                                    [else (let ([modpath `(submod ,(syntax-source #'makefile) ,(make:goal: phony))])
                                            (if (module-declared? modpath)
                                                (dynamic-require modpath #false)
                                                (eprintf "make: I don't know how to make `~a`!~n" phony)))]))))
                        (when (and (null? files) (false? (make-no-submakes)))
                          (for ([submake (in-list (cdr makefiles))])
                            (printf "make: submake: ~a~n" submake)
                            (define submain `(submod ,submake main))
                            (dynamic-require (if (module-declared? submain #true) submain submake) #false)
                            (printf "make: submade: ~a~n" submake)))}
                      (list "phony-target|file-path")
                      {lambda [--help]
                        (display (foldl {lambda [-h --help] (if (string? -h) (string-append --help -h) --help)}
                                        (string-replace --help #px"  -- : .+?-h --'."
                                                        (string-join #:before-first (format "~n where <phony-target> is one of~n  ") #:after-last (format "~n")
                                                                     '{"all : Building the entire software with generating documentation. [default]"
                                                                       "mostlyclean : Delete all files except that people normally don't want to reconstruct."
                                                                       "clean : Delete all files except that records the configuration."
                                                                       "distclean : Delete all files that are not included in the distribution."
                                                                       "maintainer-clean : Delete all files that can be reconstructed. [Maintainers Only]"}
                                                                     (format "~n  ")))
                                        (map {lambda [phony] (let ([sub `(submod ,(syntax-source #'makefile) ,(make:goal: (car phony)))])
                                                               (when (module-declared? sub) (format "  ~a : ~a~n" (car phony) (cdr phony))))}
                                             (list (cons 'install "Installing the software, then running test if testcases exist.")
                                                   (cons 'uninstall "Delete all the installed files and documentation.")
                                                   (cons 'dist "Creating a distribution file of the source files.")
                                                   (cons 'check "Performing self tests on the program this makefile builds before building.")
                                                   (cons 'installcheck "Performing installation tests on the target system after installing.")))))
                        (exit 0)}
                      {lambda [unknown]
                        (eprintf "make: I don't know what does `~a` mean!~n" unknown)
                        (exit 1)})}