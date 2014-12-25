#!/usr/bin/env racket
#lang racket

{module makefile racket/base
  (require racket/path)
  
  (require make)
  
  (provide (all-defined-out))
  (provide (all-from-out make))
  
  (define make-dry-run (make-parameter #false))
  (define make-always-run (make-parameter #false))
  (define make-just-touch (make-parameter #false))
  (define current-real-targets (make-parameter null))
  
  (make-print-dep-no-line #false)
  (make-print-checking #false)
  (make-print-reasons #false)
  
  (define makefiles (list (syntax-source #'makefile)))
  (define rootdir (path-only (car makefiles)))
  (define dgvcdir (build-path rootdir "digivice"))
  (define vllgdir (build-path rootdir "village"))
  (define stnsdir (build-path rootdir "stone"))
  (define optdirs {hash 'digitama (build-path rootdir "digitama")
                        'tamer (build-path rootdir "tamer")
                        'island (build-path rootdir "island")})
  (define hackdir (build-path (find-system-path 'temp-dir) (symbol->string (gensym "rktmk.hack"))))
  
  (define hack-target
    {lambda [t]
      (if (make-dry-run)
          (build-path hackdir (find-relative-path rootdir t))
          t)})
  
  (define hack-depends
    {lambda [ds]
      (if (make-always-run)
          (cons hackdir ds)
          ds)})
  
  (define hack-rule
    {lambda [r]
      (define t (hack-target (car r)))
      (define f {lambda [] ((caddr r) t)})
      (list t (hack-depends (cadr r))
            (if (make-just-touch) {lambda [] (file-or-directory-modify-seconds t (current-seconds) f)} f))})}

{module make:files racket
  (require (submod ".." makefile))
  (require racket/draw)
  
  (require "digitama/wikimon.rkt")
  
  (define px.village (pregexp (format "(?<=/)~a/[^/]+" (file-name-from-path vllgdir))))
  (define px.dgvc-ark (pregexp (format "/~a/.+?-ark$" (file-name-from-path dgvcdir))))
  (define px.d-ark.rkt #px"d-ark.rkt$")
  (define digimon.rkt (build-path (hash-ref optdirs 'digitama) "digimon.rkt"))
  
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
      (with-output-to-file (build-path (path-only target) "README.md") #:exists 'replace
        {thunk (define awkout (current-thread))
               (define awk-format (thread {thunk (let awk ([pipen awkout])
                                                   (define in (thread-receive))
                                                   (define out (match in
                                                                 [{pregexp #px"^#+ (\\d+\\.)+"} (regexp-replace #px"^(#+) (\\d+\\.)+" in "\\1")]
                                                                 [{pregexp #px"^\\* >\\s+>"} (regexp-replace #px"^\\* >(\\s+)>" in "\\1- ")]
                                                                 [{pregexp #px"~/"} (string-replace in "~" (format "/~a" (find-relative-path rootdir stnsdir)))]
                                                                 [{pregexp #px"^\\s*$"} (let ([next (thread-receive)])
                                                                                          (cond [(and (not (eof-object? next))
                                                                                                      (regexp-match? #px"(^\\* )|(^\\s*$)" next)) 'Skip-Empty-Line]
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
  
  (define make-digivice
    {lambda [target dentry]
      (with-output-to-file target #:exists 'replace
        {thunk (parameterize ([current-command-line-arguments (vector (path->string (file-name-from-path target)))])
                 (dynamic-require dentry #false))})}) 
  
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
                            (define ds (append (smart-dependencies readme.scrbl) makefiles
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
  
  (define dist:digivices: (let ([px.exclude (pregexp (string-join #:before-first (format "/(compiled|\\.git|~a|" (file-name-from-path stnsdir))
                                                                  (map symbol->string (hash-keys optdirs)) "|" #:after-last ")$"))])
                            (for/list ([d-ark (in-directory vllgdir (negate (curry regexp-match? px.exclude)))]
                                       #:when (regexp-match? px.dgvc-ark d-ark))
                              (define t (path-add-suffix (build-path d-ark (regexp-replace #px".+/(.+?)-ark$" (path->string d-ark) "\\1")) #".rktl"))
                              (define ds (list (build-path stnsdir "digivice.rktl")))
                              (list t ds (curryr make-digivice (car ds))))))}

{module make:all: racket
  (require (submod ".." makefile))
  
  (define desc "Building the entire software without generating documentation. [default]")
  
  {module+ make
    (define modfiles `(submod ,(syntax-source #'makefile) make:files))
    
    (dynamic-require modfiles #false)
    (current-namespace (module->namespace modfiles))
    
    (file-or-directory-modify-seconds hackdir (current-seconds) {thunk (make-directory* hackdir)})
    (define rules (map hack-rule (foldr append null (filter {lambda [val]
                                                              (with-handlers ([exn? (const #false)])
                                                                (andmap {lambda [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                                         (procedure-arity-includes? (third ?) 1))} val))}
                                                            (filter-map {lambda [var] (namespace-variable-value var #false {lambda [] #false})}
                                                                        (namespace-mapped-symbols))))))
    (make/proc (cons (list (car makefiles) null (const 'I-am-here-just-for-fun)) rules)
               (if (null? (current-real-targets)) (map car rules) (map hack-target (current-real-targets))))}}

{module+ main
  (require (submod ".." makefile))
  (parse-command-line (file-name-from-path (syntax-source #'program))
                      (current-command-line-arguments)
                      `{{usage-help ,(format "Carefully our conventions are not exactly the same as those of GNU Make.~n")}
                        {once-each
                         [{"-B" "--always-make"}
                          ,{lambda [flag] (make-always-run #true)}
                          {"Unconditionally make all need-to-update targets."}]
                         [{"-n" "--test" "--dry-run"}
                          ,{lambda [flag] (make-dry-run #true)}
                          {"Do not actually update targets, just make and display their resulting content."}]
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
                        (define-values {files phonies} (partition filename-extension targets))
                        (parameterize ([current-real-targets (map {lambda [f] (if (relative-path? f) (build-path rootdir f) f)} files)])
                          (for ([phony (in-list (if (null? phonies) (list "all") phonies))])
                            (if (regexp-match? #px"clean$" phony)
                                (let ([modpath `(submod ,(syntax-source #'makefile) make:files)]
                                      [pxclean (pregexp (format "^(.+?:)?~a:.+:" (string-join (member (string-replace phony #px"(?<!^)-?clean" "")
                                                                                                      '{"maintainer" "dist" "clean" "mostly"}) "|")))])
                                  (dynamic-require modpath #false)
                                  (for ([var (in-list (namespace-mapped-symbols (module->namespace modpath)))])
                                    (when (regexp-match? pxclean (symbol->string var))
                                      (for ([file (in-list (map {lambda [val] (hack-target (if (list? val) (car val) val))}
                                                                (namespace-variable-value var #false {lambda [] null} (module->namespace modpath))))])
                                        (when (file-exists? file) (delete-file file))
                                        (printf "make: deleted ~a.~n" (simplify-path file))))))
                                (let ([modmain `(submod ,(syntax-source #'makefile) ,(string->symbol (format "make:~a:" phony)) make)])
                                  (if (module-declared? modmain)
                                      (dynamic-require modmain #false)
                                      (eprintf "make: I don't know how to make `~a`!~n" phony))))))}
                      (list "phony-target|file-path")
                      {lambda [--help]
                        (display (foldl {lambda [-h --help] (if (string? -h) (string-append --help -h) --help)}
                                        (string-replace --help #px"  -- : .+?-h --'." (format "~n where <phony-target> is one of~n"))
                                        (map {lambda [phony] (if (symbol=? phony 'clean)
                                                                 (string-join #:before-first "  " #:after-last (format "~n")
                                                                              '{"mostlyclean : Delete all files except that people normally don't want to reconstruct."
                                                                                "clean : Delete all files except that records the configuration."
                                                                                "distclean : Delete all files that are not included in the distribution."
                                                                                "maintainer-clean : Delete all files that can be reconstructed."}
                                                                              (format "~n  "))
                                                                 (let ([sub `(submod ,(syntax-source #'makefile) ,(string->symbol (format "make:~a:" phony)))])
                                                                   (when (module-declared? sub)
                                                                     (dynamic-require sub #false)
                                                                     (format "  ~a : ~a~n" phony (namespace-variable-value 'desc #false void (module->namespace sub))))))}
                                             (list 'all 'install 'uninstall 'clean 'docs 'dist 'check 'installcheck))))
                        (exit 0)}
                      {lambda [unknown]
                        (eprintf "make: I don't know what does `~a` mean!~n" unknown)
                        (exit 1)})}
