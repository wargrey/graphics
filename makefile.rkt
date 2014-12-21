#!/usr/bin/env racket
#lang racket

(require make)

(make-print-dep-no-line #false)
(make-print-checking #false)
(make-print-reasons #false)

(define make-dry-run (make-parameter #false))
(define make-always-run (make-parameter #false))
(define make-just-touch (make-parameter #false))

{module makefile racket
  (require racket/draw)
  
  (require "digitama/wikimon.rkt")
  
  (provide makefiles rootdir stnsdir)
  
  (define-namespace-anchor makefile)
  (define makefiles (list (with-handlers ([exn:fail:contract? (const (build-path (current-directory) "makefile.rkt"))])
                            (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)))))
  (define rootdir (path-only (car makefiles)))
  (define stnsdir (build-path rootdir "stone"))
  (define dgtmdir (build-path rootdir "digitama"))
  (define vllgdir (build-path rootdir "village"))
  (define hackdir (build-path (find-system-path 'temp-dir) (symbol->string (gensym "rktmk.hack"))))
  (define px.village (pregexp (format "(?<=/)~a/[^/]+" (last (explode-path vllgdir)))))
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
      (parameterize ([current-directory (path-only dentry)]
                     [current-namespace (make-empty-namespace)])
        (namespace-attach-module (namespace-anchor->namespace makefile) 'racket/draw)
        (namespace-require 'racket)
        (with-input-from-file dentry #:mode 'text
          {thunk (read-language) ; Do nothing
                 (let repl ([?img (void)])
                   (match (read)
                     [(? eof-object? sexp) {begin (eval `(make-parent-directory* ,target))
                                                  (eval `(send ,?img save-file ,target 'png))}]
                     [{list 'require {pregexp #px"d-ark.rkt$"}} (repl (eval `(require (file ,(path->string digimon.rkt)))))]
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
  
  (define mostly:readmes: (for/list ([readme.scrbl (in-directory stnsdir)]
                                     #:when (string=? (path->string (file-name-from-path readme.scrbl)) "readme.scrbl"))
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
                                             (map bytes->string/utf-8 (call-with-input-file readme (curry regexp-match* #px"(?<=~/).+?.png")))))))}

{module+ main
  (require (submod ".." makefile))
  
  (define hackdir (build-path (find-system-path 'temp-dir) (symbol->string (gensym "rktmk.hack"))))
  (define #%makefile (#%variable-reference rootdir))
  (current-namespace (variable-reference->namespace #%makefile))
  
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
      (define t (hack-target (first r)))
      (define f {thunk ((third r) t)})
      (list t (hack-depends (second r))
            (if (make-just-touch) {thunk (file-or-directory-modify-seconds t (current-seconds) f)} f))})
  
  (define make~default:
    {lambda [unknown]
      (if (regexp-match #px"^[+][+]" unknown)
          (printf "make: I don't know how to make `~a`!~n" (substring unknown 2))
          (printf "make: I don't know what does `~a` mean!~n" unknown))
      (exit 1)})
  
  (define make~ifdefined:
    {lambda [fsymbols]
      (for-each {lambda [make-install:] (make-install:)}
                (filter {lambda [?] (and (procedure? ?) (procedure-arity-includes? ? 0))}
                        (map (curryr namespace-variable-value #false (const #false)) fsymbols)))})
  
  (define make~clean:
    {lambda [level]
      (cond [(symbol? level) (for-each (compose1 make~clean: symbol->string) (reverse (member level '{maintainer dist clean mostly})))]
            [else (for ([var (namespace-mapped-symbols)])
                    (when (regexp-match? (pregexp (format "^~a:.+:$" level)) (symbol->string var))
                      (for ([file (in-list (map (compose1 hack-target car) (namespace-variable-value var #false (const #false))))])
                        (when (file-exists? file) (delete-file file))
                        (printf "make: deleted ~a.~n" file))))])})
  
  (define make~all:
    {lambda [targets]
      (file-or-directory-modify-seconds hackdir (current-seconds) {thunk (make-directory* hackdir)})
      (define rules (map hack-rule (foldr append null (filter {lambda [val]
                                                                (with-handlers ([exn? (const #false)])
                                                                  (andmap {lambda [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                                           (procedure-arity-includes? (third ?) 1))} val))}
                                                              (filter-map (compose1 (curryr namespace-variable-value #false) string->symbol second)
                                                                          (filter-map (curry regexp-match #px"^\\s\\s.define\\s+(.+?:)\\s+")
                                                                                      (with-input-from-file (car makefiles) port->lines)))))))
      (make/proc (cons (list (car makefiles) null (const 'I-am-here-just-for-fun)) rules)
                 (if (list? targets) (map hack-target targets) (map car rules)))})
  
  (command-line #:program (file-name-from-path (car makefiles))
                #:argv (let*-values ([{argv0} (remove-duplicates (remove "--" (vector->list (current-command-line-arguments))))]
                                     [{options} (let fopt ([argv argv0])
                                                  (match argv
                                                    [{? null?} null]
                                                    [{list-rest {pregexp #px"^[^-+]"} targets} null]
                                                    [{list-rest {pregexp #px"^[-+]"} argvn} (cons (car argv) (fopt argvn))]))]
                                     [{targets} (remove* options argv0)]
                                     [{files phonies} (partition filename-extension (if (null? targets) (list "all") targets))])
                         (append options (map (curry string-append "++") phonies) (list "--") files))
                #:usage-help 
                "<option>s start with `++` are also <phony-target>s."
                "You are free to drop off the leading `++` when typing phony targets."
                #:help-labels " [Carefully our conventions are not exactly the same as those of GNU Make.]"
                ; #:ps label can cause unknown exception, maybe it's a bug of Racket
                #:once-each
                [{"++all"} "Building the entire software without generating documentation. [default]"
                           (make~all: 'all)]
                [{"++install"} "Installing the software, then running test if testcases exist."
                               (make~ifdefined: '{make~preinstall: make~install: make~postinstall: make~installcheck:})]
                [{"++uninstall"} "Delete all the installed files and documentation."
                                 (make~ifdefined: '{make~preuninstall: make~uninstall: make~postuninstall:})]
                [{"++mostlyclean"} "Delete all files except that people normally don't want to reconstruct."
                                   (make~clean: 'mostly)]
                [{"++clean"} "Delete all files except that records the configuration."
                             (make~clean: 'clean)]
                [{"++distclean"} "Delete all files that are not included in the distribution."
                                 (make~clean: 'dist)]
                [{"++maintainer-clean"} "Delete all files that can be reconstructed. Just leave this one to maintainers."
                                        (make~clean: 'maintainer)]
                [{"++docs"} "Generating documentation."
                            (make~ifdefined: '{make~docs:})]
                [{"++dist"} "Creating a distribution file of the source files."
                            (make~ifdefined: '{make~dist:})]
                [{"++check"} "Performing self tests on the program this makefile builds before building."
                             (make~ifdefined: '{make~check:})]
                [{"++installcheck"} "Performing installation tests on the target system after installing."
                                    (make~ifdefined: '{make~installcheck:})]
                [{"-B" "--always-make"} "Unconditionally make all need-to-update targets."
                                        (make-always-run #true)]
                [{"-n" "--test" "--dry-run"} "Do not actually update targets, just make and display their resulting content."
                                             (make-dry-run #true)]
                [{"-s" "--silent" "--quiet"} "Just run commands but output nothing."
                                             (current-output-port (open-output-nowhere '/dev/null #true))]
                [{"-t" "--touch"} "Touch targets instead of remaking them if the target already exists."
                                  (make-just-touch #true)]
                [{"-v" "--verbose"} "Building with verbose messages."
                                    (void (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true))]
                #:handlers
                {lambda [!voids . fns] (unless (null? fns) (make~all: (map {lambda [f] (if (relative-path? f) (build-path rootdir f) f)} fns)))}
                (list "phony-target|file-path")
                (compose1 exit (const 0) display (curryr string-replace #px"  -- : .+?-h --'." ""))
                make~default:)}
