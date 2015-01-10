#lang racket

{module make:files racket
  (require racket/draw)
  
  (require (lib "digicore/wikimon"))
  
  (define hackdir (build-path (find-system-path 'temp-dir) (symbol->string (gensym "rktmk.hack"))))
  (define rootdir (getenv "digimon-world"))
  (define zonedir (getenv "digimon-zone"))
  (define dgvcdir (getenv "digimon-digivice"))
  (define stondir (getenv "digimon-stone"))
  
  (define px.depends #px"(?<=(@include-section\\{)|(\\(require (\\(lib )?\")).+?.(scrbl|rkt)(?=(\\})|(\"\\)\\)?))")
  (define px.d-ark.rkt #px"d-ark.rkt$")
  (define digimon.rkt (build-path rootdir "digicore" "digimon.rkt"))
  
  (void (putenv "wikimon-dir" (format "~a/wikimon" stondir)))
  
  (define smart-digimon
    {lambda [stone.scrbl]
      (let ([~monpath (find-relative-path stondir (path-only stone.scrbl))])
        (if (regexp-match? #px"/$" ~monpath)
            (file-name-from-path zonedir)
            (first (explode-path ~monpath))))})
  
  (define smart-dependencies
    {lambda [entry [memory null]]
      (foldl {lambda [subpath0 memory]
               (define subpath (bytes->string/utf-8 subpath0))
               (define subsrc (cond [(regexp-match? #px"d-ark.rkt$" subpath) digimon.rkt]
                                    [(filename-extension subpath) (simplify-path (build-path (path-only entry) subpath))]
                                    [else (path-add-suffix (simplify-path (build-path rootdir subpath)) #".rkt")]))
               (cond [(member subsrc memory) memory]
                     [else (smart-dependencies subsrc memory)])}
             (append memory (list entry))
             (call-with-input-file entry (curry regexp-match* px.depends)))})

  (define make-markdown
    {lambda [target dentry]
      (define mdname (gensym 'readme))
      (parameterize ([current-directory (build-path rootdir (smart-digimon dentry))]
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
                                                                 [{pregexp #px"~/"} (string-replace in "~" (format "/~a" (find-relative-path zonedir stondir)))]
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
                                                   (define t (build-path (getenv "wikimon-dir") (format "~a.png" kana)))
                                                   (list t null {thunk (make-digimoji t romaji)})})
                                 (for/list ([index (in-range (char->integer #\a) (add1 (char->integer #\z)))])
                                   (define letter (integer->char index))
                                   (define t (build-path (getenv "wikimon-dir") (format "~a.png" letter)))
                                   (list t null (curryr make-digimoji (~a (hash-ref alphabets letter letter)))))
                                 (hash-map fields {lambda [abbr emblem]
                                                    (define t (build-path (getenv "wikimon-dir") (format "~a.png" abbr)))
                                                    (list t null (curryr make-digifield emblem))})))
  
  (define mostly:readmes: (for/list ([readme.scrbl (in-list (find-files (curry regexp-match? #px"/readme.scrbl$") stondir))])
                            (define t (build-path rootdir (find-relative-path stondir (build-path (path-only readme.scrbl) "README.md"))))
                            (define ds (append (smart-dependencies readme.scrbl)
                                               (string-split (getenv "makefiles") ":")
                                               (list (build-path rootdir (smart-digimon readme.scrbl) "info.rkt"))))
                            (list t ds (curryr make-markdown readme.scrbl))))
  
  (define dist:images: (for/fold ([images null]) ([readme (in-list (map caadr mostly:readmes:))])
                         (define digimon (smart-digimon readme))
                         (define stone (find-relative-path zonedir stondir))
                         (append images (map {lambda [image]
                                               (define t (build-path rootdir digimon stone image))
                                               (define image.rkt (path-replace-suffix (build-path stondir digimon image) #".rkt"))
                                               (define ds (append (smart-dependencies image.rkt) (map car dist:digipngs:)))
                                               (if (file-exists? image.rkt) (list t ds (curryr make-image image.rkt)) null)}
                                             (parameterize ([current-directory (build-path rootdir digimon)]
                                                            [current-namespace (make-base-namespace)])
                                               (regexp-match* #px"(?<=~/).+?.png" (format "~a" (dynamic-require readme 'doc))))))))}
