#!/usr/bin/env racket
#lang racket/load

(require make)

(make-print-dep-no-line #false)
(make-print-checking #false)
(make-print-reasons #false)

(define make-dry-run (make-parameter #false))
(define make-always-run (make-parameter #false))
(define make-just-touch (make-parameter #false))

(module makefile racket
  (require "island/stone/digimon.rkt")
  
  (provide makefiles rootdir stnsdir hackdir)
  
  (define #%digimon (#%variable-reference digimon?))
  (define makefiles (list (with-handlers ([exn:fail:contract? (const (build-path (current-directory) "makefile.rkt"))])
                          (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)))))
  (define rootdir (path-only (car makefiles)))
  (define stnsdir (path-only (variable-reference->module-source #%digimon)))
  (define vllgdir (build-path rootdir "village"))
  (define hackdir (build-path (find-system-path 'temp-dir) (symbol->string (gensym "rktmk.hack"))))

  (rosetta-stone-dir stnsdir)
  
  (define kanas (hash 'ア 'a 'イ 'Test3 'ウ 'u 'エ 'e 'オ 'o 'ヤ 'ya 'ユ 'yu 'ヨ 'yo 'ワ 'wa 'ヲ 'wo 'ン 'Test4a 'ー 'chouon2 'ヴ 'vu
                      'カ 'ka3 'キ 'ki 'ク 'ku2 'ケ 'ke 'コ 'ko 'ガ 'ga 'ギ 'gi 'グ 'gu 'ゲ 'ge 'ゴ 'go
                      'サ 'sa 'シ 'shi 'ス 'su 'セ 'se 'ソ 'so 'ザ 'za 'ジ 'ji 'ズ 'zu 'ゼ 'ze 'ゾ 'zo
                      'タ 'ta2 'チ 'chi 'ツ 'tsu 'テ 'te 'ト 'to 'ダ 'da 'デ 'de 'ド 'do
                      'ナ 'na 'ニ 'ni 'ヌ 'nu 'ネ 'ne 'ノ 'no
                      'ハ 'ha 'ヒ 'hi3 'フ 'fu 'ヘ 'he 'ホ 'ho 'バ 'ba 'ビ 'bi 'ブ 'bu 'ベ 'be 'ボ 'bo 'パ 'pa 'ピ 'pi3 'プ 'pu 'ペ 'pe 'ポ 'po
                      'マ 'ma2 'ミ 'mi 'ム 'mu 'メ 'me 'モ 'mo
                      'ラ 'ra 'リ 'ri 'ル 'ru2 'レ 're 'ロ 'ro))
  
  (define theirfaults '{ma2})
  (define alphabets (hash #\q 'q2))
  
  (define fields (hash 'NSp 'Naturespirits 'DS 'Deepsavers 'NSo 'Nightmaresoldiers 'WG 'Windguardians
                       'ME 'Metalempire 'VB 'Virusbusters 'DR 'Dragonsroar 'JT 'Jungletroopers))
  
  (define smart-dependencies
    {lambda [entry [memory null]]
      (foldl {lambda [subpath memory]
               (define subscrbl (simplify-path (build-path (path-only entry) (bytes->string/utf-8 subpath))))
               (cond [(member subscrbl memory) memory]
                     [else (smart-dependencies subscrbl memory)])}
             (append memory (list entry))
             (call-with-input-file entry (curry regexp-match* #px"(?<=(@include-section\\{)|(\\(require \")).+?.(scrbl|rkt)(?=(\\})|(\"\\)))")))})
  
  (define make-markdown
    {lambda [target dentry]
      (define mdname (gensym 'readme))
      (define scribble (find-executable-path "scribble"))
      (system (format "~a ++arg ~a --markdown --dest ~a --dest-name ~a ~a"
                      (if scribble scribble
                          (simplify-path (build-path (path-only (find-system-path 'exec-file))
                                                     (path-only (find-system-path 'collects-dir))
                                                     "bin/scribble")))
                      (let ([village? (regexp-match (pregexp (format "(?<=/)~a/[^/]+" (last (explode-path vllgdir)))) dentry)])
                        (if village? (build-path rootdir (car village?)) rootdir))
                      hackdir mdname dentry))
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
      (send (recv-image png) save-file target 'png)})
  
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
        (namespace-attach-module (variable-reference->namespace #%digimon) (variable-reference->module-source #%digimon))
        (namespace-require 'racket)
        (define img (with-input-from-file dentry #:mode 'text
                      {thunk (read-language) ; Do nothing
                             (let repl ([last-result (void)])
                               (define sexp (read))
                               (if (eof-object? sexp) last-result (repl (eval sexp))))}))
        (make-parent-directory* target)
        (send (cond [(pict? img) (pict->bitmap img)] [(flomap? img) (flomap->bitmap img)] [else img]) save-file target 'png))})
  
  (define dist:digipngs: (append (hash-map kanas {lambda [kana romaji]
                                                     (define t (build-path stnsdir (format "~a.png" kana)))
                                                     (list t null {thunk (make-digimoji t romaji)})})
                                   (for/list ([index (in-range (char->integer #\a) (add1 (char->integer #\z)))])
                                     (define letter (integer->char index))
                                     (define t (build-path stnsdir (format "~a.png" letter)))
                                     (list t null (curryr make-digimoji (~a (hash-ref alphabets letter letter)))))
                                   (hash-map fields {lambda [abbr emblem]
                                                      (define t (build-path stnsdir (format "~a.png" abbr)))
                                                      (list t null (curryr make-digifield emblem))})))
  
  (define mostly:readmes: (for/list ([readme.scrbl (in-directory stnsdir)]
                                     #:when (string=? (path->string (file-name-from-path readme.scrbl)) "readme.scrbl"))
                            (define t (build-path rootdir (find-relative-path stnsdir (build-path (path-only readme.scrbl) "README.md"))))
                            (define ds (append (smart-dependencies readme.scrbl) makefiles
                                               (let* ([village? (regexp-match (pregexp (format "(?<=/)~a/[^/]+" (last (explode-path vllgdir)))) readme.scrbl)]
                                                      [info.rkt (if village? (build-path rootdir (car village?) "info.rkt") (build-path rootdir "info.rkt"))])
                                                 (if (file-exists? info.rkt) (list info.rkt) null))))
                            (list t ds (curryr make-markdown readme.scrbl))))
  
  (define dist:images: (for/fold ([images null]) ([readme (in-list (map caadr mostly:readmes:))])
                         (define village (let ([village? (regexp-match (pregexp (format "(?<=/)~a/[^/]+" (last (explode-path vllgdir)))) readme)]) (if village? (car village?) "")))
                         (define stone (find-relative-path rootdir stnsdir))
                         (append images (map {lambda [image]
                                               (define t (build-path rootdir village stone image))
                                               (define image.rkt (path-replace-suffix (build-path rootdir stone village image) #".rkt"))
                                               (define ds (append (smart-dependencies image.rkt) (map car dist:digipngs:)))
                                               (if (file-exists? image.rkt) (list t ds (curryr make-image image.rkt)) null)}
                                             (map bytes->string/utf-8 (call-with-input-file readme (curry regexp-match* #px"(?<=~/).+?.png"))))))))

(require 'makefile)

(define make~default:
  {lambda [unknown]
    (if (regexp-match #px"^[+][+]" unknown)
        (printf "make: I don't know how to make `~a`!~n" (substring unknown 2))
        (printf "make: I don't know what does `~a` mean!~n" unknown))})

(define make~ifdefined:
  {lambda [fsymbols]
    (for-each {lambda [make-install:] (make-install:)}
              (filter {lambda [?] (and (procedure? ?) (procedure-arity-includes? ? 0))}
                      (map (curryr namespace-variable-value #false (const #false)) fsymbols)))})

(define make~clean:
  {lambda [level]
    (parameterize ([current-namespace (module->namespace ''makefile)])
      (cond [(symbol? level) (for-each (compose1 make~clean: symbol->string) (reverse (member level '{maintainer dist clean mostly})))]
            [else (for ([var (namespace-mapped-symbols)])
                    (when (regexp-match? (pregexp (format "^~a:.+:$" level)) (symbol->string var))
                      (for ([file (in-list (map car (namespace-variable-value var #false (const #false))))])
                        (when (file-exists? file) (delete-file file))
                        (printf "make: deleted ~a.~n" file))))]))})

(define make~all:
  {lambda [targets]
    (parameterize ([current-namespace (module->namespace ''makefile)])
      (file-or-directory-modify-seconds hackdir (current-seconds) {thunk (make-directory* hackdir)})
      (define {hack-target t} (if (make-dry-run) (build-path hackdir (find-relative-path rootdir t)) t))
      (define {hack-depends ds} (if (make-always-run) (cons hackdir ds) ds))
      (define {hack-rule r}
        (define t (hack-target (first r)))
        (define f {thunk ((third r) t)})
        (list t (hack-depends (second r))
              (if (make-just-touch) {thunk (file-or-directory-modify-seconds t (current-seconds) f)} f)))
      (define rules (map hack-rule (foldr append null (filter {lambda [val]
                                                                (with-handlers ([exn? (const #false)])
                                                                  (andmap {lambda [?] (and (andmap path-string? (cons (first ?) (second ?)))
                                                                                           (procedure-arity-includes? (third ?) 1))} val))}
                                                              (filter-map (compose1 (curryr namespace-variable-value #false) string->symbol second)
                                                                          (filter-map (curry regexp-match #px"^\\s\\s.define\\s+(.+?:)\\s+")
                                                                                      (with-input-from-file "makefile.rkt" port->lines)))))))
      (make/proc (cons (list (car makefiles) null (const 'I-am-here-just-for-fun)) rules)
                 (if (list? targets) (map hack-target targets) (map car rules))))})

(command-line #:program (file-name-from-path (car makefiles))
              #:argv (let*-values ([{options targets} (partition (curry regexp-match? #px"^[-+]")
                                                                 (remove-duplicates (remove "--" (vector->list (current-command-line-arguments)))))]
                                   [{files phonies} (partition filename-extension (if (null? targets) (list "all") targets))])
                       (append options (map (curry string-append "++") phonies) (list "--") files))
              #:usage-help "Be careful our conventions are not exactly the same as those of GNU Make."
              #:help-labels
              "[<option>s start with `++` are also <phony-target>s.]"
              "[You are free to drop off the leading `++` when typing phony targets.]"
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
              [{"-t" "--touch"} "Touch targets instead of remaking them if the target already exists."
                                (make-just-touch #true)]
              [{"-v" "--verbose"} "Building with verbose messages."
                                  (void (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true))]
              #:handlers
              {lambda [non-void-flags . filenames] (make~all: (map {lambda [f] (if (relative-path? f) (build-path rootdir f) f)} filenames))}
              (list "phony-target|file-path")
              (compose1 exit (const 0) display (curryr string-replace #px"  -- : .+?-h --'." ""))
              make~default:)
