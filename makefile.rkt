#!/usr/bin/env racket
#lang racket

(require make)
(require "island/stone/digimon.rkt")

(define-namespace-anchor makefile)
(define #%digimon (#%variable-reference digimon?))

(define makefiles (list (with-handlers ([exn:fail:contract? (const (build-path (current-directory) "makefile.rkt"))])
                          (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)))))
(define rootdir (path-only (car makefiles)))
(define stnsdir (path-only (variable-reference->module-source #%digimon)))
(define vllgdir (build-path rootdir "village"))

(define make-dry-run (make-parameter #false))
(make-print-dep-no-line #false)
(make-print-checking #false)
(make-print-reasons #false)

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
                    (find-system-path 'temp-dir) mdname dentry))
    (define update-target
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
             (with-input-from-file (format "~a/~a.md" (find-system-path 'temp-dir) mdname)
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
                          (awk-readme pipe0 waiteen)))})})
    (cond [(make-dry-run) (update-target)]
          [else (with-output-to-file (build-path (path-only target) "README.md") update-target #:exists 'replace)])})

(define make-digimoji
  {lambda [target name]
    (define png (format (cond [(regexp-match? #px"^Test" (~a name)) "~a.png"]
                              [(member name theirfaults) "Dc~a.png"]
                              [(symbol? name) "Dc~a_w.png"]
                              [(string? name) "Dc~a_r_w.png"])
                        name))
    (unless (directory-exists? (path-only target)) (make-directory* (path-only target)))
    (send (recv-image png) save-file target 'png)})

(define make-digifield
  {lambda [target emblem]
    (define pxpng (pregexp (format "/images/thumb[^ ]+100px-~a_emblem.png" emblem)))
    (unless (directory-exists? (path-only target)) (make-directory* (path-only target)))
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
      (make-directory* (path-only target))
      (cond [(make-dry-run) (show-pict (cond [(is-a? img bitmap%) (bitmap img)] [(flomap? img) (bitmap (flomap->bitmap img))] [else img]))]
            [else (send (cond [(pict? img) (pict->bitmap img)] [(flomap? img) (flomap->bitmap img)] [else img]) save-file target 'png)]))})

(define readmes (for/list ([readme.scrbl (in-directory stnsdir)]
                           #:when (string=? (path->string (file-name-from-path readme.scrbl)) "readme.scrbl"))
                  (define t (build-path rootdir (find-relative-path stnsdir (build-path (path-only readme.scrbl) "README.md"))))
                  (define ds (append (smart-dependencies readme.scrbl) makefiles
                                     (let* ([village? (regexp-match (pregexp (format "(?<=/)~a/[^/]+" (last (explode-path vllgdir)))) readme.scrbl)]
                                            [info.rkt (if village? (build-path rootdir (car village?) "info.rkt") (build-path rootdir "info.rkt"))])
                                       (if (file-exists? info.rkt) (list info.rkt) null))))
                  (list t ds {thunk (make-markdown t readme.scrbl)})))

(define digimojies (append (hash-map kanas {lambda [kana romaji]
                                             (define t (build-path stnsdir (format "~a.png" kana)))
                                             (list t null {thunk (make-digimoji t romaji)})})
                           (for/list ([index (in-range (char->integer #\a) (add1 (char->integer #\z)))])
                             (define letter (integer->char index))
                             (define t (build-path stnsdir (format "~a.png" letter)))
                             (list t null {thunk (make-digimoji t (~a (hash-ref alphabets letter letter)))}))))

(define digifields (hash-map fields {lambda [abbr emblem]
                                      (define t (build-path stnsdir (format "~a.png" abbr)))
                                      (list t null {thunk (make-digifield t emblem)})}))

(define images (for/fold ([images null]) ([readme (in-list (map caadr readmes))])
                 (define village (let ([village? (regexp-match (pregexp (format "(?<=/)~a/[^/]+" (last (explode-path vllgdir)))) readme)]) (if village? (car village?) "")))
                 (define stone (find-relative-path rootdir stnsdir))
                 (append images (map {lambda [image]
                                       (define t (build-path rootdir village stone image))
                                       (define image.rkt (path-replace-suffix (build-path rootdir stone village image) #".rkt"))
                                       (if (file-exists? image.rkt) (list t (smart-dependencies image.rkt) {thunk (make-image t image.rkt)}) null)}
                                     (map bytes->string/utf-8 (call-with-input-file readme (curry regexp-match* #px"(?<=~/).+?.png")))))))

(define make-default:
  {lambda [unknown]
    (if (regexp-match #px"^[+][+]" unknown)
        (printf "make: I don't know how to make ~a!~n" (substring unknown 2))
        (printf "make: I don't know what does ~a mean!~n" unknown))})

(define make~all:
  {lambda []
    (define rules (append digimojies digifields images readmes))
    (unless (null? rules) (make/proc rules (map car rules)))})

(define make~mostlyclean:
  {lambda []
    (for ([readme (in-list (map car readmes))])
      (delete-file readme)
      (printf "make: removed ~a~n" readme))})

(define make~clean:
  {lambda []
    (make~mostlyclean:)})

(define make~distclean:
  {lambda []
    (make~clean:)
    (for ([digipng (in-list (map car (append images digimojies digifields)))])
      (delete-file digipng)
      (printf "make: removed ~a~n" digipng))})

(define make~maintainer-clean:
  {lambda []
    (make~distclean:)})

(define ~targets (parameterize ([current-namespace (namespace-anchor->namespace makefile)])
                   (filter cons? (map {lambda [sym]
                                        (define phony (regexp-match #px"(?<=make~).+?(?=:)" (symbol->string sym)))
                                        (define maker (when (list? phony) (namespace-variable-value sym #false (const #false))))
                                        (when (procedure? maker) (cons (car phony) maker))}
                                      (namespace-mapped-symbols)))))

(define flag->maker
  {lambda [flag]
    (define target (assoc (substring flag 2) ~targets))
    (if (cons? target) (cdr target) (curry make-default: flag))})

(command-line #:program (file-name-from-path (car makefiles))
              #:argv (vector-map {lambda [arg] (if (regexp-match #px"^[-+]" arg) arg (format "++~a" arg))} (current-command-line-arguments))
              #:once-any
              [{"-v" "--verbose"} "Building with verbose messages." (void (make-print-dep-no-line #true) (make-print-checking #true) (make-print-reasons #true))]
              #:once-each
              [{"-n" "--test" "--dry-run"} "Do not actually update targets, just display their content." (make-dry-run #true)]
              ["++all" => flag->maker '{"Building the entire program without generating documentation. This is also the default target."}]
              ["++install" => flag->maker '{"Building and installing the program with three categories: normal ones, pre-installation commands and post-installation commands."}]
              ["++install-docs" => flag->maker '{"Generating and installing documentation."}]
              ["++uninstall" => flag->maker '{"Delete all the installed files and documentation with three categories, just like the installation commands."}]
              ["++mostlyclean" => flag->maker '{"Delete all files except that people normally don't want to reconstruct."}]
              ["++clean" => flag->maker '{"Delete all files that are normally created by running make."}]
              ["++distclean" => flag->maker '{"Delete all files that are not included in the distribution, even if the makefile itself cannot create these files."}]
              ["++maintainer-clean" => flag->maker '{"Delete all files that can be reconstructed, including sources produced by Source Generator."}]
              ["++docs" => flag->maker '{"Generating documentation. User must manually invoke it."}]
              ["++dist" => flag->maker '{"Creating a distribution file of the source files."}]
              ["++test" => flag->maker '{"Performing unit tests on the source file after building."}]
              ["++check" => flag->maker '{"Performing self tests on the program this makefile builds after installing."}]
              ["++installcheck" => flag->maker '{"Performing installation tests on the target system after installing."}]
              ["++installdirs" => flag->maker '{"Creating the directories where files are installed, and their parent directories."}]
              #:handlers
              {lambda [makers . whocares] (let ([argv (vector-filter-not (curry regexp-match #px"^[-+]") (current-command-line-arguments))])
                                            (for ([make~phony: (in-list (if (zero? (vector-length argv)) (list make~all:) makers))])
                                              (printf "~a~n" make~phony:)
                                              (make~phony:)))}
              '{"phony-target"}
              {lambda [help-info] (let* ([helps (with-input-from-string help-info {thunk (port->lines)})]
                                         [phonies (filter (curry regexp-match #px"^\\s+[+][+]") helps)])
                                    (for-each (curry printf "~a~n")
                                              (append (remove* phonies helps)
                                                      (list (format "~nwhere <phony-target> is one of the followings that follow the conventions of the GUN Make."))
                                                      (filter {lambda [phony] (assoc (car (regexp-match #px"(?<=^\\s\\s).+?(?=\\s:\\s)" phony)) ~targets)}
                                                              (map {lambda [phony] (regexp-replace #px"\\+\\+" phony "")} phonies)))))}
              make-default:)
