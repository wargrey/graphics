#!/bin/sh

#|
cd $(dirname $0) && exec racket $(basename $0);
|#

#lang racket

(require make)

(require pict)
(require racket/draw)
(require images/flomap)

(require (only-in "island/stone/composition.rkt" wikimon-recv!))

(make-print-dep-no-line #true)
(make-print-checking #true)
(make-print-reasons #true)

(define rootdir (simplify-path (build-path 'up)))
(define bookdir (path->complete-path (current-directory)))
(define stnsdir (build-path bookdir "island" "stone"))
(define mojidir (build-path stnsdir ".book" "stone"))
(define make.md (list (build-path bookdir "makemd.rkt")))

(define kanas (hash 'ア 'a 'イ 'i 'ウ 'u 'エ 'e 'オ 'o 'ヤ 'ya 'ユ 'yu 'ヨ 'yo 'ワ 'wa 'ヲ 'wo 'ン 'n 'ー 'chouon2
                    'カ 'ka 'キ 'ki 'ク 'ku 'ケ 'ke 'コ 'ko 'ガ 'ga 'ギ 'gi 'グ 'gu 'ゲ 'ge 'ゴ 'go
                    'サ 'sa 'シ 'shi 'ス 'su 'セ 'se 'ソ 'so 'ザ 'za 'ジ 'ji 'ズ 'zu 'ゼ 'ze 'ゾ 'zo
                    'タ 'ta2 'チ 'chi 'ツ 'tsu 'テ 'te 'ト 'to 'ダ 'da 'デ 'de 'ド 'do
                    'ナ 'na 'ニ 'ni 'ヌ 'nu 'ネ 'ne 'ノ 'no
                    'ハ 'ha 'ヒ 'hi 'フ 'fu 'へ 'he 'ホ 'ho 'バ 'ba 'ビ 'bi 'ブ 'bu 'ベ 'be 'ボ 'bo 'パ 'pa 'ピ 'pi 'プ 'pu 'ペ 'pe 'ポ 'po
                    'マ 'ma 'ミ 'mi 'ム 'mu 'メ 'me 'モ 'mo
                    'ラ 'ra 'リ 'ri 'ル 'ru 'レ 're 'ロ 'ro))

(define make-markdown
  {lambda [target dentry]
    (define mdname (gensym 'readme))
    (define scribble (find-executable-path "scribble"))
    (system (format "~a --markdown --dest ~a --dest-name ~a ~a"
                    (if scribble scribble
                        (simplify-path (build-path (path-only (find-system-path 'exec-file))
                                                   (path-only (find-system-path 'collects-dir))
                                                   "bin/scribble")))
                    (find-system-path 'temp-dir) mdname dentry))
    (with-output-to-file target #:exists 'replace
      {thunk (define awkout (current-thread))
             (define awk-format (thread {thunk (let awk ([pipen awkout])
                                                 (define in (thread-receive))
                                                 (define out (match in
                                                               [{pregexp #px"^#+ (\\d+\\.)+"} (regexp-replace #px"^(#+) (\\d+\\.)+" in "\\1")]
                                                               [{pregexp #px"^\\* >\\s+>"} (regexp-replace #px"^\\* >(\\s+)>" in "\\1- ")]
                                                               [{pregexp #px"\\]\\(~/"} (string-replace in "~" (substring (~a stnsdir) (sub1 (string-length (~a rootdir)))))]
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
                          (awk-readme pipe0 waiteen)))})})})

(define make-digimoji
  {lambda [target name pattern]
    (unless (directory-exists? (path-only target)) (make-directory* (path-only target)))
    (with-output-to-file target #:exists 'replace
      {thunk (define file (format (string-append "/File:" pattern) name))
             (define pxpng (pregexp (format (format "(?<=href..)/images[^>]+~a(?=.>)" pattern) name)))
             (define png (port->bytes (third (wikimon-recv! (car (regexp-match* pxpng (third (wikimon-recv! file))))))))
             (write-bytes png)})})

(define make-image
  {lambda [target dentry]
    (dynamic-require dentry #false)
    (define name (string->symbol (path->string (path-replace-suffix (file-name-from-path target) ""))))
    (parameterize ([current-namespace (module->namespace dentry)])
      (define img (namespace-variable-value name #false))
      (make-directory* (path-only target))
      (send (cond [(pict? img) (pict->bitmap img)] [(flomap? img) (flomap->bitmap img)] [else img])
            save-file target 'png))})

(define smart-dependencies
  {lambda [entry [memory null]]
    (foldl {lambda [subpath memory]
             (define subscrbl (simplify-path (build-path (path-only entry) (bytes->string/utf-8 subpath))))
             (cond [(member subscrbl memory) memory]
                   [else (smart-dependencies subscrbl memory)])}
           (append memory (list entry))
           (call-with-input-file entry (curry regexp-match* (case (filename-extension entry)
                                                              [{#"scrbl"} #px"(?<=@include-section\\{)[^\\}]+(?=\\})"]
                                                              [{#"rkt"} #px"(?<=\\(require \").+\\.rkt(?=\"\\))"]))))})

(define readmes (filter list? (for/list ([readme.scrbl (in-directory stnsdir)]
                                         #:when (string=? (path->string (file-name-from-path readme.scrbl)) "readme.scrbl"))
                                (define t (reroot-path (substring (path->string (path-replace-suffix readme.scrbl #".md")) (string-length (path->string stnsdir))) rootdir))
                                (define ds (append (smart-dependencies readme.scrbl) make.md))
                                (list t ds {thunk (make-markdown t readme.scrbl)}))))

(define digimojies (append (hash-map kanas {lambda [kana romaji]
                                             (define t (build-path mojidir (format "~a.png" kana)))
                                             (list t null {thunk (make-digimoji t romaji "Dc~a_w.png")})})
                           (for/list ([index (in-range (char->integer #\a) (add1 (char->integer #\z)))])
                             (define letter (string-downcase (string (integer->char index))))
                             (define t (build-path mojidir (format "~a.png" letter)))
                             (list t null {thunk (make-digimoji t letter "Dc~a_r_w.png")}))))

(define images (filter list? (map {lambda [image]
                                    (define t (build-path stnsdir (bytes->string/utf-8 image)))
                                    (define image.rkt (path-replace-suffix t #".rkt"))
                                    (when (file-exists? image.rkt)
                                      (define ds (append (smart-dependencies image.rkt) make.md))
                                      (list t ds {thunk (make-image t image.rkt)}))}
                                  (foldl append null (map (curryr call-with-input-file (curry regexp-match* #px"(?<=\\]\\(~/)[^)]+(?=\\))"))
                                                          (map caadr readmes))))))

(let ([rules (append readmes digimojies images)])
  (unless (null? rules)
    (make/proc rules (map car rules))))
