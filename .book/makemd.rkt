#!/bin/sh

#|
cd $(dirname $0) && exec racket $(basename $0);
|#

#lang at-exp racket

(require make)

(require pict)
(require racket/draw)
(require images/flomap)

(define rootdir (simplify-path (build-path 'up)))
(define bookdir (path->complete-path (current-directory)))
(define rsttdir (build-path bookdir "island" "rosetta"))
(define makemd (build-path bookdir "makemd.rkt"))

(define make-markdown
  {lambda [target dentry]
    (define md (make-temporary-file "~a.md"))
    (system (format "~a --markdown --dest ~a --dest-name ~a ~a"
                    (find-executable-path "scribble") (path-only md)
                    (path-replace-suffix (file-name-from-path md) #"") dentry))
    (with-output-to-file target #:exists 'replace
      {thunk (define awkout (current-thread))
             (define awk-format (thread {thunk (define pipen awkout)
                                               (let awk ()
                                                 (define line (thread-receive))
                                                 (cond [(eof-object? line) (thread-send pipen eof)]
                                                       [(regexp-match? #px"^#+ (\\d+\\.)+" line) (thread-send pipen (regexp-replace #px"^(#+) (\\d+\\.)+" line "\\1"))]
                                                       [(regexp-match? #px"^\\s*$" line) (let ([next (thread-receive)])
                                                                                           (cond [(and (not (eof-object? next))
                                                                                                       (regexp-match? #px"(^\\* )|(^\\s*$)" next)) 'Skip-Empty-Line]
                                                                                                 [else (thread-send pipen line)])
                                                                                           (thread-rewind-receive (list next)))]
                                                       [(regexp-match? #px"^\\* >\\s+>" line) (thread-send pipen (regexp-replace #px"^\\* >(\\s+)>" line "\\1- "))]
                                                       [else (thread-send pipen line)])
                                                 (unless (eof-object? line)
                                                   (awk)))}))
             (call-with-input-file md
               {lambda [markdown.md]
                 (define pipe0 awk-format)
                 (let awk-readme ([waitees (list markdown.md (thread-receive-evt))])
                   (define which (apply sync waitees))
                   (cond [(eq? markdown.md which) (let ([line (read-line markdown.md)])
                                                   (cond [(eof-object? line) (thread-send pipe0 line) (awk-readme (list (thread-receive-evt)))]
                                                         [else (thread-send pipe0 line) (awk-readme waitees)]))]
                         [(eq? (thread-receive-evt) which) (let ([line (thread-receive)])
                                                             (unless (eof-object? line)
                                                               (displayln line)
                                                               (awk-readme waitees)))]))})})})

(define make-png
  {lambda [target dentry]
    (dynamic-require dentry #false)
    (define name (string->symbol (path->string (path-replace-suffix (file-name-from-path target) ""))))
    (parameterize ([current-namespace (module->namespace dentry)])
      (namespace-set-variable-value! 'edge 300)
      (define img ((namespace-variable-value name #false)))
      (make-directory* (path-only target))
      (send (cond [(pict? img) (pict->bitmap img)] [(flomap? img) (flomap->bitmap img)] [else img])
            save-file target 'png))})

(define smart-dependencies
  {lambda [entry [memory null]]
    (foldl {lambda [subpath memory]
             (define subscrbl (simplify-path (build-path (path-only entry) (bytes->string/utf-8 subpath))))
             (cond [(member subscrbl memory) memory]
                   [else (smart-dependencies subscrbl memory)])}
           (cons entry memory)
           (call-with-input-file entry (curry regexp-match* (case (filename-extension entry)
                                                              [{#"scrbl"} #px"(?<=@include-section\\{)[^\\}]+(?=\\})"]
                                                              [{#"rkt"} @pregexp{(?<=(require\s+"[^"]+(?=")}]))))})

(define readmes (filter list? (for/list ([readme.scrbl (in-directory rsttdir)]
                                         #:when (string=? (path->string (file-name-from-path readme.scrbl)) "readme.scrbl"))
                                (with-handlers ([exn:fail:contract? {lambda [efc] (fprintf (current-error-port) "~a" efc)}])
                                  (define t (reroot-path (substring (path->string (path-replace-suffix readme.scrbl #".md")) (string-length (path->string rsttdir))) rootdir))
                                  (define ds (cons readme.scrbl (remove readme.scrbl (smart-dependencies readme.scrbl))))
                                  (list t ds {thunk (make-markdown t readme.scrbl)})))))

(unless (null? readmes)
  (make/proc readmes (map car readmes)))