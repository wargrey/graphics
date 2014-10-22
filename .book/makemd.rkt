#!/bin/sh

#|
cd $(dirname $0) && exec racket $(basename $0);
|#

#lang at-exp racket

(require make)

(define rootdir (simplify-path (build-path 'up)))
(define bookdir (path->complete-path (current-directory)))
(define rsttdir (build-path bookdir "island" "rosetta"))
(define stnedir (build-path rsttdir "stone"))
(define makemd (build-path bookdir "makemd.rkt"))

(make-print-checking #false)

(define make-markdown
  {lambda [ds t]
    (define md (make-temporary-file "~a.md"))
    (system (format "~a --markdown --dest ~a --dest-name ~a ~a > /dev/null"
                    (find-executable-path "scribble") (path-only md)
                    (path-replace-suffix (file-name-from-path md) #"") (first ds)))
    (with-output-to-file t #:exists 'replace
      {thunk (define awkout (current-thread))
             (define awk-indent (thread {thunk (define pipen awkout)
                                             (let awk ()
                                               (define line (thread-receive))
                                               (with-handlers ([exn:fail:contract? {lambda [eof!] (thread-send pipen eof)}])
                                                 (cond [(regexp-match? #px"^\\* _" line) (thread-send pipen (string-append "  " line))]
                                                       [else (thread-send pipen line)])
                                                 (awk)))}))
             (define awk-lines (thread {thunk (define pipen awk-indent)
                                              (let awk ()
                                                (define line (thread-receive))
                                                (cond [(eof-object? line) (thread-send pipen eof)]
                                                      [(regexp-match? #px"^\\s*$" line) (let ([next (thread-receive)])
                                                                                          (cond [(eof-object? next) (thread-send pipen line) (thread-send pipen eof)]
                                                                                                [(regexp-match? #px"^\\* " next) (thread-send pipen next)]
                                                                                                [(regexp-match? #px"^\\s*$" next) (thread-rewind-receive (list next))]
                                                                                                [else (thread-send pipen line) (thread-send pipen next)])
                                                                                          (unless (eof-object? next) (awk)))]
                                                      [else (thread-send pipen line) (awk)]))}))
             (define awk-title (thread {thunk (define pipen awk-lines)
                                              (let awk ()
                                                (define line (thread-receive))
                                                (with-handlers ([exn:fail:contract? {lambda [eof!] (thread-send pipen eof)}])
                                                  (cond [(regexp-match? #px"^#+ (\\d+\\.)+$" line) 'Skip-Unnamed-Title]
                                                        [(regexp-match? #px"^#+ (\\d+\\.)+" line) (thread-send pipen (regexp-replace #px"^(#+) (\\d+\\.)+" line "\\1"))]
                                                        [else (thread-send pipen line)])
                                                  (awk)))}))
             (call-with-input-file md
               {lambda [markdown.md]
                 (define pipe0 awk-title)
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
  {lambda [ds d]
    (void)})

(define make-rule
  {lambda [t ds maker]
    (list t ds {thunk (maker ds t)})})

(define smart-dependencies
  {lambda [scrbl [memory null]]
    (foldl {lambda [subpath memory]
             (define subscrbl (build-path rsttdir (~a subpath)))
             (cond [(member subscrbl memory) memory]
                   [else (smart-dependencies subscrbl memory)])}
           (cons scrbl memory)
           (call-with-input-file scrbl (curry regexp-match* #px"(?<=@include-section\\{)[^\\}]+(?=\\})")))})

(define rules (filter list? (foldl append null (for/list ([scrbl (in-directory rsttdir (const #false))] #:when (equal? #"scrbl" (filename-extension scrbl)))
                                                 (with-handlers ([exn:fail:contract? (const null)])
                                                   (define modeline (first (call-with-input-file scrbl (curry regexp-match #px"(?<=@;\\{)[^\\}]+(?=\\})"))))
                                                   (define sds (remove-duplicates (cons scrbl (smart-dependencies scrbl))))
                                                   (map {lambda [rule]
                                                          (with-handlers ([exn:fail:contract? void])
                                                            (define t (symbol->string (car rule)))
                                                            (define ds (cons makemd (map (curry build-path rsttdir) (map symbol->string (cdr rule)))))
                                                            (case (filename-extension t)
                                                              [{#"md"} (make-rule (if (absolute-path? t) (reroot-path t rootdir) (build-path bookdir t)) (append sds ds) make-markdown)]
                                                              [{#"png"} (make-rule (build-path stnedir t) ds make-png)]))}
                                                        (with-input-from-bytes modeline {thunk (let read-next ()
                                                                                                 (define rule (read))
                                                                                                 (cond [(eof-object? rule) null]
                                                                                                       [else (cons rule (read-next))]))})))))))

(unless (null? rules)
  (make/proc rules (map car rules)))
