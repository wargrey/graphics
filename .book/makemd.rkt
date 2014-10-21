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

(define sed-readme
  {lambda [mdsrc]
    (filter string?
            (let sed ([lines mdsrc])
              (with-handlers ([exn:fail:contract? {lambda [whocares] null}])
                (define-values {line rest} (values (car lines) (cdr lines)))
                (cond [(and (regexp-match? #px"^\\s*$" line) (regexp-match? #px"^#+ (\\d+\\.)+$" (car rest))) (cons 'empty-line-and-untitled-section (sed (cdr rest)))]
                      [(and (regexp-match? #px"^\\s*$" line) (regexp-match? #px"^\\* " (car rest))) (cons 'empty-line-between-items (sed rest))]
                      [(regexp-match? #px"^#+ (\\d+\\.)+" line) #| Ordered Headers |# (cons (regexp-replace #px"^(#+) (\\d+\\.)+" line "\\1") (sed rest))]
                      [(regexp-match? #px"^\\* _" line) #| Indented Items |# (cons (string-append "  " line) (sed rest))]
                      [else (cons line (sed rest))]))))})

(define make-markdown
  {lambda [ds d]
    (define md (make-temporary-file "~a.md"))
    (system (format "~a --markdown --dest ~a --dest-name ~a ~a > /dev/null"
                    (find-executable-path "scribble") (path-only md)
                    (path-replace-suffix (file-name-from-path md) #"") (first ds)))
    (display-lines-to-file (sed-readme (file->lines md)) d #:exists 'replace)})

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
