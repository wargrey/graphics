#!/bin/sh

rkt=$(racket -l racket/base -l racket/file -e '(display (make-temporary-file "~a.rkt"))');
perl -ne 'print $_ if /#lang racket\/base/ .. /End Makefile/;' $0 > ${rkt};
cd $(dirname $0) && racket ${rkt};
exit $?;

#lang racket/base

(require racket/file)
(require racket/path)
(require racket/system)
(require make)

(define readme.scrbl (path->complete-path (build-path "readme.scrbl")))
(define readme.md (simplify-path (build-path 'up "readme.md")))

(define sed-readme
  {lambda [mdsrc]
    (filter string?
            (let sed ([lines mdsrc])
              (with-handlers ([exn:fail:contract? {lambda [whocares] null}])
                (define-values {line rest} (values (car lines) (cdr lines)))
                (cond [(regexp-match? #px"^#+ \\d+. " line) (cons (regexp-replace #px"^(#+) \\d+. " line "\\1 ") (sed rest))] #| Ordered Headers |#
                      [(regexp-match? #px"^\\* _" line) (cons (string-append "  " line) (sed rest))] #| Indented Items |#
                      [(zero? (string-length line)) (cons (if (regexp-match? #px"^\\* " (car rest)) (void) line) (sed rest))] #| Empty Line |#
                      [else (cons line (sed rest))]))))})

(make/proc
 (list (list readme.md (list readme.scrbl)
             {lambda [] (let ([scrbl (make-temporary-file "~a.scrbl")]
                              [md (make-temporary-file "~a.md")])
                          (display-lines-to-file (member "#lang scribble/manual" (file->lines readme.scrbl)) scrbl #:exists 'replace)
                          (system (format "scribble --markdown --dest ~a --dest-name ~a ~a >/dev/null"
                                          (path-only md) (path-replace-suffix (file-name-from-path md) #"") scrbl))
                          (display-lines-to-file (sed-readme (file->lines md)) readme.md #:exists 'replace))}))
 readme.md)
#| ------------------------------------------------------------- End Makefile ------------------------------------------------------------- |#

#lang scribble/manual

@title{Nanomon}
Digging data from Chaos such as iPhone Backups

@section{Subprojects}
@(itemlist @item{[@bold{Tencent}](village/tencent): digging data from Tencent Caches}
           @item{[@bold{iPhone}](village/phone): digging data from iPhone Backups})

@section{Project Conventions}
@(itemlist @item{[@bold{dot book}](.book) is a mystery that sets up all the prerequistes of the world. Sounds like @tt{local}.}
           @item{[@bold{d-ark}](d-ark) is the interface for users to talk with @tt{digimon}s. Namely it works like @tt{bin}.}
           @item{[@bold{village}](village) is the birth place of @tt{digimon}s. Namely it works like @tt{src}.}
           @item{[@bold{digitama}](digitama) is the egg of @tt{digimon}s. Namely it works like @tt{libraries} or @tt{frameworks}.}
           @item{[@bold{island}](island) is the living environment of @tt{digimon}. Namely it works like @tt{share} or @tt{collection}.}
           @item{@italic{nature} defines your @tt{digimon}@literal{'}s look and feel.}
           @item{[@bold{tamer}](tamer) is the interface for developers to train the @tt{digimon}. Namely it works like @tt{test}.})
