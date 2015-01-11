#lang scribble/manual

@(require racket)

@(require scribble/core)
@(require scribble/html-properties)
@(require scribble/lp-include)

@(require setup/getinfo)

@(define info-ref (get-info/full (getenv "digimon-zone")))
@(define subdir (path->string (find-relative-path (getenv "digimon-zone")
                                                  (path-only (syntax-source #'handbook)))))

@(define part->blocks
   {lambda [psection header-level]
     (define blocks (foldl {lambda [sp bs] (append bs sp)}
                           (cons (para #:style (make-style "boxed" null)
                                       ((cond [(zero? header-level) larger]
                                              [else (apply compose1 (make-list (sub1 header-level) smaller))])
                                        (elem #:style (make-style (format "h~a" (add1 header-level)) null)
                                              (part-title-content psection))))
                                 (part-blocks psection))
                           (map {lambda [sub] (part->blocks sub (add1 header-level))}
                                (part-parts psection))))
     (cond [(> header-level 0) blocks]
           [else (let ([dver (filter document-version? (style-properties (part-style psection)))])
                   (cond [(or (null? dver) (zero? (string-length (document-version-text (car dver))))) blocks]
                         [else (cons (elem #:style (make-style "tocsubtitle" null)
                                           (format "v.~a" (document-version-text (car dver))))
                                     blocks)]))])})

@title[#:version (format "~a[~a]" (version) (info-ref 'version))]{Tamer's Handbook}

@margin-note{It's reasonable to suppose that you have already know the project conventions.
             But if you reach here through a strange way, you still have the
             @hyperlink["https://github.com/digital-world/DigiGnome"]{README} page.}
This handbook makes a standard example of applying
@hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development"]{Behavior Driven Development} with
@hyperlink["http://en.wikipedia.org/wiki/Literate_programming"]{Literate Programming} that the entire project should follow
since the BDD plays an important role in my personal software engineering process.

The suggested BDD specifications are always written in some forms share the concept of ubiquitous languages, but I don't think it is a good idea,
especially in the situation that I am an indenpendent developer and communicating costs almost nothing. So I make the decision on my risks, to write
specification in both natural lanugage and programming language at the same file with the so-called @italic{Literate Programming} skill. Nevertheless,
I would like to borrow the structure from the
@hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development#Behavioural_specifications"]{@italic{User Story Specification}}.

@table-of-contents[]

@section[#:style 'unnumbered]{Makefile: I am the guider}
In order to behave my development process and to make life joyful, I write the test specification below to test
@hyperlink[(collection-file-path "makefile.rkt" (getenv "digimon-gnome"))]{makefile.rkt} itself. Yes, it is an executable specification,
however checking the @italic{makefile.rkt} is always making nonsense but cost high. So it is designated to execute manually with command
@commandline{makefile.rkt ++only @(getenv "digimon-gnome") check @(format "~a/makefile.scrbl" subdir)}

@filebox[@hyperlink[(format "~a/~a/makefile.rkt" (getenv "digimon-zone") subdir)]{@italic{@|subdir|/makefile.rkt}}]{@(part->blocks (dynamic-require "makefile.scrbl" 'doc) 0)}
