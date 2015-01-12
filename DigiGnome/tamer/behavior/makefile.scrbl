#lang scribble/manual

@(require racket)
@(require scribble/lp-include)

@(require setup/getinfo)

@(define info-ref (get-info/full (getenv "digimon-zone")))
@(define subdir (path->string (find-relative-path (getenv "digimon-zone")
                                                  (path-only (syntax-source #'handbook)))))

@title[#:style (list 'non-toc) #:version (format "~a[~a]" (version) (info-ref 'version))]{@bold{Tamer@literal{'}s Handbook}}

@margin-note{It@literal{'}s reasonable to suppose that you have already know the project conventions.
             But if you reach here through a strange way, you still have the
             @hyperlink["https://github.com/digital-world/DigiGnome"]{README} page.}
This handbook is the sample of applying
@hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development"]{Behavior Driven Development(BDD)} with
@hyperlink["http://en.wikipedia.org/wiki/Literate_programming"]{Literate Programming} via testing
@hyperlink[(collection-file-path "makefile.rkt" (getenv "digimon-gnome"))]{makefile.rkt} itself
that the entire project should follow since the BDD plays an important role in my personal software engineering process.

The suggested BDD specifications are always written in some forms share the concept of @italic{ubiquitous languages}, but I don@literal{'}t think
it@literal{'}s a good idea, especially in the situation that I@literal{'}m an indenpendent developer and communicating costs almost nothing.
So I make the decision on my risks, to write specification in both natural lanugage and formal language in the same file with
the @italic{Literate Programming} skill. Nevertheless, I@literal{'}d like to borrow the structure from the
@hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development#Behavioural_specifications"]{User Story Specification}.

This handbook is also executable,
however checking the @italic{makefile.rkt} always makes nonsense but costs high. So it is designated to execute manually:
@nested[#:style 'code-inset]{@(itemlist #:style 'compact
                                        @item{@exec{cd @(getenv "digimon-zone")}}
                                        @item{@exec{makefile.rkt +o @(getenv "digimon-gnome") check @(format "~a/makefile.scrbl" subdir)}})}

@lp-include{makefile.rkt}
