#lang scribble/manual

@(require racket)
@(require scribble/lp-include)

@(require setup/getinfo)

@(define info-ref (get-info/full (getenv "digimon-zone")))

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
it@literal{'}s a good idea, especially in the case that I@literal{'}m an indenpendent developer and communicating costs almost nothing.
So I make the decision on my risks, to write specification in both natural lanugage and formal language in the same file with
the @italic{Literate Programming} skill. Nevertheless, I@literal{'}d like to borrow the structure from the
@hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development#Behavioural_specifications"]{User Story Specification}.

@lp-include{makefile.rkt}
