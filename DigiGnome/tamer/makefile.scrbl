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
@hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development"]{Behavior Driven Development} with
@hyperlink["http://en.wikipedia.org/wiki/Literate_programming"]{Literate Programming} via testing
@(for/first ([mkfile (in-list (list (collection-file-path "makefile.rkt" (getenv "digimon-gnome"))
                                    (build-path (getenv "digimon-world") "makefile.rkt")))]
             #:when (file-exists? mkfile))
   @hyperlink[mkfile]{makefile.rkt}) itself
that the entire project should follow since the @italic{Behavior Driven Development} plays an important role in my personal software development process.

@italic{Translating is the most complex human action in the Universe.}

The @italic{Behavior Driven Development} doesn@literal{'}t have any formal requirements for exactly how to write it down,
and the suggested specifications are always written as the @italic{ubiquitous language}s, trying to avoid the communication breakdowns
between Developers and Bussiness Stakeholders. While I@literal{'}m an indenpendent developer and communicating costs almost nothing.
So I make decision at my risk to write behavior specification and test suites apart from the production code with the @italic{Literate Programming} skill
so that the @italic{handbook} can also plays a role of the test reporter as well as the design documents.

@italic{@bold{Principal} This sample should be (rather than must be) followed due to the complexity of the real world problems.
         In fact it@literal{'}s all right to forget it after reading.}

@lp-include{makefile.rkt}
