#lang scribble/base

@(require "../digitama/digicore.rkt")
@(require (submod "../digitama/tamer.rkt" digitama))

@(require racket/list)
@(require racket/path)

@(define githublink
   {lambda [target]
     (hyperlink target (path->string (file-name-from-path target)))})

@title{@(hyperlink (~url (current-digimon)) (format "~a<sub>~a</sub>" house-garden# cat#))Digital World}
How to construct a @italic{Digital World}? Okay, we don@literal{'}t start with the @italic{File Island}, but we own some concepts from the
@hyperlink["http://en.wikipedia.org/wiki/Digimon"]{Digital Monsters}.

@section{Living Digimons}
@(itemlist #:style 'compact
           (filter-not void? (for/list ([digimon (in-list (directory-list (digimon-world) #:build? #false))])
                               (define homepage (if (string=? (path->string digimon) (digimon-gnome)) (compose1 bold hyperlink) hyperlink))
                               (define info-ref (get-info/full (build-path (digimon-world) digimon))) 
                               (when (procedure? info-ref)
                                 (item (homepage (~url (path->string digimon)) (format "~a: ~a" (info-ref 'collection) (info-ref 'pkg-desc))))))))

@section{Project Conventions}

Since I@literal{'}m an indenpendent developer and communicating costs almost nothing, I make decision at my risk to
model software system in @italic{@hyperlink["http://en.wikipedia.org/wiki/Formal_methods"]{Formal Methods}} and
document it with the @italic{@hyperlink["http://en.wikipedia.org/wiki/Literate_programming"]{Literate Programming}} approach.

@subsection{Building Scripts}
@(itemlist #:style 'compact
           @item{@bold{@githublink[@(format "/~a/prerequisites.sh" (digimon-gnome))]}: Build the latest Racket and Swi-Prolog from offical source.}
           @item{@bold{@githublink[@(format "/~a/makefile.rkt" (digimon-gnome))]}: As its name shows, it is the replacement of Makefile, and it@literal{'}s the toplevel one.}
           @item{@bold{submake.rkt}: As its name shows, it is the sub makefile that may exist in every @italic{digimon} directory.})

@subsection{Hierarchy}
For the sake of consistency and better architecture, we follow the concept of
@italic{@hyperlink["http://docs.racket-lang.org/pkg/Package_Concepts.html#%28tech._multi._collection._package%29"]{Racket Multi-collection Package}}.
Projects/subcollections listed in the root directory are organized as the @italic{digimons}, and each of them may be separated into several repositories.

@(itemlist #:style 'compact
           @nested{@bold{@githublink{/DigiGnome}} is the reserved @italic{digimon} who@literal{'}s duties are making life easy for developers and
                    sharing code base for other @italic{digimons}. Namely @italic{digimon} works like @tt{src} @bold{and} @tt{libraries}/@tt{frameworks}.
                    @(itemlist #:style 'compact
                               @item{@bold{stone} stores immutable meta-information or ancient sources to be translated. Yes, it@literal{'}s the @italic{Rosetta Stone}.}
                               @item{@bold{digitama} is the egg of @italic{digimons}.
                                      Namely sources within it are @bold{hidden} to others. @bold{Compatibility will not be maintained and Use at your own risk!}}
                               @item{@bold{digivice} is the interface for users to talk with @italic{digimons}.
                                      Namely sources within it implement executable tools that will be called by wrappers from @tt{bin}.}
                               @item{@bold{tamer} is the interface for developers to tame the @italic{digimons}. Namely it works like @tt{test}.
                                      After all the @italic{tamer/handbook.scrbl} plays the role of
                                      the @hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development"]{Test Report} along with
                                      the @hyperlink["http://en.wikipedia.org/wiki/Design_by_contract"]{Design Documentation}
                                      so that the production code could keep elegant.}
                               @item{@bold{terminus} manages guilds of @italic{digimons}. Hmm... Sounds weird, nonetheless, try @tt{htdocs}.})})

@subsection{Version}
Obviousely, our @italic{digimons} have their own life cycle.
@margin-note{The @bold{Baby I} and @bold{Baby II} are merged as the @bold{Baby} to make life simple.}

@(itemlist #:style 'compact
           @item{@bold{Baby}: The 1st stage of @italic{digimon evolution} hatching straightly from her @italic{digitama}. Namely it@literal{'}s the @tt{Alpha Version}.}
           @item{@bold{Child}: The 2nd stage of @italic{digimon evolution} evolving quickly from @bold{Baby}. Namely it@literal{'}s the @tt{Beta Version}.}
           @item{@bold{Adult}: The 3rd stage of @italic{digimon evolution} evolving from @bold{Child}. At this stage @italic{digimons} are strong enough to live on their own.})
