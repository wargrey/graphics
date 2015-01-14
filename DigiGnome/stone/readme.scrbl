#lang scribble/base

@(require racket/function)

@(require "markdown.rkt")

@title{Project Conventions}
How to build a @italic{Digital World}? Okay, we don@literal{'}t start with the @italic{File Island}, but we own some concepts from the
@hyperlink["http://en.wikipedia.org/wiki/Digimon"]{Digital Monsters}.

@section{Building Scripts}
@(itemlist #:style 'compact
           @item{[@bold{prerequisites.sh}](@(getenv "digimon-gnome")/prerequisites.sh): Build the latest Racket and Swi-Prolog from offical source.}
           @item{[@bold{makefile.rkt}](@(getenv "digimon-gnome")/makefile.rkt): As its name shows, it is the replacement of Makefile, and it@literal{'}s the toplevel one.}
           @item{[@bold{submake.rkt}](@(getenv "digimon-gnome")/submake.rkt): As its name shows, it is the sub makefile that should exist in every @italic{digimon} directory.})

@section{Hierarchy}
For the sake of consistency and better architecture, we follow the concept of
@hyperlink["http://docs.racket-lang.org/pkg/Package_Concepts.html#%28tech._multi._collection._package%29"]{Racket Multi-collection Package}.
Projects/subcollections listed in the root directory are organized as the @italic{digimons}, and each of them may be separated into several repositories.

@(itemlist #:style 'compact
           @item{@bold{DigiGnome} is the reserved @italic{digimon} who@literal{'}s duty is making things easy for developers.}
           @nested{@bold{digicore} is also reserved as the @italic{kernel} (rather than @italic{digimon}) shared by all @italic{digimons}.
                    Namely it works like @tt{src} @bold{and} @tt{libraries}/@tt{frameworks}, just the same as @italic{digimons}.
                    @(itemlist #:style 'compact
                               @item{@bold{stone} stores immutable meta-information or ancient sources to be translated. Yes, it@literal{'}s the @italic{Rosetta Stone}.}
                               @item{@bold{digitama} is the egg of @italic{digimons}.
                                      Namely sources within it are @bold{hidden} to others. @bold{Compatibility will not be maintained and Use at your own risk!}}
                               @item{@bold{digivice} is the interface for users to talk with @italic{digimons}.
                                      Namely sources within it implement executable tools that will be called by wrappers from @tt{bin}.}
                               @item{@bold{tamer} is the interface for developers to train the @italic{digimons}. Namely it works like @tt{test} that follows
                                      the principles of @hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development"]{Behavior Driven Development}.}
                               @item{@bold{terminus} manages guilds of @italic{digimons}. Hmm... Sounds weird, nonetheless, try @tt{htdocs} or @tt{webroot}.})})

@section{Version}
Obviousely, our @italic{digimons} have their own life cycle.
@margin-note{The @bold{Baby I} and @bold{Baby II} are merged as the @bold{Baby} to make life simple.}

@(let ([smart-stage (curry smart-radiobox (car (regexp-match #px"[^-]+" (info-ref 'version))))])
   @(itemlist #:style 'compact
              @smart-stage["Baby"]{The 1st stage of @italic{digimon evolution} hatching straightly from her @italic{digitama}. Namely it@literal{'}s the @tt{Alpha Version}.}
              @smart-stage["Child"]{The 2nd stage of @italic{digimon evolution} evolving quickly from @bold{Baby}. Namely it@literal{'}s the @tt{Beta Version}.}
              @smart-stage["Adult"]{The 3rd stage of @italic{digimon evolution} evolving from @bold{Child}. At this stage @italic{digimons} are strong enough to live on their own.}))
