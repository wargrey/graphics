#lang scribble/base

@(require "scribble.rkt")

@title{Project Conventions}
How to build a @italic{Digital World}? Okay, we don@literal{'}t start with the @italic{File Island}, but we own some concepts from the @italic{Digimon Series}.

@section{Hierarchy}
@bold{Note} Project or Subprojects are organized as the @italic{digimons} within @bold{village}. 
Each project may be separated into several repositories within @bold{island}, @bold{tamer}, and so on. 

@(itemlist @item{@bold{village} is the birth place of @italic{digimons}. Namely it works like @tt{src}.}
           @item{@bold{digitama} is the egg of @italic{digimons}. Namely it works like @tt{libraries} or @tt{frameworks}.}
           @item{@bold{digivice} is the interface for users to talk with @italic{digimons}. Namely it works like @tt{bin}.}
           @item{@bold{tamer} is the interface for developers to train the @italic{digimons}. Namely it works like @tt{test}.}
           @item{@bold{island} is the living environment of @italic{digimons}. Namely it works like @tt{share} or @tt{collection}.}
           @item{>  >@bold{@italic{zone}} manages @bold{subislands}. Thus they share following conventions either.}
           @item{>  >@bold{@italic{guild}} manages societies of @italic{digimons} and @italic{human beings}. Hmm... Sounds weird, nonetheless, try @tt{htdocs} or @tt{webroot}:stuck_out_tongue_winking_eye:.}
           @item{>  >@bold{@italic{board}} stores the configuration instructions that a @italic{digimon} should follow. Sounds like @tt{etc}.}
           @item{>  >@bold{@italic{stone}} stores the immutable meta-information or ancient sources to be translated. Yes, it@literal{'} the @italic{Rosetta Stone}.})

@section{Version}
Obviousely, our @italic{digimons} have their own life cycle.

@(let ([smart-stage (curry smart-radiobox (car (regexp-match #px"[^-]+" (info-ref 'version))))])
   @(itemlist @smart-stage["Baby I"]{The 1st stage of @italic{digimon evolution} hatching straighty from her @italic{digitama}. Namely it@literal{'}s the @tt{Alpha Version}.}
              @smart-stage["Baby II"]{The 2nd stage of @italic{digimon evolution} evolving quickly from @bold{Baby I}. Namely it@literal{'}s the @tt{Beta Version}.}
              @smart-stage["Child"]{The 3rd stage of @italic{digimon evolution} evolving from @bold{Baby II}. At the time @italic{digimons} are strong enough to live on their own.}))
