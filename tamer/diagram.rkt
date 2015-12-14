#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-label "tamer.rkt"))

@handbook-story{Data Modeling and Visualization}

This module is designed for data modeling in a way like
@deftech{@hyperlink["https://en.wikipedia.org/wiki/datalog"]{datalog}} and
@deftech{@hyperlink["https://en.wikipedia.org/wiki/Graphviz"]{graphviz}},
but it is not @itech{datalog} and @itech{graphviz} after all.

Generally speaking, you can make data model by writing declarative code,
after that you get all the definitions that can be used in @bold{Racket}
and their diagrams that can be rendered in @bold{Scribble}.
No duplicate work, and no other software engineering tools are required either. 
   
@tamer-smart-summary[]

@chunk[|<diagram taming start>|
       (require "tamer.rkt")
       (tamer-taming-start)

       (module tamer typed/racket
         (require (submod "tamer.rkt" typed))
         (require "../digitama/diagram.rkt")
         
         |<diagram:*>|)]

@handbook-scenario{Conceptual Data Model}

@handbook-appendix{}

@chunk[|<diagram:*>|
       (module+ story
         )]
