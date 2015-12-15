#lang scribble/lp2

@(require syntax/location)
@(require "tamer.rkt")

@(require (for-label "tamer.rkt"))

@(define tamer-diagram-zone (make-tamer-zone (path->digimon-modpath (quote-source-file) 'diagram)))
@(define tamer-schema-zone (make-tamer-zone (path->digimon-modpath (quote-source-file) 'schema)))

@(define ORM2 (make-bib #:title    "'ORM 2', On the Move to Meaningful Internet Systems"
                        #:author   (author-name "Terry" "Halpin")
                        #:date     2005
                        #:location (dissertation-location #:institution "Neumont University")
                        #:url      "http://www.orm.net/pdf/ORM2.pdf"))

@handbook-story{Information Modeling and Visualization}

This module is designed for information modeling in a way like
@deftech{@hyperlink["https://en.wikipedia.org/wiki/datalog"]{datalog}} and
@deftech{@hyperlink["https://en.wikipedia.org/wiki/Graphviz"]{graphviz}},
but it is not @itech{datalog} and @itech{graphviz} after all.

Generally speaking, you can make schema by writing declarative code,
after that you get all the definitions that can be used in @bold{Racket}
and their diagrams that can be rendered in @bold{Scribble}. No duplicate
work, nor other software engineering tools.

@tamer-smart-summary[]

@chunk[|<diagram taming start>|
       (require "tamer.rkt")
       (require "../digitama/schema.rkt")

       <schema:*>]

@handbook-scenario{Schema Language}

This @itech{tamer}

@handbook-scenario{Conceptual Schema}

All graphical notations at conceptual level in this @itech{handbook} are introduced by @~cite[ORM2].
For the simplicity, the old-school style is no longer taken into account.

@chunk[|<conceptual schema>|
       (module:schema Activity)]

@interaction[#:eval tamer-diagram-zone
             (code:comment @#,t{All text is in 12 pixels.})
              object-role-diagram]

@handbook-bibliography[]

@chunk[<schema:*>
       |<conceptual schema>|]
