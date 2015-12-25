#lang scribble/lp2

@(require syntax/location)
@(require "tamer.rkt")

@(require (for-label "tamer.rkt"))
@(require (for-label "../digitama/schema.rkt"))
@(require (for-label (submod "../digitama/schema.rkt" diagram)))

@(define tamer-diagram-zone (make-tamer-zone (tamer-partner->modpath "digitama/schema.rkt" 'diagram)))
@(define tamer-schema-zone (make-tamer-zone (path->digimon-modpath (quote-source-file) 'tamer)))

@(define ORM  (make-bib #:title "Object Role Modeling: an overview"
                        #:author   (author-name "Terry" "Halpin")
                        #:date     2001
                        #:location "Microsoft Corporation"
                        #:url      "http://www.orm.net/pdf/ORMwhitePaper.pdf"))

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
       (module tamer typed/racket
         (require "../digitama/schema.rkt")
         <schema:*>)]

@handbook-scenario{Conceptual Schema}

@deftech{Conceptual Schema} plays a critical role on designing high quality information system, and
I prefer @~cite[ORM] at first place to help ensure correctness, clarity, adaptability and productivity.

All graphical notations at conceptual level in this @itech{handbook} are introduced by @~cite[ORM2].
For the simplicity, the old-school style is no longer taken into account.

@chunk[|<conceptual schema>|
       (define-schema schema
         (define-table press as 出版社信息
           ([name       : String                 % 藏语名称]
            [name/zh    : (Option String)        % 汉语名称]
            [name/en    : (Option String)        % 英语名称]
            [address    : String                 % 出版社地址]
            [url        : String                 % 出版社网址]
            [profile    : String                 % 出版社简介])))]

@interaction[#:eval tamer-diagram-zone
             (code:comment @#,t{All text is in 12 pixels.})]

@interaction[#:eval tamer-schema-zone
             press]

@handbook-bibliography[]

@chunk[<schema:*>
       |<conceptual schema>|]
