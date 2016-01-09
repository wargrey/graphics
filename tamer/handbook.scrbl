#lang scribble/manual

@require{tamer.rkt}

@handbook-title[]

@itemlist[#:style 'compact
          @item{@italic{Translating is the most complex human activity in the Universe.}}
          @item{@italic{Discipline is the act of intentionally restricting your design choices
            so that you can work more productively at a higher level of abstraction.}}]

@tamer-smart-summary[]
@handbook-smart-table[]

@include-section[(submod "readme.rkt" doc)]
@include-section[(submod "ssh.rkt" doc)]
@include-section[(submod "schema.rkt" doc)]

@handbook-appendix[#:index? #true]
