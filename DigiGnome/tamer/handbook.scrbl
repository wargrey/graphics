#lang scribble/manual

@(require scribble/lp-include)

@(require setup/getinfo)

@require{tamer.rkt}

@(define info-ref (get-info/full (digimon-zone)))

@title[#:version (format "~a[~a]" (version) (info-ref 'version))]{@bold{Tamer@literal{'}s Handbook}}

@margin-note{@italic{Translating is the most complex human activity in the Universe.}}

This @italic{handbook} shows my
@italic{@hyperlink["https://github.com/digital-world/DigiGnome"]{Programming Methodology}}
that the entire project should follow.

Hmm... nonetheless, in fact it@literal{'}s all right to forget this sample after reading
due to the complexity of the real world problems.

Good Luck!

@tamer-smart-summary[]
@handbook-smart-table[]

@lp-include{infrastructure.rkt}
