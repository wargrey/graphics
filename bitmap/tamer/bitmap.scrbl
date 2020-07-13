#lang scribble/manual

@(require digimon/tamer)

@(define the-name (racketmodname bitmap))

@handbook-title/pkg-desc[]

@defmodule*/no-declare[(bitmap/base bitmap)]

The @the-name is a typed functional picture library which employs @cite{Cairo} and @cite{Pango} via
@racketmodname[ffi/unsafe] directly. The initial motivation is to provide efficient, professional,
yet handy graphics APIs that suitable to work with my @racketmodname[css] engine as well as to be
used standalone for developers and artists.

@emph{WARNING: This library is still experimental and everything is subject to change.}

@emph{WARNING: To keep compatible with other Racket picture libraries is not the goal.}

@;tamer-smart-summary[]

@handbook-smart-table[]

@include-section{font.scrbl}

@handbook-appendix[#:index? #true
 (url-bib-entry 'Cairo
                "Cairo: A Vector Graphics Library"
                "https://www.cairographics.org"
                #:author (authors "Keith Packard" "Carl Worth" "Behdad Esfahbod"))
 (url-bib-entry 'Pango
                "Pango: Internationalized Text Layout and Rendering"
                "https://pango.gnome.org"
                #:author (authors "Owen Taylor" "Raph Levien" "Behdad Esfahbod"))]
