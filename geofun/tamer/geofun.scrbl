#lang scribble/manual

@(require digimon/tamer)
@(require geofun/version)

@(define the-name (racketmodname geofun))

@handbook-title/pkg-desc[]

@defmodule*/no-declare[(geofun/vector)]

The @the-name is a typed functional 2d graphics library
that employs @cite{Cairo} and @cite{Pango} via @racketmodname[ffi/unsafe] directly.
The initial motivation is to provide efficient, professional,
yet handy tools for generating technical graphics for writing papers and textbooks.

@emph{WARNING: This library is still experimental and everything is subject to change.}

@emph{WARNING: To keep compatible with other Racket picture libraries is not the goal.}

@emph{NOTE: Rendering the resulting image in DrRacket for the first time might take minutes.}

@;tamer-smart-summary[]

@handbook-smart-table[]

@include-section{font.scrbl}
@include-section{color.scrbl}

@include-section{misc.scrbl}

@handbook-bonus-appendix[#:index-section? #true
 (url-bib-entry 'Cairo
                "Cairo: A Vector Graphics Library"
                "https://www.cairographics.org"
                #:author (authors "Keith Packard" "Carl Worth" "Behdad Esfahbod")
                #:note (format "[~a]" (cairo-version-string)))
 (url-bib-entry 'Pango
                "Pango: Internationalized Text Layout and Rendering"
                "https://pango.gnome.org"
                #:author (authors "Owen Taylor" "Raph Levien" "Behdad Esfahbod")
                #:note (format "[~a]" (pango-version-string)))]
