#lang scribble/manual

@(require "tamer.rkt")

@handbook-title/pkg-desc[]

@defmodule*/no-declare[(bitmap/base bitmap)]

@margin-note{The @the-name is influenced by @racketmodname[pict] and @racketmodname[images/flomap].}

The @the-name is a typed functional picture library which wraps @cite{Cairo} and @cite{Pango} directly.
The initial motivation is to provide high performance, professional, yet handy graphics APIs that
suitable to work with my @racketmodname[css] engine as well as to be used standalone by developers
and artists.

@emph{WARNING: This library is still experimental and everything is subject to change.}

@emph{WARNING: To keep compatible with other Racket picture libraries is not the goal.}

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
