#lang scribble/manual

@(require digimon/tamer)

@(require "../digitama/font.rkt")

@(require (for-label typed/racket/base))

@(define-url-bib cssfont
   "CSS Font Module Level 3" "https://drafts.csswg.org/css-fonts-3"
   #:date 2018
   #:author (org-author-name "W3C"))

@handbook-typed-module-story[#:requires (bitmap/digitama/font) bitmap/font]{Font}

A @deftech{font} provides a resource containing the visual representation of characters. At the
simplest level it contains information that maps character codes to shapes (called @deftech{glyphs})
that represent these characters. @tech{Fonts} sharing a common design style are commonly grouped
into @tech{font} families classified by a set of standard @tech{font} properties. Within a family,
the shape displayed for a given character can vary by stroke weight, slant or relative width,
among others. An individual font face is described by a unique combination of these properties.

The @tech{font} and its properties described in this section follow the @~cite[cssfont]. Nonetheless,
if properties are not supported by the backend renderer, they are accepted by APIs but will not be
displayed as expected.

@;tamer-smart-summary[]

@handbook-scenario{properties and descriptor}

@deftogether[(@defidform[Font-Weight] @defidform[Font-Style] @defidform[Font-Stretch])]{
 The basic @tech{font} properties that represented by symbols.
}

@tamer-action[css-font-stretch-options
              css-font-style-options
              css-font-weight-options]

@deftogether[(@defstruct*[font ([face String] [size Nonnegative-Flonum] [weight Font-Weight] [style Font-Style] [stretch Font-Stretch]) #:transparent]
               @defidform[Font])]{
 A structure type for the @tech{font}.
}

@defproc[(desc-font [source Font]) Font]{
  Functionally create a new @racket[Font] instance by updating properties of the source @racket[font].
}

@handbook-reference[#:auto-hide? #true]
