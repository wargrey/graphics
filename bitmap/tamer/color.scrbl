#lang scribble/manual

@(require digimon/tamer)

@(require "../digitama/font.rkt")
@(require "bridge/font.rkt")

@(require "../font.rkt")
@(require "../constructor.rkt")
@(require "../composite.rkt")

@(require (for-label typed/racket/base))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define-url-bib css:color
   "CSS Color Module Level 4" "https://drafts.csswg.org/css-color"
   #:date 2020
   #:author (org-author-name "W3C"))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define (font-weigth-element weight)
   (define indexed-elem (tamer-indexed-keyword-element weight))
   (cond [(or (eq? weight 'bolder) (eq? weight 'lighter)) indexed-elem]
         [else (list indexed-elem
                     (racketparenfont "(")
                     (racketoutput (number->string (css-font-weight->integer weight)))
                     (racketparenfont ")"))]))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-typed-module-story[bitmap/color]{Color and Colorspace}

A @deftech{color} is a definition of the human visual perception of a light or a physical object
illuminated with light. The objective study of human color perception is termed colorimetry. If two
objects have different spectra, but produce the same physical sensation, we say they have the same
@tech{color}.

A @deftech{colorspace} is an organization of @tech{color}s with respect to an underlying colorimetric
model, such that there is a clear, objectively-measurable meaning for any color in that @tech{colorspace}.
This also means that the same @tech{color} can be expressed in multiple @tech{colorspace}s, or transformed
from one @tech{colorspace} to another, while looking the same.

Real physical devices cannot yet produce every possible @tech{color} that the human eye can see. The
range of @tech{color}s that a given device can produce is termed the @deftech{gamut}. Devices with a
limited @tech{gamut} cannot produce very saturated @tech{color}s, like those found in a rainbow.

More about @tech{color} and @tech{colorspace} can be found in @~cite[css:color]. This section is
expected to hide all those complexities.

@;tamer-smart-summary[]

@handbook-scenario{Color Representations}

@deftogether[(@defidform[Color] @defidform[FlColor])]{
 @racket[Color] is the general type for any @tech{color} datum that can be represented as any type of:

  @itemlist[
 @item{@racket[Symbol]: the names of @tech{named color}s.}
 @item{@racket[FlColor]: the general type of all concrete @tech{colorspace} types.}
 @item{@racket[Integer]: the 6-digit hexadecimal @tech{color} notation for @tech{sRGB} @tech{colorspace}.
        
   As they are usually mapped to 3 consecutive bytes in the memory, negative datum is also acceptable.}]
}

@handbook-scenario{Named Colors}

@handbook-reference[#:auto-hide? #true]
