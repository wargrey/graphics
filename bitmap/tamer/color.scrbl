#lang scribble/manual

@(require digimon/tamer)

@(require "../digitama/color.rkt")

@(require "../color.rkt")

@(require (for-label typed/racket/base))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define-url-bib css:color
   "CSS Color Module Level 4" "https://drafts.csswg.org/css-color"
   #:date 2020
   #:author (org-author-name "W3C"))

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

@handbook-scenario{Color Representation and the RGB Colorspace}

@deftogether[(@defidform[Color] @defidform[FlColor])]{
 @racket[Color] is the general type for any @tech{color} datum that can be represented as any type
 of @racket[FlColor], @racket[Symbol], or @racket[Integer], where @racket[FlColor] is the abstract
 type of all concrete @tech{colorspace} types as well as special @tech{color} structures.
}

@tamer-defstruct[rgba #: FlRGBA ([red Flonum] [green Flonum] [blue Flonum] [alpha Flonum]) [#:transparent]]{
 A structure type for RGB @tech{color} which instance is associated with type name @racket[FlRGBA]. As of
 @racket[alpha], all 3 components of the @racket[FlRGBA] have range of @racketparenfont{[}@racket[0.0]@racketparenfont{,}
 @racket[1.0]@racketparenfont{]} as well.

 Since RGB @tech{colorspace} is the most common one supported by electronic systems, and for the sake
 of performance, the @racket[FlRGBA] are expected to be used as the low-level representation of @tech{color}s.
 Thus, this structure is only documented for read-only use, and to make an RGB @tech{color} instance, use
 @racket[rgb] or @racket[rgb*] instead since they also serve as the guard procedures to ensure the correctness
 of fields.
}

@defproc[(rgb [red Real] [green Real] [blue Real] [alpha Real 1.0]) FlRGBA]{
 The safer constructor of @racket[FlRGBA]. For all fields, the used value is the minorant if the corresponding
 input @racket[Real] datum is less than the minorant of the range, and is the majorant if the corresponding 
 input @racket[Real] datum is greater than the majorant of the range; Otherwise, the used value is the
 @racket[Flonum] datum coerced from the @racket[Real] datum. 
}

@defproc[(rgb* [c Color] [alpha Real 1.0]) FlRGBA]{
 Convert any representation of @tech{color} @racket[c] into an instance of @racket[FlRGBA] with a specific
 @racket[alpha] value. The used alpha is the @racket[alpha] multiplied by the alpha of the source @tech{color}
 @racket[c]. For types of @racket[c]:

 @itemlist[
 @item{@racket[FlColor]: the used RGB @tech{color} is transformed from the corresponding @tech{colorspace}
   mathematically, or looked up from the predefined tables.}
 @item{@racket[Symbol]: the used RGB @tech{color} is looked up from the table of @tech{named color}s if
  @racket[c] is a registered name.}
 @item{@racket[Integer]: the used RGB @tech{color} is decoded by treating @racket[c] as the 6-digit hexadecimal
   color notation. Note that negative @racket[c]s are also acceptable.}]

 If failed, it will start over again by replacing @racket[c] with the fallback color
 @racketvalfont[(string-append "#x" (~r fallback-color #:min-width 6 #:pad-string "0"))].
}

@defproc[(flcolor->hex [c Color]) Index]{
 Convert any representation of @tech{color} @racket[c] into its numerical representation which is suitable to
 be used as the 6-digit hexadecimal color notation in RGB @tech{colorspace}.
}

@deftogether[(@defproc[(flcolor->byte-list [c Color]) (List Byte Byte Byte)]
               @defproc[(flcolor->uint16-list [c Color]) (List Index Index Index)])]{
 Convert any representation of @tech{color} @racket[c] into its triplet representation,
 @racket[(list red green blue)], in RGB @tech{colorspace}. Human eyes may favor uint8-based representation,
 while some renderers may favor uint16-based representation.
}

@handbook-scenario{Named Colors}

@handbook-reference[#:auto-hide? #true]
