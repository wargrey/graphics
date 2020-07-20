#lang scribble/manual

@(require digimon/tamer)

@(require "composite/table/hue.rkt")

@(require "../digitama/color.rkt")

@(require "../color.rkt")
@(require "../font.rkt")

@(require (for-label typed/racket/base))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define-url-bib css:color
   "CSS Color Module Level 4" "https://drafts.csswg.org/css-color"
   #:date 2020
   #:author (org-author-name "W3C"))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-typed-module-story[bitmap/color]{Color, Color Model, and Colorspace}

A @deftech{color} is a definition of the human visual perception of a light or a physical object
illuminated with light. The objective study of human color perception is termed colorimetry. If two
objects have different spectra, but produce the same physical sensation, we say they have the same
@tech{color}.

A @deftech{color model} is an abstract mathematical model describing the way colors can be represented
as tuples of numbers, typically as three or four values or color components. 

A @deftech{colorspace} is an organization of @tech{color}s with respect to an underlying @tech{color model},
such that there is a clear, objectively-measurable meaning for any color in that @tech{colorspace}. This
also means that the same @tech{color} can be expressed in multiple @tech{colorspace}s, or transformed
from one @tech{colorspace} to another, while looking the same.

More about @tech{color} and @tech{colorspace} can be found in @~cite[css:color]. This chapter is
expected to hide all those complexities.

@;tamer-smart-summary[]

@handbook-scenario{Color Representation and the RGB Color Model}

@deftogether[(@defidform[Color] @defidform[FlColor])]{
 @racket[Color] is the general type for any @tech{color} datum that can be represented as any type
 of @racket[FlColor], @racket[Symbol], or @racket[Integer], where @racket[FlColor] is the abstract
 type of all concrete @tech{color model} types as well as special @tech{color} structures.
}

@tamer-defstruct[rgba #: FlRGBA ([red Flonum] [green Flonum] [blue Flonum] [alpha Flonum]) [#:transparent]]{
 A structure type for RGB @tech{color model} whose instance is associated with type name @racket[FlRGBA]. As of
 @racket[alpha], all 3 components of the @racket[FlRGBA] have range of @racketparenfont{[}@racket[0.0]@racketparenfont{,}
 @racket[1.0]@racketparenfont{]} as well.

 Since RGB @tech{color model} is the most common one supported by electronic systems, and for the sake
 of performance, the @racket[FlRGBA] are expected to be used as the low-level representation of @tech{color}s.
 Thus, this structure is only documented for read-only use, and to make an RGB @tech{color} instance, @racket[rgb]
 or @racket[rgb*] should be used instead since they also serve as the guard procedures to ensure the correctness
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
 @item{@racket[FlColor]: the used RGB @tech{color} is transformed from the corresponding @tech{color model}
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
 be used as the 6-digit hexadecimal color notation in RGB @tech{color model}.
}

@deftogether[(@defproc[(flcolor->byte-list [c Color]) (List Byte Byte Byte)]
               @defproc[(flcolor->uint16-list [c Color]) (List Index Index Index)])]{
 Convert any representation of @tech{color} @racket[c] into its triplet representation,
 @racket[(list red green blue)], in RGB @tech{color model}. Human eyes may favor uint8-based representation,
 while some renderers may favor uint16-based representation.
}

@handbook-scenario{Hue-based Color Models}

The RGB @tech{color model} is convenient for machines and graphic libraries, but very difficult for humans to
gain an intuitive grasp on. Say, it’s not easy to tell how to alter an RGB @tech{color} to produce a lighter
variant of the same hue. The Hue-based color models therefore are invented, they are much more intuitive to
use, but still map easily back to RGB @tech{color}s.

For all four hue-based color models, @deftech{HSL}, @deftech{HSV}, @deftech{HSI}, and @deftech{HWB}, defined
in this section, the range of hue is @racketparenfont{[}@racket[0.0]@racketparenfont{,}
 @racket[360.0]@racketparenfont{)} or @racket[+nan.0], the range of other components are
@racketparenfont{[}@racket[0.0]@racketparenfont{,} @racket[1.0]@racketparenfont{]}.

The @tech{HWB} @tech{color model} is even easier for humen to work with.

@(define font (desc-font #:weight 'bold))
@(define-values (hsb-cwidth hwb-cwidth cheigth) (values 28.0 28.0 16.0))
@(define hsb-hs% (list 1.0 0.75 0.50 0.25 0.00))
@(define hsb-vs% (list 1.0 0.88 0.75 0.63 0.50 0.38 0.25 0.13 0.00))
@(define hwb-s% (list 0.0 0.20 0.40 0.60 0.80 1.00))

@deftogether[(@tamer-defstruct[hsla #: FlHSLA ([hue Flonum] [saturation Flonum] [luminosity Flonum] [alpha Flonum]) [#:transparent]]
               @defproc[(hsl [hue Real] [saturation Real] [luminosity Real] [alpha Real 1.0]) FlHSLA]
               @defproc[(hsl* [c Color] [alpha Real 1.0]) FlHSLA])]{
 A structure type for @tech{HSL} @tech{color model} whose instance is associated with type name @racket[FlHSLA].

 Similar to @racket[rgb] and @racket[rgb*] for RGB @tech{color}s, @racket[hsl] and @racket[hsl*] are the recommanded
 constructors to make @tech{HSL} @tech{color} instance.

 @hue-colors*[hsl color-hues hsb-hs% hsb-vs% #:cell-width hsb-cwidth #:cell-height cheigth #:rotate? #false]
}

@deftogether[(@tamer-defstruct[hsva #: FlHSVA ([hue Flonum] [saturation Flonum] [value Flonum] [alpha Flonum]) [#:transparent]]
               @defproc[(hsv [hue Real] [saturation Real] [value Real] [alpha Real 1.0]) FlHSVA]
               @defproc[(hsv* [c Color] [alpha Real 1.0]) FlHSVA])]{
 A structure type for @tech{HSV} @tech{color model} whose instance is associated with type name @racket[FlHSVA].

 Similar to @racket[rgb] and @racket[rgb*] for RGB @tech{color}s, @racket[hsv] and @racket[hsv*] are the recommanded
 constructors to make @tech{HSV} @tech{color} instance.
 
 @hue-colors*[hsv color-hues hsb-hs% hsb-vs% #:cell-width hsb-cwidth #:cell-height cheigth #:rotate? #false]
}

@deftogether[(@tamer-defstruct[hsia #: FlHSIA ([hue Flonum] [saturation Flonum] [intensity Flonum] [alpha Flonum]) [#:transparent]]
               @defproc[(hsi [hue Real] [saturation Real] [intensity Real] [alpha Real 1.0]) FlHSIA]
               @defproc[(hsi* [c Color] [alpha Real 1.0]) FlHSIA])]{
 A structure type for @tech{HSI} @tech{color model} whose instance is associated with type name @racket[FlHSIA].

 Similar to @racket[rgb] and @racket[rgb*] for RGB @tech{color}s, @racket[hsi] and @racket[hsi*] are the recommanded
 constructors to make @tech{HSI} @tech{color} instance.
 
 @hue-colors*[hsi color-hues hsb-hs% hsb-vs% #:cell-width hsb-cwidth #:cell-height cheigth #:rotate? #false]
}

@deftogether[(@tamer-defstruct[hwba #: FlHWBA ([hue Flonum] [white Flonum] [black Flonum] [alpha Flonum]) [#:transparent]]
               @defproc[(hwb [hue Real] [white Real] [black Real] [alpha Real 1.0]) FlHWBA]
               @defproc[(hwb* [c Color] [alpha Real 1.0]) FlHWBA])]{
 A structure type for @tech{HWB} @tech{color model} whose instance is associated with type name @racket[FlHWBA].

 Similar to @racket[rgb] and @racket[rgb*] for RGB @tech{color}s, @racket[hwb] and @racket[hwb*] are the recommanded
 constructors to make @tech{HWB} @tech{color} instance.

 @hue-colors*[hwb color-hues hwb-s% hwb-s% #:cell-width hwb-cwidth #:cell-height cheigth #:rotate? #true]
}

@handbook-scenario{Named Colors}

@handbook-reference[#:auto-hide? #true]
