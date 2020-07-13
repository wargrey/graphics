#lang scribble/manual

@(require digimon/tamer)

@(require "../digitama/font.rkt")

@(require "../font.rkt")
@(require "../constructor.rkt")
@(require "../composite.rkt")

@(require (for-label typed/racket/base))

@(define-url-bib cssfont
   "CSS Font Module Level 3" "https://drafts.csswg.org/css-fonts-3"
   #:date 2018
   #:author (org-author-name "W3C"))

@(define exfont (desc-font #:size 'x-large))
@(define exgap (bitmap-blank))

@(define (font-style-table ex-chars options update-font hgapsize)
   (bitmap-table*
    (for/list ([property (in-list (cons '|| options))])
      (cons (bitmap-text (~a property) exfont #:color 'green)
            (apply append
                   (for/list ([exchar (in-list ex-chars)])
                     (cons exgap
                           (for/list ([style (in-list css-font-style-options)])
                             (cond [(eq? property '||) (bitmap-text (~a style) exfont)]
                                   [else (bitmap-text (~a " " exchar " ")
                                                      (update-font exfont property style))])))))))
    'cc 'cc hgapsize (* hgapsize 0.618)))

@handbook-typed-module-story[bitmap/font]{Font}

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
 The basic @tech{font} properties that represented by symbols. Unfortunately, @cite{Pango}
 still does not support "unusual" values.

 @centered{@font-style-table[(list "a" "N") css-font-stretch-options
                             (λ [f p s] (desc-font f #:stretch p #:style s))
                             (font-size exfont)]}


 @centered{@font-style-table[(list "A" "永") css-font-weight-options
                             (λ [f p s] (desc-font f #:weight p #:style s))
                             (font-size exfont)]}
}

@defStruct[font #: Font ([face String]
                         [size Nonnegative-Flonum]
                         [weight Font-Weight]
                         [style Font-Style]
                         [stretch Font-Stretch])
           [#:transparent]]{
 A structure type for the @tech{font} which instance is associated with type name @racket[Font].
 This is a read-only structure, and to make a @tech{font} instance, use @racket[desc-font] instead
 since its arguments accept a much more wider range of value types.
}

@defproc[(desc-font [basefont Font (default-font)]
                    [#:family family (U String Symbol (Listof (U String Symbol)) False) null]
                    [#:size size (U Symbol Real False) +nan.0]
                    [#:style style (Option Symbol) #false]
                    [#:weight weight (U Symbol Integer False) #false]
                    [#:stretch stretch (Option Symbol) #false])
         Font]{
 Functionally create a new @racket[Font] instance by updating properties of the @racket[basefont].
}

@handbook-reference[#:auto-hide? #true]
