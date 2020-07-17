#lang scribble/manual

@(require digimon/tamer)

@(require "../digitama/font.rkt")
@(require "bridge/font.rkt")

@(require "../font.rkt")
@(require "../constructor.rkt")
@(require "../composite.rkt")

@(require (for-label typed/racket/base))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define-url-bib css:font
   "CSS Font Module Level 3" "https://drafts.csswg.org/css-fonts-3"
   #:date 2018
   #:author (org-author-name "W3C"))

@(define-url-bib css:values
   "CSS Values and Units Module Level 4" "https://drafts.csswg.org/css-values-4/#font-relative-lengths"
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
@handbook-typed-module-story[bitmap/font]{Font}

A @deftech{font} provides a resource containing the visual representation of characters. At the
simplest level it contains information that maps character codes to shapes (called @deftech{glyphs})
that represent these characters. @tech{Fonts} sharing a common design style are commonly grouped
into @tech{font} @deftech{families} classified by a set of standard @tech{font} properties. Within a family,
the shape displayed for a given character can vary by stroke weight, slant or relative width,
among others. An individual font face is described by a unique combination of these properties.

The @tech{font} and its properties described in this section follow the @~cite[css:font]. Nonetheless,
if properties are not supported by the backend renderer, they are accepted by APIs but will not be
displayed as expected.

@;tamer-smart-summary[]

@handbook-scenario{Descriptor and Properties}

@deftogether[(@defidform[Font-Style] @defidform[Font-Stretch] @defidform[Font-Weight])]{
 The basic @tech{font} properties that represented as predefined keywords.
 Possible values are as follows:

 @itemlist[@item{@racket[Font-Style]: @tamer-keywords[@css-font-style-options].}
           @item{@racket[Font-Stretch]: @tamer-keywords[@css-font-stretch-options].} 
           @item{@racket[Font-Weight] is one of @tamer-keywords[@css-font-weight-options].}]
}

@tamer-defstruct[font #: Font ([face String]
                               [size Nonnegative-Flonum]
                               [weight Font-Weight]
                               [style Font-Style]
                               [stretch Font-Stretch])
                 [#:transparent]]{
 A structure type for the @tech{font} which instance is associated with type name @racket[Font].
 This is a read-only structure, and to make a @tech{font} instance, use @racket[desc-font] instead
 since its arguments accept a much more wider range of values.
}

@defproc[(desc-font [basefont Font (default-font)]
                    [#:family family (U String Symbol (Listof (U String Symbol)) False) null]
                    [#:size size (U Symbol Real False) +nan.0]
                    [#:style style (Option Symbol) #false]
                    [#:weight weight (U Symbol Integer False) #false]
                    [#:stretch stretch (Option Symbol) #false])
         Font]{
 Functionally create a new @racket[Font] instance by independently updating properties of the
 @racket[basefont]. The default @racket[basefont] is controlled by the parameter @racket[default-font].

 @itemlist[@item{@racket[family] argument accepts value of any type of

             @itemlist[@item{@racket[Symbol]: One of the predefined keywords in
                                     @tamer-indexed-keywords[@css-font-generic-families]. Otherwise,
                                     inherited from @racket[basefont].
                                     
                                     These predefined families are independent of platforms and devices,
                                     and the used family is the mapped face name or @tech{family} name.}
                       
                       @item{@racket[String]: The used family is the font name or @tech{family} name
                                     if @racket[family] represents a valid @cite{Pango} font description
                                     and the target is available. Otherwise, inherited from @racket[basefont].}

                       @item{@racket[list?]: The used family is based on the first valid one, either a
                                     predefined keyword or a @cite{Pango} font description. Otherwise,
                                     inherited from @racket[basefont].}

                       @item{Otherwise, inherited from @racket[basefont].}]}

           @item{@racket[size] argument accepts value of any type of

             @itemlist[@item{@racket[Symbol]: One of the predefined keywords in
                                     @tamer-indexed-keywords[@css-font-size-options]. Otherwise,
                                     inherited from @racket[basefont].
                                     
                                     For @racket['smaller] and @racket['larger], the used size is
                                     relative to the size of @racket[basefont], while for other
                                     keywords, the used size is always relative to the size of
                                     @racket[default-font].}
                       
                       @item{@racket[Real]: Nonnegative @racket[size]s are interpreted as absolute
                                     values, and the used size is exactly the same as @racket[size];
                                     Negative values are interpreted as percentages which value is
                                     the additive inverse of @racket[size], and the used size is
                                     relative to the size of @racket[basefont]. Otherwise, inherited
                                     from @racket[basefont].}

                       @item{Otherwise, inherited from @racket[basefont].}]}

           @item{@racket[style] argument accepts one of the predefined keywords in
             @tamer-indexed-keywords[@css-font-style-options]. Otherwise, inherited
             from @racket[basefont].}

           @item{@racket[weight] argument accepts value of any type of

             @itemlist[@item{@racket[Symbol]: One of the predefined keywords in
                                     @tamer-make-keywords[font-weigth-element
                                                          (append css-font-weight-options
                                                                  (list 'bolder 'lighter))].
                                     Otherwise, inherited from @racket[basefont].

                                    For @racket['smaller] and @racket['larger], the used weight is
                                     relative to the weight of @racket[basefont].}
                        
                       @item{@racket[Integer]: The used weight is the predefined keyword which
                                     numerical representation is the nearest one from @racket[weight].}
                       
                       @item{Otherwise, inherited from @racket[basefont].}]}
           
           @item{@racket[stretch] argument accepts one of the predefined keywords in
             @tamer-indexed-keywords[@css-font-stretch-options]. Otherwise, inherited
             from @racket[basefont].}]
}

@defparam[default-font font Font]{
 A parameter that defines the default @racket[font] instance. It is intended to be set at the time
 the application starts. The default is @racket['sans-serif #,(font-size (default-font))px].
}

@defproc*[([(list-font-faces) (Listof String)]
           [(list-font-families) (Listof String)])]{
 Get the list of @tech{font} face names and @tech{family} names available on the current platform.
}

@defproc*[([(list-monospace-font-faces) (Listof String)]
           [(list-monospace-font-families) (Listof String)])]{
 Get the list of monospace @tech{font} face names and @tech{family} names available on the current platform.
}

@handbook-scenario{Typographic Metrics}

The @deftech{metrics} correspond to the font-relative @deftech{units} defined in @~cite[css:values].

@defproc[(font-metrics [font Font]
                       [units (Listof Symbol) null])
         (Listof (Pairof Symbol Nonnegative-Flonum))]{
 Returns an association list of font @tech{metrics} of @racket[font]. If @racket[units] is @racket[null],
 then all @tech{metrics} are returned, otherwise, only specified @tech{metrics} are returned. Possible
 values in @racket[units] are @tamer-indexed-keywords[(map car (font-metrics (default-font)))].

 Note that, the values of @racket['lh], @racket['rem], and @racket['rlh] are useless out of the
 @racketmodname[css] environment. The root element is even undefined outside. So just do not rely
 on them.

 @tamer-action[(font-metrics (default-font) '(em ex cap ch ic))]
}

@defproc[(font-metrics-ref [font Font]
                           [unit Symbol])
         Nonnegative-Flonum]{
 Returns the specified font @tech{metric} of @racket[font]. Possible value of @racket[unit] is one
 of: @tamer-indexed-keywords[(map car (font-metrics (default-font))) ", " ", or "]. Otherwise,
 @racket[+nan.0] is returned.
}

@defproc[(text-size [text String]
                    [font Font (default-font)])
         (Values Nonnegative-Flonum Nonnegative-Flonum)]{
 Returns the width and height of the @racket[text] as it would be rendered with @racket[font].

 @tamer-action[(text-size "Sphinx")]
}

@defproc[(text-size* [text String]
                     [font Font (default-font)])
         (Values Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum)]{
 Returns the width, height, distance, ascent, and descent of the @racket[text] as it would be
 rendered with @racket[font]. Among them, the ascent and the descent are exemplified in the
 following figure, while the distance is computed downward from baseline.
 
@centered{
 @image[(build-path (digimon-path 'tamer) "stone" "Sphinx.svg")]{Common Typographic Metrics}
  
 Common Typographic Metrics}

 @tamer-action[(text-size* "Sphinx")]
}

@defproc[(text-metrics-lines [text String]
                             [font Font (default-font)])
         (Values Flonum Flonum Flonum Flonum Flonum)]{
 Returns the 5 common typographic lines of the @racket[text] as it would be
 rendered with @racket[font]. These lines are identified by their vertical
 positions computed downward from top.
 
 @tamer-action[(require bitmap/constructor)
               (require bitmap/constants)
               (define exfont (desc-font (desc-font #:family 'cursive #:size 'xx-large) #:size -3.14))
               
               (text-metrics-lines "Sphinx" exfont)
               (bitmap-text #:ascent magenta #:descent blue #:capline orange #:meanline green #:baseline red
                            "Sphinx " exfont)]
}

@handbook-scenario{A Glimpse of Properties}

@(define exfont (desc-font #:size 'x-large #:family 'system-ui))
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

This section is trivial since font properties are much more than they do should be supported.
Here exemplifies the properties of @racket['system] font: 

@centered{@font-style-table[(list "a" "N") css-font-stretch-options
                            (λ [f p s] (desc-font f #:stretch p #:style s))
                            (font-size exfont)]}

@centered{@font-style-table[(list "A" "永") css-font-weight-options
                            (λ [f p s] (desc-font f #:weight p #:style s))
                            (font-size exfont)]}

@handbook-reference[#:auto-hide? #true]
