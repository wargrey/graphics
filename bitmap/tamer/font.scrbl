#lang scribble/manual

@(require digimon/tamer)

@(require "../digitama/font.rkt")
@(require "bridge/font.rkt")

@(require "../font.rkt")
@(require "../constructor.rkt")
@(require "../composite.rkt")

@(require (for-label typed/racket/base))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define-url-bib cssfont
   "CSS Font Module Level 3" "https://drafts.csswg.org/css-fonts-3"
   #:date 2018
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

The @tech{font} and its properties described in this section follow the @~cite[cssfont]. Nonetheless,
if properties are not supported by the backend renderer, they are accepted by APIs but will not be
displayed as expected.

@;tamer-smart-summary[]

@handbook-scenario{Descriptor and Properties}

@deftogether[(@defidform[Font-Style] @defidform[Font-Stretch] @defidform[Font-Weight])]{
 The basic @tech{font} properties that represented as predefined keywords. Possible values are:

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

@handbook-scenario{Metrics}

@handbook-scenario{Glimpse}

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
