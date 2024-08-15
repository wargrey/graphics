#lang scribble/manual

@(require digimon/tamer)

@(require geofun/font)
@(require geofun/digitama/font)

@(require geofun/tamer/composite/table/font)

@(require "../constructor.rkt")
@(require "../composite.rkt")

@(require "bitmap/bridge/font.rkt")

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
@handbook-typed-module-story[#:requires [bitmap/base geofun/digitama/font] geofun/font]{Font}

A @deftech{font} provides a resource containing the visual representation of characters. At the
simplest level it contains information that maps character codes to shapes (called @deftech{glyphs})
that represent these characters. @tech{Fonts} sharing a common design style are commonly grouped
into font @deftech{families} classified by a set of standard @tech{font} properties. Within
a family, the shape displayed for a given character can vary by stroke weight, slant or relative
width, among others. An individual font face is described by a unique combination of these properties.

The @tech{font} and its properties that described in this chapter follow the @~cite[css:font].
Nonetheless, if properties are not supported by the backend renderer, they are accepted by APIs
but will not be displayed as expected.

@;tamer-smart-summary[]

@handbook-scenario{Descriptor and Properties}

@deftogether[(@defidform[Font-Style]
               @defidform[Font-Weight]
               @defidform[Font-Stretch]
               @defidform[Font-Variant])]{
 The basic @tech{font} properties that represented as predefined keywords.
 Possible values are:

 @itemlist[@item{@racket[Font-Style]: @tamer-keywords[@css-font-style-options ", " ", or "].}
           @item{@racket[Font-Weight]: @tamer-keywords[@css-font-weight-options ", " ", or "].}
           @item{@racket[Font-Stretch]: @tamer-keywords[@css-font-stretch-options ", " ", or "].}
           @item{@racket[Font-Variant]: @tamer-keywords[@css-font-variant-options ", " ", or "].}]
}

@tamer-defstruct[font #: Font ([face String]
                               [size Nonnegative-Flonum]
                               [weight Font-Weight]
                               [style Font-Style]
                               [stretch Font-Stretch]
                               [variant Font-Variant])
                 [#:transparent]]{
 A structure type for @tech{font} whose instance is associated with type name @racket[Font].
 This is a read-only structure, and to make a @tech{font} instance, use @racket[desc-font] instead
 since its arguments accept a much more wider range of values.
}

@defproc[(desc-font [basefont Font (default-font)]
                    [#:family family (U String Symbol (Listof (U String Symbol)) False) null]
                    [#:size size (U Symbol Real False) +nan.0]
                    [#:style style (Option Symbol) #false]
                    [#:weight weight (U Symbol Integer False) #false]
                    [#:stretch stretch (Option Symbol) #false]
                    [#:variant variant (Option Symbol) #false])
         Font]{
 Functionally create a new @racket[Font] instance by independently updating properties of the
 @racket[basefont]. The default @racket[basefont] is controlled by the parameter @racket[default-font].

 @itemlist[@item{@racket[family] argument accepts datum of any type of

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

           @item{@racket[size] argument accepts datum of any type of

             @itemlist[@item{@racket[Symbol]: One of the predefined keywords in
                                     @tamer-indexed-keywords[@css-font-size-options]. Otherwise,
                                     inherited from @racket[basefont].
                                     
                                     For @racket['smaller] and @racket['larger], the used size is
                                     relative to the size of @racket[basefont], while for other
                                     keywords, the used size is always relative to the size of
                                     @racket[default-font].}
                       
                       @item{@racket[Real]: Nonnegative @racket[size]s are interpreted as absolute
                                     values, and the used size is exactly the same as @racket[size];
                                     Negative @racket[size]s are interpreted as percentages which value
                                     is the additive inverse of @racket[size], and the used size is
                                     relative to the size of @racket[basefont]. Otherwise, inherited
                                     from @racket[basefont].}

                       @item{Otherwise, inherited from @racket[basefont].}]}

           @item{@racket[style] argument accepts one of the predefined keywords in
             @tamer-indexed-keywords[@css-font-style-options]. Otherwise, inherited
             from @racket[basefont].}

           @item{@racket[weight] argument accepts datum of any type of

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
             from @racket[basefont].}
           
           @item{@racket[variant] argument accepts one of the predefined keywords in
             @tamer-indexed-keywords[@css-font-variant-options]. Otherwise, inherited
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

@defproc[(find-font-families [pred? (U (-> String Boolean Boolean) Symbol) 'all]) (Listof String)]{
 Get the list of @tech{family} names using the predicate @racket[pred?] on the current platform. The
 @racket[pred?] argument accepts datum of any type of:

 @itemlist[
 @item{@racket[Procedure] that accepts two arguments and returns a @racket[Boolean] value, where
   the first argument is the @tech{family} name, while the second argument indicates if this is a
   monospace font @tech{family}.

   A font @tech{family} is found when @racket[pred?] applied to the family name returns a true value.}

 @item{@racket[Symbol] of predefined @racket[Procedure]s in @tamer-indexed-keywords['(mono all)],
   same as invoking @racket[list-monospace-font-families], and @racket[list-font-families],
   respectively. Otherwise, fall back to @racket[list-font-families].}
 ]
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

 @tamer-repl[(font-metrics (default-font) '(em ex cap ch ic))]
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
 Returns the @racket[width] and @racket[height] of the @racket[text] as it would be rendered
 with @racket[font].

 @tamer-repl[(text-size "Sphinx")]
}

@defproc[(text-size* [text String]
                     [font Font (default-font)])
         (Values Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum)]{
 Returns the @racket[width] and @racket[height] along with additional mertics of the @racket[text]
 as it would be rendered with @racket[font]. The extra metrics are:

 @itemlist[
 @item{@racket[distance]: measured downward from the @tech{baseline} to font bottom,
   included in @racket[height].}
  
 @item{@racket[ascent]: measured upward from the @tech{ascent line} to font top,
   included in @racket[height]. Do not to be confused with the @tech{ascender}.}

 @item{@racket[descent]: measured downward from the @tech{descent line} to font bottom,
   included in @racket[distance]. Do not to be confused with the @tech{descender}.}
 ]

 @margin-note{Be careful this figure is somehow misleading.}
 
 @centered{
  @image[(build-path (digimon-path 'tamer) "stone" "Sphinx.svg")]{Common Typographic Metrics}
   
  Common Typographic Metrics}
 
 @tamer-repl[(text-size* "Sphinx")]
}

@defproc[(text-metrics-lines [text String]
                             [font Font (default-font)])
         (Values Flonum Flonum Flonum Flonum Flonum)]{
 Returns the 5 common typographic lines of the @racket[text] as it would be rendered with @racket[font].
 These lines are identified by their vertical positions measured downward from font top. These lines are:

 @itemlist[
 @item{@deftech{ascent line}: an imaginary line below which all characters in @racket[text] extend, and
   the minimum distance between the line and the character is zero.}
  
 @item{@deftech{cap line}: an imaginary line below which all capital characters in @racket[font] extend,
   and the minimum distance between the line and the capital character is zero. @tt{O} is often used as the
   heuristic frame of reference.}
 
 @item{@deftech{mean line}: an imaginary line below which all flat lowercase characters in @racket[text]
   extend, and the minimum distance between the line and the flat lowercase character is zero. @tt{x} is
   often used as the heuristic frame of reference.}

 @item{@deftech{baseline}: an imaginary line where most characters in @racket[font] sit. The distance
   between the @tech{baseline} and the @tech{mean line} is called @deftech{x-height}.}
                                                
 @item{@deftech{descent line}: an imaginary line above which all characters in @racket[text] extend, and
   the minimum distance between the line and the character is zero.}
 ]
 
 @tamer-repl[(require bitmap/constructor)
             (require bitmap/composite)
             (define exfont (desc-font (desc-font #:family 'cursive #:size 'xx-large) #:size -1.618))
             (define sphinx "Sphinx 0123456789")
             
             (text-metrics-lines sphinx exfont)]
 
 @margin-note{It is normal to see these generic font families poor in @url{docs.racket-lang.org},
  and fonts in Linux might not be well selected.}
 
 @tamer-repl[(default-border (desc-stroke #:dash 'long-dash #:color 'gray #:width 1))
             (define (frame-text [family : (U String Symbol)] [text : String sphinx]) : Bitmap
               (bitmap-frame (bitmap-text #:ascent 'magenta #:descent 'blue #:capline 'orange #:meanline 'green #:baseline 'red
                                          (format "~a: ~a" family text)
                                          (desc-font #:family family exfont))))
             
             @(bitmap-vl-append* #:gapsize 16 (map frame-text css-font-generic-families))]
}

@handbook-scenario{Glyphs Utilities}

Selecting the right font might be quite a time-consuming task, especially in situations of
cross platform. These utilities are therefore at your service.

@defproc[(text-unknown-glyphs-count [text String] [font Font (default-font)]) Natural]{
 Returns the number of missing @tech{glyph}s of @racket[text] as it would be rendered with @racket[font].
}

@defproc[(text-glyphs-exist? [text String] [font Font (default-font)]) Boolean]{
 Determine if all characters in @racket[text] have @tech{glyph}s as it would be rendered with @racket[font].
}

@defproc*[([(text-ascender-exist? [text String] [font Font (default-font)] [#:overshoot-tolerance Real 1/8]) Boolean]
           [(text-descender-exist? [text String] [font Font (default-font)] [#:overshoot-tolerance Real 1/8]) Boolean])]{
 Determine if any character in @racket[text] has an @deftech{ascender} if its part extend above the @tech{mean line}
 of @racket[font], or has a @deftech{descender} if its part extend below the @tech{baseline} of @racket[font].

 @hyperlink["https://en.wikipedia.org/wiki/Overshoot_(typography)"]{Overshoot}s are often seen for many characters,
 especially for round or pointed ones (e.g. @tt{O}, @tt{A}), hence the extra argument @racket[#:overshoot-tolerance],
 which is defined as the ratio of the overshoot to the @tech{x-height}. That is, @tech{ascender} and @tech{descender}
 that less than (or equal to) @racket[(* x-height tolerance)] are considered as overshoots, and do not contribute to
 the results.
 
 @tamer-repl[(define a-z : (Listof String) (build-list 26 (λ [[i : Integer]] (string (integer->char (+ i 97))))))
             (filter text-ascender-exist? a-z)
             (filter text-descender-exist? a-z)]

 Say, now we want to find a suitable font for chess unicode characters.
 @margin-note{Do not be surprised if there is no suitable font in server @url{docs.racket-lang.org}.}

 @tamer-repl[(define chesses : String "♔♕♖♗♘♙♚♛♜♝♞♟︎")
             (define (font-okay? [family : String] [monospace? : Boolean]) : Boolean
               (define font (desc-font #:family family))
               (and (text-descender-exist? chesses font)
                    (text-glyphs-exist? chesses font)))
             
             (bitmap-vl-append*
              #:gapsize 16
              (map (λ [[face : String]] (frame-text face chesses))
                   (find-font-families font-okay?)))]
}

@handbook-scenario{A Glimpse of Properties}

@(define exfont (desc-font #:size 'large #:family 'fangsong))

@margin-note{This exemplification is trivial since font properties are much more
 than they do should be supported.}

Below exemplifies the properties of @racket['system-ui] font. 

@centered{@font-style-table[exfont
                            (list "a" "N") css-font-stretch-options
                            (λ [f p s] (desc-font f #:stretch p #:style s))]}

@centered{@font-style-table[exfont
                            (list "A" "永") css-font-weight-options
                            (λ [f p s] (desc-font f #:weight p #:style s))]}

@handbook-reference[#:auto-hide? #true]
