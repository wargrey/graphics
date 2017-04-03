#lang typed/racket

(require "../main.rkt")
(require "../sugar.rkt")
(require "configure.rkt")

(current-css-element-color 'Silver)
(css-configure-@media)

(define-predicate pen-style? Pen-Style)

(define-preference* box #:as Box.CSS
  ([width : Nonnegative-Flonum             #:= (css-vw)]
   [height : Nonnegative-Flonum            #:= (css-vh)]
   [vertical-margin : Nonnegative-Flonum   #:= 4.0]
   [horizontal-margin : Nonnegative-Flonum #:= 4.0]
   [vertical-inset : Nonnegative-Flonum    #:= 0.0]
   [horizontal-inset : Nonnegative-Flonum  #:= 0.0]
   [icon-height : Nonnegative-Flonum       #:= (real->double-flonum (default-icon-height))]
   [font : Font                            #:= (current-css-element-font)]
   [color : Color                          #:= 'currentcolor]
   [border-color : Color                   #:= 'currentcolor]
   [background-color : Color               #:= 'transparent]
   [border-style : Pen-Style               #:= 'transparent])
  #:transparent)

(define box-parsers : CSS-Declaration-Parsers
  ;;; NOTE
  ;; This is simple box, it only sets vertical/horizontal margins and paddings.
  ;; but to keep the css file portable, no new properties are introduced.
  (lambda [suitcased-name deprecated!]
    (or (css-font-parsers suitcased-name deprecated!)
        (css-color-property-parsers suitcased-name null)
        (case suitcased-name
          [(width height padding-top margin-top padding-right margin-right) (CSS<^> (<css-size>))]
          [(margin) (make-css-pair-parser (<css-size>) 'margin-top 'margin-right)]
          [(padding) (make-css-pair-parser (<css-size>) 'padding-top 'padding-right)]
          [(border-style) (CSS<^> (<css:ident> pen-style?))]
          [(icon-height) (CSS<^> (<css-line-height>))]
          [else #false]))))

(define box-filter : (CSS-Cascaded-Value-Filter Box.CSS)
  (lambda [declared-values inherited-values]
    (call-with-css-box declared-values inherited-values 1.4
      #:with [css-box-size css-box-size-ref css-box-color-ref css-box-icon-ref css-box-ref]
      (define-values (width height) (css-box-size (initial-box-width) (initial-box-height)))
      (make-box #:width width #:height height
                #:vertical-inset (css-box-size-ref 'padding-top height)
                #:horizontal-inset (css-box-size-ref 'padding-right width)
                #:vertical-margin (css-box-size-ref 'margin-top height)
                #:horizontal-margin (css-box-size-ref 'margin-right width)
                #:background-color (css-box-color-ref 'background-color)
                #:border-color (css-box-color-ref 'border-color)
                #:border-style (css-box-ref 'border-style pen-style? css:initial)))))

(define box.css : CSS-StyleSheet (read-css-stylesheet tamer/box.css))
(define ~root:n : CSS-Subject (make-css-subject #::classes '(root)))
(define ~root:s : CSS-Subject (make-css-subject #::classes '(selected)))
(define-values ($root:n *root:n) (css-cascade (list box.css) (list ~root:n) box-parsers box-filter #false))
(define-values ($root:s *root:s) (css-cascade (list box.css) (list ~root:s) box-parsers box-filter #false))

(default-css-media-preferences)
(bitmap-hb-append #:gapsize (+ (box-vertical-margin $root:n) (box-vertical-margin $root:s))
                  (bitmap-frame #:color (current-css-element-color) #:style 'dot
                                (bitmap-frame #:margin (box-vertical-margin $root:n) #:inset (box-vertical-inset $root:n)
                                              #:color (box-border-color $root:n) #:style (box-border-style $root:n)
                                              #:background-color (box-background-color $root:n) #:background-style 'solid
                                              (bitmap-desc #:color (box-color $root:n)
                                                           (pretty-format $root:n) (box-width $root:n)
                                                           (box-font $root:n))))
                  (bitmap-frame #:color (current-css-element-color) #:style 'dot
                                (bitmap-frame #:margin (box-vertical-margin $root:s) #:inset (box-vertical-inset $root:s)
                                              #:color (box-border-color $root:s) #:style (box-border-style $root:s)
                                              #:background-color (box-background-color $root:s) #:background-style 'solid
                                              (bitmap-desc #:color (box-color $root:s)
                                                           (pretty-format $root:s) (box-width $root:s)
                                                           (box-font $root:s)))))
