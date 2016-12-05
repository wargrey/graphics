#lang at-exp typed/racket

;;; https://drafts.csswg.org/css-fonts
;;; https://drafts.csswg.org/css-fonts-4

(provide (all-defined-out))

(require "bitmap.rkt")
(require "digicore.rkt")
(require "../recognizer.rkt")
(require "../racket.rkt")

(require (for-syntax syntax/parse))

(define-syntax (call-with-font stx)
  (syntax-parse stx
    [(_ font (~optional (~seq #:root? ?root?)) sexp ...)
     (with-syntax ([root? (or (attribute ?root?) #'#false)])
       ; NOTE: This operation is extremely expensive (at least 0.4s), but Racket do have its own cache strategy.
       #'(begin (unless (eq? font (unbox &font))
                  ; WARNING: 'xh' seems to be impractical, the font% size is just a nominal size
                  ;            and usually smaller than the generated text in which case the 'ex' is
                  ;            always surprisingly larger than the size, the '0w' therefore is used instead.                      
                  ;(define-values (xw xh xd xe) (send the-dc get-text-extent "x" font)]
                  (define-values (0w 0h 0d 0e) (send the-dc get-text-extent "0" font))
                  (define-values (ww wh wd we) (send the-dc get-text-extent "水" font))
                  (define em : Nonnegative-Flonum (smart-font-size font))
                  (when root? (set-flcss%-rem! length% em))
                  (set-flcss%-em! length% em)
                  (set-flcss%-ex! length% (real->double-flonum 0w))
                  (set-flcss%-ch! length% (real->double-flonum 0w))
                  (set-flcss%-ic! length% (real->double-flonum ww))
                  (set-box! &font font))
                sexp ...))]))

(define css-font-family-names/no-variants : (Listof String) (get-face-list 'all #:all-variants? #false))
(define &font : (Boxof Font) (box (default-css-font)))

(define css-font-generic-families : (Listof Symbol)
  '(default decorative roman script  swiss      modern    system    symbol
     emoji  fantasy    serif cursive sans-serif monospace system-ui math fangsong))
  
(define css-font-synthesis-options : (Listof Symbol) '(weight style small-caps))
  
(define css-font-style-option : (Listof Symbol) '(normal italic oblique slant))
(define css-font-kerning-option : (Listof Symbol) '(auto normal none))
(define css-font-variant-ligatures-options : (Listof Symbol) '(normal none))
(define css-font-position-option : (Listof Symbol) '(normal sub super))
(define css-font-caps-option : (Listof Symbol) '(normal small-caps all-small-caps petite-caps all-petite-caps unicase titling-caps))
(define css-font-weight-option : (Listof Symbol) '(normal bold bolder light lighter))
(define css-font-size-option : (Listof Symbol) '(xx-small x-small small medium large x-large xx-large smaller larger))
(define css-font-variant-options/21 : (Listof Symbol) '(normal small-caps))
(define css-font-stretch-option : (Listof Symbol) '(normal condensed expanded ultra-condensed extra-condensed semi-condensed
                                                           ultra-expanded extra-expanded semi-expanded))
  
(define-css-system-parameters-filter <css-system-font> #:-> Font
  [caption       (default-css-font)]
  [icon          (default-css-font)]
  [menu          (default-css-font)]
  [message-box   (default-css-font)]
  [small-caption (default-css-font)]
  [status-bar    (default-css-font)])
  
(define-css-racket-value-filter <racket-font> #:? font%? #:as Font)

(define css-font->longhand-properties : (->* (Font) (CSS-Longhand-Values) CSS-Longhand-Values)
  (lambda [font [longhand css-longhand]]
    (let* ([longhand++ (hash-set longhand 'font-weight (send font get-weight))]
           [longhand++ (hash-set longhand++ 'font-style (send font get-style))]
           [longhand++ (hash-set longhand++ 'font-size (smart-font-size font))])
      (hash-set longhand++ 'font-family
                (list (or (send font get-face)
                          (send font get-family)))))))

(define-css-disjoined-filter <font-stretch> #:-> (U Symbol Nonnegative-Single-Flonum)
  ;;; https://drafts.csswg.org/css-fonts-4/#font-stretch-prop
  (<css-keyword> css-font-stretch-option)
  (<css:percentage> nonnegative-single-flonum?))

(define-css-disjoined-filter <font-weight> #:-> (U Symbol Integer)
  ;;; https://drafts.csswg.org/css-fonts/#font-weight-prop
  (<css-keyword> css-font-weight-option)
  (<css:integer> 0 < 1000))
  
(define-css-disjoined-filter <font-size> #:-> (U Symbol Nonnegative-Inexact-Real CSS:Length:Font)
  ;;; https://drafts.csswg.org/css-fonts/#font-size-prop
  (<css+length>)
  (CSS:<~> (<css+%real>) exact->inexact)
  (<css-keyword> css-font-size-option))

(define-css-disjoined-filter <line-height> #:-> (U Symbol Nonnegative-Flonum CSS:Length:Font CSS-Wide-Keyword)
  ;;; http://www.w3.org/TR/CSS2/visudet.html#propdef-line-height
  (CSS:<~> (<css+%real>) real->double-flonum)
  (<css+length>)
  (CSS:<~> (<css-keyword> '(normal inherit)) css-wide-keywords-filter-map))

(define <:font-family:> : (CSS-Parser (Listof CSS-Datum))
  ;;; https://drafts.csswg.org/css-fonts/#font-family-prop
  ;;; https://drafts.csswg.org/css-fonts-4/#extended-generics
  (CSS<#> (CSS<+> (CSS<^> (CSS:<+> (<css:string>) (<css-keyword> css-font-generic-families)))
                  (CSS<!> (CSS<^> (<css:ident>))))))
  
(define <:font-shorthand:> : (Pairof CSS-Shorthand-Parser (Listof Symbol))
  ;;; https://drafts.csswg.org/css-fonts/#font-prop
  (cons (CSS<+> (CSS<^> (<css-system-font>) css-font->longhand-properties)
                (CSS<^> (<racket-font>) css-font->longhand-properties)
                (CSS<&> (CSS<*> (CSS<+> (CSS<_> (CSS<^> (<css-keyword> 'normal) '|Ignoring, some properties use it as defaults|))
                                        (CSS<^> (<font-weight>) 'font-weight)
                                        (CSS<^> (<css-keyword> css-font-style-option) 'font-style)
                                        (CSS<^> (<css-keyword> css-font-variant-options/21) 'font-variant)
                                        ; <font-stretch> also accepts percentage in css-fonts-4 specification,
                                        ; however it preempts the 'font-size
                                        (CSS<^> (<css-keyword> css-font-stretch-option) 'font-stretch)))
                        (CSS<^> (<font-size>) 'font-size)
                        (CSS<*> (CSS<?> [(<css-slash>) (CSS<^> (<line-height>) 'line-height)]) '?)
                        (CSS<^> <:font-family:> '(font-family))))
        '(font-style font-variant font-weight font-stretch font-size line-height font-family
                     font-size-adjust font-kerning font-language-override)))

(define css->font-family : (-> Symbol CSS-Datum (U String Font-Family))
  (lambda [_ value]
    (define (generic-family-map [family : Symbol]) : Font-Family
      (case family
        [(decorative fantasy) 'decorative]
        [(roman serif) 'roman]
        [(script cursive) 'script]
        [(swiss sans-serif) 'swiss]
        [(modern monospace) 'modern]
        [(system system-ui) 'system]
        [(symbol math) 'symbol]
        [else 'default]))
    (define (face-filter [family : String]) : (Option String)
      (for/or : (Option String) ([face (in-list css-font-family-names/no-variants)])
        (and (string-ci=? family face) family)))
    (let select ([families (if (list? value) value (list value))])
      (cond [(null? families) (let ([pfont (unbox &font)]) (or (send pfont get-face) (send pfont get-family)))]
            [else (let ([family (car families)])
                    (or (and (symbol? family) (generic-family-map family))
                        (and (string? family) (face-filter family))
                        (and (list? family) (face-filter (string-trim (~a family) #px"(^[(])|([)]$)")))
                        (select (cdr families))))]))))

(define css->font-size : (-> Symbol CSS-Datum Nonnegative-Real)
  (lambda [property value]
    (cond [(symbol? value)
           (let ([css-font-medium (smart-font-size (default-css-font))])
             (case value
               [(xx-large) (* 2/1 css-font-medium)]
               [(x-large)  (* 3/2 css-font-medium)]
               [(large)    (* 6/5 css-font-medium)]
               [(small)    (* 8/9 css-font-medium)]
               [(x-small)  (* 3/4 css-font-medium)]
               [(xx-small) (* 3/5 css-font-medium)]
               [(smaller)  (* 5/6 (flcss%-em length%))] ; TODO: find a better function to deal with these two keywords.
               [(larger)   (* 6/5 (flcss%-em length%))] ; http://style.cleverchimp.com/font_size_intervals/altintervals.html#bbs
               [else       css-font-medium]))]
          [(nonnegative-flonum? value) value]
          [(nonnegative-single-flonum? value) (fl* (real->double-flonum value) (flcss%-em length%))]
          [(css+length? value) (css:length->scalar value #false)]
          [(eq? property 'min-font-size) 0.0]
          [(eq? property 'max-font-size) 1024.0]
          [else +nan.0 #| used to determine whether size-in-pixels? will be inherited, see (css-extract-font) |#])))

(define css->font-weight : (-> Symbol CSS-Datum Font-Weight)
  (lambda [_ value]
    (cond [(symbol? value)
           (case value
             [(normal light bold) value]
             [(bolder) (if (eq? (send (unbox &font) get-weight) 'light) 'normal 'bold)]
             [(lighter) (if (eq? (send (unbox &font) get-weight) 'bold) 'normal 'light)]
             [else (send (default-css-font) get-weight)])]
          [(and (fixnum? value) (fx<= value 300)) 'light]
          [(and (fixnum? value) (fx>= value 700)) 'bold]
          [else (send (default-css-font) get-weight)])))

(define css->font-style : (-> Symbol CSS-Datum Font-Style)
  (lambda [_ value]
    (case value
      [(normal italic) value]
      [(oblique slant) 'slant]
      [else (send (default-css-font) get-style)])))
