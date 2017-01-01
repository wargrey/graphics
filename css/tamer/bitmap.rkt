#lang typed/racket

(provide (all-defined-out))

(require "configure.rkt")
(require "../main.rkt")

(css-configure-@media)
(current-namespace (module->namespace 'bitmap))

(define-preference* btest #:as Bitmap-TestCase #:with ([color-properties Color])
  ([symbol-color : Color                       #:= 'Blue]
   [string-color : Color                       #:= 'Orange]
   [number-color : Color                       #:= 'Tomato]
   [output-color : Color                       #:= 'Chocolate]
   [paren-color : Color                        #:= 'Firebrick]
   [border-color : Color                       #:= 'Crimson]
   [foreground-color : Color                   #:= "Grey"]
   [background-color : Color                   #:= "Snow"]
   [font : Font                                #:= (default-css-font)]
   [width : Index                              #:= 512]
   [combine? : Boolean                         #:= #false]
   [desc : String                              #:= "['desc' property is required]"]
   [prelude : Bitmap                           #:= (bitmap-text "> ")]
   [descriptors : (HashTable Symbol CSS-Datum) #:= (make-hasheq)])
  #:transparent)

(define css-descriptor-filter : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (font-parsers suitcased-name deprecated!)
        (css-color-property-parsers suitcased-name btest-color-properties)
        (css-image-property-parsers suitcased-name)
        (case suitcased-name
          [(desc) (CSS<*> (CSS<^> (<css:string>)) '+)]
          [(count width) (CSS<^> (<css-natural>))]
          [(combine) (CSS<^> (<css-keyword> '(combine none)))]
          [(at-exp) (CSS<^> (list (<css:λracket>) (<css:racket>) (<css:block>)))]
          [(prelude) (CSS<^> (<css-image>))]))))

(define css-preference-filter : (CSS-Cascaded-Value-Filter Bitmap-TestCase)
  (lambda [declared-values inherited-values]
    (define (css->desc [_ : Symbol] [value : CSS-Datum]) : (U String CSS-Wide-Keyword)
      (cond [(string? value) value]
            [(list? value) (string-join (map (λ [v] (~a v)) value))]
            [else css:initial]))
    (parameterize ([current-css-element-color (css-color-ref declared-values inherited-values)])
      (make-btest #:symbol-color (css-color-ref declared-values inherited-values 'symbol-color)
                  #:string-color (css-color-ref declared-values inherited-values 'string-color)
                  #:number-color (css-color-ref declared-values inherited-values 'number-color)
                  #:output-color (css-color-ref declared-values inherited-values 'output-color)
                  #:paren-color (css-color-ref declared-values inherited-values 'paren-color)
                  #:border-color (css-color-ref declared-values inherited-values 'border-color)
                  #:foreground-color (css-color-ref declared-values inherited-values 'foreground-color)
                  #:background-color (css-color-ref declared-values inherited-values 'background-color)
                  #:font (css-extract-font declared-values inherited-values)
                  #:width (css-ref declared-values inherited-values 'width index? css:initial)
                  #:combine? (eq? 'normal (css-ref declared-values inherited-values 'font-variant-ligatures symbol? 'normal))
                  #:desc (css-ref declared-values inherited-values 'desc css->desc)
                  #:prelude (css-ref declared-values inherited-values 'prelude css->bitmap)
                  #:descriptors (for/hash : (HashTable Symbol CSS-Datum) ([(k fv) (in-css-values declared-values)])
                                  (values k (fv)))))))

(css-root-element-type 'module)

(define tamer-sheet : CSS-StyleSheet (read-css-stylesheet tamer/bitmap.css))
(define tamer-main : CSS-Subject (make-css-subject #:type 'module #:classes '(main)))

(define :values : CSS-Values (make-css-values))
(define :root : Bitmap-TestCase
  (time-run 'tamer-main
            (let-values ([(toplevel topvalues)
                          (css-cascade (list tamer-sheet) (list tamer-main) css-descriptor-filter css-preference-filter #false)])
              (set! :values topvalues)
              toplevel)))

(define-values (bitmap-descs testcases)
  (for/fold ([bitmap-descs : (Listof Bitmap) null]
             [testcases : (Listof Bitmap-TestCase) null])
            ([i (in-range (css-ref :values #false 'count index? 8))])
    (define tamer-test : CSS-Subject (make-css-subject #:type 'test #:id (string->keyword (~a 'case i))))
    (define-values (tobj _)
      (css-cascade (list tamer-sheet) (list tamer-test tamer-main)
                   css-descriptor-filter css-preference-filter :values))

    (define-values (fgcolor bgcolor rcolor bdcolor)
      (values (btest-foreground-color tobj) (btest-background-color tobj)
              (btest-output-color tobj) (btest-border-color tobj)))

    (define-values (width words font combine?) (values (btest-width tobj) (btest-desc tobj) (btest-font tobj) (btest-combine? tobj)))
    (define desc (bitmap-desc words width font #:color fgcolor #:background-color bgcolor #:combine? combine?))
    (define-values (desc-width height) (bitmap-size desc))
      
    (values (append bitmap-descs
                    (list (bitmap-pin 1 1/2 0 1/2
                                      (btest-prelude tobj)
                                      (bitmap-text "(" #:color (btest-paren-color tobj))
                                      (bitmap-hc-append #:gapsize 7
                                                        (bitmap-text (~a "bitmap-case" i) #:color (btest-symbol-color tobj))
                                                        (bitmap-text (~s words) #:color (btest-string-color tobj))
                                                        (bitmap-text (~a width) #:color (btest-number-color tobj)))
                                      (bitmap-text ")" #:color (btest-paren-color tobj)))
                          (bitmap-text #:color rcolor
                                       (cond [(not combine?) (format "- : (Bitmap ~a ~a)" desc-width height)]
                                             [else (format "- : (Bitmap ~a ~a #:combined)" desc-width height)]))
                          (bitmap-lt-superimpose (bitmap-frame desc #:style 'transparent) ; align by frame border
                                                 (bitmap-frame (bitmap-blank width height) #:color bdcolor))))
            (cons tobj testcases))))

(when DrRacket?
  (values :root
          (apply bitmap-vl-append bitmap-descs)
          length%))
