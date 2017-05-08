#lang typed/racket

(provide (all-defined-out))

(require "bitmap.css")

(require "configure.rkt")
(require "../main.rkt")
(require "../sugar.rkt")
(require "../bonus.rkt")

(css-configure-@media)
(current-namespace (module->namespace 'bitmap))
(default-css-font (make-css-font #:size 16.0))
(css-root-element-type 'module)

(define-preference* btest #:as Bitmap.CSS #:with ([color-properties Color])
  ([symbol-color : Color                       #:= 'Blue]
   [string-color : Color                       #:= 'Orange]
   [number-color : Color                       #:= 'Tomato]
   [output-color : Color                       #:= 'Chocolate]
   [paren-color : Color                        #:= 'Firebrick]
   [foreground-color : Color                   #:= "Grey"]
   [font : Font                                #:= (current-css-element-font)]
   [borders : (Listof Pen)                     #:= null]
   [brush : Brush                              #:= 'transparent]
   [max-width : Index                          #:= 512]
   [desc : String                              #:= "['desc' property is required]"]
   [prelude : Bitmap                           #:= (bitmap-text "> ")]
   [descriptors : (Listof (Pairof Symbol Any)) #:= null])
  #:transparent)

(define btest-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font+colors-parsers suitcased-name deprecated!)
        (css-color-property-parsers suitcased-name btest-color-properties)
        (css-image-property-parsers suitcased-name '(prelude))
        (css-simple-box-property-parsers suitcased-name)
        (case suitcased-name
          [(desc) (<:css-strings:>)]
          [(desc-width) (<css-natural>)]))))

(define btest-filter : (CSS-Cascaded-Value-Filter Bitmap.CSS)
  (lambda [declared-values inherited-values]
    (define (css->desc [_ : Symbol] [value : Any]) : (U String CSS-Wide-Keyword)
      (cond [(string? value) value]
            [(list? value) (string-join (filter string? value))]
            [else css:initial]))
    (current-css-element-color (css-color-ref declared-values inherited-values))
    (make-btest #:symbol-color (css-color-ref declared-values inherited-values 'symbol-color)
                #:string-color (css-color-ref declared-values inherited-values 'string-color)
                #:number-color (css-color-ref declared-values inherited-values 'number-color)
                #:output-color (css-color-ref declared-values inherited-values 'output-color)
                #:paren-color (css-color-ref declared-values inherited-values 'paren-color)
                #:foreground-color (css-color-ref declared-values inherited-values 'foreground-color)
                #:font (css-extract-font declared-values inherited-values)
                #:borders (css-extract-border-pen declared-values inherited-values)     #|properties are not inheritable|#
                #:brush (css-extract-background-brush declared-values inherited-values) #|properties are not inheritable|#
                #:max-width (css-ref declared-values inherited-values 'desc-width index? css:initial)
                #:desc (css-ref declared-values inherited-values 'desc css->desc)
                #:prelude (css-ref declared-values inherited-values 'prelude css->bitmap)
                #:descriptors (css-values-fold declared-values (initial-btest-descriptors)
                                               (λ [[property : Symbol] [this-datum : Any] [desc++ : (Listof (Pairof Symbol Any))]]
                                                 (cond [(memq property btest-color-properties) desc++]
                                                       [else (cons (cons property (box this-datum)) desc++)]))))))

(define ~module : CSS-Subject (make-css-subject #:type 'module #:classes '(main)))
(define ~btest : CSS-Subject (make-css-subject #:type 'bitmap-desc #:classes '(test)))

(define *root : CSS-Values (make-css-values))
(define $root : Bitmap.CSS
  (time-run 'tamer-main
            (let-values ([(toplevel topvalues) (css-cascade (list bitmap.css) (list ~module) btest-parsers btest-filter #false)])
              (set! *root topvalues)
              toplevel)))

(define-values (bitmap-descs testcases)
  (let-values ([($btests _) (css-cascade* (list bitmap.css) (list ~btest ~module) btest-parsers btest-filter *root)])
    (for/fold ([bitmap-descs : (Listof Bitmap) null] [testcases : (Listof Bitmap.CSS) null])
              ([$bt (in-list $btests)])
      (define-values (fgcolor rcolor) (values (btest-foreground-color $bt) (btest-output-color $bt)))

      (define-values (max-width words font) (values (btest-max-width $bt) (btest-desc $bt) (btest-font $bt)))
      (define desc (bitmap-paragraph words font #:color fgcolor #:background (btest-brush $bt) #:max-width max-width))
      (define-values (desc-width height) (bitmap-size desc))
      (define ~s32 : (-> String String) (λ [txt] (~s txt #:max-width 32 #:limit-marker "...\"")))
      
      (values (append bitmap-descs
                      (list (bitmap-pin 1 1/2 0 1/2
                                        (btest-prelude $bt)
                                        (bitmap-text "(" #:color (btest-paren-color $bt))
                                        (bitmap-hc-append #:gapsize 7
                                                          (bitmap-text "bitmap-paragraph" #:color (btest-symbol-color $bt))
                                                          (bitmap-text (~s32 words) #:color (btest-string-color $bt))
                                                          (bitmap-text (~a max-width) #:color (btest-number-color $bt))
                                                          (bitmap-text (~s (send font get-face)) #:color (btest-string-color $bt))
                                                          (bitmap-text (~a (smart-font-size font)) #:color (btest-number-color $bt)))
                                        (bitmap-text ")" #:color (btest-paren-color $bt)))
                            (bitmap-text #:color rcolor
                                         (cond [(not (send font should-combine?)) (format "- : (Bitmap ~a ~a)" desc-width height)]
                                               [else (format "- : (Bitmap ~a ~a #:ligature)" desc-width height)]))
                            (bitmap-frame desc #:border (btest-borders $bt) #:padding (list 0 (max (- max-width desc-width) 0) 0 0))))
              (cons $bt testcases)))))

(when DrRacket?
  (values $root
          (bitmap-vl-append* bitmap-descs)
          (css-root-relative-lengths)
          (css-font-relative-lengths)))
