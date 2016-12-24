#lang typed/racket

(provide (all-defined-out))

(require "time-run.rkt")
(require "../main.rkt")

(require racket/runtime-path)

(define DrRacket? : Boolean (regexp-match? #px"DrRacket$" (find-system-path 'run-file)))
(define-runtime-path bitmap.css "../stone/bitmap.css")

(define-values (in out) (make-pipe))
(define css-logger (make-logger 'css #false))
(define css (thread (thunk (let forever ([/dev/log (make-log-receiver css-logger 'debug)])
                             (match (sync/enable-break /dev/log)
                               [(vector 'debug _ (? eof-object?) _) (close-output-port out)]
                               [(vector level message _ (or 'exn:css:syntax 'exn:css:eval 'exn:css:read))
                                (fprintf out "[~a] ~a~n" level message) (forever /dev/log)]
                               [else (forever /dev/log)])))))
  
(current-logger css-logger)
(current-namespace (module->namespace 'bitmap))
(css-cache-computed-object-value #false)
(default-css-media-type 'screen)
(default-css-media-preferences
  ((inst make-hasheq Symbol CSS-Media-Datum)
   (list (cons 'orientation 'landscape)
         (cons 'width 1440)
         (cons 'height 820))))

(define-preference btest #:as Bitmap-TestCase #:with ([color-properties Color+sRGB])
  ([symbol-color : Color+sRGB                  #:= 'Blue]
   [string-color : Color+sRGB                  #:= 'Orange]
   [number-color : Color+sRGB                  #:= 'Tomato]
   [output-color : Color+sRGB                  #:= 'Chocolate]
   [paren-color : Color+sRGB                   #:= 'Firebrick]
   [border-color : Color+sRGB                  #:= 'Crimson]
   [foreground-color : Color+sRGB              #:= "Grey"]
   [background-color : Color+sRGB              #:= "Snow"]
   [font : Font                                #:= (default-css-font)]
   [width : Index                              #:= 512]
   [combine? : Boolean                         #:= #false]
   [desc : String                              #:= "['desc' property is required]"]
   [prelude : Bitmap                           #:= (bitmap-text "> ")]
   [descriptors : (HashTable Symbol CSS-Datum) #:= (make-hasheq)])
  #:transparent)

(define css-descriptor-filter : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-text-decoration-property-parsers suitcased-name)
        (css-color-property-parsers suitcased-name btest-color-properties)
        (css-image-property-parsers suitcased-name)
        (case suitcased-name
          [(desc) (CSS<*> (CSS<^> (<css:string>)) '+)]
          [(count width) (CSS<^> (<css-natural>))]
          [(combine) (CSS<^> (<css-keyword> '(combine none)))]
          [(at-exp) (CSS<^> (list (<css:λracket>) (<css:racket>) (<css:block>)))]
          [(prelude) (CSS<^> (<css-image>))]))))

(define css-preference-filter : (CSS-Cascaded-Value-Filter Bitmap-TestCase)
  (lambda [declared-values initial-values inherit-values]
    (define (css->desc [_ : Symbol] [value : CSS-Datum]) : String
      (cond [(string? value) value]
            [(list? value) (string-join (map (λ [v] (~a v)) value))]
            [else (btest-desc initial-values)]))
    (parameterize ([current-css-element-color (css-ref declared-values inherit-values 'color)])
      (make-btest #:symbol-color (css-ref declared-values inherit-values 'symbol-color css->color)
                  #:string-color (css-ref declared-values inherit-values 'string-color css->color)
                  #:number-color (css-ref declared-values inherit-values 'number-color css->color)
                  #:output-color (css-ref declared-values inherit-values 'output-color css->color)
                  #:paren-color (css-ref declared-values inherit-values 'paren-color css->color)
                  #:border-color (css-ref declared-values inherit-values 'border-color css->color)
                  #:foreground-color (css-ref declared-values inherit-values 'foreground-color css->color)
                  #:background-color (css-ref declared-values inherit-values 'background-color css->color)
                  #:font (css-extract-font declared-values inherit-values)
                  #:width (css-ref declared-values inherit-values 'width index? css:initial)
                  #:combine? (eq? 'normal (css-ref declared-values inherit-values 'font-variant-ligatures symbol? 'normal))
                  #:desc (css-ref declared-values inherit-values 'desc css->desc)
                  #:prelude (css-ref declared-values inherit-values 'prelude css->bitmap)
                  #:descriptors (for/hash : (HashTable Symbol CSS-Datum) ([(k fv) (in-css-values declared-values)])
                                  (values k (fv)))))))

(css-root-element-type 'module)
(define tamer-sheet : CSS-StyleSheet (read-css-stylesheet bitmap.css))
(define tamer-main : CSS-Subject (make-css-subject #:type 'module #:classes '(main)))

(define :values : CSS-Values (make-css-values))
(define :root : Bitmap-TestCase
  (time-run 'tamer-main
            (let-values ([(toplevel topvalues)
                          (css-cascade (list tamer-sheet) (list tamer-main)
                                       css-descriptor-filter css-preference-filter
                                       (make-btest) #false)])
              (set! :values topvalues)
              toplevel)))

(define-values (bitmap-descs testcases)
  (for/fold ([bitmap-descs : (Listof Bitmap) null]
             [testcases : (Listof Bitmap-TestCase) null])
            ([i (in-range (css-ref :values #false 'count index? 8))])
    (define tamer-test : CSS-Subject (make-css-subject #:type 'test #:id (string->keyword (~a 'case i))))
    (define-values (tobj _)
      (css-cascade (list tamer-sheet) (list tamer-test tamer-main)
                   css-descriptor-filter css-preference-filter
                   :root :values))

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
                          (bitmap-lt-superimpose (bitmap-frame desc #:margin 1 #:border-style 'transparent #:style 'transparent)
                                                 (bitmap-frame (bitmap-blank width height) #:border-color bdcolor))))
            (cons tobj testcases))))

(when DrRacket?
  (values tamer-sheet
          :root
          (apply bitmap-vl-append bitmap-descs)
          length%))

(log-message css-logger 'debug "exit" eof)
(when DrRacket? (copy-port in (current-output-port)))
