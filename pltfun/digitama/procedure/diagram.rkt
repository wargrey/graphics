#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require racket/symbol)
(require racket/keyword)

(require geofun/vector)
(require colorspace/palette)

(require geofun/digitama/richtext/self)
(require geofun/digitama/richtext/plain)
(require geofun/digitama/richtext/realize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plt-Procedure-Label-Datum (U False Void Symbol Geo-Rich-Text Keyword))
(define-type Plt-Procedure-Label (U Plt-Procedure-Label-Datum (-> Nonnegative-Flonum Plt-Procedure-Label-Datum)))
(define-type Plt-Procedure-IO-Fill (-> Index Plt-Procedure-Label-Datum Symbol Fill-Paint))

(define default-procedure-label-font : (Parameterof Font) (make-parameter (desc-font #:family 'math #:size 48)))
(define default-procedure-datum-font : (Parameterof Font) (make-parameter (desc-font #:family 'monospace #:size 48)))
(define default-procedure-input-format : (Parameterof String) (make-parameter "~a"))
(define default-procedure-output-format : (Parameterof String) (make-parameter "~a"))

(define default-procedure-border : (Parameterof Pen) (make-parameter (desc-stroke #:color 'GhostWhite #:width 2.0)))
(define default-procedure-body-fill : (Parameterof Fill-Paint) (make-parameter 'DimGrey))
(define default-procedure-text-color : (Parameterof Color) (make-parameter 'LightCyan))
(define default-procedure-datum-color : (Parameterof Color) (make-parameter 'DimGrey))

(define default-procedure-iofill : (Parameterof Plt-Procedure-IO-Fill)
  (make-parameter (λ [[idx : Index] [label : Plt-Procedure-Label-Datum] [type : Symbol]] : Fill-Paint
                    (if (eq? type 'Input)
                        (cdr (the-oklch-palette idx #false))
                        (let ([text (plt-procedure-caption-text label)])
                          (cond [(or (not text) (regexp-match? #px"^\\s*$" text)) (default-procedure-body-fill)]
                                [else (car (the-oklch-palette (char->integer (string-ref text 0)) #false))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-procedure
  (lambda [#:min-width [min-width : Real 0.0] #:min-height [min-height : Real 0.0]
           #:io-width [io-width : Length+% (&% 161.8)] #:io-datum-width [iov-width : Length+% (&% 95)]
           #:label-font [label-font : Font (default-procedure-label-font)]
           #:datum-font [datum-font : Font (default-procedure-datum-font)]
           #:text-color [text-color : Color (default-procedure-text-color)]
           #:datum-color [datum-color : Color (default-procedure-datum-color)]
           #:data-append [data-append* : (Option (-> (Listof Geo) [#:gapsize Real] Geo)) geo-vc-append*]
           #:data-gapsize [gapsize : Real 0.0]
           #:input-format [input-format : String (default-procedure-input-format)]
           #:output-format [output-format : String (default-procedure-output-format)]
           #:iofill [iofill-color : Plt-Procedure-IO-Fill (default-procedure-iofill)]
           #:border [border : Pen (default-procedure-border)]
           #:body-operator [body-op : (Option Geo-Pin-Operator) 'over]
           #:body-fill [b:fill : Fill-Paint (default-procedure-body-fill)]
           #:body-position [body-pos : Complex 0.5]
           #:corner-radius [cr : Length+% (&% 12.5)]
           #:variables [vars : (Option (Listof Symbol)) #false]
           #:id [id : (Option Symbol) #false]
           [desc : Plt-Procedure-Label]
           [is : (U (Listof Plt-Procedure-Label) (Immutable-Vectorof Plt-Procedure-Label)) null]
           [opt-os : (U Null Plt-Procedure-Label (Immutable-Vectorof Plt-Procedure-Label)) null]
           [args : (U (Listof Any) (Vectorof Any)) null]
           . [results : Any *]] : Geo
    (parameterize ([default-font-metrics (font-metrics label-font)])
      (define em : Nonnegative-Flonum (font-metrics-ref label-font 'em))
      (define io:width : Nonnegative-Flonum (~dimension io-width em (λ [] (* em 1.618))))
      (define v:width : Nonnegative-Flonum (~dimension iov-width io:width (λ [] io:width)))
      (define b:height : Nonnegative-Flonum (pen-width border))
      (define io:height : Nonnegative-Flonum (* io:width 1.618))
      (define io:gapsize : Nonnegative-Flonum (* em 0.618))
      (define v:gapsize : Nonnegative-Flonum (* em 0.0618))
      
      (define os : (Immutable-Vectorof Plt-Procedure-Label) (cond [(vector? opt-os) opt-os] [(null? opt-os) (vector-immutable)] [else (vector-immutable opt-os)]))
      (define icount : Index (if (list? is) (length is) (vector-length is)))
      (define ocount : Index (vector-length os))
      
      (define plt-procedure-datum : (case-> [Any True  String -> Geo]
                                            [Any False String -> (Option Geo)])
        (lambda [v allow-void? fmt]
          (cond [(geo? v) v]
                [(procedure? v) (geo-text (object-name v) datum-font #:color datum-color #:alignment 'center)]
                [(or allow-void? (not (void? v))) (geo-text (format fmt v) datum-font #:color datum-color #:alignment 'center)]
                [else #false])))
      
      (define plt-procedure-pipe : (-> Index Plt-Procedure-Label Symbol Nonnegative-Flonum String Any Geo)
        (lambda [idx maybe-label type vpos fmt value]
          (define label (plt-procedure-caption maybe-label em label-font text-color))
          (define fill-color (iofill-color idx (if (procedure? maybe-label) label maybe-label) type))
          (define iobox (geo-sandglass io:width io:height #:neck-width (&% 32) #:neck-height (* b:height 3.0) #:fill fill-color #:stroke border))
          (define pipe ((if (eq? type 'Input) geo-ct-crop geo-cb-crop) iobox io:width (* io:height 0.5)))
          
          (define datum : (Option Geo)
            (if (and data-append* (list? value))
                (data-append* #:gapsize gapsize
                              (for/list : (Listof Geo) ([subv (in-list value)])
                                (plt-procedure-datum subv #true fmt)))
                (plt-procedure-datum value #false fmt)))
          
          (define port
            (if (and label)
                (geo-dsfit-composite #:hfit% 0.65 #:vfit% 0.85
                                     pipe 0.5 vpos
                                     (cond [(geo? label) label]
                                         [else (geo-text #:color text-color #:alignment 'center
                                                         maybe-label label-font)])
                                     0.5 0.5)
                pipe))
          
          (if (and datum)
              (let ([s (min 1.0 (/ v:width (geo-width datum)))])
                (if (eq? type 'Input)
                    (geo-vc-append #:gapsize v:gapsize (geo-scale datum s) port)
                    (geo-vc-append #:gapsize v:gapsize port (geo-scale datum s))))
              port)))
      
      (define description : (Option Geo) (plt-procedure-caption desc em label-font text-color))
      (define-values (body-wratio body-hratio)
        (if (real? body-pos)
            (values 0.5 (max 0.0 (real->double-flonum body-pos)))
            (values (max 0.0 (real->double-flonum (real-part body-pos)))
                    (max 0.0 (real->double-flonum (imag-part body-pos))))))
      
      (define ipo:i : Geo
        (let ([boundary (if (list? args) (length args) (vector-length args))])
          (geo-trim
           (geo-hb-append* #:gapsize io:gapsize
                           (for/list : (Listof Geo) ([in (if (list? is) (in-list is) (in-vector is))]
                                                     [idx (in-naturals 0)])
                             (plt-procedure-pipe (assert idx index?) in 'Input 0.42 input-format
                                                 (when (< idx boundary)
                                                   (if (list? args)
                                                       (list-ref args idx)
                                                       (vector-ref args idx)))))))))
      
      (define ipo:p : Geo
        (let* ([wunit (+ io:gapsize io:width)]
               [body-width (max min-width (* wunit (max icount ocount)) (if (not description) (* wunit 1.0) (+ (geo-width description) io:gapsize)))]
               [body-height (max min-height (if (not description) (* body-width 0.618) (+ (geo-height description) io:gapsize)))]
               [body (geo-rounded-rectangle body-width body-height cr #:fill b:fill #:stroke border)])
          (cond [(not description) body]
                [else (geo-dsfit-composite #:hfit% 1.00 #:vfit% 1.00
                                           body body-wratio body-hratio
                                           (geo-frame #:base-operator body-op #:border #false #:background #false
                                                      description))])))
      
      (define ipo:o : Geo
        (let ([boundary (length results)])
          (geo-trim
           (geo-ht-append* #:gapsize io:gapsize
                           (for/list : (Listof Geo) ([out (in-vector os)]
                                                     [idx (in-naturals 0)])
                             (plt-procedure-pipe (assert idx index?) out 'Output 0.55 output-format
                                                 (when (< idx boundary)
                                                   (list-ref results idx))))))))
      
      (cond [(= icount ocount 0) ipo:p]
            [else (let* ([offset (* b:height -1.0)]
                         [self (cond [(= icount 0) ipo:p]
                                     [else (geo-vc-append #:operator 'dest-over #:gapsize (+ offset 0.0) ipo:i ipo:p)])]
                         [self (cond [(= ocount 0) self]
                                     [else (geo-vc-append #:operator 'over #:gapsize (+ offset 2.0) self ipo:o)])])
                    self)]))))
  
(define plt-procedure-caption-text : (-> Plt-Procedure-Label (Option String))
  (lambda [desc]
    (cond [(geo-rich-text? desc) (geo-rich-text->plain-text desc)]
          [(symbol? desc) (symbol->immutable-string desc)]
          [(keyword? desc) (keyword->immutable-string desc)]
          [else #false])))

(define plt-procedure-caption : (-> Plt-Procedure-Label Nonnegative-Flonum Font Color (Option Geo))
  (lambda [desc em font fgcolor]
    (cond [(geo? desc) desc]
          [(geo-rich-text? desc) (geo-rich-text-realize desc font fgcolor)]
          [(procedure? desc) (plt-procedure-caption (desc em) em font fgcolor)]
          [(string? desc) (geo-rich-text-realize desc font fgcolor)]
          [(symbol? desc) (geo-rich-text-realize (format " ~a " desc) font fgcolor)]
          [(keyword? desc) (geo-rich-text-realize (format " ~a " desc) font fgcolor)]
          [else #false])))
  