#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require racket/symbol)
(require racket/keyword)

(require geofun/vector)
(require geofun/digitama/markup)

(require colorspace/palette)

(require "../node/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Procedure-Label-Datum (U False Void String Symbol PExpr-Element Keyword Geo))
(define-type Dia-Procedure-Label (U Dia-Procedure-Label-Datum (-> Nonnegative-Flonum Geo)))
(define-type Dia-Procedure-IO-Fill (-> Index Dia-Procedure-Label-Datum Symbol Fill-Paint))

(define default-procedure-label-font : (Parameterof Font) (make-parameter (desc-font #:family 'math #:size 48)))
(define default-procedure-datum-font : (Parameterof Font) (make-parameter (desc-font #:family 'monospace #:size 48)))
(define default-procedure-input-format : (Parameterof String) (make-parameter "~a"))
(define default-procedure-output-format : (Parameterof String) (make-parameter "~a"))

(define default-procedure-border : (Parameterof Pen) (make-parameter (desc-stroke #:color 'GhostWhite #:width 2.0)))
(define default-procedure-body-fill : (Parameterof Fill-Paint) (make-parameter 'DimGrey))
(define default-procedure-text-color : (Parameterof Color) (make-parameter 'LightCyan))
(define default-procedure-datum-color : (Parameterof Color) (make-parameter 'DimGrey))

(define default-procedure-iofill : (Parameterof Dia-Procedure-IO-Fill)
  (make-parameter (λ [[idx : Index] [label : Dia-Procedure-Label-Datum] [type : Symbol]] : Fill-Paint
                    (if (eq? type 'Input)
                        (cdr (the-oklch-palette idx #false))
                        (let label->fill ([lbl : Dia-Procedure-Label-Datum label])
                          (cond [(or (not lbl) (eq? lbl '||) (eq? lbl '#:||) (void? lbl)) (default-procedure-body-fill)]
                                [(geo? lbl) (label->fill (geo-id lbl))]
                                [else (let ([label (cond [(symbol? lbl)  (string->bytes/utf-8 (symbol->immutable-string lbl))]
                                                         [(keyword? lbl) (string->bytes/utf-8 (keyword->immutable-string lbl))]
                                                         [(string? lbl)  (string->bytes/utf-8 lbl)]
                                                         [else (dc-markup-datum->text lbl)])])
                                        (cond [(bytes=? label #"") (default-procedure-body-fill)]
                                              [else (car (the-oklch-palette (bytes-ref label 0) #false))]))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-procedure
  (lambda [#:min-width [min-width : Real 0.0] #:min-height [min-height : Real 0.0]
           #:io-width [io-width : Real+% '(161.8 %)] #:io-datum-width [iov-width : Real+% '(95 %)]
           #:label-font [label-font : Font (default-procedure-label-font)]
           #:datum-font [datum-font : Font (default-procedure-datum-font)]
           #:text-color [text-color : Color (default-procedure-text-color)]
           #:datum-color [datum-color : Color (default-procedure-datum-color)]
           #:data-append [data-append* : (Option (-> (Listof Geo) [#:gapsize Real] Geo)) geo-vc-append*]
           #:data-gapsize [gapsize : Real 0.0]
           #:input-format [input-format : String (default-procedure-input-format)]
           #:output-format [output-format : String (default-procedure-output-format)]
           #:iofill [iofill-color : Dia-Procedure-IO-Fill (default-procedure-iofill)]
           #:border [border : Pen (default-procedure-border)]
           #:body-operator [body-op : (Option Geo-Pin-Operator) 'over]
           #:body-fill [b:fill : Fill-Paint (default-procedure-body-fill)]
           #:body-position [body-pos : Complex 0.5]
           #:corner-radius [cr : Real+% '(12.5 %)]
           #:variables [vars : (Option (Listof Symbol)) #false]
           [desc : Dia-Procedure-Label]
           [is : (U (Listof Dia-Procedure-Label) (Immutable-Vectorof Dia-Procedure-Label)) null]
           [opt-os : (U Null Dia-Procedure-Label (Immutable-Vectorof Dia-Procedure-Label)) null]
           [args : (U (Listof Any) (Vectorof Any)) null]
           . [results : Any *]] : Geo
    (define em : Nonnegative-Flonum (font-metrics-ref label-font 'em))
    (define io:width : Nonnegative-Flonum (~length io-width em (λ [] (* em 1.618))))
    (define v:width : Nonnegative-Flonum (~length iov-width io:width (λ [] io:width)))
    (define b:height : Nonnegative-Flonum (pen-width border))
    (define io:height : Nonnegative-Flonum (* io:width 1.618))
    (define io:gapsize : Nonnegative-Flonum (* em 0.618))
    (define v:gapsize : Nonnegative-Flonum (* em 0.0618))
    
    (define os : (Immutable-Vectorof Dia-Procedure-Label) (cond [(vector? opt-os) opt-os] [(null? opt-os) (vector-immutable)] [else (vector-immutable opt-os)]))
    (define icount : Index (if (list? is) (length is) (vector-length is)))
    (define ocount : Index (vector-length os))

    (define dia-procedure-datum : (case-> [Any True  String -> Geo]
                                          [Any False String -> (Option Geo)])
      (lambda [v allow-void? fmt]
        (cond [(geo? v) v]
              [(procedure? v) (geo-text (object-name v) datum-font #:color datum-color #:alignment 'center)]
              [(or allow-void? (not (void? v))) (geo-text (format fmt v) datum-font #:color datum-color #:alignment 'center)]
              [else #false])))
    
    (define dia-procedure-pipe : (-> Index Dia-Procedure-Label Symbol Nonnegative-Flonum String Any Geo)
      (lambda [idx maybe-label type vpos fmt value]
        (define label (dia-procedure-caption maybe-label em label-font text-color))
        (define fill-color (iofill-color idx (if (procedure? maybe-label) label maybe-label) type))
        (define iobox (geo-sandglass io:width io:height #:neck-width '(32 %) #:neck-height (* b:height 3.0) #:fill fill-color #:stroke border))
        (define pipe ((if (eq? type 'Input) geo-ct-crop geo-cb-crop) iobox io:width (* io:height 0.5)))

        (define datum : (Option Geo)
          (if (and data-append* (list? value))
              (data-append* #:gapsize gapsize
                            (for/list : (Listof Geo) ([subv (in-list value)])
                              (dia-procedure-datum subv #true fmt)))
              (dia-procedure-datum value #false fmt)))
          
        (define port
          (if (and label)
              (create-dia-node #:type type #false
                               #:fit-ratio 0.65 0.85
                               #:position 0.5 vpos
                               pipe (cond [(geo? label) label]
                                          [else (geo-text #:color text-color #:alignment 'center
                                                          maybe-label label-font)]))
              pipe))

        (if (and datum)
            (let ([s (min 1.0 (/ v:width (geo-width datum)))])
              (if (eq? type 'Input)
                  (geo-vc-append #:gapsize v:gapsize (geo-scale datum s) port)
                  (geo-vc-append #:gapsize v:gapsize port (geo-scale datum s))))
            port)))

    (define description : (Option Geo) (dia-procedure-caption desc em label-font text-color))
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
                           (dia-procedure-pipe (assert idx index?) in 'Input 0.42 input-format
                                               (when (< idx boundary)
                                                 (if (list? args)
                                                     (list-ref args idx)
                                                     (vector-ref args idx)))))))))
    
    (define ipo:p : Geo
      (let* ([wunit (+ io:gapsize io:width)]
             [body-width (max min-width (* wunit (max icount ocount)) (if (not description) (* wunit 1.0) (+ (geo-width description) io:gapsize)))]
             [body-height (max min-height (if (not description) (* body-width 0.618) (+ (geo-height description) io:gapsize)))]
             [body (geo-rectangle body-width body-height cr #:fill b:fill #:stroke border)])
        (cond [(not description) body]
              [else (create-dia-node #:type 'Process #false
                                     #:fit-ratio 1.00 1.00 #:position body-wratio body-hratio
                                     body (geo-frame #:base-operator body-op #:border #false #:background #false
                                                     description))])))

    (define ipo:o : Geo
      (let ([boundary (length results)])
        (geo-trim
         (geo-ht-append* #:gapsize io:gapsize
                         (for/list : (Listof Geo) ([out (in-vector os)]
                                                   [idx (in-naturals 0)])
                           (dia-procedure-pipe (assert idx index?) out 'Output 0.55 output-format
                                               (when (< idx boundary)
                                                 (list-ref results idx))))))))

    (cond [(= icount ocount 0) ipo:p]
          [else (let* ([offset (* b:height -1.0)]
                       [self (cond [(= icount 0) ipo:p]
                                   [else (geo-vc-append #:operator 'dest-over #:gapsize (+ offset 0.0) ipo:i ipo:p)])]
                       [self (cond [(= ocount 0) self]
                                   [else (geo-vc-append #:operator 'over #:gapsize (+ offset 2.0) self ipo:o)])])
                  self)])))

(define dia-procedure-caption : (-> Dia-Procedure-Label Nonnegative-Flonum Font Color (Option Geo))
  (lambda [desc em font fgcolor]
    (cond [(geo? desc) desc]
          [(pexpr-element? desc) (geo-markup desc font #:color fgcolor #:error-color 'Crimson)]
          [(procedure? desc) (desc em)]
          [(string? desc) (geo-text desc font #:color fgcolor #:alignment 'center)]
          [(symbol? desc) (geo-text (format " ~a " desc) font #:color fgcolor #:id desc #:alignment 'center)]
          [(keyword? desc) (geo-text #:color fgcolor #:id (string->symbol (keyword->immutable-string desc)) #:alignment 'center
                                     (format " ~a " desc) font)]
          [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (dia-procedure #false null)
  (dia-procedure #false null #(=))
  (dia-procedure #false #(C))
  (dia-procedure "V - E + F = 2" #(V E F) #(=) #(8 12 6) 2)
  
  (let* ([ds (list 1 2 3 4 5 6)]
         [result (apply + ds)])
    (dia-procedure #:body-fill 'LightGrey
                   (geo-scale (dia-procedure #:body-fill 'Grey
                                             '+
                                             (build-list (length ds)
                                                         (λ [[i : Index]] : Dia-Procedure-Label-Datum
                                                           (pexpr 'span (list "a" (pexpr 'sub (number->string (add1 i)))))))
                                             #(sum)
                                             ds result)
                              0.36)
                   #(λ list) #(sum)
                   (list '+ ds) result)))
  