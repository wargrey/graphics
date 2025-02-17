#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/keyword)

(require geofun/vector)
(require geofun/digitama/markup)

(require "../node/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Avatar-Procedure-Label-Datum (U False String Symbol PExpr-Element Keyword Geo))
(define-type Avatar-Procedure-Label (U Avatar-Procedure-Label-Datum (-> Nonnegative-Flonum Geo)))
(define-type Avatar-Procedure-IO-Fill (-> Avatar-Procedure-Label-Datum Symbol Fill-Paint))

(define default-procedure-font : (Parameterof Font) (make-parameter (desc-font #:family 'math #:size 48)))
(define default-procedure-border : (Parameterof Stroke) (make-parameter (desc-stroke #:color 'GhostWhite #:width 2.0)))
(define default-procedure-body-fill : (Parameterof Fill-Paint) (make-parameter 'DimGrey))
(define default-procedure-text-color : (Parameterof Color) (make-parameter 'LightCyan))
(define default-procedure-datum-color : (Parameterof Color) (make-parameter 'DimGrey))

(define default-procedure-iofill : (Parameterof Avatar-Procedure-IO-Fill)
  (make-parameter (λ [[label : Avatar-Procedure-Label-Datum] [type : Symbol]] : Fill-Paint
                    (let label->fill ([lbl : Avatar-Procedure-Label-Datum label])
                      (cond [(or (not lbl) (eq? lbl '||) (eq? lbl '#:||)) (default-procedure-body-fill)]
                            [(geo? lbl) (label->fill (geo-id lbl))]
                            [else (let ([label (cond [(symbol? lbl)  (string->bytes/utf-8 (symbol->immutable-string lbl))]
                                                     [(keyword? lbl) (string->bytes/utf-8 (keyword->immutable-string lbl))]
                                                     [(string? lbl)  (string->bytes/utf-8 lbl)]
                                                     [else (dc-markup-datum->text lbl)])])
                                    (cond [(bytes=? label #"") (default-procedure-body-fill)]
                                          [else (xterm (bytes-ref label 0))]))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-procedure
  (lambda [#:min-width [min-width : Real 0.0] #:min-height [min-height : Real 0.0]
           #:io-width [io-width : Real -1.618] #:io-datum-width [iov-width : Real -0.95]
           #:font [font : Font (default-procedure-font)]
           #:text-color [text-color : Color (default-procedure-text-color)]
           #:datum-color [datum-color : Color (default-procedure-datum-color)]
           #:iofill [iofill-color : Avatar-Procedure-IO-Fill (default-procedure-iofill)]
           #:border [border : Stroke (default-procedure-border)]
           #:body-operator [body-op : (Option Geo-Pin-Operator) 'over]
           #:body-fill [b:fill : Fill-Paint (default-procedure-body-fill)]
           #:body-position [body-pos : Complex 0.5]
           #:corner-radius [cr : Real -0.125]
           [desc : Avatar-Procedure-Label]
           [is : (Listof Avatar-Procedure-Label)]
           [os : (U Avatar-Procedure-Label (Listof Avatar-Procedure-Label)) null]
           [args : (Listof Any) null]
           . [results : Any *]] : Geo
    (define em : Nonnegative-Flonum (font-metrics-ref font 'em))
    (define b:height : Nonnegative-Flonum (stroke-width border))
    (define io:width : Nonnegative-Flonum (let ([w (real->double-flonum io-width)]) (cond [(> w 0.0) w] [(< w 0.0) (* em (- w))] [else (* em 1.618)])))
    (define v:width : Nonnegative-Flonum (let ([w (real->double-flonum iov-width)]) (cond [(> w 0.0) w] [(< w 0.0) (* io:width (- w))] [else io:width])))
    (define io:height : Nonnegative-Flonum (* io:width 1.618))
    (define io:gapsize : Nonnegative-Flonum (* em 0.618))
    (define v:gapsize : Nonnegative-Flonum (* em 0.0618))
    (define icount : Index (length is))
    (define ocount : Index (if (list? os) (length os) 1))
    
    (define geo-procedure-pipe : (-> Avatar-Procedure-Label Symbol Nonnegative-Flonum Any Geo)
      (lambda [maybe-label type vpos value]
        (define label (avatar-caption maybe-label em font text-color))
        (define datum (and value (if (geo? value) value (geo-text (if (procedure? value) (object-name value) value) font #:color datum-color #:alignment 'center))))
        (define fill-color (iofill-color (if (procedure? maybe-label) label maybe-label) type))
        (define iobox (geo-sandglass io:width io:height #:neck-width -0.32 #:neck-height (* b:height 3.0) #:fill fill-color #:stroke border))
        (define pipe ((if (eq? type 'Input) geo-ct-crop geo-cb-crop) iobox io:width (* io:height 0.5)))

        (define port
          (cond [(not label) pipe]
                [(geo? label) (create-dia-node #:type type #false #:fit-ratio 0.75 0.85 #:position 0.5 vpos pipe label)]
                [else (create-dia-node #:type type #false #:fit-ratio 0.75 0.85 #:position 0.5 vpos
                                       pipe (geo-text maybe-label font #:color text-color #:alignment 'center))]))

        (if (and datum)
            (let ([s (min 1.0 (/ v:width (geo-width datum)))])
              (if (eq? type 'Input)
                  (geo-vc-append #:gapsize v:gapsize (geo-scale datum s) port)
                  (geo-vc-append #:gapsize v:gapsize port (geo-scale datum s))))
            port)))

    (define description : (Option Geo) (avatar-caption desc em font text-color))
    (define-values (body-wratio body-hratio)
      (if (real? body-pos)
          (values 0.5 (max 0.0 (real->double-flonum body-pos)))
          (values (max 0.0 (real->double-flonum (real-part body-pos)))
                  (max 0.0 (real->double-flonum (imag-part body-pos))))))

    (define ipo:i : Geo
      (let ([boundary (length args)])
        (geo-trim
         (geo-hb-append* #:gapsize io:gapsize
                         (for/list : (Listof Geo) ([in (in-list is)]
                                                   [idx (in-naturals 0)])
                           (geo-procedure-pipe in 'Input 0.45
                                               (and (< idx boundary)
                                                    (list-ref args idx))))))))
    
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
                         (for/list : (Listof Geo) ([out (cond [(not (list? os)) (in-value os)]
                                                              [(pexpr-element? os) (in-value os)]
                                                              [else (in-list os)])]
                                                   [idx (in-naturals 0)])
                           (geo-procedure-pipe out 'Output 0.55
                                               (and (< idx boundary)
                                                    (list-ref results idx))))))))

    (cond [(= icount ocount 0) ipo:p]
          [else (let* ([offset (* b:height -1.0)]
                       [self (cond [(= icount 0) ipo:p]
                                   [else (geo-vc-append #:operator 'dest-over #:gapsize (+ offset 0.0) ipo:i ipo:p)])]
                       [self (cond [(= ocount 0) self]
                                   [else (geo-vc-append #:operator 'over #:gapsize (+ offset 2.0) self ipo:o)])])
                  self)])))

(define avatar-caption : (-> Avatar-Procedure-Label Nonnegative-Flonum Font Color (Option Geo))
  (lambda [desc em font fgcolor]
    (cond [(not desc) #false]
          [(geo? desc) desc]
          [(procedure? desc) (desc em)]
          [(string? desc) (geo-text desc font #:color fgcolor #:alignment 'center)]
          [(symbol? desc) (geo-text (format " ~a " desc) font #:color fgcolor #:id desc #:alignment 'center)]
          [(not (keyword? desc)) (geo-markup desc font #:color fgcolor #:error-color 'Crimson)]
          [else (geo-text #:color fgcolor #:id (string->symbol (keyword->immutable-string desc)) #:alignment 'center
                          (format " ~a " desc) font)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (dia-procedure #false null)
  (dia-procedure #false null '(=))
  (dia-procedure #false '(C))
  (dia-procedure "V - E + F = 2" '(V E F) '(=) (list 8 12 6) 2)
  
  (let* ([ds (list 1 2 3 4 5 6)]
         [result (apply + ds)])
    (dia-procedure #:body-fill 'LightGrey
                   (geo-scale (dia-procedure #:body-fill 'Grey
                                             '+ (build-list (length ds)
                                                            (λ [[i : Index]] : Avatar-Procedure-Label-Datum
                                                              `(span ("a" (sub (,(number->string (add1 i))))))))
                                             '(sum) ds result)
                              0.36)
                   (list 'λ 'list)
                   '(sum)
                   (list '+ ds)
                   result)))
  