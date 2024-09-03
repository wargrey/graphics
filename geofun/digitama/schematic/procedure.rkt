#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require racket/symbol)

(require "../avatar/bacteriophage.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Schematic-Procedure-IO-Fill (-> Symbol Fill-Paint))

(define default-procedure-font : (Parameterof Font) (make-parameter (desc-font #:family 'math #:size 48)))
(define default-procedure-border : (Parameterof Stroke) (make-parameter (desc-stroke #:color 'GhostWhite #:width 2.0)))
(define default-procedure-body-fill : (Parameterof Fill-Paint) (make-parameter 'DimGray))
(define default-procedure-iotext-color : (Parameterof Color) (make-parameter 'LightCyan))

(define default-procedure-iofill : (Parameterof Schematic-Procedure-IO-Fill)
  (make-parameter (λ [[var : Symbol]]
                    (cond [(eq? var '||) (xterm 0)]
                          [else (xterm (remainder (char->integer (string-ref (symbol->immutable-string var) 0)) 255))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-procedure : (->* ((U False String Geo (-> Nonnegative-Flonum Geo)) (Listof Symbol))
                             ((U Symbol (Listof Symbol)) #:min-width Real #:min-height Real #:io-width Real #:font Font
                                                         #:iotext-color Color #:iofill Schematic-Procedure-IO-Fill
                                                         #:border Stroke #:body-fill Fill-Paint)
                             Geo)
  (lambda [#:min-width [min-width 0.0] #:min-height [min-height 0.0] #:io-width [io-width -1.618] #:font [font (default-procedure-font)]
           #:iotext-color [text-color (default-procedure-iotext-color)] #:iofill [iofill-color (default-procedure-iofill)]
           #:border [border (default-procedure-border)] #:body-fill [b:fill (default-procedure-body-fill)]
           desc is [os null]]
    (define em : Nonnegative-Flonum (font-metrics-ref font 'em))
    (define b:height : Nonnegative-Flonum (stroke-width border))
    (define io:width : Nonnegative-Flonum (let ([w (real->double-flonum io-width)]) (cond [(> w 0.0) w] [(< w 0.0) (* em (- w))] [else (* em 1.618)])))
    (define io:height : Nonnegative-Flonum (* io:width 1.618))
    (define io:gapsize : Nonnegative-Flonum (* em 0.618))
    (define icount : Index (length is))
    (define ocount : Index (if (list? os) (length os) 1))
    
    (define geo-procedure-pipe : (->* ((Option String) Nonnegative-Flonum Nonnegative-Flonum Font Fill-Paint Color Stroke)
                                         ((-> Geo Nonnegative-Real Nonnegative-Real Geo))
                                         Geo)
      (lambda [label width height font color text-color border [crop geo-ct-crop]]
        (define pipe (crop (geo-sandglass width height #:neck-width -0.32 #:neck-height (* b:height 3.0) #:fill color #:stroke border)
                           width (* height 0.5)))
        
        (cond [(not label) pipe]
              [else (geo-cc-superimpose pipe (geo-text label font #:color text-color))])))

    (define caption : (Option Geo)
      (cond [(not desc) #false]
            [(geo? desc) desc]
            [(not (string? desc)) (desc em)]
            [else (geo-text (string-append " " desc " ") font #:color text-color)]))

    (define ipo:i : Geo
      (geo-trim
       (geo-hc-append* #:gapsize io:gapsize
                          (for/list : (Listof Geo) ([in (in-list is)]
                                                    [idx (in-naturals 0)])
                            (geo-procedure-pipe (string-append " " (symbol->immutable-string in) " ")
                                                   io:width io:height font (iofill-color in) text-color border)))))
    
    (define ipo:p : Geo
      (let* ([wunit (+ io:gapsize io:width)]
             [body-width (max min-width (* wunit (max icount ocount)) (if (not caption) (* wunit 1.0) (+ (geo-width caption) io:gapsize)))]
             [body-height (max min-height (if (not caption) (* body-width 0.618) (+ (geo-height caption) io:gapsize)))]
             [body (geo-rectangle body-width body-height -0.125 #:fill b:fill #:stroke border)])
        (cond [(not caption) body]
              [else (geo-cc-superimpose body caption)])))

    (define ipo:o : Geo
      (geo-trim
       (geo-hc-append* #:gapsize io:gapsize
                          (for/list : (Listof Geo) ([out (if (list? os) (in-list os) (in-value os))]
                                                    [idx (in-naturals 0)])
                            (geo-procedure-pipe (string-append " " (symbol->immutable-string out) " ")
                                                   io:width io:height font (iofill-color out) text-color border geo-cb-crop)))))

    (cond [(= icount ocount 0) ipo:p]
          [else (let* ([offset (* b:height -1.0)]
                       [self (cond [(= icount 0) ipo:p]
                                   [else (geo-vc-append #:operator 'dest-over #:gapsize (+ offset 0.0) ipo:i ipo:p)])]
                       [self (cond [(= ocount 0) self]
                                   [else (geo-vc-append #:operator 'over #:gapsize (- offset 1.0) self ipo:o)])])
                  self)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-procedure #false null)
  (geo-procedure #false null '(=))
  (geo-procedure #false '(C))
  (geo-procedure "V - E + F = 2" '(V E F) '(=)))
