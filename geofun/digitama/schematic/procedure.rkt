#lang typed/racket/base

(provide (all-defined-out))

(require bitmap)
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
(define bitmap-procedure : (->* ((U False String Bitmap (-> Nonnegative-Flonum Bitmap)) (Listof Symbol))
                                ((U Symbol (Listof Symbol)) #:min-width Real #:min-height Real #:io-width Real #:font Font
                                                            #:iotext-color Color #:iofill Schematic-Procedure-IO-Fill
                                                            #:border Stroke #:body-fill Fill-Paint)
                                Bitmap)
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
    
    (define bitmap-procedure-pipe : (->* ((Option String) Nonnegative-Flonum Nonnegative-Flonum Font Fill-Paint Color Stroke)
                                         ((-> Bitmap Nonnegative-Real Nonnegative-Real Bitmap))
                                         Bitmap)
      (lambda [label width height font color text-color border [crop bitmap-ct-crop]]
        (define pipe (crop (bitmap-sandglass width height #:neck-width -0.32 #:neck-height (* b:height 3.0) #:fill color #:border border)
                           width (* height 0.5)))
        
        (cond [(not label) pipe]
              [else (bitmap-cc-superimpose pipe (bitmap-text label font #:color text-color))])))

    (define caption : (Option Bitmap)
      (cond [(not desc) #false]
            [(bitmap? desc) desc]
            [(not (string? desc)) (desc em)]
            [else (bitmap-text (string-append " " desc " ") font #:color text-color)]))

    (define ipo:i : Bitmap
      (bitmap-trim
       (bitmap-hc-append* #:gapsize io:gapsize
                          (for/list : (Listof Bitmap) ([in (in-list is)]
                                                       [idx (in-naturals 0)])
                            (bitmap-procedure-pipe (string-append " " (symbol->immutable-string in) " ")
                                                   io:width io:height font (iofill-color in) text-color border)))))
    
    (define ipo:p : Bitmap
      (let* ([wunit (+ io:gapsize io:width)]
             [body-width (max min-width (* wunit (max icount ocount)) (if (not caption) (* wunit 1.0) (+ (bitmap-width caption) io:gapsize)))]
             [body-height (max min-height (if (not caption) (* body-width 0.618) (+ (bitmap-height caption) io:gapsize)))]
             [body (bitmap-rectangle body-width body-height -0.125 #:fill b:fill #:border border)])
        (cond [(not caption) body]
              [else (bitmap-cc-superimpose body caption)])))

    (define ipo:o : Bitmap
      (bitmap-trim
       (bitmap-hc-append* #:gapsize io:gapsize
                          (for/list : (Listof Bitmap) ([out (if (list? os) (in-list os) (in-value os))]
                                                       [idx (in-naturals 0)])
                            (bitmap-procedure-pipe (string-append " " (symbol->immutable-string out) " ")
                                                   io:width io:height font (iofill-color out) text-color border bitmap-cb-crop)))))

    (cond [(= icount ocount 0) ipo:p]
          [else (let* ([offset (* b:height -1.0)]
                       [self (cond [(= icount 0) ipo:p]
                                   [else (parameterize ([default-composition-operator 'dest-over])
                                           (bitmap-vc-append #:gapsize (+ offset 0.0) ipo:i ipo:p))])]
                       [self (cond [(= ocount 0) self]
                                   [else (parameterize ([default-composition-operator 'over])
                                           (bitmap-vc-append #:gapsize (- offset 1.0) self ipo:o))])])
                  self)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (bitmap-procedure #false null)
  (bitmap-procedure #false null '(=))
  (bitmap-procedure #false '(C))
  (bitmap-procedure "V - E + F = 2" '(V E F) '(=)))
