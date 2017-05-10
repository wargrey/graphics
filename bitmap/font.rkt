#lang typed/racket

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "digitama/cheat.rkt")
(require "digitama/font.rkt")
(require "digitama/inspectable.rkt")

(require "digitama/unsafe/font.rkt")

(define-type Font (Instance CSS-Font%))

(define-type CSS-Font%
  (Class #:implements Inspectable-Font%
         (init-field [face String]
                     [ligature Symbol #:optional]
                     [lines (Listof Symbol) #:optional])
         (init [size Real #:optional]
               [style Font-Style #:optional]
               [weight Font-Weight #:optional]
               [smoothing Font-Smoothing #:optional]
               [hinting Font-Hinting #:optional])
         [em (-> Nonnegative-Flonum)]
         [ex (-> Nonnegative-Flonum)]
         [cap (-> Nonnegative-Flonum)]
         [ch (-> Nonnegative-Flonum)]
         [ic (-> Nonnegative-Flonum)]
         [lh (-> Nonnegative-Flonum)]
         [get-face* (-> String)]
         [get-metrics (->* () ((Listof Symbol)) (Listof (Pairof Symbol Nonnegative-Flonum)))]
         [get-text-size (->* (String) ((Instance DC<%>)) (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [draw-text (-> (Instance DC<%>) String Real Real Void)]
         [should-combine? (-> Boolean)]
         [get-source (-> Font-Description)]))

(define/make-is-a? css-font% : CSS-Font%
  (class inspectable-font%
    (init-field face [ligature 'normal] [lines null])
    (init [size 12.0] [style 'normal] [weight 'normal] [smoothing 'default] [hinting 'aligned])
    
    (super-make-object size face 'default style weight (pair? lines) smoothing #true hinting)

    (define-syntax (define-metrics-accessor stx)
      (syntax-case stx []
        [(_ (get-ms [units null]) #:with [m ...] body ...)
         #'(begin (define/public (m) (fill-metrics!) (metrics-ref 'm)) ...
                  (define/public (get-ms [units null])
                    body ...))]))

    (define metrics : (HashTable Symbol Nonnegative-Flonum) (make-hasheq))
    (define &desc : (Boxof (Option Font-Description)) (box #false))

    (define-metrics-accessor (get-metrics [units null]) #:with [em ex cap ch ic lh]
      (fill-metrics!)
      (cond [(null? units) (hash->list metrics)]
            [else (for/list : (Listof (Pairof Symbol Nonnegative-Flonum)) ([unit (in-list units)])
                    (cons unit (metrics-ref unit)))]))

    (define/public (get-face*) face)
    (define/public (should-combine?) (not (eq? ligature 'none)))

    (define/public (get-text-size text [dc the-dc])
      (text-size text this (should-combine?) #:with-dc dc))

    (define/public (draw-text dc text x y)
      (define saved-font (send dc get-font))
      (cond [(eq? saved-font this) (send dc draw-text text x y (should-combine?))]
            [else (send* dc
                    (set-font this)
                    (draw-text text x y (should-combine?))
                    (set-font saved-font))]))

    (define/public (get-source)
      (or (unbox &desc)
          (let ([desc (bitmap_create_font_desc face (send this get-size) (send this get-style) (send this get-weight))])
            (set-box! &desc desc)
            desc)))

    (define/override (get-family)
      (let try-next ([families : (Listof Font-Family) '(swiss roman modern decorative script symbol system)])
        (cond [(null? families) (super get-family)]
              [(string=? face (font-family->font-face (car families))) (car families)]
              [else (try-next (cdr families))])))
    
    (define/override (inspect)
      (list face ligature lines (get-metrics) (get-family) (send this get-style) (send this get-weight)))
    
    (define/private (fill-metrics!) : Void
      (when (zero? (hash-count metrics))
        (define-values (ex cap ch ic layout-height) (get-font-metrics this))
        (hash-set! metrics 'em (smart-font-size this))
        (hash-set! metrics 'ex ex)
        (hash-set! metrics 'cap cap)
        (hash-set! metrics 'ch ch)
        (hash-set! metrics 'ic ic)
        (hash-set! metrics 'lh layout-height)))

    (define/private (metrics-ref [unit : Symbol]) : Nonnegative-Flonum
      (hash-ref metrics unit (thunk +nan.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-css-font : (->* ()
                             ((Instance Font%) #:size (U Symbol Nonnegative-Real) #:family (U String Symbol (Listof (U String Symbol)))
                                               #:style (Option Symbol) #:weight (Option Symbol) #:ligature (U Boolean Symbol)
                                               #:hinting (Option Font-Hinting) #:lines (Option (Listof Symbol))
                                               #:smoothing (Option Font-Smoothing))
                             Font)
  (let ([fontbase : (HashTable (Listof Any) (Instance CSS-Font%)) (make-hash)])
    (lambda [[basefont (default-css-font)] #:size [size +nan.0] #:family [face null] #:style [style #false] #:weight [weight #false]
                                           #:hinting [hinting #false] #:smoothing [smoothing #false] #:lines [lines #false]
                                           #:ligature [ligature #false]]
      (define font-size-raw : Nonnegative-Real
        (cond [(symbol? size) (generic-font-size-map size basefont (default-css-font))]
              [(nan? size) (smart-font-size basefont)]
              [(single-flonum? size) (* size (smart-font-size basefont))]
              [else size]))
      (define font-face : String
        (cond [(string? face) face]
              [(symbol? face) (font-family->font-face face)]
              [else (or (and (pair? face) (select-font-face face font-family->font-face))
                        (send basefont get-face)
                        (font-family->font-face (send basefont get-family)))]))
      (define font-size : Flonum (flmin 1024.0 (real->double-flonum font-size-raw)))
      (define font-style : Font-Style (generic-font-style-map (or style 'inherit) basefont))
      (define font-weight : Font-Weight (generic-font-weight-map (or weight 'inherit) basefont))
      (define font-smoothing : Font-Smoothing (or smoothing (send basefont get-smoothing)))
      (define font-lines : (Listof Symbol) (or lines (if (send basefont get-underlined) '(underline) null)))
      (define font-hinting : Font-Hinting (or hinting (send basefont get-hinting)))
      (define font-ligature : Symbol
        (cond [(symbol? ligature) ligature]
              [(css-font%? basefont) (if (send basefont should-combine?) 'normal 'none)]
              [else 'normal]))
      (hash-ref! fontbase
                 (list font-face font-ligature font-lines font-size font-style font-weight font-smoothing font-hinting)
                 (λ [] (make-object css-font% font-face font-ligature font-lines
                         font-size font-style font-weight font-smoothing font-hinting))))))

(define racket-font-family->font-face : (-> Symbol String)
  (lambda [family]
    (case family
      [(swiss default) (case os [(macosx) "Lucida Grande"] [(windows) "Tahoma"] [else "Sans"])]
      [(roman)         (case os [(macosx) "Times"] [(windows) "Times New Roman"] [else "Serif"])]
      [(modern)        (case os [(macosx) "Courier"] [(windows) "Courier New"] [else "Monospace"])]
      [(decorative)    (case os [(macosx) "Helvetica"] [(windows) "Arial"] [else "Helvetica"])]
      [(script)        (case os [(macosx) "Apple Chancery, Italic"] [(windows) "Palatino Linotype, Italic"] [else "Chancery"])]
      [(symbol)        (case os [else "Symbol"])]
      [(system)        (system-ui 'normal-control-font (case os [(macosx) "Helvetica Neue"] [(windows) "Verdana"] [else "Sans"]))]
      [else (racket-font-family->font-face (generic-font-family-map family (default-css-font)))])))

(define default-font-family->font-face : (Parameterof (-> Symbol (Option String))) (make-parameter racket-font-family->font-face))

(define font-family->font-face : (-> Symbol String)
  (lambda [family]
    (or ((default-font-family->font-face) family)
        (racket-font-family->font-face family))))

(define default-css-font : (Parameterof (Instance CSS-Font%))
  (make-parameter (make-object css-font% (racket-font-family->font-face 'default))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define get-font-metrics : (-> (Instance Font%) (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                                                        Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [font]
    (get_font_metrics (font->font-description font))))

(define get-font-metrics-lines : (-> (Instance Font%) String (Values Flonum Flonum Flonum Flonum Flonum))
  (lambda [font content]
    (get_font_metrics_lines (font->font-description font) content)))

(define font->font-description : (-> (Instance Font%) Font-Description)
  (lambda [font]
    (cond [(css-font%? font) (send font get-source)]
          [else (bitmap_create_font_desc (or (send font get-face)
                                             (font-family->font-face (send font get-family)))
                                         (smart-font-size font)
                                         (send font get-style)
                                         (send font get-weight))])))

(define text-size : (->* (String (Instance Font%)) (Boolean #:with-dc (Instance DC<%>)) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [text font [combine? #true] #:with-dc [dc the-dc]]
    (define-values (w h d a) (send dc get-text-extent text font combine?))
    (values (real->double-flonum w) (real->double-flonum h))))
