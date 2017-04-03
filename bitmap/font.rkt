#lang typed/racket

(provide (all-defined-out))

(require typed/racket/draw)

(require "digitama/digicore.rkt")
(require "digitama/cheat.rkt")
(require "digitama/font.rkt")
(require "digitama/unsafe.rkt")

(define-type CSS-Font (Instance CSS-Font%))

(define-type CSS-Font%
  (Class #:implements Font%
         (init-field [face String]
                     [combine? Boolean #:optional])
         (init [size Real #:optional]
               [style Font-Style #:optional]
               [weight Font-Weight #:optional]
               [underline? Boolean #:optional]
               [smoothing Font-Smoothing #:optional]
               [hinting Font-Hinting #:optional])
         (init-rest Null)
         [get-combine? (-> Boolean)]
         [get-metrics (->* () ((Listof Symbol)) (Listof (Pairof Symbol Nonnegative-Flonum)))]))

(define css-font% : CSS-Font%
  (class font%
    (init-field face [combine? #true])
    (init [size 12.0] [style 'normal] [weight 'normal] [underline? #false] [smoothing 'default] [hinting 'aligned])
    
    (super-make-object size face 'default style weight underline? smoothing #true hinting)

    (define metrics : (HashTable Symbol Nonnegative-Flonum) (make-hasheq))
    
    (define/public (get-metrics [units null])
      (define (metrics-ref [unit : Symbol]) : Nonnegative-Flonum (hash-ref metrics unit (thunk +nan.0)))
      (when (zero? (hash-count metrics))
        (define-values (ex cap ch ic) (get-font-metrics this))
        (hash-set! metrics 'em (smart-font-size this))
        (hash-set! metrics 'ex ex)
        (hash-set! metrics 'cap cap)
        (hash-set! metrics 'ch ch)
        (hash-set! metrics 'ic ic)
        (hash-set! metrics 'lh +nan.0))
      (cond [(null? units) (hash->list metrics)]
            [else (for/list : (Listof (Pairof Symbol Nonnegative-Flonum)) ([unit (in-list units)])
                    (cons unit (metrics-ref unit)))]))
    
    (define/public (get-combine?)
      combine?)
    
    (define/override (get-family)
      (let try-next ([families : (Listof Font-Family) '(swiss roman modern decorative script symbol system)])
        (cond [(null? families) (super get-family)]
              [(string=? face (font-family->font-face (car families))) (car families)]
              [else (try-next (cdr families))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cheat-opaque css-font%? #:is-a? CSS-Font% css-font%)

(define make-css-font : (->* ()
                             (Font #:size (U Symbol Nonnegative-Real) #:family (U String Symbol (Listof (U String Symbol)))
                                   #:style (Option Symbol) #:weight (Option Symbol) #:combine? (U Boolean Symbol)
                                   #:hinting (Option Font-Hinting) #:underlined? (U Boolean Symbol) #:smoothing (Option Font-Smoothing))
                             (Instance CSS-Font%))
  (let ([fontbase : (HashTable (Listof Any) (Instance CSS-Font%)) (make-hash)])
    (lambda [[basefont (default-css-font)] #:size [size +nan.0] #:family [face null] #:style [style #false] #:weight [weight #false]
                                           #:hinting [hinting #false] #:smoothing [smoothing #false] #:underlined? [underlined 'inherit]
                                           #:combine? [combine? 'inherit]]
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
      (define font-underlined? : Boolean (if (boolean? underlined) underlined (send basefont get-underlined)))
      (define font-hinting : Font-Hinting (or hinting (send basefont get-hinting)))
      (define font-combine? : Boolean
        (cond [(boolean? combine?) combine?]
              [(css-font%? basefont) (send basefont get-combine?)]
              [else #true]))
      (hash-ref! fontbase
                 (list font-face font-combine? font-size font-style font-weight font-underlined? font-smoothing font-hinting)
                 (λ [] (make-object css-font% font-face font-combine? font-size
                         font-style font-weight font-underlined? font-smoothing font-hinting))))))

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
(define get-font-metrics : (-> Font (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [font]
    (get_font_metrics (or (send font get-face)
                          (font-family->font-face (send font get-family)))
                      (smart-font-size font)
                      (send font get-style)
                      (send font get-weight))))

(define get-font-metrics-lines : (-> Font String (Values Flonum Flonum Flonum Flonum Flonum))
  (lambda [font content]
    (get_font_metrics_lines (or (send font get-face)
                                (font-family->font-face (send font get-family)))
                            (smart-font-size font)
                            (send font get-style)
                            (send font get-weight)
                            content)))

(define text-size : (->* (String Font) (Boolean #:with-dc (Instance DC<%>)) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [text font [combined? #true] #:with-dc [dc the-dc]]
    (define-values (w h d a) (send dc get-text-extent text font combined?))
    (values (real->double-flonum w) (real->double-flonum h))))
