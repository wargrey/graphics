#lang typed/racket

(provide (all-defined-out))

(require typed/racket/draw)

(require "digitama/digicore.rkt")
(require "digitama/cheat.rkt")
(require "digitama/font.rkt")

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
         [get-font-metrics (-> (Listof Symbol) (Listof (Pairof Symbol Nonnegative-Flonum)))]))

(define css-font% : CSS-Font%
  (class font%
    (init-field face [combine? #true])
    (init [size 12.0] [style 'normal] [weight 'normal] [underline? #true] [smoothing 'default] [hinting 'aligned])
    
    (super-make-object size face 'default style weight underline? smoothing #true hinting)

    (define metrics : (HashTable Symbol Nonnegative-Flonum) (make-hasheq))
    
    (define/public (get-font-metrics units)
      (define (metrics-ref [unit : Symbol]) : Nonnegative-Flonum (hash-ref metrics unit (thunk +nan.0)))
      (when (zero? (hash-count metrics))
        ;;; WARNING
        ;; 'xh' seems to be impractical, the font% size is just a nominal size
        ;; and usually smaller than the generated text in which case the 'ex' is
        ;; always surprisingly larger than the size, the '0w' therefore is used instead,
        ;; same consideration to 'cap'.
        #;(define-values (xw xh xd xs) (send the-dc get-text-extent "x" this))
        (define-values (0w 0h 0d 0s) (send the-dc get-text-extent "0" this))
        (define-values (ww wh wd ws) (send the-dc get-text-extent "水" this))
        (hash-set! metrics 'em (smart-font-size this))
        (hash-set! metrics 'ex (real->double-flonum 0w))
        (hash-set! metrics 'cap (real->double-flonum (max (- (smart-font-size this) 0d 0s) 0d)))
        (hash-set! metrics 'ch (real->double-flonum 0w))
        (hash-set! metrics 'ic (real->double-flonum ww)))
      (for/list : (Listof (Pairof Symbol Nonnegative-Flonum)) ([unit (in-list units)])
        (cons unit (metrics-ref unit))))
    
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
                             (Font #:size Real #:family (U String Symbol False) #:style (Option Symbol) #:weight (Option Symbol)
                                   #:hinting (Option Font-Hinting) #:underlined? (U Boolean Symbol) #:smoothing (Option Font-Smoothing)
                                   #:combine? (U Boolean Symbol))
                             (Instance CSS-Font%))
  (let ([fontbase : (HashTable (Listof Any) (Instance CSS-Font%)) (make-hash)])
    (lambda [[basefont (default-css-font)] #:size [size +nan.0] #:family [face #false] #:style [style #false] #:weight [weight #false]
                                           #:hinting [hinting #false] #:smoothing [smoothing #false] #:underlined? [underlined 'inherit]
                                           #:combine? [combine? 'inherit]]
      (define font-size : Real
        (min 1024.0 (cond [(positive? size) size]
                          [(or (zero? size) (nan? size)) (smart-font-size basefont)]
                          [else (* (- size) (smart-font-size basefont))])))
      (define font-face : String
        (cond [(string? face) face]
              [(symbol? face) (font-family->font-face face)]
              [(send basefont get-face) => values]
              [else (font-family->font-face (send basefont get-family))]))
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
      [else (racket-font-family->font-face (generic-font-family-map family))])))

(define default-font-family->font-face : (Parameterof (-> Symbol (Option String))) (make-parameter racket-font-family->font-face))

(define font-family->font-face : (-> Symbol String)
  (lambda [family]
    (or ((default-font-family->font-face) family)
        (racket-font-family->font-face family))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define text-size : (->* (String Font) (Boolean #:with-dc (Instance DC<%>)) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [text font [combined? #true] #:with-dc [dc the-dc]]
    (define-values (w h d a) (send dc get-text-extent text font combined?))
    (values (real->double-flonum w) (real->double-flonum h))))
