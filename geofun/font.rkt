#lang typed/racket/base

(provide (all-defined-out) font-family->face)
(provide Font-Weight Font-Style Font-Stretch Font-Variant Font-Unit)
(provide list-font-families list-font-faces)
(provide list-monospace-font-families list-monospace-font-faces)
(provide (rename-out [Font-Style Font-Style-Datum]))

(require digimon/struct)
(require digimon/measure)

(require "digitama/font.rkt")
(require "digitama/unsafe/font.rkt")
(require "digitama/unsafe/math.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Option-Font (Option Font))
(define-type Font-Family-Datum (U String Symbol (Listof (U String Symbol))))
(define-type Font-Size-Datum (U Symbol Length+%))
(define-type Font-Weight-Datum (U Font-Weight Integer))

(define-type Font+Tweak (U Font Font:Tweak))
(define-type Option-Font+Tweak (Option Font+Tweak))

(struct font
  ([face : String]
   [size : Nonnegative-Flonum]
   [weight : Font-Weight]
   [style : Font-Style]
   [stretch : Font-Stretch]
   [variant : Font-Variant])
  #:transparent
  #:type-name Font)

(define-struct font:tweak : Font:Tweak
  ([family : (U Font-Family-Datum False) #false]
   [size : (U Font-Size-Datum False) #false]
   [weight : (Option Font-Weight-Datum) #false]
   [style : (Option Font-Style) #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-font : (Parameterof Font) (make-parameter (font (font-family->face 'sans-serif) 12.0 'normal 'normal 'normal 'normal)))
(define default-art-font : (Parameterof Font) (make-parameter (font (font-family->face 'fantasy) 24.0 'normal 'normal 'normal 'normal)))

(default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref (default-font) unit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desc-font
  (lambda [#:family [face : (U Font-Family-Datum False) #false]
           #:size [size : (U Font-Size-Datum False) #false]
           #:weight [weight : (U Symbol Integer False) #false]
           #:style [style : (Option Symbol) #false]
           #:stretch [stretch : (Option Symbol) #false]
           #:variant [variant : (Option Symbol) #false]
           [basefont : Font (default-font)]] : Font
    (font (cond [(string? face) (or (face-filter face) (font-face basefont))]
                [(symbol? face) (or (font-family->face face) (font-face basefont))]
                [(pair? face) (or (select-font-face face) (font-face basefont))]
                [else (font-face basefont)])
          (cond [(not size) (font-size basefont)]
                [(symbol? size) (generic-font-size-filter size (font-size basefont) (font-size (default-font)))]
                [(not (&L? size)) (~dimension size (font-size basefont))]
                [else (parameterize ([default-font-metrics (font-metrics basefont)])
                        (~dimension size (font-size basefont)))])
          (cond [(not weight) (font-weight basefont)]
                [(css-font-weight-option? weight) weight]
                [(eq? weight 'bolder) (memcadr css-font-weight-options (font-weight basefont))]
                [(eq? weight 'lighter) (memcadr (reverse css-font-weight-options) (font-weight basefont))]
                [(integer? weight) (integer->font-weight weight)]
                [else (font-weight basefont)])
          (if (and style (css-font-style-option? style)) style (font-style basefont))
          (if (and stretch (css-font-stretch-option? stretch)) stretch (font-stretch basefont))
          (if (and variant (css-font-variant-option? variant)) variant (font-variant basefont)))))

(define desc-font*
  (lambda [#:tweak [tweak : (Option Font:Tweak) #false]
           #:family [fc : (U Font-Family-Datum False) #false]
           #:size [sz : (U Font-Size-Datum False) #false]
           #:weight [wght : (U Symbol Integer False) #false]
           #:style [styl : (Option Symbol) #false]
           #:stretch [stretch : (Option Symbol) #false]
           #:variant [variant : (Option Symbol) #false]
           [basefont : Font (default-font)]] : Font
    (define-values (face size weight style)
      (if (or tweak)
          (values (or fc (font:tweak-family tweak))
                  (or sz (font:tweak-size tweak))
                  (or wght (font:tweak-weight tweak))
                  (or styl (font:tweak-style tweak)))
          (values fc sz wght styl)))
    
    (if (or face size weight style stretch variant)
        (desc-font #:family face #:size size
                   #:weight weight #:style style
                   #:stretch stretch #:variant variant
                   basefont)
        basefont)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define font-face->family : (-> String (Option Font-Family))
  (lambda [face]
    (let try-next ([families : (Listof Font-Family) css-font-generic-families])
      (cond [(null? families) #false]
            [(string=? face (font-family->face (car families))) (car families)]
            [else (try-next (cdr families))]))))

(define font-face->family* : (-> String (U Font-Family String))
  (lambda [face]
    (or (font-face->family face) face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define font-description : (-> Font Font-Description)
  (let ([&fonts : (Weak-HashTable Any Font-Description) (make-weak-hash)])
    (lambda [font]
      (hash-ref! &fonts font
                 (λ [] (let ([weight (font-weight font)]
                             [style (font-style font)]
                             [stretch (font-stretch font)]
                             [variant (font-variant font)])
                         (geo_create_font_desc (font-face font)
                                                 (font-size font)
                                                 (and (not (eq? weight 'normal)) (font-weight->integer weight))
                                                 (and (not (eq? style 'normal)) (font-style->integer style))
                                                 (and (not (eq? stretch 'normal)) (font-stretch->integer stretch))
                                                 (and (not (eq? variant 'normal)) (font-variant->integer variant)))))))))

(define font-metrics : (->* (Font) ((Listof Font-Unit)) (Listof (Pairof Font-Unit Nonnegative-Flonum)))
  (let ([&metrics : (Weak-HashTable Any (Listof (Pairof Font-Unit Nonnegative-Flonum))) (make-weak-hash)])
    (lambda [font [units null]]
      (define metrics : (Listof (Pairof Font-Unit Nonnegative-Flonum))
        (hash-ref! &metrics font (λ [] (font_get_metrics (font-description font)))))
      (cond [(null? units) metrics]
            [else (for/list : (Listof (Pairof Font-Unit Nonnegative-Flonum))
                    ([m (in-list metrics)] #:when (memq (car m) units))
                    m)]))))

(define font-metrics-ref : (Font Font-Unit -> Nonnegative-Flonum)
  (lambda [font unit]
    (define metrics (font-metrics font))
    
    (let ([?m (assq unit metrics)])
      (if ?m (cdr ?m) +nan.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define text-metrics-lines : (->* (String) (Font) (Values Flonum Flonum Flonum Flonum Flonum))
  (lambda [content [font (default-font)]]
    (font_get_metrics_lines* (font-description font) content)))

(define text-unknown-glyphs-count : (->* (String) (Font) Natural)
  (lambda [content [font (default-font)]]
    (font_get_unknown_glyphs_count (font-description font) content)))

(define text-glyphs-exist? : (->* (String) (Font) Boolean)
  (lambda [text [font (default-font)]]
    (zero? (text-unknown-glyphs-count text font))))

(define text-ascender-exist? : (->* (String) (Font #:overshoot-tolerance Real) Boolean)
  (lambda [text [font (default-font)] #:overshoot-tolerance [tolerance 1/8]]
    (define-values (ascender capline meanline baseline descender)
      (font_get_metrics_lines (font-description font) text))
    (define overshoot : Flonum (* (exact->inexact (- baseline meanline)) (real->double-flonum (abs tolerance))))
    (or (> (- capline ascender) 0)
        (> (exact->inexact (- meanline ascender)) overshoot))))

(define text-descender-exist? : (->* (String) (Font #:overshoot-tolerance Flonum) Boolean)
  (lambda [text [font (default-font)] #:overshoot-tolerance [tolerance 1/8]]
    (define-values (ascender capline meanline baseline descender)
      (font_get_metrics_lines (font-description font) text))
    (define overshoot : Flonum (* (exact->inexact (- baseline meanline)) (real->double-flonum (abs tolerance))))
    (and (> (- descender baseline) PANGO_SCALE)
         (> (exact->inexact (- descender baseline)) overshoot))))

(define text-size : (->* (String) (Font) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [text [font (default-font)]]
    (font_get_text_extent (font-description font) text)))

(define text-size* : (->* (String) (Font) (Values Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum))
  (lambda [text [font (default-font)]]
    (font_get_text_extent* (font-description font) text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-font-families : (->* () ((U (-> String Boolean Boolean) Symbol)) (Listof String))
  (lambda [[pred? 'all]]
    (cond [(procedure? pred?) (filter-font-families pred?)]
          [(eq? pred? 'mono) (list-monospace-font-families)]
          [else (list-font-families)])))

(define list-math-font-families : (->* () (String) (Listof String))
  (lambda [[math-text "ₖₗₘₙ"]]
    (list_math_font_families math-text)))

(define list-math-font-faces : (->* () (String) (Listof String))
  (lambda [[math-text "ₖₗₘₙ"]]
    (list_math_font_faces math-text)))

