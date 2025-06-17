#lang typed/racket/base

(provide (all-defined-out) font-family->face)
(provide Font-Weight Font-Style Font-Stretch Font-Variant)
(provide list-font-families list-font-faces)
(provide list-monospace-font-families list-monospace-font-faces)

(require racket/math)

(require "digitama/font.rkt")
(require "digitama/unsafe/font.rkt")

(struct font
  ([face : String]
   [size : Nonnegative-Flonum]
   [weight : Font-Weight]
   [style : Font-Style]
   [stretch : Font-Stretch]
   [variant : Font-Variant])
  #:transparent
  #:type-name Font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-font : (Parameterof Font) (make-parameter (font (font-family->face 'sans-serif) 12.0 'normal 'normal 'normal 'normal)))
(define default-art-font : (Parameterof Font) (make-parameter (font (font-family->face 'fantasy) 24.0 'normal 'normal 'normal 'normal)))

(define desc-font : (->* ()
                         (Font #:family (U String Symbol (Listof (U String Symbol)) False) #:size (U Symbol Real False)
                               #:style (Option Symbol) #:weight (U Symbol Integer False) #:stretch (Option Symbol) #:variant (Option Symbol))
                         Font)
  (lambda [[basefont (default-font)] #:family [face null] #:size [size +nan.0] #:weight [weight #false] #:style [style #false]
                                     #:stretch [stretch #false] #:variant [variant #false]]
    (font (cond [(string? face) (or (face-filter face) (font-face basefont))]
                [(symbol? face) (or (font-family->face face) (font-face basefont))]
                [(pair? face) (or (select-font-face face) (font-face basefont))]
                [else (font-face basefont)])
          (cond [(symbol? size) (generic-font-size-filter size (font-size basefont) (font-size (default-font)))]
                [(or (not size) (nan? size)) (font-size basefont)]
                [(negative? size) (* (- (real->double-flonum size)) (font-size basefont))]
                [else (real->double-flonum size)])
          (cond [(css-font-weight-option? weight) weight]
                [(eq? weight 'bolder) (memcadr css-font-weight-options (font-weight basefont))]
                [(eq? weight 'lighter) (memcadr (reverse css-font-weight-options) (font-weight basefont))]
                [(integer? weight) (integer->font-weight weight)]
                [else (font-weight basefont)])
          (cond [(css-font-style-option? style) style]
                [(eq? face 'math) 'italic]
                [else (font-style basefont)])
          (if (css-font-stretch-option? stretch) stretch (font-stretch basefont))
          (if (css-font-variant-option? variant) variant (font-variant basefont)))))

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
  (let ([&fonts : (HashTable Any (Ephemeronof Font-Description)) (make-weak-hash)])
    (lambda [font]
      (define &font (hash-ref &fonts font (λ _ #false)))
      (or (and &font (ephemeron-value &font))
          (let ([weight (font-weight font)]
                [style (font-style font)]
                [stretch (font-stretch font)]
                [variant (font-variant font)])
            (define desc : Font-Description
              (geo_create_font_desc (font-face font)
                                    (font-size font)
                                    (and (not (eq? weight 'normal)) (font-weight->integer weight))
                                    (and (not (eq? style 'normal)) (font-style->integer style))
                                    (and (not (eq? stretch 'normal)) (font-stretch->integer stretch))
                                    (and (not (eq? variant 'normal)) (font-variant->integer variant))))
            (hash-set! &fonts font (make-ephemeron font desc))
            desc)))))

(define font-metrics : (->* (Font) ((Listof Symbol)) (Listof (Pairof Symbol Nonnegative-Flonum)))
  (let ([&metrics : (HashTable Any (Ephemeronof (Listof (Pairof Symbol Nonnegative-Flonum)))) (make-weak-hash)])
    (lambda [font [units null]]
      (define &m (hash-ref &metrics font (λ _ #false)))
      (define metrics : (Listof (Pairof Symbol Nonnegative-Flonum))
        (or (and &m (ephemeron-value &m))
            (let ([metrics (font_get_metrics (font-description font))])
              (hash-set! &metrics font (make-ephemeron font metrics))
              metrics)))
      (cond [(null? units) metrics]
            [else (for/list : (Listof (Pairof Symbol Nonnegative-Flonum))
                    ([m (in-list metrics)] #:when (memq (car m) units))
                    m)]))))

(define font-metrics-ref : (Font Symbol -> Nonnegative-Flonum)
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
