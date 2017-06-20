#lang typed/racket/base

(provide (all-defined-out) font-family->face)
(provide (rename-out [get-face-list list-font-faces]))

(require "digitama/digicore.rkt")
(require "digitama/font.rkt")
(require "digitama/misc.rkt")

(require "digitama/unsafe/source.rkt")
(require "digitama/unsafe/font.rkt")

(struct: font : Font
  ([face : String]
   [size : Nonnegative-Flonum]
   [weight : Font-Weight]
   [style : Font-Style]
   [stretch : Font-Stretch]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-font : (Parameterof Font) (make-parameter (font (font-family->face 'sans-serif) 12.0 'normal 'normal 'normal)))

(define desc-font : (->* ()
                         (Font #:family (U String Symbol (Listof (U String Symbol)) False) #:size (U Symbol Nonnegative-Real False)
                               #:style (Option Symbol) #:weight (U Symbol Integer False) #:stretch (Option Symbol))
                         Font)
  (lambda [[basefont (default-font)] #:family [face null] #:size [size +nan.0] #:weight [weight #false] #:style [style #false]
                                     #:stretch [stretch #false]]
    (font (cond [(string? face) (or (face-filter face) (font-face basefont))]
                [(symbol? face) (or (font-family->face face) (font-face basefont))]
                [(pair? face) (or (select-font-face face) (font-face basefont))]
                [else (font-face basefont)])
          (cond [(symbol? size) (generic-font-size-filter size (font-size basefont) (font-size (default-font)))]
                [(or (not size) (nan? size)) (font-size basefont)]
                [(single-flonum? size) (* (real->double-flonum size) (font-size basefont))]
                [else (real->double-flonum size)])
          (cond [(css-font-weight-option? weight) weight]
                [(eq? weight 'bolder) (memcadr css-font-weight-options (font-weight basefont))]
                [(eq? weight 'lighter) (memcadr (reverse css-font-weight-options) (font-weight basefont))]
                [(integer? weight) (integer->font-weight weight)]
                [else (font-weight basefont)])
          (if (css-font-style-option? style) style (font-style basefont))
          (if (css-font-stretch-option? stretch) stretch (font-stretch basefont)))))

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
          (let-values ([(weight style stretch) (values (font-weight font) (font-style font) (font-stretch font))])
            (define desc : Font-Description
              (bitmap_create_font_desc (font-face font)
                                       (font-size font)
                                       (and (not (eq? weight 'normal)) (font-weight->integer weight))
                                       (and (not (eq? style 'normal)) (font-style->integer style))
                                       (and (not (eq? stretch 'normal)) (font-stretch->integer stretch))))
            (hash-set! &fonts font (make-ephemeron font desc))
            desc)))))

(define font-metrics-ref : (case-> [Font -> (Listof (Pairof Symbol Nonnegative-Flonum))]
                                   [Font (Listof Symbol) -> (Listof (Pairof Symbol Nonnegative-Flonum))]
                                   [Font Symbol -> Nonnegative-Flonum])
  (let ([&metrics : (HashTable Any (Ephemeronof (Listof (Pairof Symbol Nonnegative-Flonum)))) (make-weak-hash)])
    (case-lambda
      [(font)
       (let ([&m (hash-ref &metrics font (λ _ #false))])
         (or (and &m (ephemeron-value &m))
             (let ([metrics (font_get_metrics (font-description font))])
               (hash-set! &metrics font (make-ephemeron font metrics))
               metrics)))]
      [(font units)
       (let ([metrics (font-metrics-ref font)])
         (if (symbol? units)
             (let ([?m (assq units metrics)])
               (if ?m (cdr ?m) +nan.0))
             (for/list : (Listof (Pairof Symbol Nonnegative-Flonum))
               ([m (in-list metrics)] #:when (memq (car m) units))
               m)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define text-metrics-lines : (->* (String) (Font) (Values Flonum Flonum Flonum Flonum Flonum))
  (lambda [content [font (default-font)]]
    (font_get_metrics_lines (font-description font) content)))

(define text-size : (->* (String) (Font) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [text [font (default-font)]]
    (define-values (width height distance ascent descent) (font_get_text_extent (font-description font) text))
    (values width height)))

(define text-size* : (->* (String) (Font) (Values Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum))
  (lambda [text [font (default-font)]]
    (font_get_text_extent (font-description font) text)))
