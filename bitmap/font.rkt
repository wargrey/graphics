#lang typed/racket

(provide (all-defined-out))
(provide (rename-out [generic-font-family-map font-family->face]))

(require "digitama/digicore.rkt")
(require "digitama/font.rkt")
(require "digitama/misc.rkt")

(require "digitama/unsafe/font.rkt")

(struct: font : Font
  ([face : String]
   [size : Nonnegative-Flonum]
   [weight : CSS:Font-Weight]
   [style : CSS:Font-Style]
   [stretch : CSS:Font-Stretch]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-font : (Parameterof Font) (make-parameter (font (generic-font-family-map 'sans-serif) 12.0 'normal 'normal 'normal)))

(define desc-font : (->* ()
                         (Font #:size (U Symbol Nonnegative-Real) #:family (U String Symbol (Listof (U String Symbol)))
                               #:style (Option Symbol) #:weight (U False Symbol Integer) #:stretch (Option Symbol))
                         Font)
  (lambda [[basefont (default-font)] #:size [size +nan.0] #:family [face null] #:weight [weight #false] #:style [style #false]
                                     #:stretch [stretch #false]]
    (font (cond [(string? face) face]
                [(css-font-generic-family? face) (generic-font-family-map face)]
                [else (or (and (pair? face) (select-font-face face generic-font-family-map))
                          (font-face basefont))])
          (cond [(symbol? size) (generic-font-size-map size (font-size basefont) (font-size (default-font)))]
                [(nan? size) (font-size basefont)]
                [(single-flonum? size) (* (real->double-flonum size) (font-size basefont))]
                [else (real->double-flonum size)])
          (cond [(css-font-weight-option? weight) weight]
                [(eq? weight 'bolder) (memcadr css-font-weight-options (font-weight basefont))]
                [(eq? weight 'lighter) (memcadr (reverse css-font-weight-options) (font-weight basefont))]
                [(integer? weight) (integer->font-weight weight)]
                [else (font-weight basefont)])
          (if (css-font-style-option? style) style (font-style basefont))
          (if (css-font-stretch-option? stretch) stretch (font-stretch basefont)))))

(define font-face->family : (-> String (Option CSS:Font-Family))
  (lambda [face]
    (let try-next ([families : (Listof CSS:Font-Family) css-font-generic-families])
      (cond [(null? families) #false]
            [(string=? face (generic-font-family-map (car families))) (car families)]
            [else (try-next (cdr families))]))))

(define font-face->family* : (-> String (U CSS:Font-Family String))
  (lambda [face]
    (or (font-face->family face) face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define font-handle : (-> Font Font-Description)
  (let ([&fonts : (HashTable Any (Ephemeronof Font-Description)) (make-weak-hash)])
    (lambda [font]
      (define &font (hash-ref &fonts font (thunk #false)))
      (or (and &font (ephemeron-value &font))
          (let ([desc (bitmap_create_font_desc (font-face font) (font-size font)
                                               (font-weight->integer (font-weight font))
                                               (font-style->integer (font-style font))
                                               (font-stretch->integer (font-stretch font)))])
            (hash-set! &fonts font (make-ephemeron font desc))
            desc)))))

(define font-metrics-ref : (case-> [Font -> (Listof (Pairof Symbol Nonnegative-Flonum))]
                                   [Font (Listof Symbol) -> (Listof (Pairof Symbol Nonnegative-Flonum))]
                                   [Font Symbol -> Nonnegative-Flonum])
  (let ([&metrics : (HashTable Any (Ephemeronof (Listof (Pairof Symbol Nonnegative-Flonum)))) (make-weak-hash)])
    (case-lambda
      [(font)
       (let ([&m (hash-ref &metrics font (thunk #false))])
         (or (and &m (ephemeron-value &m))
             (let ([metrics (get_font_metrics (font-handle font))])
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
(define text-metrics-lines : (-> Font String (Values Flonum Flonum Flonum Flonum Flonum))
  (lambda [font content]
    (get_font_metrics_lines (font-handle font) content)))

#;(define text-size : (->* (String (Instance Font%)) (Boolean #:with-dc (Instance DC<%>)) (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [text font [combine? #true] #:with-dc [dc the-dc]]
    (define-values (w h d a) (send dc get-text-extent text font combine?))
    (values (real->double-flonum w) (real->double-flonum h))))
