#lang typed/racket/base

(provide (all-defined-out))

(require file/convertible)
(require colorspace/misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Color (U Symbol Real Keyword FlColor))
(define-type Stroke-Dash-Datum (U (U 'solid 'dot 'dot-dash 'short-dash 'long-dash) (Vectorof Nonnegative-Flonum)))
(define-type Stroke-Dash+Offset (U Stroke-Dash-Datum (Pairof Stroke-Dash-Datum (Option Real))))

(struct flcolor () #:transparent #:type-name FlColor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rgba flcolor ([red : Flonum] [green : Flonum] [blue : Flonum] [alpha : Flonum])
  #:type-name FlRGBA
  #:transparent
  
  #:property prop:convertible
  (λ [[self : FlRGBA] [mime : Symbol] [fallback : Any]]
    (case mime
      [(rgb-byte-list rgb-uint8-list)
       (list (gamut->byte (rgba-red self))
             (gamut->byte (rgba-green self))
             (gamut->byte (rgba-blue self)))]
      [(rgb-uint16-list rgb-word-list)
       (list (gamut->uint16 (rgba-red self))
             (gamut->uint16 (rgba-green self))
             (gamut->uint16 (rgba-blue self)))]
      [(rgb-uint24 RRGGBB)
       (rgb-gamuts->hex (rgba-red self)
                        (rgba-green self)
                        (rgba-blue self))]
      [else fallback])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-bitmap-density : (Parameterof Positive-Flonum) (make-parameter 2.0))
(define default-bitmap-icon-height : (Parameterof Nonnegative-Flonum) (make-parameter 24.0))

(define color? : (-> Any Boolean : Color)
  (lambda [v]
    (or (flcolor? v)
        (symbol? v)
        (real? v)
        (keyword? v))))
