#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/font)
(require geofun/color)

(require geofun/digitama/markup)
(require geofun/digitama/layer/type)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-tick-anchor : (-> Plot-Axis-Style Symbol Geo-Pin-Anchor)
  (lambda [self direction]
    (define anchor (plot-axis-style-tick-anchor self))
    (if (eq? direction 'x)
        (if (pair? anchor) (car anchor) anchor)
        (if (pair? anchor) (cdr anchor) anchor))))

(define #:forall (C) plot-axis-visual-values : (case-> [Plot-Axis-Style -> (Values Font Font Font FlRGBA Color Color Color Color)]
                                                       [Plot-Axis-Style (-> Color C) -> (Values Font Font Font C C C C C)])
  (case-lambda
    [(self)
     (let ([axis-font (plot-axis-style-font self)]
           [axis-color (rgb* (plot-axis-style-color self))]
           [label-color (plot-axis-style-label-color self)])    
       (values (or (plot-axis-style-digit-font self) axis-font)
               (or (plot-axis-style-label-font self) axis-font)
               (or (plot-axis-style-desc-font self) axis-font)
               axis-color
               (or (plot-axis-style-digit-color self) axis-color)
               (or (plot-axis-style-tick-color self) axis-color)
               (or label-color axis-color)
               (or (plot-axis-style-desc-color self) label-color axis-color)))]
    [(self adjust)
     (let* ([axis-font (plot-axis-style-font self)]
            [axis-color (adjust (rgb* (plot-axis-style-color self)))]
            [digit-color (plot-axis-style-digit-color self)]
            [tick-color (plot-axis-style-tick-color self)]
            [label-color (if (plot-axis-style-label-color self) (adjust (plot-axis-style-label-color self)) axis-color)]
            [desc-color (plot-axis-style-label-color self)])    
       (values (or (plot-axis-style-digit-font self) axis-font)
               (or (plot-axis-style-label-font self) axis-font)
               (or (plot-axis-style-desc-font self) axis-font)
               axis-color
               (if (not digit-color) axis-color (adjust digit-color))
               (if (not tick-color) axis-color (adjust tick-color))
               label-color
               (if (not desc-color) label-color (adjust desc-color))))]))

(define plot-axis-digit-position-values : (-> Plot-Axis-Style Symbol (Values Flonum Geo-Pin-Anchor))
  (lambda [self direction]
    (define-values (xpos ypos) (plot-cartesian-settings (plot-axis-style-digit-position self)))
    
    (if (eq? direction 'x)
        (values xpos (if (< xpos 0.0) 'cc 'cc))
        (values ypos (if (< ypos 0.0) 'rc 'lc)))))

(define plot-axis-tick-length : (-> Plot-Axis-Style Symbol Flonum)
  (lambda [self direction]
    (plot-cartesian-value (plot-axis-style-tick-length self) direction)))

(define plot-axis-length-values : (case-> [Plot-Axis-Style Plot-Axis-Tip-Style Real
                                                           -> (Values Nonnegative-Flonum Nonnegative-Flonum
                                                                      Nonnegative-Flonum Nonnegative-Flonum)]
                                          [Plot-Axis-Style Plot-Axis-Tip-Style Real Nonnegative-Flonum
                                                           -> (Values Nonnegative-Flonum Nonnegative-Flonum
                                                                      Nonnegative-Flonum Nonnegative-Flonum)])
  (case-lambda
    [(self tip length 100%) (plot-axis-length-values* self tip (~length length 100%))]
    [(self tip length) (plot-axis-length-values* self tip (~length length))]))

(define plot-axis-height-values : (-> Plot-Axis-Style Plot-Axis-Tip-Style Nonnegative-Flonum Real
                                      (Values Nonnegative-Flonum Nonnegative-Flonum
                                              Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self tip view-width ratio]
    (define view-height : Nonnegative-Flonum (max (real->double-flonum (* view-width ratio)) 1.0))
    (define-values (neg-margin pos-margin) (plot-axis-margin-values tip view-width))

    (values (+ view-height neg-margin pos-margin (plot-axis-style-thickness self))
            view-height neg-margin pos-margin)))

(define plot-axis-margin-values : (-> Plot-Axis-Tip-Style Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self fllength]
    (define-values (n-margin p-margin) (plot-cartesian-settings (plot-axis-tip-style-margin self)))

    (values (~length n-margin fllength)
            (~length p-margin fllength))))

(define plot-axis-length-values* : (-> Plot-Axis-Style Plot-Axis-Tip-Style Nonnegative-Flonum
                                       (Values Nonnegative-Flonum Nonnegative-Flonum
                                               Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self tip fllength]
    (define-values (neg-margin pos-margin) (plot-axis-margin-values tip fllength))
    
    (values fllength
            (max (- fllength neg-margin pos-margin (plot-axis-style-thickness self)) 1.0)
            neg-margin pos-margin)))

(define plot-mark-style-values : (-> (Option Plot-Mark-Style) Plot-Axis-Style
                                     (Values Font Color Flonum Geo-Pin-Anchor Nonnegative-Flonum))
  (lambda [self master]
    (if (or self)
        (values (or (plot-mark-style-font self)
                    (plot-axis-style-font master))
                (or (plot-mark-style-color self)
                    (plot-axis-style-color master))
                (plot-mark-style-position self)
                (plot-mark-style-anchor self)
                (~length (plot-mark-style-dot-radius self)
                         (plot-axis-style-thickness master)))
        (plot-mark-style-values (default-plot-mark-style) master))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) plot-cartesian-settings : (case-> [Complex -> (Values Flonum Flonum)]
                                                       [Complex (-> Real T) -> (Values T T)])
  (case-lambda
    [(config) (plot-cartesian-settings config real->double-flonum)]
    [(config real->datum)
     (define x (real->datum (real-part config)))
     (define y (if (real? config) x (real->datum (imag-part config))))
     
     (values x y)]))

(define #:forall (T) plot-cartesian-maybe-settings : (case-> [Complex -> (Values Flonum Flonum)]
                                                             [Complex (-> Real T) -> (Values T T)]
                                                             [(Option Complex) -> (Values (Option Flonum) (Option Flonum))]
                                                             [(Option Complex) (-> Real T) -> (Values (Option T) (Option T))])
  (case-lambda
    [(config) (plot-cartesian-maybe-settings config real->double-flonum)]
    [(config real->datum)
     (cond [(not config) (values #false #false)]
           [else (plot-cartesian-settings config real->datum)])]))

(define #:forall (T) plot-cartesian-value : (case-> [Complex Symbol -> Flonum]
                                                    [Complex Symbol (-> Real T) -> T]
                                                    [(Option Complex) Symbol -> (Option Flonum)]
                                                    [(Option Complex) Symbol (-> Real T) -> (Option T)])
  (case-lambda
    [(config direction) (plot-cartesian-value config direction real->double-flonum)]
    [(config direction real->datum)
     (and config
          (let* ([x (real->datum (real-part config))]
                 [y (if (real? config) x (real->datum (imag-part config)))])
            (if (eq? direction 'x) x y)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-label-settings : (-> (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text)))
                                       (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text)))
                                       Flonum Flonum Flonum
                                       (Listof (List (Option DC-Markup-Text) Flonum (Option DC-Markup-Text) Geo-Pin-Anchor)))
  (lambda [label desc flmin flmax offset]
    (define descs (if (pair? desc) desc (cons #false desc)))
    
    (if (pair? label)
        (list (list (car label) (- flmin offset) (car descs) 'rc)
              (list (cdr label) (+ flmax offset) (cdr descs) 'lc))
        (list (list label (+ flmax offset) (cdr descs) 'lc)))))
