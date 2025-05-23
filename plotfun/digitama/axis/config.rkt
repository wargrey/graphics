#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/color)
(require geofun/font)

(require geofun/digitama/layer/type)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-tick-anchor : (-> Plot-Axis-Style Symbol Geo-Pin-Anchor)
  (lambda [self direction]
    (define anchor (plot-axis-style-tick-anchor self))
    (if (eq? direction 'x)
        (if (pair? anchor) (car anchor) anchor)
        (if (pair? anchor) (cdr anchor) anchor))))

(define plot-axis-visual-values : (-> Plot-Axis-Style (Values Font Font FlRGBA Color Color Color))
  (lambda [self]
    (define digit-font : Font (plot-axis-style-digit-font self))
    (define axis-color : FlRGBA (rgb* (plot-axis-style-color self)))
    
    (values digit-font
            (or (plot-axis-style-label-font self) digit-font)
            axis-color
            (or (plot-axis-style-digit-color self) axis-color)
            (or (plot-axis-style-tick-color self) axis-color)
            (or (plot-axis-style-label-color self) axis-color))))

(define plot-axis-digit-position-values : (-> Plot-Axis-Style Symbol (Values Flonum Geo-Pin-Anchor))
  (lambda [self direction]
    (define-values (xpos ypos) (plot-cartesian-settings (plot-axis-style-digit-position self)))
    
    (if (eq? direction 'x)
        (values xpos (if (< xpos 0.0) 'ct 'cb))
        (values ypos (if (< ypos 0.0) 'rc 'lc)))))

(define plot-axis-tick-length : (-> Plot-Axis-Style Symbol Flonum)
  (lambda [self direction]
    (plot-cartesian-value (plot-axis-style-tick-length self) direction)))

(define plot-axis-length-values : (case-> [Plot-Axis-Style Real -> (Values Nonnegative-Flonum Nonnegative-Flonum
                                                                           Nonnegative-Flonum Nonnegative-Flonum)]
                                          [Plot-Axis-Style Real Nonnegative-Flonum -> (Values Nonnegative-Flonum Nonnegative-Flonum
                                                                                              Nonnegative-Flonum Nonnegative-Flonum)])
  (case-lambda
    [(self length 100%) (plot-axis-length-values* self (~length length 100%))]
    [(self length) (plot-axis-length-values* self (~length length))]))

(define plot-axis-height-values : (-> Plot-Axis-Style Nonnegative-Flonum Real (Values Nonnegative-Flonum Nonnegative-Flonum
                                                                                      Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self view-width ratio]
    (define view-height : Nonnegative-Flonum (max (real->double-flonum (* view-width ratio)) 1.0))
    (define-values (neg-margin pos-margin) (plot-axis-margin-values self view-width))

    (values (+ view-height neg-margin pos-margin)
            view-height neg-margin pos-margin)))

(define plot-axis-margin-values : (-> Plot-Axis-Style Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self fllength]
    (define-values (n-margin p-margin) (plot-cartesian-settings (plot-axis-style-margin self)))

    (values (~length n-margin fllength)
            (~length p-margin fllength))))

(define plot-axis-length-values* : (-> Plot-Axis-Style Nonnegative-Flonum
                                       (Values Nonnegative-Flonum Nonnegative-Flonum
                                               Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self fllength]
    (define-values (neg-margin pos-margin) (plot-axis-margin-values self fllength))
    
    (values fllength
            (max (- fllength neg-margin pos-margin) 1.0)
            neg-margin pos-margin)))

(define plot-axis-real-style-values : (-> (Option Plot-Axis-Real-Style) Plot-Axis-Style
                                          (Values Font Color Flonum Geo-Pin-Anchor Nonnegative-Flonum))
  (lambda [self master]
    (if (not self)
        (values (plot-axis-style-digit-font master)
                (or (plot-axis-style-digit-color master)
                    (plot-axis-style-color master))
                ((default-plot-axis-real-position))
                ((default-plot-axis-real-anchor))
                (~length ((default-plot-axis-real-dot-radius))
                         (plot-axis-style-thickness master)))
        (values (or (plot-axis-real-style-font self)
                    (plot-axis-style-digit-font master))
                (or (plot-axis-real-style-color self)
                    (plot-axis-style-digit-color master)
                    (plot-axis-style-color master))
                (plot-axis-real-style-position self)
                (plot-axis-real-style-anchor self)
                (~length (plot-axis-real-style-dot-radius self)
                         (plot-axis-style-thickness master))))))

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
