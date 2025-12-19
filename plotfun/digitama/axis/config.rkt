#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)
(require digimon/metrics)

(require geofun/font)
(require geofun/color)
(require geofun/stroke)

(require geofun/digitama/markup)
(require geofun/digitama/layer/type)
(require geofun/digitama/paint/self)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-tip : (-> Plot-Axis-Style Symbol Plot-Axis-Tip-Style)
  (lambda [self direction]
    (define tip (plot-axis-style-tip self))
    (if (eq? direction 'x)
        (if (pair? tip) (car tip) tip)
        (if (pair? tip) (cdr tip) tip))))

(define plot-axis-visual-values : (case-> [Plot-Axis-Style -> (Values Font Font Font Font Pen Nonnegative-Flonum Color Color Color Color)]
                                          [Plot-Axis-Style (-> FlRGBA FlRGBA) -> (Values Font Font Font Font Pen Nonnegative-Flonum FlRGBA FlRGBA FlRGBA FlRGBA)])
  (case-lambda
    [(self)
     (let* ([axis-pen (plot-axis-style-pen self)]
            [axis-color (pen-color axis-pen)]
            [label-color (plot-axis-style-label-color self)]
            [axis-font (plot-axis-style-font self)]
            [label-font (or (plot-axis-style-label-font self) axis-font)])    
       (values axis-font
               (or (plot-axis-style-digit-font self) axis-font)
               label-font
               (or (plot-axis-style-desc-font self) label-font)
               axis-pen (pen-width axis-pen)
               (or (plot-axis-style-digit-color self) axis-color)
               (or (plot-axis-style-tick-color self) axis-color)
               (or label-color axis-color)
               (or (plot-axis-style-desc-color self) label-color axis-color)))]
    [(self adjust)
     (let* ([axis-pen (plot-axis-style-pen self)]
            [axis-color (pen-color axis-pen)]
            [adjusted-color (adjust axis-color)]
            [digit-color (plot-axis-style-digit-color self)]
            [tick-color (plot-axis-style-tick-color self)]
            [label-color (let ([lc (plot-axis-style-label-color self)]) (if (not lc) adjusted-color (adjust (rgb* lc))))]
            [desc-color (plot-axis-style-label-color self)]
            [axis-font (plot-axis-style-font self)]
            [label-font (or (plot-axis-style-label-font self) axis-font)])    
       (values axis-font
               (or (plot-axis-style-digit-font self) axis-font)
               label-font
               (or (plot-axis-style-desc-font self) label-font)
               (if (equal? adjusted-color axis-color) axis-pen (desc-stroke axis-pen #:color adjusted-color))
               (pen-width axis-pen)
               (if (not digit-color) adjusted-color (adjust (rgb* digit-color)))
               (if (not tick-color) adjusted-color (adjust (rgb* tick-color)))
               label-color
               (if (not desc-color) label-color (adjust (rgb* desc-color)))))]))

(define plot-axis-digit-position-values : (-> Plot-Axis-Style Symbol (Values Flonum Geo-Pin-Anchor Geo-Pin-Anchor))
  (lambda [self direction]
    (define pos-cfg (plot-axis-style-digit-position self))
    (define-values (xpos ypos) (plot-cartesian-settings pos-cfg))
    
    (if (eq? direction 'x)
        (values xpos
                (if (< xpos 0.0) 'cc 'cc)
                (if (< xpos 0.0) 'cc 'cc))
        (values (if (real? pos-cfg) (* ypos 0.5) ypos) ; given that digits in x-axis are aligned by 'cc
                (if (< ypos 0.0) 'rc 'lc)
                (if (< ypos 0.0) 'lc 'rc)))))

(define plot-axis-length-values : (case-> [Plot-Axis-Style Plot-Axis-Tip-Style Real
                                                           -> (Values Nonnegative-Flonum Nonnegative-Flonum
                                                                      Nonnegative-Flonum Nonnegative-Flonum)]
                                          [Plot-Axis-Style Plot-Axis-Tip-Style Real+% Nonnegative-Flonum
                                                           -> (Values Nonnegative-Flonum Nonnegative-Flonum
                                                                      Nonnegative-Flonum Nonnegative-Flonum)])
  (case-lambda
    [(self tip length 100%) (plot-axis-length-values* self tip (~length length 100%))]
    [(self tip length) (plot-axis-length-values* self tip (~length length))]))

(define plot-axis-height-values : (-> Plot-Axis-Style Plot-Axis-Tip-Style Nonnegative-Flonum Real
                                      (Values Nonnegative-Flonum Nonnegative-Flonum
                                              Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self tip view-width ratio]
    (define view-height : Nonnegative-Flonum (max (real->double-flonum (* view-width ratio)) 0.0))
    (define-values (neg-margin pos-margin) (plot-axis-margin-values tip view-width))

    (values (+ view-height neg-margin pos-margin (pen-width (plot-axis-style-pen self)))
            view-height neg-margin pos-margin)))

(define plot-axis-margin-values : (-> Plot-Axis-Tip-Style Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self fllength]
    (define n-margin (plot-axis-tip-style-negative-margin self))
    (define p-margin (plot-axis-tip-style-positive-margin self))
    
    (values (~length n-margin fllength)
            (~length p-margin fllength))))

(define plot-axis-length-values* : (-> Plot-Axis-Style Plot-Axis-Tip-Style Nonnegative-Flonum
                                       (Values Nonnegative-Flonum Nonnegative-Flonum
                                               Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self tip fllength]
    (define-values (neg-margin pos-margin) (plot-axis-margin-values tip fllength))

    (values fllength
            (max (- fllength neg-margin pos-margin
                    (~length (plot-axis-style-tick-thickness self)
                             (pen-width (plot-axis-style-pen self)))) 0.0)
            neg-margin pos-margin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) plot-cartesian-settings : (case-> [Complex -> (Values Flonum Flonum)]
                                                       [Complex (-> Real T) -> (Values T T)])
  (case-lambda
    [(config) (plot-cartesian-settings config real->double-flonum)]
    [(config real->datum)
     (define x (real->datum (real-part config)))
     (define y (if (real? config) x (real->datum (imag-part config))))
     
     (values x y)]))

(define plot-cartesian-maybe-settings : (case-> [Complex -> (Values Real Real)]
                                                [Complex+% -> (Values Real+% Real+%)]
                                                [(Option Complex) -> (Values (Option Real) (Option Real))]
                                                [(Option Complex+%) -> (Values (Option Real+%) (Option Real+%))])
  (case-lambda
    [(config)
     (cond [(not config) (values #false #false)]
           [(real? config) (values config config)]
           [(complex? config) (plot-cartesian-settings config)]
           [else (let-values ([(x y) (plot-cartesian-settings (car config))]
                              [(rto) (cadr config)])
                   (values (list x rto) (list y rto)))])]))

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
(define plot-axis-label-settings : (case-> [(U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text)))
                                            (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text)))
                                            Flonum Flonum Flonum Symbol
                                            -> (Listof (List (Option DC-Markup-Text) Flonum (Option DC-Markup-Text) Geo-Pin-Anchor))]
                                           [(U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text)))
                                            (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text)))
                                            Float-Complex Float-Complex
                                            -> (Listof (List (Option DC-Markup-Text) Float-Complex (Option DC-Markup-Text) Boolean))])
  (case-lambda
    [(label desc flmin flmax offset which)
     (let ([descs (if (pair? desc) desc (cons #false desc))])
       (if (pair? label)
           (list (list (car label) (- flmin offset) (car descs) (if (eq? which 'x) 'rc 'cc))
                 (list (cdr label) (+ flmax offset) (cdr descs) (if (eq? which 'x) 'lc 'cc)))
           (list (list label (+ flmax offset) (cdr descs) (if (eq? which 'x) 'lc 'cc)))))]
    [(label desc src tgt)
     (let ([descs (if (pair? desc) desc (cons #false desc))])
       (if (pair? label)
           (list (list (car label) src (car descs) #false)
                 (list (cdr label) tgt (cdr descs) #true))
           (list (list label tgt (cdr descs) #true))))]))

(define plot-screen-axis-label-adjust : (-> (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) Boolean
                                            (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))))
  (lambda [label screen?]
    (cond [(or (not screen?) (not label)) label]
          [(pair? label) (cons (cdr label) (cdr label))]
          [else (cons label #false)])))
