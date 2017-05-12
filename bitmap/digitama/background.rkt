#lang typed/racket

(provide (all-defined-out))

(require "digicore.rkt")
(require "cheat.rkt")
(require "../color.rkt")

(require "unsafe/source.rkt")

(module cheat racket/base
  (provide (all-defined-out))

  (require racket/class)
  (require racket/draw)
  
  (define smart-pen
    (lambda [hint make-css-pen]
      (cond [(is-a? hint pen%) (make-css-pen hint)]
            [(not (pair? hint)) (make-css-pen #:color hint #:width 1 #:style 'solid)]
            [(pair? (cdr hint)) (make-css-pen #:color (car hint) #:width (cadr hint) #:style (cddr hint))]
            [(real? (cdr hint)) (make-css-pen #:color (car hint) #:width (cdr hint) #:style 'solid)]
            [else (make-css-pen #:color (car hint) #:width 1 #:style (cdr hint))])))
  
  (define smart-brush
    (lambda [hint make-css-brush]
      (cond [(is-a? hint brush%) (make-css-brush hint)]
            [(pair? hint) (make-css-brush #:color (car hint) #:style (cdr hint))]
            [else (make-css-brush #:color hint #:style 'solid)]))))

(require/typed/provide
 (submod "." cheat)
 [smart-pen (All (a) (-> Pen+Color (->* () ((Instance Pen%) #:color Color+sRGB #:width Real #:style Pen-Style) a) a))]
 [smart-brush (All (a) (-> Brush+Color (->* () ((Instance Brush%) #:color Color+sRGB #:style Brush-Style) a) a))])

(define transparent-source : FlVector (flvector 0.0 0.0 0.0 0.0))
(define hilite-source : FlVector (flvector 0.0 0.0 0.0 0.3))

(define-cheat-opaque linear-gradient%? #:is-a? Linear-Gradient% linear-gradient%)
(define-cheat-opaque radial-gradient%? #:is-a? Radial-Gradient% radial-gradient%)

(define generic-pen-width-map : (->* (Symbol) ((Instance Pen%)) Real)
  (lambda [width [basepen (make-pen)]]
    (cond [(eq? width 'thin) 1.0]
          [(eq? width 'medium) 3.0]
          [(eq? width 'thick) 5.0]
          [else (send basepen get-width)])))

(define generic-brush->pattern : (-> (Instance Brush%) Bitmap-Source)
  (lambda [brush]
    (or (let ([?src (send brush get-handle)])
          (and (bitmap-surface? ?src) ?src))
        (let ([?stipple (send brush get-stipple)])
          (and ?stipple (assert (send ?stipple get-handle) bitmap-surface?)))
        (let ([?gradient (send brush get-gradient)])
          (or (and (linear-gradient%? ?gradient)
                   (call-with-values (thunk (inspect-linear-gradient ?gradient))
                     bitmap-linear-gradient))
              (and (radial-gradient%? ?gradient)
                   (call-with-values (thunk (inspect-radial-gradient ?gradient))
                     bitmap-radial-gradient))))
        (case (send brush get-style)
          [(transparent) transparent-source]
          [(hilite) hilite-source]
          [else (color->source (send brush get-color))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define inspect-linear-gradient : (-> (Instance Linear-Gradient%) (Values Real Real Real Real (Listof (Pairof Real FlVector))))
  (lambda [gradient]
    (define-values (x0 y0 x1 y1) (send gradient get-line))
    (define-values (stops) (send gradient get-stops))
    (values x0 y0 x1 y1 (map gradient-stop-normalize stops))))

(define inspect-radial-gradient : (-> (Instance Radial-Gradient%) (Values Real Real Real Real Real Real (Listof (Pairof Real FlVector))))
  (lambda [gradient]
    (define-values (x0 y0 r0 x1 y1 r1) (send gradient get-circles))
    (define-values (stops) (send gradient get-stops))
    (values x0 y0 r0 x1 y1 r1 (map gradient-stop-normalize stops))))

(define gradient-stop-normalize : (-> (List Real (Instance Color%)) (Pairof Real FlVector))
  (lambda [stop]
    (cons (car stop)
          (color->source (cadr stop)))))
