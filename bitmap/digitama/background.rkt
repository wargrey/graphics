#lang typed/racket

(provide smart-pen)
(provide smart-brush)

(require typed/racket/unsafe)

(require "digicore.rkt")

(module cheat racket/base
  (provide (all-defined-out))

  (require racket/class)
  (require racket/draw)
  
  (define smart-pen
    (lambda [hint make-css-pen]
      (cond [(is-a? hint pen%) (make-css-pen hint)]
            [(pair? hint) (make-css-pen #:color (car hint) #:style (cdr hint))]
            [(list? hint) (make-css-pen #:color (car hint) #:width (cadr hint) #:style (caddr hint))]
            [else (make-css-pen #:color hint)])))
  
  (define smart-brush
    (lambda [hint make-css-brush]
      (cond [(is-a? hint brush%) (make-css-brush hint)]
            [(pair? hint) (make-css-brush #:color (car hint) #:style (cdr hint))]
            [else (make-css-brush #:color hint)]))))

(unsafe-require/typed
 (submod "." cheat)
 [smart-pen (All (a) (-> Pen+Color (->* () ((Instance Pen%) #:color Color+sRGB #:style Pen-Style) a) a))]
 [smart-brush (All (a) (-> Brush+Color (->* () ((Instance Brush%) #:color Color+sRGB #:style Brush-Style) a) a))])
