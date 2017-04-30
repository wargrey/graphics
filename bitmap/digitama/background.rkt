#lang typed/racket

(provide (all-defined-out))

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
            [(not (pair? hint)) (make-css-pen #:color hint #:width 1 #:style 'solid)]
            [(pair? (cdr hint)) (make-css-pen #:color (car hint) #:width (cadr hint) #:style (cddr hint))]
            [(real? (cdr hint)) (make-css-pen #:color (car hint) #:width (cdr hint) #:style 'solid)]
            [else (make-css-pen #:color (car hint) #:width 1 #:style (cdr hint))])))
  
  (define smart-brush
    (lambda [hint make-css-brush]
      (cond [(is-a? hint brush%) (make-css-brush hint)]
            [(pair? hint) (make-css-brush #:color (car hint) #:style (cdr hint))]
            [else (make-css-brush #:color hint #:style 'solid)]))))

(unsafe-require/typed
 (submod "." cheat)
 [smart-pen (All (a) (-> Pen+Color (->* () ((Instance Pen%) #:color Color+sRGB #:style Pen-Style) a) a))]
 [smart-brush (All (a) (-> Brush+Color (->* () ((Instance Brush%) #:color Color+sRGB #:style Brush-Style) a) a))])

(define generic-pen-width-map : (->* (Symbol) ((Instance Pen%)) Real)
  (lambda [width [basepen (make-pen)]]
    (cond [(eq? width 'thin) 1.0]
          [(eq? width 'medium) 3.0]
          [(eq? width 'thick) 5.0]
          [else (send basepen get-width)])))
