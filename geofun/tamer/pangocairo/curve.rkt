#lang racket

(require racket/draw/unsafe/cairo)
(require bitmap/digitama/convert)

;;; https://www.cairographics.org/samples/curve_to/
;;; https://pomax.github.io/bezierinfo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define density 2.0)
(define-values (curve-width line-width) (values 2.0 1.0))

(define (cairo-linear-curve p1 p2)
  (define-values (x1 y1) (values (real-part p1) (imag-part p1)))
  (define-values (x2 y2) (values (real-part p2) (imag-part p2)))
  (define-values (width height) (values (+ (max x1 x2) line-width) (+ (max y1 y2) line-width)))
  (define-values (bmp cr) (create-argb-bitmap width height density #true))

  (cairo_move_to cr x1 y1)
  (cairo_line_to cr x2 y2)
  (cairo_set_source_rgba cr 1.0 0.2 0.2 0.6)
  (cairo_set_line_width cr line-width)
  (cairo_stroke cr)
  
  bmp)

(define (cairo-quadratic-curve p1 p2 p3)
  (cairo-cubic-curve p1 (/ (+ p1 p2 p2) 3) (/ (+ p3 p2 p2) 3) p3))

(define (cairo-cubic-curve p1 p2 p3 p4)
  (define-values (x1 y1) (values (real-part p1) (imag-part p1)))
  (define-values (x2 y2) (values (real-part p2) (imag-part p2)))
  (define-values (x3 y3) (values (real-part p3) (imag-part p3)))
  (define-values (x4 y4) (values (real-part p4) (imag-part p4)))
  (define-values (x0 xm) (values (min x1 x2 x3 x4) (max x1 x2 x3 x4)))
  (define-values (y0 ym) (values (min y1 y2 y3 y4) (max y1 y2 y3 y4)))
  (define width (+ xm (max curve-width line-width)))
  (define height (+ ym (max curve-width line-width)))
  (define-values (bmp cr) (create-argb-bitmap width height density #true))

  (cairo_move_to cr x1 y1)
  (cairo_curve_to cr x2 y2 x3 y3 x4 y4)
  (cairo_set_line_width cr curve-width)
  (cairo_stroke cr)
  
  (cairo_move_to cr x1 y1)
  (cairo_line_to cr x2 y2)
  (cairo_line_to cr x3 y3)
  (cairo_line_to cr x4 y4)
  (cairo_set_source_rgba cr 1.0 0.2 0.2 0.6)
  (cairo_set_line_width cr line-width)
  (cairo_stroke cr)
  
  bmp)

(define cairo-bezier
  (case-lambda
    [(p) (cairo-linear-curve p p)]
    [(p1 p2) (cairo-linear-curve p1 p2)]
    [(p1 p2 p3) (cairo-quadratic-curve p1 p2 p3)]
    [(p1 p2 p3 p4) (cairo-cubic-curve p1 p2 p3 p4)]
    [(p1 p2 p3 p4 . rest)
     (let derive ([points (list* p1 p2 p3 p4 rest)]
                  [n:next (+ (length rest) 3) #;(sub1 (length points))])
       (cond [(= n:next 3) (cairo-cubic-curve (car points) (cadr points) (caddr points) (cadddr points))]
             [else (derive (for/list ([idx (in-range n:next)])
                             (* n:next
                                (- (list-ref points (add1 idx))
                                   (list-ref points idx))))
                           (sub1 n:next))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (cairo-bezier 25.0+128.0i)
  (cairo-bezier 25.0+128.0i  230.4+128.0i)
  (cairo-bezier 100.4+75.4i  20.0+110.0i  70.0+155.0i)
  (cairo-bezier 25.0+128.0i  102.4+230.4i 153.6+25.6i  230.4+128.0i)
  (cairo-bezier 60.0+105.0i  75.0+30.0i   215.0+115.0i 140.0+160.0i)
  (cairo-bezier 120.0+160.0i 35.0+200.0i  220.0+260.0i 220.0+40.0i)
  
  ;;; the bitmap size is overflow
  #;(cairo-bezier 198.0+18.0i  34.0+57.0i   18.0+156.0i  221.0+90.0i
                  186.0+177.0i 14.0+82.0i   12.0+236.0i  45.0+290.0i
                  218.0+294.0i 248.0+188.0i))
  