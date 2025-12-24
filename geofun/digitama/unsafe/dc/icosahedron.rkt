#lang typed/racket/base

(provide (all-defined-out))

(require "../paint.rkt")
(require "../typed/cairo.rkt")
(require "../../geometry/radius.rkt")
(require "../../geometry/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Decagon-Vertices
  (Vector Float-Complex Float-Complex Float-Complex Float-Complex Float-Complex
          Float-Complex Float-Complex Float-Complex Float-Complex Float-Complex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_icosahedron_side_proj : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) (Option Pen) Any)
  (lambda [cr x0 y0 flwidth flheight edge-stroke background border-stroke]
    (define-values (+a1 +aφ) (icosahedron-side-outline-size->outline flwidth))
    (define-values (-a1 -aφ) (values (- +a1) (- +aφ)))

    (cairo_translate cr (+ x0 +aφ) (+ y0 +aφ))
    
    ;;; fill the shape borders
    (icosahedron-side-border-path cr +a1 +aφ -a1 -aφ)
    (unless (not background)
      (cairo-render-with-fill cr background))

    ;;; draw inside edges
    (unless (not edge-stroke)
      (cairo_new_path cr)
      (cairo-add-line cr -aφ -a1 +aφ -a1)
      (cairo-add-line cr +aφ +a1 -aφ +a1)
      (cairo-add-line cr 0.0 -aφ -a1 -a1 -aφ +a1)
      (cairo-add-line cr 0.0 -aφ 0.0 -a1 -a1 +a1)
      (cairo-add-line cr 0.0 -aφ +a1 -a1 +aφ +a1)
      (cairo-add-line cr 0.0 +aφ -a1 +a1 -aφ -a1)
      (cairo-add-line cr 0.0 +aφ 0.0 +a1 +a1 -a1)
      (cairo-add-line cr 0.0 +aφ +a1 +a1 +aφ -a1)
      (cairo-add-line cr 0.0 -a1 +a1 +a1)
      (cairo-add-line cr 0.0 +a1 -a1 -a1)
      (cairo-render-with-stroke cr edge-stroke))

    ;;; draw the border
    (when (or border-stroke edge-stroke)
      (cairo_new_path cr)
      (icosahedron-side-border-path cr +a1 +aφ -a1 -aφ)
      (cairo-render-with-stroke cr (or border-stroke edge-stroke)))))

(define dc_icosahedron_over_proj : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum (Option Pen) (Option Brush) (Option Pen) Any)
  (lambda [cr x0 y0 flwidth flheight rotation edge background border0]
    (define border (or border0 edge))
    (define delta (* 2pi 0.10))
    (define R (* flwidth 0.5))
    (define vts : Decagon-Vertices
      (vector 0.0+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i
              0.0+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i 0.0+0.0i))
    
    (let collect-vectices ([idx 0]
                           [theta rotation])
      (when (< idx 10)
        (vector-set! vts idx (make-polar R theta))
        (collect-vectices (+ idx 1) (+ theta delta))))

    (cairo_translate cr (+ x0 R) (+ y0 R))

    ;;; fill the shape
    (icosahedron-over-border-path cr vts)
    (unless (not background)
      (cairo-render-with-fill cr background))

    ;; draw edges inside
    (unless (not edge)
      (cairo_new_path cr)
      
      (for ([vt (in-vector vts)])
        (cairo-add-line cr 0.0 0.0 (real-part vt) (imag-part vt)))

      (cairo-add-line cr (vector-ref vts 8) (vector-ref vts 0))
      (cairo-add-line cr (vector-ref vts 9) (vector-ref vts 1))
      (for ([cidx (in-range 2 10)])
        (define pidx (- cidx 2))
        (cairo-add-line cr (vector-ref vts pidx) (vector-ref vts cidx)))
      (cairo-render-with-stroke cr edge))

    ;; draw the border
    (unless (not border)
      (cairo_new_path cr)
      (icosahedron-over-border-path cr vts)
      (cairo-render-with-stroke cr border))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define icosahedron-side-border-path : (-> Cairo-Ctx Flonum Flonum Flonum Flonum Void)
  (lambda [cr +a1 +aφ -a1 -aφ]
    (cairo_move_to cr -aφ 0.0)
    (cairo_line_to cr -aφ +a1)
    (cairo_line_to cr 0.0 +aφ)
    (cairo_line_to cr +aφ +a1)
    (cairo_line_to cr +aφ -a1)
    (cairo_line_to cr 0.0 -aφ)
    (cairo_line_to cr -aφ -a1)
    (cairo_close_path cr)))

(define icosahedron-over-border-path : (-> Cairo-Ctx Decagon-Vertices Void)
  (lambda [cr vts]
    (for ([vt (in-vector vts)])
      (cairo_line_to cr (real-part vt) (imag-part vt)))
    (cairo_close_path cr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define icosahedron-edge-length->circumsphere-radius : (-> Nonnegative-Flonum Nonnegative-Flonum)
  (lambda [a]
    (* a sin72º)))

(define icosahedron-radius->edge-length : (-> Nonnegative-Flonum 3D-Radius-Type Nonnegative-Flonum)
  (lambda [r type]
    (cond [(eq? type 'edge)   (* r 2.0 1/phi)] ; midsphere    R = a•φ/2
          [(eq? type 'face)   (* r 2√3 1/φ²)]  ; insphere     R = a•φ²/(2√3)
          [(eq? type 'vertex) (* r 1/sin72º)]  ; circumsphere R = a•sin72º = a•√(φ²+1)/2
          [else '#:deadcode r])))

(define icosahedron-radius->circumsphere-radius : (-> Nonnegative-Flonum 3D-Radius-Type Nonnegative-Flonum)
  (lambda [r type]
    (cond [(eq? type 'vertex) r]
          [else (icosahedron-edge-length->circumsphere-radius
                 (icosahedron-radius->edge-length r type))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define icosahedron-edge-length->outline : (-> Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [a]
    (define a1 (* a 0.5))
    (define aφ (* a1 phi))
    
    #;(define vertices
        (vector (pos 0 +1 +φ) (pos 0 +1 -φ)
                (pos 0 -1 +φ) (pos 0 -1 -φ)
                
                (pos +φ 0 +1) (pos +φ 0 -1)
                (pos -φ 0 +1) (pos -φ 0 -1)
                
                (pos +1 +φ 0) (pos +1 -φ 0)
                (pos -1 +φ 0) (pos -1 -φ 0)))
    
    (values a1 aφ)))

(define icosahedron-side-outline-size->outline : (-> Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [fllength]
    (define aφ (* fllength 0.5))
    (values (* aφ 1/phi) aφ)))

(define icosahedron-edge-length->side-outline-size : (-> Nonnegative-Flonum Nonnegative-Flonum)
  (lambda [a]
    (define-values (a1 aφ) (icosahedron-edge-length->outline a))
    (* aφ 2.0)))
