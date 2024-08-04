#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../../base.rkt")
(require "../source.rkt")
(require "../surface/type.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/flonum)
  
  (require "../../constants.rkt")
  (require "../pangocairo.rkt")
  (require "../paint.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_icosahedron_side_proj create-surface radius type edge background border0 density)
    (define edge-size (icosahedron-radius->edge-length radius type))
    (define-values (a1 aφ) (icoashedron-edge-length->outline edge-size))
    (define fllength (unsafe-fl* aφ 2.0))
    (define-values (sfc cr) (create-surface fllength fllength density #true))
    (define border (or border0 edge))
    (define offset (if (struct? border) (unsafe-fl* 0.5 (unsafe-struct-ref border 1)) 0.0))
    (define-values (-a1 +a1) (values (unsafe-fl+ (unsafe-fl- 0.0 a1) offset) (unsafe-fl- a1 offset)))
    (define-values (-aφ +aφ) (values (unsafe-fl+ (unsafe-fl- 0.0 aφ) offset) (unsafe-fl- aφ offset)))

    (cairo_translate cr aφ aφ)
    
    ;;; fill the shape borders
    (icosahedron-side-border-path cr +a1 +aφ -a1 -aφ)
    (unless (not background)
      (cairo-render-with-fill cr background))

    ;;; draw edges inside
    (unless (not edge)
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
      (cairo-render-with-stroke cr edge))

    ;;; draw the border
    (cairo_new_path cr)
    (icosahedron-side-border-path cr +a1 +aφ -a1 -aφ)
    (cairo-render-with-stroke cr border)
    
    (cairo_destroy cr)
    
    sfc)

  (define (dc_icosahedron_over_proj create-surface radius0 type rotation edge background border0 radian? density)
    (define radius (icosahedron-radius->circumsphere-radius radius0 type))
    (define fllength (unsafe-fl* radius 2.0))
    (define-values (sfc cr) (create-surface fllength fllength density #true))
    (define border (or border0 edge))
    (define delta (unsafe-fl/ 2pi 10.0))
    (define offset (if (struct? border) (unsafe-fl* 0.5 (unsafe-struct-ref border 1)) 0.0))
    (define R (unsafe-fl- radius offset))
    (define xs (make-flvector 10))
    (define ys (make-flvector 10))
    
    (let collect-vectices ([idx 0]
                           [theta (if (not radian?) (~radian rotation) rotation)])
      (when (unsafe-fx< idx 10)
        (unsafe-flvector-set! xs idx (unsafe-fl* R (unsafe-flcos theta)))
        (unsafe-flvector-set! ys idx (unsafe-fl* R (unsafe-flsin theta)))
        (collect-vectices (unsafe-fx+ idx 1) (unsafe-fl+ theta delta))))

    (cairo_translate cr radius radius)

    ;;; fill the shape
    (icosahedron-over-border-path cr xs ys)
    (unless (not background)
      (cairo-render-with-fill cr background))

    ;; draw edges inside
    (unless (not edge)
      (cairo_new_path cr)
      
      (for ([x (in-flvector xs)]
            [y (in-flvector ys)])
       (cairo-add-line cr 0.0 0.0 x y))

      (cairo-add-line cr (unsafe-flvector-ref xs 8) (unsafe-flvector-ref ys 8) (unsafe-flvector-ref xs 0) (unsafe-flvector-ref ys 0))
      (cairo-add-line cr (unsafe-flvector-ref xs 9) (unsafe-flvector-ref ys 9) (unsafe-flvector-ref xs 1) (unsafe-flvector-ref ys 1))
      (for ([cidx (in-range 2 10)])
        (define pidx (unsafe-fx- cidx 2))
        (cairo-add-line cr (unsafe-flvector-ref xs pidx) (unsafe-flvector-ref ys pidx) (unsafe-flvector-ref xs cidx) (unsafe-flvector-ref ys cidx)))
      (cairo-render-with-stroke cr edge))

    ;; draw the border
    (cairo_new_path cr)
    (icosahedron-over-border-path cr xs ys)
    (cairo-render-with-stroke cr border)
    
    (cairo_destroy cr)
    
    sfc)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define icosahedron-side-border-path
    (lambda [cr +a1 +aφ -a1 -aφ]
      (cairo_move_to cr -aφ 0.0)
      (cairo_line_to cr -aφ +a1)
      (cairo_line_to cr 0.0 +aφ)
      (cairo_line_to cr +aφ +a1)
      (cairo_line_to cr +aφ -a1)
      (cairo_line_to cr 0.0 -aφ)
      (cairo_line_to cr -aφ -a1)
      (cairo_close_path cr)))

  (define icosahedron-over-border-path
    (lambda [cr xs ys]
      (for ([x (in-flvector xs)]
            [y (in-flvector ys)])
       (cairo_line_to cr x y))
      (cairo_close_path cr)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define 2√3 (unsafe-fl* 2.0 (unsafe-flsqrt 3.0)))
  (define φ² (unsafe-fl* phi phi))
  (define sin72º (unsafe-flsin (unsafe-fl* 2pi 0.2)))
  
  (define icosahedron-edge-length->circumsphere-radius
    (lambda [a]
      (unsafe-fl* a sin72º)))
  
  (define icosahedron-radius->edge-length
    (lambda [r type]
      (cond [(eq? type 'edge)   (unsafe-fl/ (unsafe-fl* r 2.0) phi)]    ; midsphere    R = a•φ/2
            [(eq? type 'face)   (unsafe-fl/ (unsafe-fl* r 2√3) φ²)]     ; insphere     R = a•φ²/(2√3)
            [(eq? type 'vertex) (unsafe-fl/ r sin72º)] ; circumsphere R = a•sin72º = a•√(φ²+1)/2
            [else '#:deadcode r])))

  (define icosahedron-radius->circumsphere-radius
    (lambda [r type]
      (cond [(eq? type 'vertex) r]
            [else (icosahedron-edge-length->circumsphere-radius
                   (icosahedron-radius->edge-length r type))])))

  (define icoashedron-edge-length->outline
    (lambda [a]
      (define a1 (unsafe-fl* a 0.5))
      (define aφ (unsafe-fl* a1 phi))

      #;(define vertices
          (vector (pos 0 +1 +φ) (pos 0 +1 -φ)
                  (pos 0 -1 +φ) (pos 0 -1 -φ)
                  
                  (pos +φ 0 +1) (pos +φ 0 -1)
                  (pos -φ 0 +1) (pos -φ 0 -1)
                  
                  (pos +1 +φ 0) (pos +1 -φ 0)
                  (pos -1 +φ 0) (pos -1 -φ 0)))
      
      (values a1 aφ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type 3D-Radius-Type (U 'face 'vertex 'edge))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_icosahedron_side_proj (All (S) (-> (Cairo-Surface-Create S) Flonum 3D-Radius-Type (Option Paint) (Option Fill-Source) (Option Paint) Flonum S))]
 [dc_icosahedron_over_proj (All (S) (-> (Cairo-Surface-Create S) Flonum 3D-Radius-Type Flonum (Option Paint) (Option Fill-Source) (Option Paint) Boolean Flonum S))])
