#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../../base.rkt")
(require "../source.rkt")
(require "../visual/ctype.rkt")

(require "../../geometry/radius.rkt")
(require "../../geometry/constants.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/flonum)
  
  (require "../../geometry/constants.rkt")
  (require "../pangocairo.rkt")
  (require "../paint.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_icosahedron_side_proj create-surface edge-size edge-stroke background border0 density)
    (define-values (a1 aφ) (icosahedron-edge-length->outline edge-size))
    (define fllength (unsafe-fl* aφ 2.0))
    (define-values (sfc cr) (create-surface fllength fllength density #true))
    (define border (or border0 edge-stroke))
    (define offset (~bdwidth border))
    (define-values (-a1 +a1) (values (unsafe-fl+ (unsafe-fl- 0.0 a1) offset) (unsafe-fl- a1 offset)))
    (define-values (-aφ +aφ) (values (unsafe-fl+ (unsafe-fl- 0.0 aφ) offset) (unsafe-fl- aφ offset)))

    (cairo_translate cr aφ aφ)
    
    ;;; fill the shape borders
    (icosahedron-side-border-path cr +a1 +aφ -a1 -aφ)
    (unless (not background)
      (cairo-render-with-fill cr background))

    ;;; draw edges inside
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
    (unless (not border)
      (cairo_new_path cr)
      (icosahedron-side-border-path cr +a1 +aφ -a1 -aφ)
      (cairo-render-with-stroke cr border))
      
    (cairo_destroy cr)
    
    sfc)

  (define (dc_icosahedron_over_proj create-surface radius rotation edge background border0 density)
    (define fllength (unsafe-fl* radius 2.0))
    (define-values (sfc cr) (create-surface fllength fllength density #true))
    (define border (or border0 edge))
    (define delta (unsafe-fl* 2pi 0.10))
    (define offset (~bdwidth border))
    (define R (unsafe-fl- radius offset))
    (define xs (make-flvector 10))
    (define ys (make-flvector 10))
    
    (let collect-vectices ([idx 0]
                           [theta rotation])
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
    (unless (not border)
      (cairo_new_path cr)
      (icosahedron-over-border-path cr xs ys)
      (cairo-render-with-stroke cr border))
    
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
  (define icosahedron-edge-length->outline
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
      
      (values a1 aφ)))

  (define icosahedron-edge-length->side-outline-size
    (lambda [a]
      (define-values (a1 aφ) (icosahedron-edge-length->outline a))
      (unsafe-fl* aφ 2.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [icosahedron-edge-length->side-outline-size (-> Nonnegative-Flonum Nonnegative-Flonum)]
 [dc_icosahedron_side_proj
  (All (S) (-> (Cairo-Surface-Create S)
               Nonnegative-Flonum (Option Paint) (Option Fill-Source) (Option Paint)
               Flonum S))]
 [dc_icosahedron_over_proj
  (All (S) (-> (Cairo-Surface-Create S)
               Nonnegative-Flonum Flonum (Option Paint) (Option Fill-Source) (Option Paint)
               Flonum S))])

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
