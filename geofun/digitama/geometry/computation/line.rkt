#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require digimon/sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Intersection (Immutable-Vector Float-Complex Flonum Flonum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-line-line-perpendicular? : (-> Float-Complex Float-Complex Float-Complex Float-Complex Boolean)
  (lambda [A B C D]
    (zero? (real-part (/ (- B A)
                         (- D C))))))

(define geo-line-line-parallel? : (-> Float-Complex Float-Complex Float-Complex Float-Complex Boolean)
  (lambda [A B C D]
    (zero? (imag-part (* (- B A)
                         (conjugate (- D C)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-line-line-intersect : (-> Float-Complex Float-Complex Float-Complex Float-Complex (Option Geo-Intersection))
  (lambda [A B C D]
    #| Theorem
       In Euclidean Vector Space, as well as in Complex Plane,
       A line can be represented as L = P0 + tV,
       the interval of parameter `t` can be used to detect the type of line.
          More precisely, L is:
            an infinitely long line if t <- (-inf, +inf);
            a line segment if t <- [0, 1];
            a ray if t <- [0, +inf).
         
          a). L1 = A + t1(B - A) = A + t1 * Vab
          b). L2 = C + t2(D - C) = C + t2 * Vcd
           ==> t1 = [V2y * (x21 - x11) - V2x * (y21 - y11)] / (V1x * V2y - V1y * V2x)
               t2 = [V1y * (x21 - x11) - V1x * (y21 - y11)] / (V1x * V2y - V1y * V2x)
           <=> t1 = Im(Vcd * ~ac) / Im(~ab * Vcd)
               t2 = Im(Vab * ~ac) / Im(~ab * Vcd)
     |#

    (define-values (Vab Vcd Vac) (values (- B A) (- D C) (- C A)))
    (define ~ab : Float-Complex (conjugate Vab))
    (define ~ac : Float-Complex (conjugate Vac))
    (define denominator : Flonum (imag-part (* ~ab Vcd)))

    (and (not (zero? denominator))
         (let ([t1 (/ (imag-part (* Vcd ~ac)) denominator)]
               [t2 (/ (imag-part (* Vab ~ac)) denominator)])
           (vector-immutable (+ A (* t1 Vab)) #| or (+ C (* t1 Vcd)) |#
                             t1 t2)))))

(define geo-perpendicular-point : (->* (Float-Complex Float-Complex Flonum) (Flonum) Float-Complex)
  ;;; find the |d|-distance perpendicular point of the direction vector v located at A,
  ;     the resulting point should be on the left side(d > 0.0) or right side(d < 0.0) of the vector,
  ;     and its foot is blended by `t` with respect to A.
  (lambda [A v d [t 0.0]]
    (define Pv : Float-Complex
      (* (/ v (magnitude v))
         (make-polar d (* pi -0.5))))
    
    (+ A Pv (* v t))))

(define geo-parallel-segment : (-> Float-Complex Float-Complex Flonum (Values Float-Complex Float-Complex))
  ;;; find the |d|-distance segment of segment AB
  ;     the resulting segment should be on the left side(d > 0.0) or right side(d < 0.0) of AB
  (lambda [A B d]
    (define V : Float-Complex (- B A))
    (define P : Float-Complex (geo-perpendicular-point A V d))
    
    (values P (+ P V))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-line-polyline-intersect : (->* (Float-Complex Float-Complex (Listof Float-Complex)) (Boolean #:translate Float-Complex) (Listof Geo-Intersection))
  (lambda [A B vertices [closed? #true] #:translate [offset 0.0+0.0i]]
    (if (and (pair? vertices) (pair? (cdr vertices)))
        (let ([head (+ (car vertices) offset)])
          (let intersect ([prev : Float-Complex head]
                          [vtcs : (Listof Float-Complex) (cdr vertices)]
                          [inter-pts : (Listof Geo-Intersection) null])
            (if (pair? vtcs)
                (let ([vtx (+ (car vtcs) offset)])
                  (intersect vtx (cdr vtcs)
                             (?cons (geo-line-line-intersect A B prev vtx) inter-pts)))
                (reverse (cond [(not closed?) inter-pts]
                               [else (?cons (geo-line-line-intersect A B prev head) inter-pts)])))))
        null)))

(define geo-line-polygon-intersect : (->* (Float-Complex Float-Complex (Listof Float-Complex)) (#:translate Float-Complex) (Listof Float-Complex))
  (lambda [A B vertices #:translate [offset 0.0+0.0i]]
    (if (and (pair? vertices) (pair? (cdr vertices)))
        (let ([head (+ (car vertices) offset)])
          (let intersect ([prev : Float-Complex head]
                          [vtcs : (Listof Float-Complex) (cdr vertices)]
                          [inter-pts : (Listof Geo-Intersection) null])
            (if (pair? vtcs)
                (let ([vtx (+ (car vtcs) offset)])
                  (intersect vtx (cdr vtcs)
                             (?cons (geo-line-line-intersect A B prev vtx) inter-pts)))
                (let filter-reverse ([inter-pts : (Listof Geo-Intersection) (?cons (geo-line-line-intersect A B prev head) inter-pts)]
                                     [interpts : (Listof Float-Complex) null])
                  (if (pair? inter-pts)
                      (let ([pt (geo-actually-intersected-point (car inter-pts))])
                        (filter-reverse (cdr inter-pts)
                                        (if (and pt (not (member pt interpts)))
                                            (cons pt interpts)
                                            interpts)))
                      interpts)))))
        null)))

(define geo-line-polygon-intersect/first : (->* (Float-Complex Float-Complex (List* Float-Complex Float-Complex (Listof Float-Complex)))
                                                (#:translate Float-Complex)
                                                (Option Float-Complex))
  (lambda [A B vertices #:translate [offset 0.0+0.0i]]
    (define head (+ (car vertices) offset))
    
    (let intersect ([prev : Float-Complex head]
                    [vtcs : (Listof Float-Complex) (cdr vertices)])
      (if (pair? vtcs)
          (let ([vtx (+ (car vtcs) offset)])
            (or (geo-actually-intersected-point (geo-line-line-intersect A B prev vtx))
                (intersect vtx (cdr vtcs))))
          (geo-actually-intersected-point
           (geo-line-line-intersect A B prev head))))))

(define geo-line-polygon-intersect/codir-first : (->* (Float-Complex Float-Complex (List* Float-Complex Float-Complex (Listof Float-Complex)))
                                                      (#:translate Float-Complex)
                                                      (Option Float-Complex))
  (lambda [A B vertices #:translate [offset 0.0+0.0i]]
    (define head (+ (car vertices) offset))

    (let intersect ([prev : Float-Complex head]
                    [vtcs : (Listof Float-Complex) (cdr vertices)])
      (if (pair? vtcs)
          (let ([vtx (+ (car vtcs) offset)])
            (or (geo-codir-intersected-point (geo-line-line-intersect A B prev vtx))
                (intersect vtx (cdr vtcs))))
          (geo-codir-intersected-point
           (geo-line-line-intersect A B prev head))))))

(define geo-line-polygon-intersect/first* : (->* (Float-Complex Float-Complex (List* Float-Complex Float-Complex (Listof Float-Complex)))
                                                 (#:translate Float-Complex)
                                                 (Option Float-Complex))
  (lambda [A B vertices #:translate [offset 0.0+0.0i]]
    (or (geo-line-polygon-intersect/first A B vertices #:translate offset)
        (geo-line-polygon-intersect/codir-first A B vertices #:translate offset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-actually-intersected? : (-> Geo-Intersection Boolean)
  (lambda [intersection]
    (and (<= 0.0 (vector-ref intersection 1) 1.0)
         (<= 0.0 (vector-ref intersection 2) 1.0))))

(define geo-codir-intersected? : (-> Geo-Intersection Boolean)
  (lambda [intersection]
    (and (>= (vector-ref intersection 1) 0.0)
         (<= 0.0 (vector-ref intersection 2) 1.0))))

(define geo-actually-intersected-point : (-> (Option Geo-Intersection) (Option Float-Complex))
  (lambda [intersection]
    (and intersection
         (geo-actually-intersected? intersection)
         (vector-ref intersection 0))))

(define geo-codir-intersected-point : (-> (Option Geo-Intersection) (Option Float-Complex))
  (lambda [intersection]
    (and intersection
         (geo-codir-intersected? intersection)
         (vector-ref intersection 0))))
