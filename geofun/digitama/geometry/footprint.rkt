#lang typed/racket/base

(provide (all-defined-out))

(require "bbox.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Print (Pairof Char (U Float-Complex GPath:Print False)))

(struct gpath:print
  ([end-here : Float-Complex])
  #:type-name GPath:Print #:transparent)

(struct gpp:arc gpath:print
  ([center : Float-Complex]
   [rx : Float]
   [ry : Float]
   [start : Float]
   [end : Float]
   [clockwise? : Boolean])
  #:transparent)

(struct gpp:bezier gpath:print
  ([ctrl1 : Float-Complex]
   [ctrl2 : Float-Complex])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-print-position : (->* (Geo-Path-Print) ((Option Float-Complex)) (Option Float-Complex))
  (lambda [self [curpos #false]]
    (define gpp (cdr self))
    (cond [(complex? gpp) (if (and curpos (memq (car self) '(#\m #\l))) (+ gpp curpos) gpp)]
          [(gpath:print? gpp) (gpath:print-end-here gpp)]
          [else #false])))

(define geo-path-ink-box* : (-> (Listof Geo-Path-Print)
                                (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                        Float-Complex Float-Complex))
  (lambda [footprints]
    (let detect ([lx : Flonum +inf.0]
                 [ty : Flonum +inf.0]
                 [rx : Flonum -inf.0]
                 [by : Flonum -inf.0]
                 [prints : (Listof Geo-Path-Print) footprints]
                 [home : (Option Float-Complex) #false]
                 [curpos : Float-Complex 0.0+0.0i])
      (if (pair? prints)
          (let-values ([(self rest) (values (car prints) (cdr prints))])
            ; TODO: deal with curves
            (define pt (geo-path-print-position self curpos))
            (if (or pt)
                (let-values ([(x y) (values (real-part pt) (imag-part pt))])
                  (detect (min x lx) (min y ty) (max rx x) (max by y) rest (or home pt) pt))
                (detect lx ty rx by rest home curpos)))
          (let*-values ([(w h) (values (- rx lx) (- by ty))]
                        [(x width)  (if (>= w 0.0) (values lx w) (values 0.0 0.0))]
                        [(y height) (if (>= h 0.0) (values ty h) (values 0.0 0.0))])
            (values x y width height (or home curpos) curpos))))))

(define geo-path-ink-box : (-> (Listof Geo-Path-Print) (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [footprints]
    (define-values (x y width height _s _e) (geo-path-ink-box* footprints))
    (values x y width height)))
