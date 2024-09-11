#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Clean-Print-Datum (U Float-Complex GPath:Print))
(define-type Geo-Path-Print-Datum (U Geo-Path-Clean-Print-Datum False))

(define-type Geo-Path-Print (Pairof Char Geo-Path-Print-Datum))
(define-type Geo-Path-Clean-Print (Pairof Char Geo-Path-Clean-Print-Datum))

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
(define geo-path-clean-print-position : (-> Geo-Path-Clean-Print Float-Complex)
  (lambda [self]
    (define gpp (cdr self))
    (cond [(complex? gpp) gpp]
          [else (gpath:print-end-here gpp)])))

(define geo-path-print-position : (->* (Geo-Path-Print) ((Option Float-Complex)) (Option Float-Complex))
  (lambda [self [curpos #false]]
    (define gpp (cdr self))
    (cond [(complex? gpp) (if (and curpos (memq (car self) '(#\m #\l))) (+ gpp curpos) gpp)]
          [(gpath:print? gpp) (gpath:print-end-here gpp)]
          [else #false])))

(define geo-path-cleanse : (-> (Listof Geo-Path-Print) (Listof Geo-Path-Clean-Print))
  (lambda [footprints]
    (let traverse ([prints : (Listof Geo-Path-Print) footprints]
                   [start : (Option Float-Complex) #false]
                   [stnirp : (Listof Geo-Path-Clean-Print) null]
                   [curpos : (Option Float-Complex) #false])
      (if (pair? prints)
          ; TODO: deal with curves
          (let*-values ([(self rest) (values (car prints) (cdr prints))]
                        [(op print) (values (car self) (cdr self))])
            (define pt (geo-path-print-position self curpos))
            (cond [(or pt)
                   (case op
                     [(#\M #\m) (traverse rest pt (cons (cons #\M pt) stnirp) pt)]
                     [(#\L #\l) (traverse rest pt (cons (cons (if (not start) #\M #\L) pt) stnirp) pt)]
                     [else (traverse rest (or start pt) (if (gpath:print? print) (cons self stnirp) stnirp) pt)])]
                  [(or start) #; #:close-the-path (traverse rest #false (cons (cons #\L start) stnirp) pt)]
                  [else #;#:ignore (traverse rest #false stnirp pt)]))
          (reverse stnirp)))))

(define geo-path-end-points
  : (case-> [(List* Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print)) -> (Values Float-Complex Flonum Float-Complex Flonum)]
            [(Listof Geo-Path-Clean-Print) -> (Values (Option Float-Complex) Flonum (Option Float-Complex) Flonum)])
  (lambda [footprints]
    (if (and (pair? footprints) (pair? (cdr footprints)))
        (let ([h1st (geo-path-clean-print-position (car footprints))]
              [h2nd (geo-path-clean-print-position (cadr footprints))])
          (let traverse ([t2nd : Float-Complex h1st]
                         [t1st : Float-Complex h2nd]
                         [prints : (Listof Geo-Path-Clean-Print) (cddr footprints)])
            (if (pair? prints)
                (traverse t1st (geo-path-clean-print-position (car prints)) (cdr prints))
                (values h1st (angle (- h2nd h1st)) t1st (angle (- t1st t2nd))))))
        (values #false 0.0 #false 0.0))))

(define geo-path-ink-box : (-> (Listof Geo-Path-Clean-Print) (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [footprints]
    (let traverse ([lx : Flonum +inf.0]
                   [ty : Flonum +inf.0]
                   [rx : Flonum -inf.0]
                   [by : Flonum -inf.0]
                   [prints : (Listof Geo-Path-Clean-Print) footprints])
      (if (pair? prints)
          ; TODO: deal with curves
          (let*-values ([(self rest) (values (car prints) (cdr prints))]
                        [(pt) (geo-path-clean-print-position self)]
                        [(x y) (values (real-part pt) (imag-part pt))])
            (traverse (min x lx) (min y ty) (max rx x) (max by y) rest))
          (let*-values ([(w h) (values (- rx lx) (- by ty))]
                        [(x width)  (if (>= w 0.0) (values lx w) (values 0.0 0.0))]
                        [(y height) (if (>= h 0.0) (values ty h) (values 0.0 0.0))])
            (values x y width height))))))
