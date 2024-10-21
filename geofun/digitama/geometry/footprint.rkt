#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Prints (Listof GPath:Datum))
(define-type Geo-Path-Clean-Prints (Listof GPath:Print))
(define-type Geo-Path-Clean-Prints+ (List* GPath:Print GPath:Print Geo-Path-Clean-Prints))

(struct gpath:datum
  ([cmd : Char])
  #:type-name GPath:Datum
  #:transparent)

(struct gpp:close gpath:datum () #:type-name GPP:Close #:transparent)

(struct gpath:print gpath:datum
  ([end-here : Float-Complex])
  #:type-name GPath:Print
  #:transparent)

(struct gpp:point gpath:print () #:type-name GPP:Point #:transparent)
(struct gpp:vector gpath:print () #:type-name GPP:Vector #:transparent)

(struct gpp:arc gpath:print
  ([center : Float-Complex]
   [rx : Nonnegative-Flonum]
   [ry : Nonnegative-Flonum]
   [start : Float]
   [end : Float]
   [clockwise? : Boolean])
  #:type-name GPP:Arc
  #:transparent)

(struct gpp:bezier gpath:print
  ([ctrl1 : Float-Complex]
   [ctrl2 : Float-Complex])
  #:type-name GPP:Bezier
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-clean-print-position : (-> GPath:Print Float-Complex)
  (lambda [self]
    (gpath:print-end-here self)))

(define geo-path-print-position : (->* (GPath:Datum) ((Option Float-Complex)) (Option Float-Complex))
  (lambda [self [curpos #false]]
    (and (gpath:print? self)
         (if (gpp:vector? self)
             (let ([pos (gpath:print-end-here self)])
               (if (and curpos (memq (gpath:datum-cmd self) '(#\m #\l)))
                   (+ pos curpos)
                   pos))
             (gpath:print-end-here self)))))

(define geo-path-cleanse : (->* ((Listof GPath:Datum)) (Float-Complex) (Listof GPath:Print))
  (lambda [footprints [pos0 0.0+0.0i]]
    (let traverse ([prints : (Listof GPath:Datum) footprints]
                   [path0 : (Option Float-Complex) #false]
                   [stnirp : (Listof GPath:Print) null]
                   [curpos : Float-Complex pos0])
      (if (pair? prints)
          ; TODO: deal with curves
          (let-values ([(self rest) (values (car prints) (cdr prints))])
            (cond [(gpp:close? self)
                   (if (and path0)
                       (traverse rest #false (cons (gpp:point #\L path0) stnirp) path0)
                       (traverse rest path0 stnirp curpos))]
                  [(gpp:vector? self)
                   (let ([cmd (gpath:datum-cmd self)]
                         [pos (+ curpos (gpath:print-end-here self))])
                     (cond [(eq? cmd #\m) (traverse rest pos (cons (gpp:point #\M pos) stnirp) pos)]
                           [(not path0) (traverse rest pos (cons (gpp:point #\M pos) stnirp) pos)]
                           [else (traverse rest path0 (cons (gpp:point #\L pos) stnirp) pos)]))]
                  [(gpath:print? self) (traverse rest path0 (cons self stnirp) (gpath:print-end-here self))]
                  [else '#:deadcode (traverse rest path0 stnirp curpos)]))
          (reverse stnirp)))))

(define geo-path-end-points
  : (case-> [Geo-Path-Clean-Prints+ -> (Values Float-Complex Flonum Float-Complex Flonum)]
            [Geo-Path-Clean-Prints -> (Values (Option Float-Complex) Flonum (Option Float-Complex) Flonum)])
  (lambda [footprints]
    (if (and (pair? footprints) (pair? (cdr footprints)))
        (let ([h1st (geo-path-clean-print-position (car footprints))]
              [h2nd (geo-path-clean-print-position (cadr footprints))])
          (let traverse ([t2nd : Float-Complex h1st]
                         [t1st : Float-Complex h2nd]
                         [prints : (Listof GPath:Print) (cddr footprints)])
            (if (pair? prints)
                (traverse t1st (geo-path-clean-print-position (car prints)) (cdr prints))
                (values h1st (angle (- h2nd h1st)) t1st (angle (- t1st t2nd))))))
        (values #false 0.0 #false 0.0))))

(define geo-path-ink-box : (-> Geo-Path-Clean-Prints (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [footprints]
    (let traverse ([lx : Flonum +inf.0]
                   [ty : Flonum +inf.0]
                   [rx : Flonum -inf.0]
                   [by : Flonum -inf.0]
                   [prints : Geo-Path-Clean-Prints footprints])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-Z : GPP:Close (gpp:close #\Z))
