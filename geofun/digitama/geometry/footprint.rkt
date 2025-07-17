#lang typed/racket/base

(provide (all-defined-out))

(require digimon/complex)
(require racket/list)

(require "bezier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Prints (Listof GPath:Datum))
(define-type Geo-Path-Clean-Prints (Listof GPath:Print))
(define-type Geo-Path-Clean-Prints+ (Pairof GPath:Print Geo-Path-Clean-Prints))
(define-type Geo-Path-Clean-Prints* (List* GPath:Print GPath:Print Geo-Path-Clean-Prints))

(struct gpath:datum
  ([cmd : Char])
  #:type-name GPath:Datum
  #:transparent)

(struct gpp:close gpath:datum () #:type-name GPP:Close #:transparent)
(struct gpp:vector gpath:datum
  ([rel-end : Float-Complex])
  #:type-name GPP:Vector
  #:transparent)

(struct gpath:print gpath:datum
  ([end-here : Float-Complex])
  #:type-name GPath:Print
  #:transparent)

(struct gpp:point gpath:print () #:type-name GPP:Point #:transparent)

(struct gpp:arc gpath:print
  ([center : Float-Complex]
   [rx : Nonnegative-Flonum]
   [ry : Nonnegative-Flonum]
   [start : Float]
   [end : Float]
   [clockwise? : Boolean])
  #:type-name GPP:Arc
  #:transparent)

;;; TODO: NOTE:
; Both the head and tail tips may require some offset,
;   in which case the resulting curves may differ in a subtle way.
(struct gpp:bezier gpath:print
  ([start-here : Float-Complex])
  #:type-name GPP:Bezier
  #:transparent)

(struct gpp:bezier:quadratic gpp:bezier
  ([ctrl : Float-Complex])
  #:type-name GPP:Bezier:Quadratic
  #:transparent)

(struct gpp:bezier:cubic gpp:bezier
  ([ctrl1 : Float-Complex]
   [ctrl2 : Float-Complex])
  #:type-name GPP:Bezier:Cubic
  #:transparent)

(struct gpp:bezier:nth gpp:bezier
  ([ctrls+endpoint : (Listof Float-Complex)]
   [samples : Index])
  #:type-name GPP:Bezier:Nth
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

(define geo-path-cleanse : (->* ((Listof (U GPath:Datum Float-Complex (Listof Float-Complex))))
                                (Float-Complex #:bezier-samples Index)
                                Geo-Path-Clean-Prints)
  (lambda [footprints [pos0 0.0+0.0i] #:bezier-samples [samples 200]]
    (let traverse ([prints : (Listof (U GPath:Datum Float-Complex (Listof Float-Complex))) footprints]
                   [p-head : (Option Float-Complex) #false]
                   [stnirp : (Listof GPath:Print) null]
                   [curpos : Float-Complex pos0])
      (if (pair? prints)
          ; TODO: deal with ellipses, remove duplicate prints
          (let-values ([(self rest) (values (car prints) (cdr prints))])
            (cond [(gpp:close? self)
                   (if (and p-head)
                       (traverse rest #false (cons (gpp:point #\L p-head) stnirp) p-head)
                       (traverse rest p-head stnirp curpos))]
                  [(gpp:vector? self)
                   (let ([cmd (gpath:datum-cmd self)]
                         [pos (+ curpos (gpp:vector-rel-end self))])
                     (cond [(not (flc-rational? pos)) (traverse rest #false stnirp curpos)]
                           [(eq? cmd #\m) (traverse rest pos (cons (gpp:point #\M pos) stnirp) pos)]
                           [(not p-head) (traverse rest pos (cons (gpp:point #\M pos) stnirp) pos)]
                           [(= pos curpos) (traverse rest p-head stnirp pos)]
                           [else (traverse rest p-head (cons (gpp:point #\L pos) stnirp) pos)]))]
                  [(gpath:print? self) ; take care curves
                   (let ([cmd (gpath:datum-cmd self)]
                         [pos (gpath:print-end-here self)])
                     (cond [(not (flc-rational? pos)) (traverse rest #false stnirp curpos)]
                           [(eq? cmd #\M) (traverse rest pos (cons self stnirp) pos)]
                           [(not p-head) (traverse rest pos (cons (gpp:point #\M pos) stnirp) pos)]
                           [(= pos curpos) (traverse rest p-head stnirp pos)]
                           [else (traverse rest p-head (cons self stnirp) pos)]))]
                  [(complex? self)
                   (cond [(not (flc-rational? self)) (traverse rest #false stnirp curpos)]
                         [(not p-head) (traverse rest self (cons (gpp:point #\M self) stnirp) self)]
                         [(= curpos self) (traverse rest p-head stnirp curpos)]
                         [else (traverse rest p-head (cons (gpp:point #\L self) stnirp) self)])]
                  [(list? self) ; bezier curves 
                   (cond [(null? self) (traverse rest p-head stnirp curpos)]
                         [(null? (cdr self))
                          ; decay to a point
                          (traverse (cons (car self) rest) p-head stnirp curpos)]
                         [(null? (cddr self))
                          (if (or p-head)
                              (let ([Q (gpp:bezier:quadratic #\Q (cadr self) curpos (car self))])
                                (traverse rest p-head (cons Q stnirp) (gpath:print-end-here Q)))
                              ; decay to two points
                              (traverse (append self rest) p-head stnirp curpos))]
                         [(null? (cdddr self))
                          (if (or p-head)
                              (let ([C (gpp:bezier:cubic #\C (caddr self) curpos (car self) (cadr self))])
                                (traverse rest p-head (cons C stnirp) (gpath:print-end-here C)))
                              ; decay to the head point and a bezier stroage of controls
                              (traverse (cons (car self) (cons (cdr self) rest)) p-head stnirp curpos))]
                         [else ; n-th order bezier
                          (if (or p-head)
                              (let-values ([(B) (gpp:bezier:nth #\null (last self) curpos self samples)])
                                (traverse rest p-head (cons B stnirp) (gpath:print-end-here B)))
                              ; decay to the head point and a bezier stroage of controls
                              (traverse (cons (car self) (cons (cdr self) rest)) p-head stnirp curpos))])]
                  [else '#:deadcode (traverse rest p-head stnirp curpos)]))
          (reverse stnirp)))))

(define geo-path-end-points
  : (case-> [Geo-Path-Clean-Prints+ -> (Values Float-Complex Flonum Float-Complex Flonum)]
            [Geo-Path-Clean-Prints -> (Values (Option Float-Complex) Flonum (Option Float-Complex) Flonum)])
  (lambda [footprints]
    (if (pair? footprints)
        (cond [(pair? (cdr footprints))
               (let ([h1st : GPath:Print (car footprints)]
                     [h2nd : GPath:Print (cadr footprints)])
                 (let traverse ([t2nd : GPath:Print h1st]
                                [t1st : GPath:Print h2nd]
                                [prints : (Listof GPath:Print) (cddr footprints)])
                   (if (null? prints)
                       (let-values ([(hpt hrad)
                                     (cond [(gpp:bezier? h2nd) ; WARNING: A bezier storage couldn't be the first one, an the storage know the head point.
                                            (let-values ([(hpt hrad tpt trad) (geo-path-bezier-endpoints h2nd)])
                                              (values hpt hrad))]
                                           [(gpp:bezier? h1st) ; WARNING: rarely happen, but who knows.
                                            (let-values ([(hpt hrad tpt trad) (geo-path-bezier-endpoints h1st)])
                                              (values hpt hrad))]
                                           [else ; otherwise
                                            (let ([hpt1 (geo-path-clean-print-position h1st)]
                                                  [hpt2 (geo-path-clean-print-position h2nd)])
                                              (values hpt1 (angle (- hpt2 hpt1))))])]
                                    [(tpt trad)
                                     (cond [(gpp:bezier? t1st)
                                            (let-values ([(hpt hrad tpt trad) (geo-path-bezier-endpoints t1st)])
                                              (values tpt trad))]
                                           [else ; otherwise
                                            (let ([tpt1 (geo-path-clean-print-position t1st)]
                                                  [tpt2 (geo-path-clean-print-position t2nd)])
                                              (values tpt1 (angle (- tpt1 tpt2))))])])
                         (values hpt hrad tpt trad))
                       (traverse t1st (car prints) (cdr prints)))))]
              [(gpp:bezier? (car footprints)) '#:should-be-deadcode (geo-path-bezier-endpoints (car footprints))]
              [else ; otherwise, interpreted as vector
               (let* ([pos (geo-path-clean-print-position (car footprints))]
                      [rad (angle pos)])
                 (values pos rad pos rad))])
        (values #false 0.0 #false 0.0))))

(define geo-path-ink-box : (-> Geo-Path-Clean-Prints (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [footprints]
    (let traverse ([lx : Flonum +inf.0]
                   [ty : Flonum +inf.0]
                   [rx : Flonum -inf.0]
                   [by : Flonum -inf.0]
                   [prints : Geo-Path-Clean-Prints footprints])
      (if (pair? prints)
          (let-values ([(self rest) (values (car prints) (cdr prints))])
            (cond [(gpp:bezier? self)
                   (let ([spt (gpp:bezier-start-here self)]
                         [ept (gpath:print-end-here self)])
                     (define-values (rmin imin rmax imax)
                       (cond [(gpp:bezier:quadratic? self)
                              (let ([ctrl (gpp:bezier:quadratic-ctrl self)])
                                (flc-interval (list* spt ept (bezier-quadratic-extremities spt ctrl ept)) lx ty rx by))]
                             [(gpp:bezier:cubic? self)
                              (let ([ctrl1 (gpp:bezier:cubic-ctrl1 self)]
                                    [ctrl2 (gpp:bezier:cubic-ctrl2 self)])
                                (flc-interval (list* spt ept (bezier-cubic-extremities spt ctrl1 ctrl2 ept)) lx ty rx by))]
                             [(gpp:bezier:nth? self)
                              (let ([tail (gpp:bezier:nth-ctrls+endpoint self)])
                                (flc-interval (list* spt ept (bezier-nth-extremities spt tail (gpp:bezier:nth-samples self))) lx ty rx by))]
                             [else '#:deadcode (values lx ty rx by)]))
                     (traverse rmin imin rmax imax rest))]
                  [else ; TODO: deal with arcs
                   (let*-values ([(pt) (geo-path-clean-print-position self)]
                                 [(x y) (values (real-part pt) (imag-part pt))])
                     (traverse (min x lx) (min y ty) (max rx x) (max by y) rest))]))
          (let*-values ([(w h) (values (- rx lx) (- by ty))]
                        [(x width)  (if (>= w 0.0) (values lx w) (values 0.0 0.0))]
                        [(y height) (if (>= h 0.0) (values ty h) (values 0.0 0.0))])
            (values x y width height))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-bezier-endpoints : (-> GPP:Bezier (Values Float-Complex Flonum Float-Complex Flonum))
  (let ([pts-db : (HashTable Any (List Float-Complex Flonum Float-Complex Flonum)) (make-weak-hash)])
    (lambda [self]
      (define results
        (hash-ref! pts-db self
                   (λ [] (let ([spt (gpp:bezier-start-here self)]
                               [ept (gpath:print-end-here self)])
                           (define derivative : (Option (-> Flonum Float-Complex))
                             (cond [(gpp:bezier:cubic? self)
                                    (bezier-function #:derivative 1 spt (list (gpp:bezier:cubic-ctrl1 self) (gpp:bezier:cubic-ctrl2 self) ept))]
                                   [(gpp:bezier:quadratic? self)
                                    (bezier-function #:derivative 1 spt (list (gpp:bezier:quadratic-ctrl self) ept))]
                                   [(gpp:bezier:nth? self)
                                    (bezier-function #:derivative 1 spt (gpp:bezier:nth-ctrls+endpoint self))]
                                   [else '#:deadcode (bezier-function #:derivative 1 spt (list ept))]))
                           
                           (if (not derivative)
                               (list spt 0.0 ept 0.0)
                               (list spt (angle (derivative 0.0))
                                     ept (angle (derivative 1.0))))))))
      (values (car results)   (cadr results)
              (caddr results) (cadddr results)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-Z : GPP:Close (gpp:close #\Z))
(define the-M0 : GPP:Point (gpp:point #\M 0.0+0.0i))
