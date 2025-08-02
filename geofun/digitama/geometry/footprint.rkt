#lang typed/racket/base

(provide (all-defined-out))

(require digimon/complex)
(require digimon/digitama/unsafe/ops)

(require racket/list)
(require racket/math)

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
  ([start-here : Float-Complex]
   [samples : Index])
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
  ([ctrls+endpoint : (Listof Float-Complex)])
  #:type-name GPP:Bezier:Nth
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gpp-clean-position : (-> GPath:Print Float-Complex)
  (lambda [self]
    (gpath:print-end-here self)))

(define gpp-position : (->* (GPath:Datum) ((Option Float-Complex)) (Option Float-Complex))
  (lambda [self [curpos #false]]
    (and (gpath:print? self)
         (if (gpp:vector? self)
             (let ([pos (gpath:print-end-here self)])
               (if (and curpos (memq (gpath:datum-cmd self) '(#\m #\l)))
                   (+ pos curpos)
                   pos))
             (gpath:print-end-here self)))))

(define gpp-cleanse : (->* ((Listof (U GPath:Datum Float-Complex (Listof Float-Complex))))
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
                              (let ([Q (gpp:bezier:quadratic #\Q (cadr self) curpos samples (car self))])
                                (traverse rest p-head (cons Q stnirp) (gpath:print-end-here Q)))
                              ; decay to two points
                              (traverse (append self rest) p-head stnirp curpos))]
                         [(null? (cdddr self))
                          (if (or p-head)
                              (let ([C (gpp:bezier:cubic #\C (caddr self) curpos samples (car self) (cadr self))])
                                (traverse rest p-head (cons C stnirp) (gpath:print-end-here C)))
                              ; decay to the head point and a bezier stroage of controls
                              (traverse (cons (car self) (cons (cdr self) rest)) p-head stnirp curpos))]
                         [else ; n-th order bezier
                          (if (or p-head)
                              (let-values ([(B) (gpp:bezier:nth #\null (last self) curpos samples self)])
                                (traverse rest p-head (cons B stnirp) (gpath:print-end-here B)))
                              ; decay to the head point and a bezier stroage of controls
                              (traverse (cons (car self) (cons (cdr self) rest)) p-head stnirp curpos))])]
                  [else '#:deadcode (traverse rest p-head stnirp curpos)]))
          (reverse stnirp)))))

(define gpp-endpoint-vectors
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
                                     (cond [(gpp:bezier? h2nd) ; NOTE: A bezier storage couldn't be the first one, an the storage know the head point.
                                            (let-values ([(hpt hrad tpt trad) (gpp-bezier-endpoints h2nd)])
                                              (values hpt hrad))]
                                           [(gpp:bezier? h1st) ; WARNING: rarely happen, but who knows.
                                            (let-values ([(hpt hrad tpt trad) (gpp-bezier-endpoints h1st)])
                                              (values hpt hrad))]
                                           [else ; otherwise
                                            (let ([hpt1 (gpp-clean-position h1st)]
                                                  [hpt2 (gpp-clean-position h2nd)])
                                              (values hpt1 (angle (- hpt2 hpt1))))])]
                                    [(tpt trad)
                                     (cond [(gpp:bezier? t1st)
                                            (let-values ([(hpt hrad tpt trad) (gpp-bezier-endpoints t1st)])
                                              (values tpt trad))]
                                           [else ; otherwise
                                            (let ([tpt1 (gpp-clean-position t1st)]
                                                  [tpt2 (gpp-clean-position t2nd)])
                                              (values tpt1 (angle (- tpt1 tpt2))))])])
                         (values hpt hrad tpt trad))
                       (traverse t1st (car prints) (cdr prints)))))]
              [(gpp:bezier? (car footprints)) '#:should-be-deadcode (gpp-bezier-endpoints (car footprints))]
              [else ; otherwise, interpreted as vector
               (let* ([pos (gpp-clean-position (car footprints))]
                      [rad (angle pos)])
                 (values pos rad pos rad))])
        (values #false 0.0 #false 0.0))))

(define gpp-directional-vector : (case-> [Geo-Path-Clean-Prints (Option Integer) Real -> (Option (Pairof Float-Complex Float-Complex))]
                                         [Geo-Path-Clean-Prints Real -> (Option (Pairof Float-Complex Float-Complex))])
  (case-lambda
    [(footprints idx0 t-by-length)
     (if (or idx0)
         (let ([count (gpp-segment-count footprints)])
           (and (> count 0)
                (let traverse ([curpos : (Option Float-Complex) #false]
                               [prints : Geo-Path-Clean-Prints footprints]
                               [idx : Nonnegative-Fixnum (let wrap ([idx : Integer idx0])
                                                           (cond [(>= idx 0) (remainder idx count)]
                                                                 [else (wrap (+ idx count))]))])
                  (and (pair? prints)
                       (let-values ([(self rest) (values (car prints) (cdr prints))])
                         (cond [(gpp:point? self)
                                (let ([endpos (gpp-clean-position self)])
                                  (cond [(or (not curpos) (eq? (gpath:datum-cmd self) #\M)) (traverse endpos rest idx)]
                                        [(> idx 0) (traverse endpos rest (- idx 1))]
                                        [else (let ([dir (- endpos curpos)])
                                                (cons (+ (* dir t-by-length) curpos)
                                                      (/ dir (magnitude dir))))]))]
                               [(gpp:bezier? self) ; NOTE: A bezier storage couldn't be the first one, an the storage know the head point.
                                (cond [(> idx 0) (traverse (gpp-clean-position self) rest (- idx 1))]
                                      [else (let ([bezier (gpp-bezier-function self 0)]
                                                  [derivative (gpp-bezier-function self 1)])
                                              (and bezier derivative
                                                   (let* ([t (gpp-bezier-reparameterize-by-arclength self t-by-length)]
                                                          [pos (bezier t)]
                                                          [dp (derivative t)])
                                                     (cons pos (/ dp (magnitude dp))))))])]
                               [else '#:deadcode (traverse curpos rest idx)]))))))
         (gpp-directional-vector footprints t-by-length))]
    [(footprints t-by-length)
     (let traverse ([curpos : (Option Float-Complex) #false]
                    [prints : Geo-Path-Clean-Prints footprints]
                    [distance : Flonum (* (gpp-arclength footprints) (real->double-flonum t-by-length))])
       (and (pair? prints) (>= distance 0.0)
            (let-values ([(self rest) (values (car prints) (cdr prints))])
              (cond [(gpp:point? self)
                     (let ([endpos (gpp-clean-position self)])
                       (cond [(or (not curpos) (eq? (gpath:datum-cmd self) #\M)) (traverse endpos rest distance)]
                             [else (let* ([dir (- endpos curpos)]
                                          [len (magnitude dir)]
                                          [diff (- distance len)])
                                     (cond [(positive? diff) (traverse endpos rest diff)]
                                           [else (cons (+ (* dir (/ distance len)) curpos)
                                                       (/ dir (magnitude dir)))]))]))]
                    [(gpp:bezier? self) ; NOTE: A bezier storage couldn't be the first one, an the storage know the head point.
                     (let* ([len (gpp-bezier-arclength self)]
                            [diff (- distance len)])
                       (cond [(positive? diff) (traverse (gpp-clean-position self) rest diff)]
                             [else (let ([bezier (gpp-bezier-function self 0)]
                                         [derivative (gpp-bezier-function self 1)])
                                     (and bezier derivative
                                          (let* ([t (gpp-bezier-reparameterize-by-arclength self (/ distance len))]
                                                 [pos (bezier t)]
                                                 [dp (derivative t)])
                                            (cons pos (/ dp (magnitude dp))))))]))]
                    [else '#:deadcode (traverse curpos rest distance)]))))]))

(define gpp-ink-box : (-> Geo-Path-Clean-Prints (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
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
                                (flc-interval (list* spt ept (bezier-nth-extremities spt tail (gpp:bezier-samples self))) lx ty rx by))]
                             [else '#:deadcode (values lx ty rx by)]))
                     (traverse rmin imin rmax imax rest))]
                  [else ; TODO: deal with arcs
                   (let*-values ([(pt) (gpp-clean-position self)]
                                 [(x y) (values (real-part pt) (imag-part pt))])
                     (traverse (min x lx) (min y ty)
                               (max rx x) (max by y)
                               rest))]))
          (let*-values ([(w h) (values (- rx lx) (- by ty))]
                        [(x width)  (if (>= w 0.0) (values lx w) (values 0.0 0.0))]
                        [(y height) (if (>= h 0.0) (values ty h) (values 0.0 0.0))])
            (values x y width height))))))

(define gpp-arclength : (-> Geo-Path-Clean-Prints Nonnegative-Flonum)
  (lambda [footprints]
    (let traverse ([prints : Geo-Path-Clean-Prints footprints]
                   [prev : (Option Float-Complex) #false]
                   [size : Nonnegative-Flonum 0.0])
      (if (pair? prints)
          (let-values ([(self rest) (values (car prints) (cdr prints))])
            (cond [(gpp:bezier? self)
                   (traverse rest
                             (gpath:print-end-here self)
                             (+ size (gpp-bezier-arclength self)))]
                  [else ; TODO: deal with arcs
                   (let ([cmd (gpath:datum-cmd self)]
                         [pt (gpp-clean-position self)])
                     (cond [(eq? cmd #\M) (traverse rest pt size)]
                           [(not prev) (traverse rest pt size)]
                           [else (traverse rest pt (+ size (magnitude (- pt prev))))]))]))
          size))))

(define gpp-segment-count : (-> Geo-Path-Clean-Prints Index)
  (lambda [footprints]
    (let traverse ([prints : Geo-Path-Clean-Prints footprints]
                   [count : Index 0])
      (if (pair? prints)
          (let*-values ([(self rest) (values (car prints) (cdr prints))]
                        [(cmd) (gpath:datum-cmd self)])
            (cond [(eq? cmd #\M) (traverse rest count)]
                  [else (traverse rest (unsafe-idx+ count 1))]))
          count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gpp-bezier-endpoints : (-> GPP:Bezier (Values Float-Complex Flonum Float-Complex Flonum))
  (let ([pts-db : (HashTable Any (List Float-Complex Flonum Float-Complex Flonum)) (make-weak-hash)])
    (lambda [self]
      (define results
        (hash-ref! pts-db self
                   (λ [] (let ([spt (gpp:bezier-start-here self)]
                               [ept (gpath:print-end-here self)])
                           (define derivative : (Option (-> Flonum Float-Complex)) (gpp-bezier-function self 1))
                           
                           (if (not derivative)
                               (list spt 0.0 ept 0.0)
                               (list spt (angle (derivative 0.0))
                                     ept (angle (derivative 1.0))))))))
      (values (car results)   (cadr results)
              (caddr results) (cadddr results)))))

(define gpp-bezier-tail-points : (-> GPP:Bezier (Listof Float-Complex))
  (lambda [self]
    (define ept (gpath:print-end-here self))
    
    (cond [(gpp:bezier:quadratic? self)
           (let ([ctrl (gpp:bezier:quadratic-ctrl self)])
             (list ctrl ept))]
          [(gpp:bezier:cubic? self)
           (let ([ctrl1 (gpp:bezier:cubic-ctrl1 self)]
                 [ctrl2 (gpp:bezier:cubic-ctrl2 self)])
             (list ctrl1 ctrl2 ept))]
          [(gpp:bezier:nth? self)
           (gpp:bezier:nth-ctrls+endpoint self)]
          [else '#:deadcode null])))

(define gpp-bezier-arclength : (-> GPP:Bezier Nonnegative-Flonum)
  (let ([size-db : (HashTable Any Nonnegative-Flonum) (make-weak-hash)])
    (lambda [self]
      (hash-ref! size-db self
                 (λ [] (bezier-length (gpp:bezier-start-here self)
                                      (gpp-bezier-tail-points self)
                                      (gpp:bezier-samples self)))))))

(define gpp-bezier-reparameterize-by-arclength : (-> GPP:Bezier Real Nonnegative-Flonum)
  (let ([lookup-table : (Weak-HashTable Any (HashTable Real (Pairof Nonnegative-Exact-Rational Nonnegative-Flonum))) (make-weak-hash)])
    (lambda [self t]
      (define reparameterize : (-> (HashTable Real (Pairof Nonnegative-Exact-Rational Nonnegative-Flonum)) Real
                                   (Pairof Nonnegative-Exact-Rational Nonnegative-Flonum))
        (lambda [lut t]
          (define bsize (gpp-bezier-arclength self))
          (define distance (abs (* bsize (real->double-flonum t))))
          (define head (gpp:bezier-start-here self))
          (define tail (gpp-bezier-tail-points self))
          (define samples (gpp:bezier-samples self))
                                                    
          (define ts (sort (hash-keys lut) <))
          (define i (index-of ts (λ [[cached-t : Flonum]] (> t cached-t))))

          (define-values (t0 tn len)
            (cond [(or i)
                   (let ([prev-i (- i 1)]
                         [cache (hash-ref lut (list-ref ts i))])
                     (cond [(< prev-i 0) (values 0 (car cache) distance)]
                           [else (let ([pc (hash-ref lut (list-ref ts prev-i))])
                                   (values (car pc) (car cache) (max (- distance (cdr pc)) 0.0)))]))]
                  [(pair? ts)
                   (let ([cache (hash-ref lut (last ts))])
                     (values (car cache) 1 (max (- distance (cdr cache)) 0.0)))]
                  [else ; for the first one
                   (values 0 1 distance)]))
          (define t-by-len : Nonnegative-Exact-Rational
            (bezier-reparameterize-by-length #:t0 t0 #:tn tn
                                             head tail len
                                             (exact-round (* (- tn t0)
                                                             samples))))
            
          (cons t-by-len distance)))
      
      (cond [(<= t 0) 0.0]
            [(>= t 1) 1.0]
            [else (let ([lut (hash-ref! lookup-table self (inst make-hasheqv Real (Pairof Nonnegative-Exact-Rational Nonnegative-Flonum)))])
                    (real->double-flonum (car (hash-ref! lut t (λ [] (reparameterize lut t))))))]))))

(define gpp-bezier-function : (-> GPP:Bezier Byte (Option (-> Flonum Float-Complex)))
  (lambda [self order]
    (define spt (gpp:bezier-start-here self))
    (define ept (gpath:print-end-here self))
    
    (cond [(gpp:bezier:cubic? self)
           (bezier-function #:derivative order spt (list (gpp:bezier:cubic-ctrl1 self) (gpp:bezier:cubic-ctrl2 self) ept))]
          [(gpp:bezier:quadratic? self)
           (bezier-function #:derivative order spt (list (gpp:bezier:quadratic-ctrl self) ept))]
          [(gpp:bezier:nth? self)
           (bezier-function #:derivative order spt (gpp:bezier:nth-ctrls+endpoint self))]
          [else '#:deadcode (bezier-function #:derivative order spt (list ept))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-Z : GPP:Close (gpp:close #\Z))
(define the-M0 : GPP:Point (gpp:point #\M 0.0+0.0i))
