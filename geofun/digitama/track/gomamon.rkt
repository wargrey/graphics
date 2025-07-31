#lang typed/racket/base

(provide (all-defined-out))

(require "datum.rkt")
(require "../base.rkt")
(require "../dc/track.rkt")

(require "../geometry/bbox.rkt")
(require "../geometry/trail.rkt")
(require "../geometry/anchor.rkt")
(require "../geometry/footprint.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/math))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Step-Datum (U Real Geo-Anchor-Name))
(define-type Geo-Bezier-Datum (U Geo-Print-Datum Geo-Anchor-Name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-gomamon-line-move! stx)
  (syntax-case stx []
    [(_ move1 move2 #:+> xsgn ysgn)
     (with-syntax ([move! (format-id #'move1 "gomamon-move-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [jump! (format-id #'move1 "gomamon-jump-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [xstep : Geo-Step-Datum 1.0] [ystep : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (geo-track-L goma xstep ystep xsgn ysgn anchor info))

                (define (jump! [goma : Gomamon] [xstep : Geo-Step-Datum 1.0] [ystep : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-track-M goma xstep ystep xsgn ysgn anchor)))))]
    [(_ move #:-> xsgn)
     (with-syntax ([move! (format-id #'move "gomamon-move-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "gomamon-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (geo-track-L goma step 0.0 xsgn 0.0 anchor info))
                
                (define (jump! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-track-M goma step 0.0 xsgn 0.0 anchor)))))]
    [(_ move #:!> ysgn)
     (with-syntax ([move! (format-id #'move "gomamon-move-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "gomamon-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (geo-track-L goma 0.0 step 0.0 ysgn anchor info))
                
                (define (jump! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-track-M goma 0.0 step 0.0 ysgn anchor)))))]))

(define-syntax (define-gomamon-turn-move! stx)
  (syntax-case stx []
    [(_ move1 move2 #:+> [start end clockwise-args ...] #:-> [c-start c-end counterclockwise-args ...])
     (with-syntax ([turn-1-2! (format-id #'move1 "gomamon-turn-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [turn-2-1! (format-id #'move2 "gomamon-turn-~a-~a!" (syntax->datum #'move2) (syntax->datum #'move1))]
                   [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                   [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))]
                   [rcstart (datum->syntax #'c-start (degrees->radians (syntax->datum #'c-start)))]
                   [rcend (datum->syntax #'c-end (degrees->radians (syntax->datum #'c-end)))])
       (syntax/loc stx
         (begin (define (turn-1-2! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-track-turn goma (gomamon-txradius goma) (gomamon-tyradius goma)
                                  clockwise-args ... rstart rend anchor #true #false))
                
                (define (turn-2-1! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-track-turn goma (gomamon-txradius goma) (gomamon-tyradius goma)
                                  counterclockwise-args ... rcstart rcend anchor #false #false)))))]
    [(_ move clockwise? [start end args ...] guard)
     (with-syntax* ([u-turn-move! (format-id #'move "gomamon-turn-~a!" (syntax->datum #'move))]
                    [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                    [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))])
       (syntax/loc stx
         (begin (define (u-turn-move! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-track-turn goma (gomamon-uxradius goma) (gomamon-uyradius goma)
                                  args ... rstart rend anchor clockwise? guard)))))]
    [(_ move #:+> args #:boundary-guard guard) (syntax/loc stx (define-gomamon-turn-move! move #true  args guard))]
    [(_ move #:-> args #:boundary-guard guard) (syntax/loc stx (define-gomamon-turn-move! move #false args guard))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct gomamon geo:track
  ([xstepsize : Nonnegative-Flonum]
   [ystepsize : Nonnegative-Flonum]
   [txradius : Nonnegative-Flonum]
   [tyradius : Nonnegative-Flonum]
   [uxradius : Nonnegative-Flonum]
   [uyradius : Nonnegative-Flonum])
  #:type-name Gomamon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-turn-scales : (-> Geo-Print-Datum Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [scale default-scale]
    (define (turn-scale [datum : Real]) : Nonnegative-Flonum
      (if (> datum 0) (real->double-flonum datum) default-scale))
    
    (cond [(flonum? scale) (let ([s (turn-scale scale)]) (values s s))]
          [(list? scale) (values (turn-scale (car scale)) (turn-scale (cadr scale)))]
          [(pair? scale) (values (turn-scale (car scale)) (turn-scale (cdr scale)))]
          [else (values (turn-scale (real-part scale)) (turn-scale (imag-part scale)))])))

(define geo-track-bezier-point : (->* (Geo:Track Geo-Bezier-Datum) (Flonum Flonum) Float-Complex)
  (lambda [self dpos [sx 1.0] [sy 1.0]]
    (define cpos : Float-Complex (geo:track-here self))
    
    (cond [(real? dpos) (+ cpos (make-rectangular (* (real->double-flonum dpos) sx) 0.0))]
          [(list? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cadr dpos)) sy)))]
          [(pair? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cdr dpos)) sy)))]
          [(complex? dpos) (+ cpos (make-rectangular (* (real->double-flonum (real-part dpos)) sx) (* (real->double-flonum (imag-part dpos)) sy)))]
          [else (geo-trail-ref (geo:track-trail self) dpos)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-turn : (-> Geo:Track Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum Flonum
                             Flonum Flonum (Option Geo-Anchor-Name) Boolean (Option (U Float Float-Complex))
                             Void)
  (lambda [self rx ry cx cy ex ey start end anchor clockwise? guard]
    (define cpos : Float-Complex (geo:track-here self))
    (define cpos++ : Float-Complex (+ (make-rectangular (* ex rx) (* ey ry)) cpos))
    (define path:arc (gpp:arc #\A cpos++ (+ (make-rectangular (* cx rx) (* cy ry)) cpos) rx ry start end clockwise?))

    (geo-bbox-fit! (geo:track-bbox self) cpos++)
    (when (and guard)
      (geo-bbox-fit! (geo:track-bbox self) cpos
                     (real->double-flonum (* (real-part guard) rx))
                     (real->double-flonum (* (imag-part guard) ry))))

    (geo-trail-try-set! (geo:track-trail self) anchor cpos++)
    (set-geo:track-here! self cpos++)
    (set-geo:track-footprints! self (cons path:arc (geo:track-footprints self)))))

(define geo-track-jump-to : (case-> [Geo:Track (U Geo-Anchor-Name Complex) (Option Geo-Anchor-Name) -> Void]
                                    [Geo:Track Complex (Option Geo-Anchor-Name) Float-Complex -> Void]
                                    [Geo:Track Float-Complex -> Void])
  (case-lambda
    [(self anchor pos-anchor)
     (define pos : Float-Complex (geo-track-target-position self anchor))
     
     (when (complex? anchor)
       (geo-trail-try-set! (geo:track-trail self) pos-anchor pos))
     
     (geo-track-jump-to self pos)]
    [(self target anchor offset)
     (let ([pos (geo-track-target-position self target offset)])
       (geo-trail-try-set! (geo:track-trail self) anchor pos)
       (geo-track-jump-to self pos))]
    [(self pos)
     (set-geo:track-origin! self pos)
     (set-geo:track-here! self pos)
     (set-geo:track-footprints! self (cons (gpp:point #\M pos) (geo:track-footprints self)))]))

(define geo-track-connect-to : (case-> [Geo:Track (U Geo-Anchor-Name Complex) (Option Geo-Anchor-Name) Any Float-Complex -> Void]
                                       [Geo:Track (U Geo-Anchor-Name Complex) (Option Geo-Anchor-Name) Any -> Void]
                                       [Geo:Track Float-Complex Any -> Void])
  (case-lambda
    [(self target pos-anchor info) (geo-track-connect-to self target pos-anchor info 0.0+0.0i)]
    [(self target pos-anchor info offset)
     (let ([pos (geo-track-target-position self target offset)])
       (when (complex? target)
         (geo-trail-try-set! (geo:track-trail self) pos-anchor pos))
       (geo-track-connect-to self pos info))]
    [(self pos info)
     (unless (not info)
       (hash-set! (geo:track-foot-infos self)
                  (cons (geo:track-here self) pos)
                  (geo-track-info info)))
       
     (set-geo:track-here! self pos)
     (set-geo:track-footprints! self (cons (gpp:point #\L pos) (geo:track-footprints self)))]))

(define geo-track-linear-bezier : (-> Geo:Track Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt anchor]
    (geo-track-do-move self endpt #\L anchor #false #false)))

(define geo-track-quadratic-bezier : (-> Geo:Track Float-Complex Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt ctrl anchor]
    (define path:bezier (gpp:bezier:quadratic #\Q endpt (geo:track-here self) (default-bezier-samples) ctrl))

    (geo-bbox-fit! (geo:track-bbox self) endpt)
    (geo-bbox-fit! (geo:track-bbox self) ctrl)
    (geo-trail-try-set! (geo:track-trail self) anchor endpt)
    (set-geo:track-here! self endpt)
    (set-geo:track-footprints! self (cons path:bezier (geo:track-footprints self)))))

(define geo-track-cubic-bezier : (-> Geo:Track Float-Complex Float-Complex Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt ctrl1 ctrl2 anchor]
    (define path:bezier (gpp:bezier:cubic #\C endpt (geo:track-here self) (default-bezier-samples) ctrl1 ctrl2))

    (geo-bbox-fit! (geo:track-bbox self) endpt)
    (geo-bbox-fit! (geo:track-bbox self) ctrl1)
    (geo-bbox-fit! (geo:track-bbox self) ctrl2)
    (geo-trail-try-set! (geo:track-trail self) anchor endpt)
    (set-geo:track-here! self endpt)
    (set-geo:track-footprints! self (cons path:bezier (geo:track-footprints self)))))

(define geo-track-close : (-> Geo:Track Void)
  (lambda [self]
    (define init-pos (geo:track-origin self))
    
    (set-geo:track-here! self init-pos)
    (set-geo:track-footprints! self (cons the-Z (geo:track-footprints self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-do-move : (-> Geo:Track Float-Complex Char (Option Geo-Anchor-Name) Any Boolean Void)
  (lambda [self endpt op anchor info subpath?]
    (unless (not info)
      (hash-set! (geo:track-foot-infos self)
                 (cons (geo:track-here self) endpt)
                 (geo-track-info info)))
    
    (geo-bbox-fit! (geo:track-bbox self) endpt)
    (geo-trail-try-set! (geo:track-trail self) anchor endpt)
    (set-geo:track-here! self endpt)
    (set-geo:track-footprints! self (cons (gpp:point op endpt) (geo:track-footprints self)))

    (when (and subpath?)
      (set-geo:track-origin! self endpt))))

(define geo-track-M : (-> Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum (Option Geo-Anchor-Name) Void)
  (lambda [self xstep ystep xsgn ysgn anchor]
    (geo-track-do-move self (geo-track-target-position self xstep ystep xsgn ysgn) #\M anchor #false #true)))

(define geo-track-L : (-> Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum (Option Geo-Anchor-Name) Any Void)
  (lambda [self xstep ystep xsgn ysgn anchor info]
    (geo-track-do-move self (geo-track-target-position self xstep ystep xsgn ysgn) #\L anchor info #false)))

(define geo-track-target-position : (case-> [Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum -> Float-Complex]
                                            [Geo:Track (U Geo-Anchor-Name Complex) Float-Complex -> Float-Complex]
                                            [Geo:Track (U Geo-Anchor-Name Complex) -> Float-Complex])
  (case-lambda
    [(self xstep ystep xsgn ysgn)
     (make-rectangular
     
      (if (real? xstep)
          (+ (real-part (geo:track-here self))
             (* (gomamon-xstepsize self) (real->double-flonum xstep) xsgn))
          (real-part (geo-trail-ref (geo:track-trail self) xstep)))
      
      (if (real? ystep)
          (+ (imag-part (geo:track-here self))
             (* (gomamon-ystepsize self) (real->double-flonum ystep) ysgn))
          (imag-part (geo-trail-ref (geo:track-trail self) ystep))))]
    [(self target)
     (cond [(not (complex? target)) (geo-trail-ref (geo:track-trail self) target)]
           [(gomamon? self)
            (make-rectangular (* (gomamon-xstepsize self) (real->double-flonum (real-part target)))
                              (* (gomamon-ystepsize self) (real->double-flonum (imag-part target))))]
           [else (make-rectangular (real->double-flonum (real-part target))
                                   (real->double-flonum (imag-part target)))])]
    [(self target offset) (+ offset (geo-track-target-position self target))]))
