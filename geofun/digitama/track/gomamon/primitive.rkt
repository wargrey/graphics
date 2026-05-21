#lang typed/racket/base

(provide (all-defined-out))

(require "../self.rkt")
(require "../trace.rkt")
(require "../anchor.rkt")
(require "../primitives.rkt")

(require "../../geometry/footprint.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/math))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-gomamon-line-move! stx)
  (syntax-case stx []
    [(_ move1 move2 #:+> xsgn ysgn)
     (with-syntax ([move! (format-id #'move1 "gomamon-move-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [jump! (format-id #'move1 "gomamon-jump-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [shuffle! (format-id #'move1 "gomamon-shuffle-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [sidestep! (format-id #'move1 "gomamon-sidestep-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [xstep : Geo-Step-Datum 1.0] [ystep : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-regular-move goma xstep ystep xsgn ysgn anchor info))

                (define (jump! [goma : Gomamon] [xstep : Geo-Step-Datum 1.0] [ystep : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-regular-move goma xstep ystep xsgn ysgn anchor))

                (define (shuffle! [goma : Gomamon] [xstep : Real 1.0] [ystep : Real 1.0]
                                  [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-move-a-little goma xstep ystep xsgn ysgn anchor info))

                (define (sidestep! [goma : Gomamon] [xstep : Real 1.0] [ystep : Real 1.0]
                                   [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-move-a-little goma xstep ystep xsgn ysgn anchor)))))]
    [(_ move #:-> xsgn)
     (with-syntax ([move! (format-id #'move "gomamon-move-~a!" (syntax->datum #'move))]
                   [move-towards! (format-id #'move "gomamon-move-~awards!" (syntax->datum #'move))]
                   [move-pass! (format-id #'move "gomamon-move-~a-pass!" (syntax->datum #'move))]
                   [jump! (format-id #'move "gomamon-jump-~a!" (syntax->datum #'move))]
                   [jump-towards! (format-id #'move "gomamon-jump-~awards!" (syntax->datum #'move))]
                   [jump-pass! (format-id #'move "gomamon-jump-~a-pass!" (syntax->datum #'move))]
                   [shuffle! (format-id #'move "gomamon-shuffle-~a!" (syntax->datum #'move))]
                   [sidestep! (format-id #'move "gomamon-sidestep-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-regular-move goma step 0.0 xsgn 0.0 anchor info))

                (define (move-towards! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                                       [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-approach goma step 0.0 xsgn 0.0 anchor -1.0 info))

                (define (move-pass! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                                    [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-approach goma step 0.0 xsgn 0.0 anchor +1.0 info))
                
                (define (jump! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-regular-move goma step 0.0 xsgn 0.0 anchor))

                (define (jump-towards! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                                       [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-approach goma step 0.0 xsgn 0.0 anchor -1.0))

                (define (jump-pass! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                                    [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-approach goma step 0.0 xsgn 0.0 anchor +1.0))

                (define (shuffle! [goma : Gomamon] [step : Real 1.0] [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-move-a-little goma step 0.0 xsgn 0.0 anchor info))

                (define (sidestep! [goma : Gomamon] [step : Real 1.0] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-move-a-little goma step 0.0 xsgn 0.0 anchor)))))]
    [(_ move #:!> ysgn)
     (with-syntax ([move! (format-id #'move "gomamon-move-~a!" (syntax->datum #'move))]
                   [move-towards! (format-id #'move "gomamon-move-~awards!" (syntax->datum #'move))]
                   [move-pass! (format-id #'move "gomamon-move-~a-pass!" (syntax->datum #'move))]
                   [jump! (format-id #'move "gomamon-jump-~a!" (syntax->datum #'move))]
                   [jump-towards! (format-id #'move "gomamon-jump-~awards!" (syntax->datum #'move))]
                   [jump-pass! (format-id #'move "gomamon-jump-~a-pass!" (syntax->datum #'move))]
                   [shuffle! (format-id #'move "gomamon-shuffle-~a!" (syntax->datum #'move))]
                   [sidestep! (format-id #'move "gomamon-sidestep-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-regular-move goma 0.0 step 0.0 ysgn anchor info))

                (define (move-towards! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                                       [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-approach goma 0.0 step 0.0 ysgn anchor -1.0 info))

                (define (move-pass! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                                    [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-approach goma 0.0 step 0.0 ysgn anchor +1.0 info))

                (define (jump! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-regular-move goma 0.0 step 0.0 ysgn anchor))

                (define (jump-towards! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                                       [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-approach goma 0.0 step 0.0 ysgn anchor -1.0))

                (define (jump-pass! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                                    [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-approach goma 0.0 step 0.0 ysgn anchor +1.0))

                (define (shuffle! [goma : Gomamon] [step : Real 1.0] [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (gomamon-move-a-little goma 0.0 step 0.0 ysgn anchor info))
                
                (define (sidestep! [goma : Gomamon] [step : Real 1.0] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-move-a-little goma 0.0 step 0.0 ysgn anchor)))))]))

(define-syntax (define-gomamon-turn-move! stx)
  (syntax-case stx []
    [(_ move1 move2 #:+> [start end clockwise-args ...] #:-> [c-start c-end counterclockwise-args ...])
     (with-syntax ([turn-1-2! (format-id #'move1 "gomamon-turn-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [turn-2-1! (format-id #'move2 "gomamon-turn-~a-~a!" (syntax->datum #'move2) (syntax->datum #'move1))]
                   [turn-1-2*! (format-id #'move1 "gomamon-turn-~a-~a*!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [turn-2-1*! (format-id #'move2 "gomamon-turn-~a-~a*!" (syntax->datum #'move2) (syntax->datum #'move1))]
                   [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                   [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))]
                   [rcstart (datum->syntax #'c-start (degrees->radians (syntax->datum #'c-start)))]
                   [rcend (datum->syntax #'c-end (degrees->radians (syntax->datum #'c-end)))])
       (syntax/loc stx
         (begin (define (turn-1-2! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-turn goma (gomamon-txradius goma) (gomamon-tyradius goma)
                                clockwise-args ... rstart rend anchor #true #false))
                
                (define (turn-2-1! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-turn goma (gomamon-txradius goma) (gomamon-tyradius goma)
                                counterclockwise-args ... rcstart rcend anchor #false #false))

                (define (turn-1-2*! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false] #:scale [s : Nonnegative-Real 2.0]) : Void
                  (let ([fls (real->double-flonum s)])
                    (gomamon-turn goma (* fls (gomamon-txradius goma)) (* fls (gomamon-tyradius goma))
                                  clockwise-args ... rstart rend anchor #true #false)))
                
                (define (turn-2-1*! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false] #:scale [s : Nonnegative-Real 2.0]) : Void
                  (let ([fls (real->double-flonum s)])
                    (gomamon-turn goma (* fls (gomamon-txradius goma)) (* fls (gomamon-tyradius goma))
                                  counterclockwise-args ... rcstart rcend anchor #false #false))))))]
    [(_ move clockwise? [start end args ...] guard)
     (with-syntax* ([U-turn-move! (format-id #'move "gomamon-turn-~a!" (syntax->datum #'move))]
                    [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                    [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))])
       (syntax/loc stx
         (begin (define (U-turn-move! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (gomamon-turn goma (gomamon-txradius goma) (gomamon-tyradius goma)
                                args ... rstart rend anchor clockwise? guard)))))]
    [(_ move #:+> args #:boundary-guard guard) (syntax/loc stx (define-gomamon-turn-move! move #true  args guard))]
    [(_ move #:-> args #:boundary-guard guard) (syntax/loc stx (define-gomamon-turn-move! move #false args guard))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct gomamon geo:track
  ([xstepsize : Positive-Flonum]
   [ystepsize : Positive-Flonum]
   [txradius : Nonnegative-Flonum]
   [tyradius : Nonnegative-Flonum])
  #:type-name Gomamon
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gomamon-turn : (-> Geo:Track Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum Flonum
                           Flonum Flonum (Option Geo-Anchor-Name) Boolean (Option (U Float Float-Complex))
                           Void)
  (lambda [self rx ry cx cy ex ey start end anchor clockwise? guard]
    (define cpos : Float-Complex (geo:track-here self))
    (define cpos++ : Float-Complex (+ (make-rectangular (* ex rx) (* ey ry)) cpos))
    (define path:arc (gpp:arc #\A cpos++ (+ (make-rectangular (* cx rx) (* cy ry)) cpos) rx ry start end clockwise?))

    (when (and guard)
      (geo-track-try-fit! self
                          (+ cpos (make-rectangular (real->double-flonum (* (real-part guard) rx))
                                                    (real->double-flonum (* (imag-part guard) ry))))))

    (geo-track-try-fit! self anchor cpos++)
    (set-geo:track-here! self cpos++)
    (set-geo:track-footprints! self (cons path:arc (geo:track-footprints self)))))

(define gomamon-regular-move : (case-> [Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum (Option Geo-Anchor-Name) -> Void]
                                       [Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum (Option Geo-Anchor-Name) Any -> Void])
  (case-lambda
    [(self xstep ystep xsgn ysgn anchor)
     (geo-track-do-move self (gomamon-target-position self xstep ystep xsgn ysgn) #\M anchor #false #true)]
    [(self xstep ystep xsgn ysgn anchor info)
     (geo-track-do-move self (gomamon-target-position self xstep ystep xsgn ysgn) #\L anchor info #false)]))

(define gomamon-approach : (case-> [Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum (Option Geo-Anchor-Name) Flonum -> Void]
                                   [Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum (Option Geo-Anchor-Name) Flonum  Any -> Void])
  (case-lambda
    [(self xstep ystep xsgn ysgn anchor clearance-sgn)
     (geo-track-do-move self (gomamon-near-position self xstep ystep xsgn ysgn clearance-sgn) #\M anchor #false #true)]
    [(self xstep ystep xsgn ysgn anchor clearance-sgn info)
     (geo-track-do-move self (gomamon-near-position self xstep ystep xsgn ysgn clearance-sgn) #\L anchor info #false)]))

(define gomamon-move-a-little : (case-> [Gomamon Real Real Flonum Flonum (Option Geo-Anchor-Name) -> Void]
                                        [Gomamon Real Real Flonum Flonum (Option Geo-Anchor-Name) Any -> Void])
  (case-lambda
    [(self xstep ystep xsgn ysgn anchor)
     (geo-track-do-move self (gomamon-offset-position self xstep ystep xsgn ysgn) #\M anchor #false #true)]
    [(self xstep ystep xsgn ysgn anchor info)
     (geo-track-do-move self (gomamon-offset-position self xstep ystep xsgn ysgn) #\L anchor info #false)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gomamon-target-position : (-> Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum Float-Complex)
  (lambda [self xstep ystep xsgn ysgn]
    (make-rectangular
     (if (real? xstep)
         (+ (real-part (geo:track-here self))
            (* (gomamon-xstepsize self) (real->double-flonum xstep) xsgn))
         (real-part (geo-trace-ref (geo:track-trace self) xstep)))
     
     (if (real? ystep)
         (+ (imag-part (geo:track-here self))
            (* (gomamon-ystepsize self) (real->double-flonum ystep) ysgn))
         (imag-part (geo-trace-ref (geo:track-trace self) ystep))))))

(define gomamon-grid-position : (-> Gomamon Complex Float-Complex)
  (lambda [self pos]
    (make-rectangular (* (real->double-flonum (real-part pos)) (gomamon-xstepsize self))
                      (* (real->double-flonum (imag-part pos)) (gomamon-ystepsize self)))))

(define gomamon-near-position : (case-> [Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum Flonum -> Float-Complex])
  (case-lambda
    [(self xstep ystep xsgn ysgn clearance-sgn)
     (+ (gomamon-target-position self xstep ystep xsgn ysgn)
        (make-rectangular (* (gomamon-txradius self) xsgn clearance-sgn)
                          (* (gomamon-tyradius self) ysgn clearance-sgn)))]))

(define gomamon-offset-position : (case-> [Gomamon Real Real Flonum Flonum -> Float-Complex])
  (case-lambda
    [(self xstep ystep xsgn ysgn)
     (+ (geo:track-here self)
        (make-rectangular (* (gomamon-txradius self) (real->double-flonum xstep) xsgn)
                          (* (gomamon-tyradius self) (real->double-flonum ystep) ysgn)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gomamon-turn-scales : (-> Geo-Print-Datum Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [scale default-scale]
    (define (turn-scale [datum : Real]) : Nonnegative-Flonum
      (if (> datum 0) (real->double-flonum datum) default-scale))
    
    (cond [(flonum? scale) (let ([s (turn-scale scale)]) (values s s))]
          [(list? scale) (values (turn-scale (car scale)) (turn-scale (cadr scale)))]
          [(pair? scale) (values (turn-scale (car scale)) (turn-scale (cdr scale)))]
          [else (values (turn-scale (real-part scale)) (turn-scale (imag-part scale)))])))

