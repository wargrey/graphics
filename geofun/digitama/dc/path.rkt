#lang typed/racket/base

(provide (all-defined-out))

(require "ink.rkt")
(require "paint.rkt")
(require "../../paint.rkt")
(require "../../stroke.rkt")
(require "../convert.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/bbox.rkt")
(require "../geometry/trail.rkt")
(require "../geometry/anchor.rkt")
(require "../geometry/footprint.rkt")

(require "../unsafe/path.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/math))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Print-Datum Point2D)
(define-type Geo-Step-Datum (U Real Geo-Anchor-Name))
(define-type Geo-Bezier-Datum (U Geo-Print-Datum Geo-Anchor-Name))
(define-type Geo-Path-Infobase (HashTable (Pairof Float-Complex Float-Complex) Any))

(struct geo:path geo
  ([trail : Geo-Trail]
   [bbox : Geo-BBox]
   [origin : Float-Complex]
   [here : Float-Complex]
   [footprints : Geo-Path-Prints]
   [foot-infos : Geo-Path-Infobase]
   [sticker-offset : (Option Flonum)])
  #:type-name Geo:Path
  #:transparent
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-gomamon-line-move! stx)
  (syntax-case stx []
    [(_ move1 move2 #:+> xsgn ysgn)
     (with-syntax ([move! (format-id #'move1 "gomamon-move-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [jump! (format-id #'move1 "gomamon-jump-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [xstep : Geo-Step-Datum 1.0] [ystep : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (geo-path-L goma xstep ystep xsgn ysgn anchor info))

                (define (jump! [goma : Gomamon] [xstep : Geo-Step-Datum 1.0] [ystep : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-M goma xstep ystep xsgn ysgn anchor)))))]
    [(_ move #:-> xsgn)
     (with-syntax ([move! (format-id #'move "gomamon-move-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "gomamon-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (geo-path-L goma step 0.0 xsgn 0.0 anchor info))
                
                (define (jump! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-M goma step 0.0 xsgn 0.0 anchor)))))]
    [(_ move #:!> ysgn)
     (with-syntax ([move! (format-id #'move "gomamon-move-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "gomamon-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (move! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false] [info : Any #false]) : Void
                  (geo-path-L goma 0.0 step 0.0 ysgn anchor info))
                
                (define (jump! [goma : Gomamon] [step : Geo-Step-Datum 1.0]
                               [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-M goma 0.0 step 0.0 ysgn anchor)))))]))

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
                  (geo-path-turn goma (gomamon-txradius goma) (gomamon-tyradius goma)
                                 clockwise-args ... rstart rend anchor #true #false))
                
                (define (turn-2-1! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-turn goma (gomamon-txradius goma) (gomamon-tyradius goma)
                                 counterclockwise-args ... rcstart rcend anchor #false #false)))))]
    [(_ move clockwise? [start end args ...] guard)
     (with-syntax* ([u-turn-move! (format-id #'move "gomamon-turn-~a!" (syntax->datum #'move))]
                    [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                    [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))])
       (syntax/loc stx
         (begin (define (u-turn-move! [goma : Gomamon] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-turn goma (gomamon-uxradius goma) (gomamon-uyradius goma)
                                 args ... rstart rend anchor clockwise? guard)))))]
    [(_ move #:+> args #:boundary-guard guard) (syntax/loc stx (define-gomamon-turn-move! move #true  args guard))]
    [(_ move #:-> args #:boundary-guard guard) (syntax/loc stx (define-gomamon-turn-move! move #false args guard))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define start-of-track : Char #\M)

(struct gomamon geo:path
  ([xstepsize : Nonnegative-Flonum]
   [ystepsize : Nonnegative-Flonum]
   [txradius : Nonnegative-Flonum]
   [tyradius : Nonnegative-Flonum]
   [uxradius : Nonnegative-Flonum]
   [uyradius : Nonnegative-Flonum])
  #:type-name Gomamon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-turn-scales : (-> Geo-Print-Datum Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [scale default-scale]
    (define (turn-scale [datum : Real]) : Nonnegative-Flonum
      (if (> datum 0) (real->double-flonum datum) default-scale))
    
    (cond [(flonum? scale) (let ([s (turn-scale scale)]) (values s s))]
          [(list? scale) (values (turn-scale (car scale)) (turn-scale (cadr scale)))]
          [(pair? scale) (values (turn-scale (car scale)) (turn-scale (cdr scale)))]
          [else (values (turn-scale (real-part scale)) (turn-scale (imag-part scale)))])))

(define geo-path-bezier-point : (->* (Geo:Path Geo-Bezier-Datum) (Flonum Flonum) Float-Complex)
  (lambda [self dpos [sx 1.0] [sy 1.0]]
    (define cpos : Float-Complex (geo:path-here self))
    
    (cond [(real? dpos) (+ cpos (make-rectangular (* (real->double-flonum dpos) sx) 0.0))]
          [(list? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cadr dpos)) sy)))]
          [(pair? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cdr dpos)) sy)))]
          [(complex? dpos) (+ cpos (make-rectangular (* (real->double-flonum (real-part dpos)) sx) (* (real->double-flonum (imag-part dpos)) sy)))]
          [else (geo-trail-ref (geo:path-trail self) dpos)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-turn : (-> Geo:Path Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum Flonum
                            Flonum Flonum (Option Geo-Anchor-Name) Boolean (Option (U Float Float-Complex))
                            Void)
  (lambda [self rx ry cx cy ex ey start end anchor clockwise? guard]
    (define cpos : Float-Complex (geo:path-here self))
    (define cpos++ : Float-Complex (+ (make-rectangular (* ex rx) (* ey ry)) cpos))
    (define path:arc (gpp:arc #\A cpos++ (+ (make-rectangular (* cx rx) (* cy ry)) cpos) rx ry start end clockwise?))

    (geo-bbox-fit! (geo:path-bbox self) cpos++)
    (when (and guard)
      (geo-bbox-fit! (geo:path-bbox self) cpos
                     (real->double-flonum (* (real-part guard) rx))
                     (real->double-flonum (* (imag-part guard) ry))))

    (geo-trail-try-set! (geo:path-trail self) anchor cpos++)
    (set-geo:path-here! self cpos++)
    (set-geo:path-footprints! self (cons path:arc (geo:path-footprints self)))))

(define geo-path-jump-to : (-> Geo:Path (U Geo-Anchor-Name Complex False) (Option Geo-Anchor-Name) Void)
  (lambda [self target pos-anchor]
    (define anchor : (U Geo-Anchor-Name Complex) (or target (geo-trail-head-anchor (geo:path-trail self))))
    (define pos : Float-Complex (geo-path-target-position self anchor))

    (if (complex? anchor)
        (geo-trail-try-set! (geo:path-trail self) pos-anchor pos)
        (geo-trail-pop! (geo:path-trail self) anchor))
    
    (set-geo:path-origin! self pos)
    (set-geo:path-here! self pos)
    (set-geo:path-footprints! self (cons (gpp:point #\M pos) (geo:path-footprints self)))))

(define geo-path-connect-to : (-> Geo:Path (U Geo-Anchor-Name Complex) (Option Geo-Anchor-Name) Any Void)
  (lambda [self target pos-anchor info]
    (define pos : Float-Complex (geo-path-target-position self target))

    (unless (not info)
      (hash-set! (geo:path-foot-infos self)
                 (cons (geo:path-here self) pos)
                 info))

    (when (complex? target)
        (geo-trail-try-set! (geo:path-trail self) pos-anchor pos))
    
    (set-geo:path-here! self pos)
    (set-geo:path-footprints! self (cons (gpp:point #\L pos) (geo:path-footprints self)))))

(define geo-path-linear-bezier : (-> Geo:Path Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt anchor]
    (geo-path-do-move self endpt #\L anchor #false #false)))

(define geo-path-quadratic-bezier : (-> Geo:Path Float-Complex Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt ctrl anchor]
    (define path:bezier (gpp:bezier #\Q endpt (geo:path-here self) ctrl))

    (geo-bbox-fit! (geo:path-bbox self) endpt)
    (geo-bbox-fit! (geo:path-bbox self) ctrl)
    (geo-trail-try-set! (geo:path-trail self) anchor endpt)
    (set-geo:path-here! self endpt)
    (set-geo:path-footprints! self (cons path:bezier (geo:path-footprints self)))))

(define geo-path-cubic-bezier : (-> Geo:Path Float-Complex Float-Complex Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt ctrl1 ctrl2 anchor]
    (define path:bezier (gpp:bezier #\C endpt ctrl1 ctrl2))

    (geo-bbox-fit! (geo:path-bbox self) endpt)
    (geo-bbox-fit! (geo:path-bbox self) ctrl1)
    (geo-bbox-fit! (geo:path-bbox self) ctrl2)
    (geo-trail-try-set! (geo:path-trail self) anchor endpt)
    (set-geo:path-here! self endpt)
    (set-geo:path-footprints! self (cons path:bezier (geo:path-footprints self)))))

(define geo-path-close : (-> Geo:Path Void)
  (lambda [self]
    (define init-pos (geo:path-origin self))
    
    (set-geo:path-here! self init-pos)
    (set-geo:path-footprints! self (cons the-Z (geo:path-footprints self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-do-move : (-> Geo:Path Float-Complex Char (Option Geo-Anchor-Name) Any Boolean Void)
  (lambda [self endpt op anchor info subpath?]
    (unless (not info)
      (hash-set! (geo:path-foot-infos self)
                 (cons (geo:path-here self) endpt)
                 info))
    
    (geo-bbox-fit! (geo:path-bbox self) endpt)
    (geo-trail-try-set! (geo:path-trail self) anchor endpt)
    (set-geo:path-here! self endpt)
    (set-geo:path-footprints! self (cons (gpp:point op endpt) (geo:path-footprints self)))

    (when (and subpath?)
      (set-geo:path-origin! self endpt))))

(define geo-path-M : (-> Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum (Option Geo-Anchor-Name) Void)
  (lambda [self xstep ystep xsgn ysgn anchor]
    (geo-path-do-move self (geo-path-target-position self xstep ystep xsgn ysgn) #\M anchor #false #true)))

(define geo-path-L : (-> Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum (Option Geo-Anchor-Name) Any Void)
  (lambda [self xstep ystep xsgn ysgn anchor info]
    (geo-path-do-move self (geo-path-target-position self xstep ystep xsgn ysgn) #\L anchor info #false)))

(define geo-path-target-position : (case-> [Gomamon Geo-Step-Datum Geo-Step-Datum Flonum Flonum -> Float-Complex]
                                           [Geo:Path (U Geo-Anchor-Name Complex) -> Float-Complex])
  (case-lambda
    [(self xstep ystep xsgn ysgn)
     (make-rectangular
     
      (if (real? xstep)
          (+ (real-part (geo:path-here self))
             (* (gomamon-xstepsize self) (real->double-flonum xstep) xsgn))
          (real-part (geo-trail-ref (geo:path-trail self) xstep)))
      
      (if (real? ystep)
          (+ (imag-part (geo:path-here self))
             (* (gomamon-ystepsize self) (real->double-flonum ystep) ysgn))
          (imag-part (geo-trail-ref (geo:path-trail self) ystep))))]
    [(self target)
     (cond [(not (complex? target)) (geo-trail-ref (geo:path-trail self) target)]
           [(gomamon? self)
            (make-rectangular (* (gomamon-xstepsize self) (real->double-flonum (real-part target)))
                              (* (gomamon-ystepsize self) (real->double-flonum (imag-part target))))]
           [else (make-rectangular (real->double-flonum (real-part target))
                                   (real->double-flonum (imag-part target)))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-sticker-offset : (-> Geo:Path Float-Complex)
  (lambda [self]
    (define maybe-offset : (Option Flonum) (geo:path-sticker-offset self))

    (cond [(and maybe-offset) (make-rectangular maybe-offset maybe-offset)]
          [else (let* ([fallback (current-stroke-source)])
                  (if (stroke? fallback)
                      (let ([fallback-offset (* (stroke-width fallback) 0.5)])
                        (make-rectangular fallback-offset fallback-offset))
                      0.0+0.0i))])))

(define geo-path-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:path?])
      (define-values (width height pos) (geo-bbox-values (geo:path-bbox self)))
      (values width height (make-geo-ink pos width height)))))

(define geo-draw-path : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:path? self)
        (define-values (xoff yoff) (geo-bbox-offset-values (geo:path-bbox self)))
        (dc_path cr (+ x0 xoff) (+ y0 yoff) width height (reverse (geo:path-footprints self))
                    (geo-select-stroke-paint* alt-stroke) (geo-select-fill-source alt-fill)
                    (default-fill-rule))))))
