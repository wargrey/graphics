#lang typed/racket/base

(provide (all-defined-out))

(require "paint.rkt")
(require "../../paint.rkt")
(require "../convert.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/bbox.rkt")
(require "../geometry/trail.rkt")

(require "../unsafe/path.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/math))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Print-Datum Point2D)
(define-type Geo-Bezier-Datum (U Geo-Print-Datum Geo-Anchor-Name))

(struct geo-path geo
  ([footprints : (Pairof Path-Print (Listof Path-Print))]
   [trail : Geo-Trail]
   [bbox : Geo-BBox]
   [origin : Float-Complex]
   [here : Float-Complex])
  #:type-name Geo-Path
  #:transparent
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-dryland-wani-line-move! stx)
  (syntax-case stx []
    [(_ move #:+> sgn)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))]
                   [(xsgn ysgn) (let ([dxy (syntax->datum #'sgn)]) (list (datum->syntax #'sgn (real-part dxy)) (datum->syntax #'sgn (imag-part dxy))))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [xstep : Real 1.0] [ystep : Real 1.0] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani)
                                 (* (real->double-flonum xstep) xsgn) (* (real->double-flonum ystep) ysgn)
                                 anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [xstep : Real 1.0] [ystep : Real 1.0] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani)
                                 (* (real->double-flonum xstep) xsgn) (* (real->double-flonum ystep) ysgn)
                                 anchor #true)))))]
    [(_ move #:-> xsgn)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Real 1] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) (* (real->double-flonum step) xsgn) 0.0 anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Real 1] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) (* (real->double-flonum step) xsgn) 0.0 anchor #true)))))]
    [(_ move #:!> ysgn)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Real 1] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) 0.0 (* (real->double-flonum step) ysgn) anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Real 1] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) 0.0 (* (real->double-flonum step) ysgn) anchor #true)))))]))

(define-syntax (define-dryland-wani-turn-move! stx)
  (syntax-case stx []
    [(_ move1 move2 #:+> [start end clockwise-args ...] #:-> [c-start c-end counterclockwise-args ...])
     (with-syntax ([turn-1-2! (format-id #'move1 "dryland-wani-turn-~a-~a!" (syntax->datum #'move1) (syntax->datum #'move2))]
                   [turn-2-1! (format-id #'move2 "dryland-wani-turn-~a-~a!" (syntax->datum #'move2) (syntax->datum #'move1))]
                   [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                   [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))]
                   [rcstart (datum->syntax #'c-start (degrees->radians (syntax->datum #'c-start)))]
                   [rcend (datum->syntax #'c-end (degrees->radians (syntax->datum #'c-end)))])
       (syntax/loc stx
         (begin (define (turn-1-2! [wani : Dryland-Wani] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-turn wani (dryland-wani-txradius wani) (dryland-wani-tyradius wani)
                              clockwise-args ... rstart rend anchor #true #false))
                
                (define (turn-2-1! [wani : Dryland-Wani] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-turn wani (dryland-wani-txradius wani) (dryland-wani-tyradius wani)
                              counterclockwise-args ... rcstart rcend anchor #false #false)))))]
    [(_ move clockwise? [start end args ...] guard)
     (with-syntax* ([u-turn-move! (format-id #'move "dryland-wani-turn-~a!" (syntax->datum #'move))]
                    [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                    [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))])
       (syntax/loc stx
         (begin (define (u-turn-move! [wani : Dryland-Wani] [anchor : (Option Geo-Anchor-Name) #false]) : Void
                  (geo-path-turn wani (dryland-wani-uxradius wani) (dryland-wani-uyradius wani)
                              args ... rstart rend anchor clockwise? guard)))))]
    [(_ move #:+> args #:boundary-guard guard) (syntax/loc stx (define-dryland-wani-turn-move! move #true args guard))]
    [(_ move #:-> args #:boundary-guard guard) (syntax/loc stx (define-dryland-wani-turn-move! move #false args guard))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define start-of-track : Char #\M)

(struct dryland-wani geo-path
  ([xstepsize : Nonnegative-Flonum]
   [ystepsize : Nonnegative-Flonum]
   [txradius : Nonnegative-Flonum]
   [tyradius : Nonnegative-Flonum]
   [uxradius : Nonnegative-Flonum]
   [uyradius : Nonnegative-Flonum])
  #:type-name Dryland-Wani)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-turn-scales : (-> Geo-Print-Datum Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [scale default-scale]
    (define (turn-scale [datum : Real]) : Nonnegative-Flonum
      (if (> datum 0) (real->double-flonum datum) default-scale))
    
    (cond [(flonum? scale) (let ([s (turn-scale scale)]) (values s s))]
          [(list? scale) (values (turn-scale (car scale)) (turn-scale (cadr scale)))]
          [(pair? scale) (values (turn-scale (car scale)) (turn-scale (cdr scale)))]
          [else (values (turn-scale (real-part scale)) (turn-scale (imag-part scale)))])))

(define geo-path-bezier-point : (->* (Geo-Path Geo-Bezier-Datum) (Flonum Flonum) Float-Complex)
  (lambda [self dpos [sx 1.0] [sy 1.0]]
    (define cpos : Float-Complex (geo-path-here self))
    
    (cond [(real? dpos) (+ cpos (make-rectangular (* (real->double-flonum dpos) sx) 0.0))]
          [(list? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cadr dpos)) sy)))]
          [(pair? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cdr dpos)) sy)))]
          [(complex? dpos) (+ cpos (make-rectangular (* (real->double-flonum (real-part dpos)) sx) (* (real->double-flonum (imag-part dpos)) sy)))]
          [else (geo-trail-ref (geo-path-trail self) dpos)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-move : (-> Geo-Path Float-Complex Float-Complex Char (Option Geo-Anchor-Name) Boolean Void)
  (lambda [self step-args endpt op anchor subpath?]
    (geo-bbox-fit! (geo-path-bbox self) endpt)
    (and anchor (geo-trail-set! (geo-path-trail self) anchor endpt))
    (set-geo-path-here! self endpt)
    (set-geo-path-footprints! self (cons (cons op step-args) (geo-path-footprints self)))

    (when (and subpath?)
      (set-geo-path-origin! self endpt))))

(define geo-path-turn : (-> Geo-Path Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum (Option Geo-Anchor-Name) Boolean (Option (U Float Float-Complex)) Void)
  (lambda [self rx ry cx cy ex ey start end anchor clockwise? guard]
    (define cpos : Float-Complex (geo-path-here self))
    (define cpos++ : Float-Complex (+ (make-rectangular (* ex rx) (* ey ry)) cpos))
    (define path:arc : Path-Args (arc (+ cpos (make-rectangular (* cx rx) (* cy ry))) rx ry start end clockwise?))

    (geo-bbox-fit! (geo-path-bbox self) cpos++)
    (when (and guard)
      (geo-bbox-fit! (geo-path-bbox self) cpos
                     (real->double-flonum (* (real-part guard) rx))
                     (real->double-flonum (* (imag-part guard) ry))))

    (and anchor (geo-trail-set! (geo-path-trail self) anchor cpos++))
    (set-geo-path-here! self cpos++)
    (set-geo-path-footprints! self (cons (cons #\A path:arc) (geo-path-footprints self)))))

(define geo-path-jump-to : (-> Geo-Path (Option Geo-Anchor-Name) Void)
  (lambda [self auto-anchor]
    (define anchor (or auto-anchor (geo-trail-head-anchor (geo-path-trail self))))
    (define pos (geo-trail-ref (geo-path-trail self) anchor))

    (geo-trail-pop! (geo-path-trail self) anchor)
    
    (set-geo-path-origin! self pos)
    (set-geo-path-here! self pos)
    (set-geo-path-footprints! self (cons (cons #\M pos) (geo-path-footprints self)))))

(define geo-path-connect-to : (-> Geo-Path Geo-Anchor-Name Void)
  (lambda [self anchor]
    (define pos (geo-trail-ref (geo-path-trail self) anchor))
    
    (set-geo-path-here! self pos)
    (set-geo-path-footprints! self (cons (cons #\L pos) (geo-path-footprints self)))))

(define geo-path-linear-bezier : (-> Geo-Path Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt anchor]
    (geo-path-move self endpt endpt #\L anchor #false)))

(define geo-path-quadratic-bezier : (-> Geo-Path Float-Complex Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt ctrl anchor]
    (define path:bezier : Path-Args (bezier (geo-path-here self) ctrl endpt))

    (geo-bbox-fit! (geo-path-bbox self) endpt)
    (geo-bbox-fit! (geo-path-bbox self) ctrl)
    (and anchor (geo-trail-set! (geo-path-trail self) anchor endpt))
    (set-geo-path-here! self endpt)
    (set-geo-path-footprints! self (cons (cons #\Q path:bezier) (geo-path-footprints self)))))

(define geo-path-cubic-bezier : (-> Geo-Path Float-Complex Float-Complex Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt ctrl1 ctrl2 anchor]
    (define path:bezier : Path-Args (bezier ctrl1 ctrl2 endpt))

    (geo-bbox-fit! (geo-path-bbox self) endpt)
    (geo-bbox-fit! (geo-path-bbox self) ctrl1)
    (geo-bbox-fit! (geo-path-bbox self) ctrl2)
    (and anchor (geo-trail-set! (geo-path-trail self) anchor endpt))
    (set-geo-path-here! self endpt)
    (set-geo-path-footprints! self (cons (cons #\C path:bezier) (geo-path-footprints self)))))

(define geo-path-close : (-> Geo-Path Void)
  (lambda [self]
    (define init-pos (geo-path-origin self))
    
    (set-geo-path-here! self init-pos)
    (set-geo-path-footprints! self (cons (cons #\Z #false) (geo-path-footprints self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-move-to : (-> Geo-Path Flonum Flonum Flonum Flonum (Option Geo-Anchor-Name) Boolean Void)
  (lambda [self xsize ysize mx my anchor relative?]
    (define arg : Float-Complex (make-rectangular (* xsize mx) (* ysize my)))
    
    (if (not relative?)
        (geo-path-move self arg arg #\M anchor #true)
        (geo-path-move self arg (+ (geo-path-here self) arg) #\m anchor #true))))

(define geo-path-line-to : (-> Geo-Path Flonum Flonum Flonum Flonum (Option Geo-Anchor-Name) Boolean Void)
  (lambda [self xsize ysize mx my anchor relative?]
    (define tpos : Float-Complex (make-rectangular (* xsize mx) (* ysize my)))
    
    (if (not relative?)
        (geo-path-move self tpos tpos #\L anchor #false)
        (geo-path-move self tpos (+ (geo-path-here self) tpos) #\l anchor #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-bounding-box : Geo-Calculate-BBox
  (lambda [self]
    (with-asserts ([self geo-path?])
      (geo-bbox-values (geo-path-bbox self)))))

(define geo-path-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo-path?])
      (define-values (xoff yoff) (geo-bbox-offset-values (geo-path-bbox self)))
      (path_stamp (geo-path-footprints self) xoff yoff
                  (current-stroke-source*) (current-fill-source) (default-fill-rule)
                  (default-geometry-density)))))
