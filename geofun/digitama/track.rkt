#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/dot)
(require bitmap/digitama/source)

(require "bbox.rkt")
(require "anchor.rkt")
(require "unsafe/path.rkt")
(require "unsafe/convert.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/math))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Track-Print-Datum Point2D)
(define-type Track-Bezier-Datum (U Track-Print-Datum Geo-Path-Anchor-Name))

(struct track geo
  ([footprints : (Pairof Path-Print (Listof Path-Print))]
   [path : Geo-Path]
   [bbox : Geo-BBox]
   [origin : Float-Complex]
   [here : Float-Complex])
  #:type-name Track
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
         (begin (define (step! [wani : Dryland-Wani] [xstep : Real 1.0] [ystep : Real 1.0] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani)
                                 (* (real->double-flonum xstep) xsgn) (* (real->double-flonum ystep) ysgn)
                                 anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [xstep : Real 1.0] [ystep : Real 1.0] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani)
                                 (* (real->double-flonum xstep) xsgn) (* (real->double-flonum ystep) ysgn)
                                 anchor #true)))))]
    [(_ move #:-> xsgn)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Real 1] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) (* (real->double-flonum step) xsgn) 0.0 anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Real 1] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) (* (real->double-flonum step) xsgn) 0.0 anchor #true)))))]
    [(_ move #:!> ysgn)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Real 1] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) 0.0 (* (real->double-flonum step) ysgn) anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Real 1] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) 0.0 (* (real->double-flonum step) ysgn) anchor #true)))))]))

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
         (begin (define (turn-1-2! [wani : Dryland-Wani] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-turn wani (dryland-wani-txradius wani) (dryland-wani-tyradius wani)
                              clockwise-args ... rstart rend anchor #true #false))
                
                (define (turn-2-1! [wani : Dryland-Wani] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-turn wani (dryland-wani-txradius wani) (dryland-wani-tyradius wani)
                              counterclockwise-args ... rcstart rcend anchor #false #false)))))]
    [(_ move clockwise? [start end args ...] guard)
     (with-syntax* ([u-turn-move! (format-id #'move "dryland-wani-turn-~a!" (syntax->datum #'move))]
                    [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                    [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))])
       (syntax/loc stx
         (begin (define (u-turn-move! [wani : Dryland-Wani] [anchor : (Option Geo-Path-Anchor-Name) #false]) : Void
                  (track-turn wani (dryland-wani-uxradius wani) (dryland-wani-uyradius wani)
                              args ... rstart rend anchor clockwise? guard)))))]
    [(_ move #:+> args #:boundary-guard guard) (syntax/loc stx (define-dryland-wani-turn-move! move #true args guard))]
    [(_ move #:-> args #:boundary-guard guard) (syntax/loc stx (define-dryland-wani-turn-move! move #false args guard))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define start-of-track : Char #\M)

(struct dryland-wani track
  ([xstepsize : Nonnegative-Flonum]
   [ystepsize : Nonnegative-Flonum]
   [txradius : Nonnegative-Flonum]
   [tyradius : Nonnegative-Flonum]
   [uxradius : Nonnegative-Flonum]
   [uyradius : Nonnegative-Flonum])
  #:type-name Dryland-Wani)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-turn-scales : (-> Track-Print-Datum Nonnegative-Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [scale default-scale]
    (define (turn-scale [datum : Real]) : Nonnegative-Flonum
      (if (> datum 0) (real->double-flonum datum) default-scale))
    
    (cond [(flonum? scale) (let ([s (turn-scale scale)]) (values s s))]
          [(list? scale) (values (turn-scale (car scale)) (turn-scale (cadr scale)))]
          [(pair? scale) (values (turn-scale (car scale)) (turn-scale (cdr scale)))]
          [else (values (turn-scale (real-part scale)) (turn-scale (imag-part scale)))])))

(define track-bezier-point : (->* (Track Track-Bezier-Datum) (Flonum Flonum) Float-Complex)
  (lambda [self dpos [sx 1.0] [sy 1.0]]
    (define cpos : Float-Complex (track-here self))
    
    (cond [(real? dpos) (+ cpos (make-rectangular (* (real->double-flonum dpos) sx) 0.0))]
          [(list? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cadr dpos)) sy)))]
          [(pair? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cdr dpos)) sy)))]
          [(complex? dpos) (+ cpos (make-rectangular (* (real->double-flonum (real-part dpos)) sx) (* (real->double-flonum (imag-part dpos)) sy)))]
          [else (geo-path-ref (track-path self) dpos)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-move : (-> Track Float-Complex Float-Complex Char (Option Geo-Path-Anchor-Name) Boolean Void)
  (lambda [self step-args endpt op anchor subpath?]
    (geo-bbox-fit! (track-bbox self) endpt)
    (and anchor (geo-path-set! (track-path self) anchor endpt))
    (set-track-here! self endpt)
    (set-track-footprints! self (cons (cons op step-args) (track-footprints self)))

    (when (and subpath?)
      (set-track-origin! self endpt))))

(define track-turn : (-> Track Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum (Option Geo-Path-Anchor-Name) Boolean (Option (U Float Float-Complex)) Void)
  (lambda [self rx ry cx cy ex ey start end anchor clockwise? guard]
    (define cpos : Float-Complex (track-here self))
    (define cpos++ : Float-Complex (+ (make-rectangular (* ex rx) (* ey ry)) cpos))
    (define path:arc : Path-Args (arc (+ cpos (make-rectangular (* cx rx) (* cy ry))) rx ry start end clockwise?))

    (geo-bbox-fit! (track-bbox self) cpos++)
    (when (and guard)
      (geo-bbox-fit! (track-bbox self) cpos
                     (real->double-flonum (* (real-part guard) rx))
                     (real->double-flonum (* (imag-part guard) ry))))

    (and anchor (geo-path-set! (track-path self) anchor cpos++))
    (set-track-here! self cpos++)
    (set-track-footprints! self (cons (cons #\A path:arc) (track-footprints self)))))

(define track-jump-to : (-> Track (Option Geo-Path-Anchor-Name) Void)
  (lambda [self auto-anchor]
    (define anchor (or auto-anchor (geo-path-head-anchor (track-path self))))
    (define pos (geo-path-ref (track-path self) anchor))

    (geo-path-pop! (track-path self) anchor)
    
    (set-track-origin! self pos)
    (set-track-here! self pos)
    (set-track-footprints! self (cons (cons #\M pos) (track-footprints self)))))

(define track-connect-to : (-> Track Geo-Path-Anchor-Name Void)
  (lambda [self anchor]
    (define pos (geo-path-ref (track-path self) anchor))
    
    (set-track-here! self pos)
    (set-track-footprints! self (cons (cons #\L pos) (track-footprints self)))))

(define track-linear-bezier : (-> Track Float-Complex (Option Geo-Path-Anchor-Name) Void)
  (lambda [self endpt anchor]
    (track-move self endpt endpt #\L anchor #false)))

(define track-quadratic-bezier : (-> Track Float-Complex Float-Complex (Option Geo-Path-Anchor-Name) Void)
  (lambda [self endpt ctrl anchor]
    (define path:bezier : Path-Args (bezier (track-here self) ctrl endpt))

    (geo-bbox-fit! (track-bbox self) endpt)
    (geo-bbox-fit! (track-bbox self) ctrl)
    (and anchor (geo-path-set! (track-path self) anchor endpt))
    (set-track-here! self endpt)
    (set-track-footprints! self (cons (cons #\Q path:bezier) (track-footprints self)))))

(define track-cubic-bezier : (-> Track Float-Complex Float-Complex Float-Complex (Option Geo-Path-Anchor-Name) Void)
  (lambda [self endpt ctrl1 ctrl2 anchor]
    (define path:bezier : Path-Args (bezier ctrl1 ctrl2 endpt))

    (geo-bbox-fit! (track-bbox self) endpt)
    (geo-bbox-fit! (track-bbox self) ctrl1)
    (geo-bbox-fit! (track-bbox self) ctrl2)
    (and anchor (geo-path-set! (track-path self) anchor endpt))
    (set-track-here! self endpt)
    (set-track-footprints! self (cons (cons #\C path:bezier) (track-footprints self)))))

(define track-close : (-> Track Void)
  (lambda [self]
    (define init-pos (track-origin self))
    
    (set-track-here! self init-pos)
    (set-track-footprints! self (cons (cons #\Z #false) (track-footprints self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-move-to : (-> Track Flonum Flonum Flonum Flonum (Option Geo-Path-Anchor-Name) Boolean Void)
  (lambda [self xsize ysize mx my anchor relative?]
    (define arg : Float-Complex (make-rectangular (* xsize mx) (* ysize my)))
    
    (if (not relative?)
        (track-move self arg arg #\M anchor #true)
        (track-move self arg (+ (track-here self) arg) #\m anchor #true))))

(define track-line-to : (-> Track Flonum Flonum Flonum Flonum (Option Geo-Path-Anchor-Name) Boolean Void)
  (lambda [self xsize ysize mx my anchor relative?]
    (define tpos : Float-Complex (make-rectangular (* xsize mx) (* ysize my)))
    
    (if (not relative?)
        (track-move self tpos tpos #\L anchor #false)
        (track-move self tpos (+ (track-here self) tpos) #\l anchor #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-surface : Geo-Surface-Create
  (lambda [self [paint (default-stroke)] [fill #false] [fstyle 'winding]]
    (with-asserts ([self track?])
      (define-values (xoff yoff) (geo-bbox-offset-values (track-bbox self)))
      (path_stamp (track-footprints self) xoff yoff
                  (stroke-paint->source paint) (fill-paint->source* fill)
                  fstyle))))
