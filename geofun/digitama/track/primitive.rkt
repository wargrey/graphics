#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")
(require "datum.rkt")
(require "trail.rkt")
(require "anchor.rkt")

(require "../base.rkt")
(require "../self.rkt")
(require "../layer/type.rkt")
(require "../layer/sticker.rkt")
(require "../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Step-Datum (U Real Geo-Anchor-Name))
(define-type Geo-Bezier-Datum (U Geo-Print-Datum Geo-Anchor-Name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) geo-track-position-identity : (-> T Complex Float-Complex)
  (lambda [self pos]
    (make-rectangular (real->double-flonum (real-part pos))
                      (real->double-flonum (imag-part pos)))))

(define geo-track-do-move : (-> Geo:Track Float-Complex Char (Option Geo-Anchor-Name) Any Boolean Void)
  (lambda [self endpt op anchor info subpath?]
    (unless (not info)
      (hash-set! (geo:track-foot-infos self)
                 (cons (geo:track-here self) endpt)
                 (geo-track-info info)))
    
    (geo-track-try-fit! self anchor endpt)
    (set-geo:track-here! self endpt)
    (set-geo:track-footprints! self (cons (gpp:point op endpt) (geo:track-footprints self)))

    (when (and subpath?)
      (set-geo:track-origin! self endpt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-drift : (-> Geo:Track Geo-Bezier-Datum (Listof Geo-Bezier-Datum) (Option Geo-Anchor-Name) Flonum Flonum Void)
  (lambda [goma end-step ctrl-steps anchor xsize ysize]
    (define endpt : Float-Complex (geo-track-bezier-point goma end-step xsize ysize))
    (define ctrls : (Listof Float-Complex)
      (for/list : (Listof Float-Complex) ([ctrl (in-list ctrl-steps)])
        (geo-track-bezier-point goma ctrl xsize ysize)))

    (cond [(null? ctrls) (geo-track-linear-bezier goma endpt anchor)]
          [(null? (cdr ctrls)) (geo-track-quadratic-bezier goma endpt (car ctrls) anchor)]
          [else (geo-track-cubic-bezier goma endpt (car ctrls) (cadr ctrls) anchor)])))

(define geo-track-close : (-> Geo:Track Void)
  (lambda [self]
    (define init-pos (geo:track-origin self))
    
    (set-geo:track-here! self init-pos)
    (set-geo:track-footprints! self (cons the-Z (geo:track-footprints self)))))

(define geo-track-stamp : (case-> [Geo:Track Geo-Sticker -> Void]
                                  [Geo:Track Geo -> Void]
                                  [Geo:Track Geo Geo-Pin-Anchor -> Void])
  (case-lambda
    [(goma gobj)
     (define sticker : Geo-Sticker
       (cond [(geo-sticker? gobj) gobj]
             [else (make-sticker gobj 'cc)]))
     
     (set-geo:track-stickers! goma
                              (cons (cons sticker (geo:track-here goma))
                                    (geo:track-stickers goma)))]
    [(goma gobj anchor) (geo-track-stamp goma (make-sticker gobj anchor))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) geo-track-move-to
  : (case-> [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) Geo-Anchor-Name Any (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) Complex (Option Geo-Anchor-Name) (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) Complex (Option Geo-Anchor-Name) Any (-> T Complex Float-Complex) -> Void])
  (case-lambda
    [(goma target transform)
     ((inst geo-track-connect-to T) goma target #false #false transform)]
    [(goma target argument transform)
     (if (complex? target)
         ((inst geo-track-connect-to T) goma target argument #false transform)
         ((inst geo-track-connect-to T) goma target #false argument transform))]
    [(goma target anchor info transform)
     ((inst geo-track-connect-to T) goma target anchor info transform)]))

(define #:forall (T) geo-track-jump-to
  : (case-> [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) (Option Geo-Anchor-Name) (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) Complex (Option Geo-Anchor-Name) Float-Complex (-> T Complex Float-Complex) -> Void]
            [Geo:Track Float-Complex -> Void])
  (case-lambda
    [(self anchor pos-anchor transform)
     (define pos : Float-Complex ((inst geo-track-target-position T) self anchor transform))
     (when (complex? anchor) (geo-track-try-fit! self pos-anchor pos))
     (geo-track-jump-to self pos)]
    [(self target anchor offset transform) ; used by the `radial back`
     (let ([pos ((inst geo-track-target-position T) self target offset transform)])
       (geo-track-try-fit! self anchor pos)
       (geo-track-jump-to self pos))]
    [(self abs-pos) ; used by the `radial move` and `geo-back-jump-to`
     (set-geo:track-origin! self abs-pos)
     (set-geo:track-here! self abs-pos)
     (set-geo:track-footprints! self (cons (gpp:point #\M abs-pos) (geo:track-footprints self)))]))

(define #:forall (T) geo-track-connect-to
  : (case-> [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) (Option Geo-Anchor-Name) Any Float-Complex (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) (Option Geo-Anchor-Name) Any (-> T Complex Float-Complex) -> Void]
            [Geo:Track Float-Complex Any -> Void])
  (case-lambda
    [(self target pos-anchor info transform) ((inst geo-track-connect-to T) self target pos-anchor info 0.0+0.0i transform)]
    [(self target pos-anchor info offset transform) ; used by the `radial move`
     (let ([pos ((inst geo-track-target-position T) self target offset transform)])
       (when (complex? target) (geo-track-try-fit! self pos-anchor pos))
       (geo-track-connect-to self pos info))]
    [(self abs-pos info) ; used by the `radial back` and `geo-track-connect-to`
     (unless (not info)
       (hash-set! (geo:track-foot-infos self)
                  (cons (geo:track-here self) abs-pos)
                  (geo-track-info info)))
       
     (set-geo:track-here! self abs-pos)
     (set-geo:track-footprints! self (cons (gpp:point #\L abs-pos) (geo:track-footprints self)))]))

(define #:forall (T) geo-track-target-position : (case-> [Geo:Track (U Geo-Anchor-Name Complex) -> Float-Complex]
                                                         [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) (-> T Complex Float-Complex) -> Float-Complex]
                                                         [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) Float-Complex (-> T Complex Float-Complex) -> Float-Complex])
  (case-lambda
    [(self target) (if (complex? target) (geo-track-position-identity self target) (geo-trail-ref (geo:track-trail self) target))]
    [(self target transform) (if (complex? target) (transform self target) (geo-trail-ref (geo:track-trail self) target))]
    [(self target offset transform) (+ offset ((inst geo-track-target-position T) self target transform))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-bezier-point : (->* (Geo:Track Geo-Bezier-Datum) (Flonum Flonum) Float-Complex)
  (lambda [self dpos [sx 1.0] [sy 1.0]]
    (define cpos : Float-Complex (geo:track-here self))
    
    (cond [(real? dpos) (+ cpos (make-rectangular (* (real->double-flonum dpos) sx) 0.0))]
          [(list? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cadr dpos)) sy)))]
          [(pair? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cdr dpos)) sy)))]
          [(complex? dpos) (+ cpos (make-rectangular (* (real->double-flonum (real-part dpos)) sx) (* (real->double-flonum (imag-part dpos)) sy)))]
          [else (geo-trail-ref (geo:track-trail self) dpos)])))

(define geo-track-linear-bezier : (-> Geo:Track Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt anchor]
    (geo-track-do-move self endpt #\L anchor #false #false)))

(define geo-track-quadratic-bezier : (-> Geo:Track Float-Complex Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt ctrl anchor]
    (define path:bezier (gpp:bezier:quadratic #\Q endpt (geo:track-here self) (default-bezier-samples) ctrl))

    (geo-track-try-fit! self anchor endpt ctrl)
    (set-geo:track-here! self endpt)
    (set-geo:track-footprints! self (cons path:bezier (geo:track-footprints self)))))

(define geo-track-cubic-bezier : (-> Geo:Track Float-Complex Float-Complex Float-Complex (Option Geo-Anchor-Name) Void)
  (lambda [self endpt ctrl1 ctrl2 anchor]
    (define path:bezier (gpp:bezier:cubic #\C endpt (geo:track-here self) (default-bezier-samples) ctrl1 ctrl2))

    (geo-track-try-fit! self anchor endpt ctrl1 ctrl2)
    (set-geo:track-here! self endpt)
    (set-geo:track-footprints! self (cons path:bezier (geo:track-footprints self)))))
