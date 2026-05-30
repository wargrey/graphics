#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")
(require "datum.rkt")
(require "trace.rkt")
(require "anchor.rkt")

(require "../base.rkt")
(require "../self.rkt")
(require "../layer/type.rkt")
(require "../layer/sticker.rkt")
(require "../geometry/bezier.rkt")
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
(define #:forall (T) geo-track-drift : (-> (∩ T Geo:Track) Geo-Bezier-Datum (Listof Geo-Bezier-Datum) (Option Geo-Anchor-Name)
                                           (-> (∩ T Geo:Track) Number Float-Complex) Flonum)
  (lambda [goma end-step ctrl-steps anchor position]
    (define endpt ((inst geo-track-bezier-point T) goma end-step position))
    (define ctrls : (Listof Float-Complex)
      (for/list : (Listof Float-Complex) ([ctrl (in-list ctrl-steps)])
        ((inst geo-track-bezier-point T) goma ctrl position)))
    
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

(define geo-track-stamp-append : (-> Geo:Track Geo-Sticker Void)
  (lambda [goma sticker]
    (set-geo:track-stickers! goma
                             (append (geo:track-stickers goma)
                                     (list (cons sticker (geo:track-here goma)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) geo-track-jump-to
  : (case-> [(∩ T Geo:Track) Geo-Anchor-Name -> Void]
            [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) Complex (Option Geo-Anchor-Name) (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) Complex (Option Geo-Anchor-Name) Float-Complex (-> T Complex Float-Complex) -> Void])
  (case-lambda
    [(self target) (geo-track-jump-to-position self (geo-track-resolve-position self target))]
    [(self target transform)
     (let ([pos ((inst geo-track-resolve-position T) self target transform)])
       (when (complex? target) (geo-track-try-fit! self pos))
       (geo-track-jump-to-position self pos))]
    [(self target anchor transform) ((inst geo-track-jump-to T) self target anchor 0.0+0.0i transform)]
    [(self target anchor offset transform)
     (let ([pos ((inst geo-track-resolve-position T) self target offset transform)])
       (geo-track-try-fit! self anchor pos)
       (geo-track-jump-to-position self pos))]))

(define #:forall (T) geo-track-connect-to
  : (case-> [(∩ T Geo:Track) Geo-Anchor-Name Any -> Void]
            [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) Any (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) Complex (Option Geo-Anchor-Name) Any (-> T Complex Float-Complex) -> Void]
            [(∩ T Geo:Track) Complex (Option Geo-Anchor-Name) Any Float-Complex (-> T Complex Float-Complex) -> Void])
  (case-lambda
    [(self target info) (geo-track-connect-to-position self (geo-track-resolve-position self target) info)]
    [(self target info transform)
     (let ([pos ((inst geo-track-resolve-position T) self target transform)])
       (when (complex? target) (geo-track-try-fit! self pos))
       (geo-track-connect-to-position self pos info))]
    [(self target anchor info transform) ((inst geo-track-connect-to T) self target anchor info 0.0+0.0i transform)]
    [(self target anchor info offset transform) ; used by the `radial move`
     (let ([pos ((inst geo-track-resolve-position T) self target offset transform)])
       (geo-track-try-fit! self anchor pos)
       (geo-track-connect-to-position self pos info))]))

(define #:forall (T) geo-track-resolve-position : (case-> [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) -> Float-Complex]
                                                          [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) (-> T Complex Float-Complex) -> Float-Complex]
                                                          [(∩ T Geo:Track) (U Geo-Anchor-Name Complex) Float-Complex (-> T Complex Float-Complex) -> Float-Complex])
  (case-lambda
    [(self target) (if (complex? target) (geo-track-position-identity self target) (geo-trace-ref (geo:track-trace self) target))]
    [(self target transform) (if (complex? target) (transform self target) (geo-trace-ref (geo:track-trace self) target))]
    [(self target offset transform) (+ offset ((inst geo-track-resolve-position T) self target transform))]))

(define geo-track-jump-to-position : (-> Geo:Track Float-Complex Void)
  (lambda [self abs-pos]
    (set-geo:track-origin! self abs-pos)
    (set-geo:track-here! self abs-pos)
    (set-geo:track-footprints! self (cons (gpp:point #\M abs-pos)
                                          (geo:track-footprints self)))))

(define geo-track-connect-to-position : (-> Geo:Track Float-Complex Any Void)
  (lambda [self abs-pos info]
    (unless (not info)
      (hash-set! (geo:track-foot-infos self)
                 (cons (geo:track-here self) abs-pos)
                 (geo-track-info info)))
    
    (set-geo:track-here! self abs-pos)
    (set-geo:track-footprints! self (cons (gpp:point #\L abs-pos)
                                          (geo:track-footprints self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) geo-track-bezier-point : (-> (∩ T Geo:Track) Geo-Bezier-Datum (-> (∩ T Geo:Track) Number Float-Complex) Float-Complex)
  (lambda [self dpos position]
    (define cpos : Float-Complex (geo:track-here self))
    
    (cond [(complex? dpos) (+ cpos (position self dpos))]
          [(list? dpos) (+ cpos (position self (make-rectangular (car dpos) (cadr dpos))))]
          [(pair? dpos) (+ cpos (position self (make-rectangular (car dpos) (cdr dpos))))]
          [else (geo-trace-ref (geo:track-trace self) dpos)])))

(define geo-track-linear-bezier : (-> Geo:Track Float-Complex (Option Geo-Anchor-Name) Flonum)
  (lambda [self endpt anchor]
    (define here (geo:track-here self))
    
    (geo-track-do-move self endpt #\L anchor #false #false)
    (angle (- endpt here))))

(define geo-track-quadratic-bezier : (-> Geo:Track Float-Complex Float-Complex (Option Geo-Anchor-Name) Flonum)
  (lambda [self endpt ctrl anchor]
    (define head (geo:track-here self))
    (define path:bezier (gpp:bezier:quadratic #\Q endpt head (default-bezier-samples) ctrl))

    (geo-track-try-fit! self anchor endpt (bezier-quadratic-extremities head ctrl endpt))
    (set-geo:track-here! self endpt)
    (set-geo:track-footprints! self (cons path:bezier (geo:track-footprints self)))

    (let-values ([(_hpt _hrad _tpt trad) (gpp-bezier-endpoints path:bezier)])
      trad)))

(define geo-track-cubic-bezier : (-> Geo:Track Float-Complex Float-Complex Float-Complex (Option Geo-Anchor-Name) Flonum)
  (lambda [self endpt ctrl1 ctrl2 anchor]
    (define head (geo:track-here self))
    (define path:bezier (gpp:bezier:cubic #\C endpt head (default-bezier-samples) ctrl1 ctrl2))

    (geo-track-try-fit! self anchor endpt (bezier-cubic-extremities head ctrl1 ctrl2 endpt))
    (set-geo:track-here! self endpt)
    (set-geo:track-footprints! self (cons path:bezier (geo:track-footprints self)))

    (let-values ([(_hpt _hrad _tpt trad) (gpp-bezier-endpoints path:bezier)])
      trad)))
