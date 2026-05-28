#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

(require digimon/measure)
(require digimon/flonum)
(require digimon/struct)

(require "../self.rkt")
(require "../anchor.rkt")
(require "../primitives.rkt")

(require "../../self.rkt")
(require "../../dc/resize.rkt")
(require "../../layer/sticker.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Maybe-Avatar (U Geo False Void))
(define-type Maybe-Renamon-Ribbon (U Renamon-Ribbon False Void))
(define-type Renamon-Ribbon (-> Renamon Float-Complex Float-Complex (U Geo False Void)))

(define default-renamon-description-format : (Parameterof String) (make-parameter "~a (n = ~a, δ = ~a°)"))
(define default-renamon-order : (Parameterof Integer) (make-parameter 0))
(define default-renamon-terminal-order : (Parameterof Byte) (make-parameter 12))
(define default-renamon-angle : (Parameterof Flonum) (make-parameter pi/4))

(define current-renamon-primitive-name : (Parameterof (Option String)) (make-parameter #false))
(define current-renamon-anchor-format : (Parameterof (Option String)) (make-parameter #false))
(define current-renamon-ribbon : (Parameterof Maybe-Renamon-Ribbon) (make-parameter (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct renamon-info : Renamon-Info
  ([heading : Flonum]
   [avatar : (Option Geo)]
   [ribbon : (Option Renamon-Ribbon)])
  #:transparent)

(struct renamon geo:track
  ([box : (Boxof Renamon-Info)]
   [stepsize : Positive-Flonum]
   [angle-delta : (Option Flonum)]
   [tp-sgn : Flonum])
  #:type-name Renamon
  #:transparent)

(define renamon-heading : (-> Renamon Flonum)
  (lambda [self]
    (renamon-info-heading (unbox (renamon-box self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define renamon-turn : (-> Renamon Flonum Void)
  (lambda [self rad]
    (renamon-turn-to self (+ (renamon-heading self) rad))))

(define renamon-turn-to : (-> Renamon Flonum Void)
  (lambda [self rad]
    (define &box (renamon-box self))
    
    (set-box! &box
              (remake-renamon-info #:heading (~wrap rad 2pi)
                                   (unbox &box)))))

(define renamon-towards : (-> Renamon (U Geo-Anchor-Name Complex) (Option Flonum))
  (lambda [self target]
    (define spt : Float-Complex (geo:track-here self))
    (define ept : Float-Complex (geo-track-resolve-position self target renamon-position))
    
    (if (and (flnear? (real-part ept) (real-part spt))
             (flnear? (imag-part ept) (imag-part spt)))
        #false
        (~wrap (* (angle (- ept spt)) (renamon-tp-sgn self)) 2pi))))

(define renamon-stamp : (case-> [Renamon Geo -> Void]
                                [Renamon Geo Complex -> Void])
  (case-lambda
    [(self gobj) (geo-track-stamp self (renamon-rotate self gobj))]
    [(self gobj offset)
     (geo-track-stamp self
                      (make-sticker (renamon-rotate self gobj)
                                    'cc
                                    (renamon-position self offset)))]))

(define renamon-pave : (->* (Renamon Float-Complex Float-Complex) (Maybe-Renamon-Ribbon) Void)
  (lambda [self start end [maybe-ribbon (current-renamon-ribbon)]] ; for parameteric L-systems
    (define &box (renamon-box self))
    (define datum (unbox &box))
    (define ribbon (renamon-info-ribbon datum))

    (unless (void? maybe-ribbon)
      (unless (eq? ribbon maybe-ribbon)
        (set-box! &box
                  (remake-renamon-info #:ribbon maybe-ribbon
                                       datum))))

    (define trail
      (cond [(void? maybe-ribbon) (when ribbon (ribbon self start end))]
            [(or maybe-ribbon) (maybe-ribbon self start end)]))

    (when (geo? trail)
      (geo-track-stamp self
                       (make-sticker (renamon-rotate self trail)
                                     'cc (* 0.5 (- start end)))))))

(define renamon-evolve : (->* (Renamon) (Maybe-Avatar) Void)
  (lambda [self [avatar (void)]]
    (define &box (renamon-box self))
    (define datum (unbox &box))
    (define rena (renamon-info-avatar datum))

    (unless (void? avatar)
      (unless (eq? rena avatar)
        (set-box! &box
                  (remake-renamon-info #:avatar avatar
                                       datum))))
    
    (cond [(void? avatar) (when rena (renamon-stamp self rena))]
          [(or avatar) (renamon-stamp self avatar)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define renamon-position : (-> Renamon Complex Float-Complex)
  (lambda [self pos]
    (make-rectangular (* (real->double-flonum (real-part pos)) (renamon-stepsize self))
                      (* (real->double-flonum (imag-part pos)) (renamon-stepsize self) (renamon-tp-sgn self)))))

(define renamon-rotate : (-> Renamon Geo Geo)
  (lambda [self gobj]
    (geo-rotate gobj (* (renamon-heading self)
                        (renamon-tp-sgn self)))))

(define renamon-anchor-prefix : (-> Symbol String)
  (lambda [name]
    (define fmt (current-renamon-anchor-format))
    (define prefix (current-renamon-primitive-name))

    (cond [(and fmt prefix) (format fmt prefix name)]
          [else (symbol->immutable-string name)])))

(define renamon-anchor-rewrite : (-> (Option Geo-Anchor-Name) (Option Geo-Anchor-Name))
  (lambda [name]
    (and name
         (let ([fmt (current-renamon-anchor-format)]
               [prefix (current-renamon-primitive-name)])
           (if (and fmt prefix)
               (if (keyword? name)
                   (string->keyword (format fmt prefix name))
                   (string->symbol (format fmt prefix name)))
               name)))))
