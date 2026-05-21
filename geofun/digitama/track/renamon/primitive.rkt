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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Maybe-Avatar (U Geo False Void))

(define current-renamon-primitive-name : (Parameterof (Option String)) (make-parameter #false))
(define current-renamon-anchor-format : (Parameterof (Option String)) (make-parameter #false))

(define default-renamon-description-format : (Parameterof String) (make-parameter "~a (order = ~a)"))
(define default-renamon-order : (Parameterof Byte) (make-parameter 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct renamon-info : Renamon-Info
  ([heading : Flonum]
   [avatar : (Option Geo)])
  #:transparent)

(struct renamon geo:track
  ([box : (Boxof Renamon-Info)]
   [stepsize : Positive-Flonum]
   [angle-delta : Flonum]
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

(define renamon-stamp : (-> Renamon Geo Void)
  (lambda [self gobj]
    (geo-track-stamp self
                     (geo-rotate gobj (* (renamon-heading self)
                                         (renamon-tp-sgn self))))))

(define renamon-evolve : (-> Renamon Maybe-Avatar Void)
  (lambda [self avatar]
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
