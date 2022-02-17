#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Track-Node (Pairof Char Float-Complex))
(define-type Track-Anchor (U False Symbol Keyword))
(define-type Track-Node-Datum (U Complex (Pairof Real Real)))

(struct track
  ([footprints : (Pairof Track-Node (Listof Track-Node))]
   [anchors : (HashTable Any Float-Complex)]
   [trace : (Option Keyword)]
   [cpos : Float-Complex]
   [lx : Flonum]
   [ty : Flonum]
   [rx : Flonum]
   [by : Flonum])
  #:type-name Track
  #:transparent
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (with-dryland-wani! stx)
  (syntax-case stx []
    [(_ wani (move argl ...) ...)
     (with-syntax* ([(dryland-wani-move ...)
                     (for/list ([<move> (in-list (syntax->list #'(move ...)))])
                       (format-id <move> "dryland-wani-~a!" (syntax->datum <move>)))])
       (syntax/loc stx
         (let ([self wani])
           (dryland-wani-move self argl ...)
           ...
           self)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define start-of-track : Char #\M)

(struct dryland-wani track
  ([xstepsize : Nonnegative-Flonum]
   [ystepsize : Nonnegative-Flonum]
   [xradius : Nonnegative-Flonum]
   [yradius : Nonnegative-Flonum])
  #:type-name Dryland-Wani)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-node-datum : (-> Track-Node-Datum Float-Complex)
  (lambda [pos]
    (cond [(real? pos) (make-rectangular (real->double-flonum pos) 0.0)]
          [(pair? pos) (make-rectangular (real->double-flonum (car pos)) (real->double-flonum (cdr pos)))]
          [else (make-rectangular (real->double-flonum (real-part pos)) (real->double-flonum (imag-part pos)))])))

(define track-anchor : (-> Track Track-Anchor Void)
  (lambda [self anchor]
    (unless (not anchor)
      (when (keyword? anchor)
        (hash-set! (track-anchors self)
                   anchor (cdar (track-footprints self)))
        (set-track-trace! self anchor)))))

(define track-line-to : (-> Track Float-Complex Track-Anchor Boolean Void)
  (lambda [self dpos anchor relative?]
    (define op : Char (if (not relative?) #\L #\l))
    (define cpos : Float-Complex (track-cpos self))
    (define cpos++ : Float-Complex (+ cpos dpos))

    (let ([cx++ (real-part cpos++)]
          [cy++ (imag-part cpos++)])
      (cond [(< cx++ (track-lx self)) (set-track-lx! self cx++)]
            [(> cx++ (track-rx self)) (set-track-rx! self cx++)])
      (cond [(< cy++ (track-ty self)) (set-track-ty! self cy++)]
            [(> cy++ (track-by self)) (set-track-by! self cy++)]))

    (when (keyword? anchor)
      (set-track-trace! self anchor))
    
    (set-track-cpos! self cpos++)
    (set-track-footprints! self (cons (cons op dpos) (track-footprints self)))))
