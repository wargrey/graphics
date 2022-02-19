#lang typed/racket/base

(provide (all-defined-out))

(require "unsafe/path.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Track-Node (Pairof Char (U Float-Complex Path-Args)))
(define-type Track-Anchor (U Symbol Keyword))
(define-type Track-Node-Datum (U Complex (Pairof Real Real)))

(struct track
  ([footprints : (Pairof Track-Node (Listof Track-Node))]
   [anchors : (HashTable Any Float-Complex)]
   [traces : (Pairof Keyword (Listof Keyword))]
   [initial-position : Float-Complex]
   [current-position : Float-Complex]
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

(define-syntax (define-dryland-wani-step/jump-move! stx)
  (syntax-case stx []
    [(_ move #:with wani step ... #:=> sexp)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Integer 1] ... [anchor : (Option Track-Anchor) #false]) : Void
                  (track-line-to wani sexp anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Integer 1] ... [anchor : (Option Track-Anchor) #false]) : Void
                    (track-move-to wani sexp anchor #true)))))]))

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

(define track-anchor : (-> Track (Option Track-Anchor) Float-Complex Void)
  (lambda [self anchor pos]
    (unless (not anchor)
      (hash-set! (track-anchors self) anchor pos)
      (when (keyword? anchor)
        (set-track-traces! self (cons anchor (track-traces self)))))))

(define track-anchor-location : (-> Track Track-Anchor Float-Complex)
  (lambda [self anchor]
    (hash-ref (track-anchors self) anchor
              (λ [] (hash-ref (track-anchors self) '#:home
                              (λ [] 0.0+0.0i))))))

(define track-check-bounding-box! : (-> Track Float-Complex Void)
  (lambda [self pos]
    (define cx++ : Flonum (real-part pos))
    (define cy++ : Flonum (imag-part pos))
    
    (cond [(< cx++ (track-lx self)) (set-track-lx! self cx++)]
          [(> cx++ (track-rx self)) (set-track-rx! self cx++)])
    (cond [(< cy++ (track-ty self)) (set-track-ty! self cy++)]
          [(> cy++ (track-by self)) (set-track-by! self cy++)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-move : (-> Track Float-Complex Char (Option Track-Anchor) Boolean Void)
  (lambda [self dpos op anchor subpath?]
    (define cpos : Float-Complex (track-current-position self))
    (define cpos++ : Float-Complex (+ cpos dpos))

    (track-check-bounding-box! self cpos++)
    (track-anchor self anchor cpos++)
    
    (set-track-current-position! self cpos++)
    (set-track-footprints! self (cons (cons op dpos) (track-footprints self)))

    (when (and subpath?)
      (set-track-initial-position! self cpos++))))

(define track-jump-to : (-> Track (Option Track-Anchor) Void)
  (lambda [self anchor]
    (define pos (track-anchor-location self (or anchor (car (track-traces self)))))

    (when (not anchor)
      (define traces-- (cdr (track-traces self)))

      (when (pair? traces--)
        (set-track-traces! self traces--)))

    (set-track-initial-position! self pos)
    (set-track-current-position! self pos)
    (set-track-footprints! self (cons (cons #\M pos) (track-footprints self)))))

(define track-connect-to : (-> Track Track-Anchor Void)
  (lambda [self anchor]
    (define pos (track-anchor-location self anchor))
    
    (set-track-current-position! self pos)
    (set-track-footprints! self (cons (cons #\L pos) (track-footprints self)))))

(define track-close : (-> Track Void)
  (lambda [self]
    (define init-pos (track-initial-position self))
    
    (set-track-current-position! self init-pos)
    (set-track-footprints! self (cons (cons #\Z path:none) (track-footprints self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-move-to : (-> Track Float-Complex (Option Track-Anchor) Boolean Void)
  (lambda [self dpos anchor relative?]
    (track-move self dpos (if (not relative?) #\M #\m) anchor #true)))

(define track-line-to : (-> Track Float-Complex (Option Track-Anchor) Boolean Void)
  (lambda [self dpos anchor relative?]
    (track-move self dpos (if (not relative?) #\L #\l) anchor #false)))

(define track-clockwise-turn : (-> Track Float Float (Option Track-Anchor) Boolean Void)
  (lambda [self cx cy anchor relative?]
    (void)))
