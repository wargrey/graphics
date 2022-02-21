#lang typed/racket/base

(provide (all-defined-out))

(require file/convertible)

(require "unsafe/path.rkt")
(require "unsafe/convert.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/math))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Track-Node (Pairof Char (U Float-Complex Path-Args)))
(define-type Track-Anchor (U Symbol Keyword))
(define-type Track-Node-Datum (U Complex (Pairof Real Real)))

(struct track visual-object<%>
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

(define-syntax (define-dryland-wani-line-move! stx)
  (syntax-case stx []
    [(_ move #:+> delta)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))]
                   [(xdelta ydelta) (let ([dxy (syntax->datum #'delta)]) (list (datum->syntax #'delta (real-part dxy)) (datum->syntax #'delta (imag-part dxy))))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [xstep : Real 1.0] [ystep : Real 1.0] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani)
                                 (* (real->double-flonum xstep) xdelta) (* (real->double-flonum ystep) ydelta)
                                 anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [xstep : Real 1.0] [ystep : Real 1.0] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani)
                                 (* (real->double-flonum xstep) xdelta) (* (real->double-flonum ystep) ydelta)
                                 anchor #true)))))]
    [(_ move #:-> xdelta)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Integer 1] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) (* (real->double-flonum step) xdelta) 0.0 anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Integer 1] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) (* (real->double-flonum step) xdelta) 0.0 anchor #true)))))]
    [(_ move #:!> ydelta)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Integer 1] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) 0.0 (* (real->double-flonum step) ydelta) anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Integer 1] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) 0.0 (* (real->double-flonum step) ydelta) anchor #true)))))]))

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
         (begin (define (turn-1-2! [wani : Dryland-Wani] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-turn wani (dryland-wani-xradius wani) (dryland-wani-yradius wani)
                              clockwise-args ... rstart rend anchor #true #false))
                
                (define (turn-2-1! [wani : Dryland-Wani] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-turn wani (dryland-wani-xradius wani) (dryland-wani-yradius wani)
                              counterclockwise-args ... rcstart rcend anchor #false #false)))))]
    [(_ move #:+> [start end clockwise-args ...] #:boundary-guard guard)
     (with-syntax ([u-turn-move! (format-id #'move "dryland-wani-turn-~a!" (syntax->datum #'move))]
                   [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                   [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))])
       (syntax/loc stx
         (begin (define (u-turn-move! [wani : Dryland-Wani] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-turn wani (dryland-wani-xradius wani) (dryland-wani-yradius wani)
                              clockwise-args ... rstart rend anchor #true guard)))))]
    [(_ move #:-> [start end c-clockwise-args ...] #:boundary-guard guard)
     (with-syntax ([u-turn-move! (format-id #'move "dryland-wani-turn-~a!" (syntax->datum #'move))]
                   [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                   [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))])
       (syntax/loc stx
         (begin (define (u-turn-move! [wani : Dryland-Wani] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-turn wani (dryland-wani-xradius wani) (dryland-wani-yradius wani)
                              c-clockwise-args ... rstart rend anchor #false guard)))))]))

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
(define track-move : (-> Track Flonum Flonum Flonum Flonum Char (Option Track-Anchor) Boolean Void)
  (lambda [self xsize ysize mx my op anchor subpath?]
    (define dpos : Float-Complex (make-rectangular (* xsize mx) (* ysize my)))
    (define cpos : Float-Complex (track-current-position self))
    (define cpos++ : Float-Complex (+ cpos dpos))

    (track-check-bounding-box! self cpos++)
    (track-anchor self anchor cpos++)
    
    (set-track-current-position! self cpos++)
    (set-track-footprints! self (cons (cons op dpos) (track-footprints self)))

    (when (and subpath?)
      (set-track-initial-position! self cpos++))))

(define track-turn : (-> Track Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum (Option Track-Anchor) Boolean (Option (U Float Float-Complex)) Void)
  (lambda [self rx ry cx cy ex ey start end anchor clockwise? guard]
    (define cpos : Float-Complex (track-current-position self))
    (define cpos++ : Float-Complex (+ (make-rectangular (* ex rx) (* ey ry)) cpos))
    (define path:arc : Path-Args (arc (+ cpos (make-rectangular (* cx rx) (* cy ry))) rx ry start end clockwise?))

    (track-check-bounding-box! self cpos++)
    (when (and guard) (track-check-bounding-box! self (+ cpos (make-rectangular (* (real-part guard) rx) (* (imag-part guard) ry)))))
    (track-anchor self anchor cpos++)
    
    (set-track-current-position! self cpos++)
    (set-track-footprints! self (cons (cons #\A path:arc) (track-footprints self)))))

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
(define track-move-to : (-> Track Flonum Flonum Flonum Flonum (Option Track-Anchor) Boolean Void)
  (lambda [self xsize ysize mx my anchor relative?]
    (track-move self xsize ysize mx my (if (not relative?) #\M #\m) anchor #true)))

(define track-line-to : (-> Track Flonum Flonum Flonum Flonum (Option Track-Anchor) Boolean Void)
  (lambda [self xsize ysize mx my anchor relative?]
    (track-move self xsize ysize mx my (if (not relative?) #\L #\l) anchor #false)))
