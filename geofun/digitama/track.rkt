#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/unsafe/path)
(require bitmap/digitama/unsafe/visual/object)
(require bitmap/digitama/unsafe/visual/abstract)

(require bitmap/digitama/base)
(require bitmap/digitama/source)

(require bitmap/paint)

(require (for-syntax racket/base))
(require (for-syntax racket/math))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Track-Anchor (U Symbol Keyword))
(define-type Track-Print-Datum Point2D)
(define-type Track-Bezier-Datum (U Track-Print-Datum Track-Anchor))

(struct track visual-object<%>
  ([footprints : (Pairof Path-Print (Listof Path-Print))]
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
(define-syntax (define-dryland-wani-line-move! stx)
  (syntax-case stx []
    [(_ move #:+> sgn)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))]
                   [(xsgn ysgn) (let ([dxy (syntax->datum #'sgn)]) (list (datum->syntax #'sgn (real-part dxy)) (datum->syntax #'sgn (imag-part dxy))))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [xstep : Real 1.0] [ystep : Real 1.0] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani)
                                 (* (real->double-flonum xstep) xsgn) (* (real->double-flonum ystep) ysgn)
                                 anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [xstep : Real 1.0] [ystep : Real 1.0] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani)
                                 (* (real->double-flonum xstep) xsgn) (* (real->double-flonum ystep) ysgn)
                                 anchor #true)))))]
    [(_ move #:-> xsgn)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Integer 1] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) (* (real->double-flonum step) xsgn) 0.0 anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Integer 1] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-move-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) (* (real->double-flonum step) xsgn) 0.0 anchor #true)))))]
    [(_ move #:!> ysgn)
     (with-syntax ([step! (format-id #'move "dryland-wani-step-~a!" (syntax->datum #'move))]
                   [jump! (format-id #'move "dryland-wani-jump-~a!" (syntax->datum #'move))])
       (syntax/loc stx
         (begin (define (step! [wani : Dryland-Wani] [step : Integer 1] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-line-to wani (dryland-wani-xstepsize wani) (dryland-wani-ystepsize wani) 0.0 (* (real->double-flonum step) ysgn) anchor #true))
                
                (define (jump! [wani : Dryland-Wani] [step : Integer 1] [anchor : (Option Track-Anchor) #false]) : Void
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
         (begin (define (turn-1-2! [wani : Dryland-Wani] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-turn wani (dryland-wani-txradius wani) (dryland-wani-tyradius wani)
                              clockwise-args ... rstart rend anchor #true #false))
                
                (define (turn-2-1! [wani : Dryland-Wani] [anchor : (Option Track-Anchor) #false]) : Void
                  (track-turn wani (dryland-wani-txradius wani) (dryland-wani-tyradius wani)
                              counterclockwise-args ... rcstart rcend anchor #false #false)))))]
    [(_ move clockwise? [start end args ...] guard)
     (with-syntax* ([u-turn-move! (format-id #'move "dryland-wani-turn-~a!" (syntax->datum #'move))]
                    [rstart (datum->syntax #'start (degrees->radians (syntax->datum #'start)))]
                    [rend (datum->syntax #'end (degrees->radians (syntax->datum #'end)))])
       (syntax/loc stx
         (begin (define (u-turn-move! [wani : Dryland-Wani] [anchor : (Option Track-Anchor) #false]) : Void
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
    (define cpos : Float-Complex (track-current-position self))
    
    (cond [(real? dpos) (+ cpos (make-rectangular (* (real->double-flonum dpos) sx) 0.0))]
          [(list? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cadr dpos)) sy)))]
          [(pair? dpos) (+ cpos (make-rectangular (* (real->double-flonum (car dpos)) sx) (* (real->double-flonum (cdr dpos)) sy)))]
          [(complex? dpos) (+ cpos (make-rectangular (* (real->double-flonum (real-part dpos)) sx) (* (real->double-flonum (imag-part dpos)) sy)))]
          [else (track-anchor-location self dpos)])))

(define track-anchor-location : (-> Track Track-Anchor Float-Complex)
  (lambda [self anchor]
    (hash-ref (track-anchors self) anchor
              (λ [] (hash-ref (track-anchors self) '#:home
                              (λ [] 0.0+0.0i))))))

(define track-try-anchor! : (-> Track (Option Track-Anchor) Float-Complex Void)
  (lambda [self anchor pos]
    (unless (not anchor)
      (hash-set! (track-anchors self) anchor pos)
      (when (keyword? anchor)
        (set-track-traces! self (cons anchor (track-traces self)))))))

(define track-check-bounding-box! : (-> Track Float-Complex Void)
  (lambda [self pos]
    (define cx++ : Flonum (real-part pos))
    (define cy++ : Flonum (imag-part pos))
    
    (cond [(< cx++ (track-lx self)) (set-track-lx! self cx++)]
          [(> cx++ (track-rx self)) (set-track-rx! self cx++)])
    (cond [(< cy++ (track-ty self)) (set-track-ty! self cy++)]
          [(> cy++ (track-by self)) (set-track-by! self cy++)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-move : (-> Track Float-Complex Float-Complex Char (Option Track-Anchor) Boolean Void)
  (lambda [self args endpt op anchor subpath?]
    (track-check-bounding-box! self endpt)
    (track-try-anchor! self anchor endpt)
    (set-track-current-position! self endpt)
    (set-track-footprints! self (cons (cons op args) (track-footprints self)))

    (when (and subpath?)
      (set-track-initial-position! self endpt))))

(define track-turn : (-> Track Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum (Option Track-Anchor) Boolean (Option (U Float Float-Complex)) Void)
  (lambda [self rx ry cx cy ex ey start end anchor clockwise? guard]
    (define cpos : Float-Complex (track-current-position self))
    (define cpos++ : Float-Complex (+ (make-rectangular (* ex rx) (* ey ry)) cpos))
    (define path:arc : Path-Args (arc (+ cpos (make-rectangular (* cx rx) (* cy ry))) rx ry start end clockwise?))

    (track-check-bounding-box! self cpos++)
    (when (and guard)
      (track-check-bounding-box! self (+ cpos (make-rectangular (* (real-part guard) rx) (* (imag-part guard) ry)))))

    (track-try-anchor! self anchor cpos++)
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

(define track-linear-bezier : (-> Track Float-Complex (Option Track-Anchor) Void)
  (lambda [self endpt anchor]
    (track-move self endpt endpt #\L anchor #false)))

(define track-quadratic-bezier : (-> Track Float-Complex Float-Complex (Option Track-Anchor) Void)
  (lambda [self endpt ctrl anchor]
    (define path:bezier : Path-Args (bezier (track-current-position self) ctrl endpt))

    (track-check-bounding-box! self endpt)
    (track-check-bounding-box! self ctrl)
    
    (track-try-anchor! self anchor endpt)
    (set-track-current-position! self endpt)
    (set-track-footprints! self (cons (cons #\Q path:bezier) (track-footprints self)))))

(define track-cubic-bezier : (-> Track Float-Complex Float-Complex Float-Complex (Option Track-Anchor) Void)
  (lambda [self endpt ctrl1 ctrl2 anchor]
    (define path:bezier : Path-Args (bezier ctrl1 ctrl2 endpt))

    (track-check-bounding-box! self endpt)
    (track-check-bounding-box! self ctrl1)
    (track-check-bounding-box! self ctrl2)
    
    (track-try-anchor! self anchor endpt)
    (set-track-current-position! self endpt)
    (set-track-footprints! self (cons (cons #\C path:bezier) (track-footprints self)))))

(define track-close : (-> Track Void)
  (lambda [self]
    (define init-pos (track-initial-position self))
    
    (set-track-current-position! self init-pos)
    (set-track-footprints! self (cons (cons #\Z #false) (track-footprints self)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-move-to : (-> Track Flonum Flonum Flonum Flonum (Option Track-Anchor) Boolean Void)
  (lambda [self xsize ysize mx my anchor relative?]
    (define tpos : Float-Complex (make-rectangular (* xsize mx) (* ysize my)))
    
    (if (not relative?)
        (track-move self tpos tpos #\M anchor #true)
        (track-move self tpos (+ (track-current-position self) tpos) #\m anchor #true))))

(define track-line-to : (-> Track Flonum Flonum Flonum Flonum (Option Track-Anchor) Boolean Void)
  (lambda [self xsize ysize mx my anchor relative?]
    (define tpos : Float-Complex (make-rectangular (* xsize mx) (* ysize my)))
    
    (if (not relative?)
        (track-move self tpos tpos #\L anchor #false)
        (track-move self tpos (+ (track-current-position self) tpos) #\l anchor #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-surface : (->* (Track) (Stroke-Paint (Option Fill-Paint) Symbol Positive-Flonum) Abstract-Surface)
  (lambda [wani [paint (default-stroke)] [fill #false] [fstyle 'winding]]
    (path_stamp (track-footprints wani)
                (- (track-lx wani)) (- (track-ty wani))
                (stroke-paint->source paint) (fill-paint->source* fill)
                fstyle)))

(define track-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    (with-asserts ([self track?])
      (case mime
        [(pdf-bytes)    (abstract-surface->stream-bytes (track-surface self) 'pdf '/dev/pdfout 1.0)]
        [(svg-bytes)    (abstract-surface->stream-bytes (track-surface self) 'svg '/dev/svgout 1.0)]
        [(png@2x-bytes) (abstract-surface->stream-bytes (track-surface self) 'png '/dev/p2xout 2.0)]
        [(png-bytes)    (abstract-surface->stream-bytes (track-surface self) 'png '/dev/pngout 1.0)]
        [else fallback]))))
