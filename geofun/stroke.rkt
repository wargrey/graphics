#lang typed/racket/base

(provide (all-defined-out))

(require "color.rkt")

(require "digitama/base.rkt")
(require "digitama/stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://svgwg.org/svg2-draft/painting.html
(define-type CSS-Gradient-Stop-Color (Pairof Real FlRGBA))
(define-type CSS-Linear-Gradient (Vector Real Real Real Real (Listof CSS-Gradient-Stop-Color)))
(define-type CSS-Radial-Gradient (Vector Real Real Real Real Real Real (Listof CSS-Gradient-Stop-Color)))

; Don't forget unsafe/paint.rkt if changing the Stroke
(struct stroke paint
  ([color : FlRGBA]
   [width : Nonnegative-Flonum]
   [linecap : Stroke-Cap-Style]
   [linejoin : Stroke-Join-Style]
   [miterlimit : Flonum]
   [dash : (Vectorof Nonnegative-Flonum)]
   [offset : Flonum])
  #:type-name Stroke
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-stroke : (Parameterof Stroke) (make-parameter (stroke black 1.0 'butt 'miter +nan.0 solid-dash 0.0)))
(define default-border : (Parameterof Stroke) (make-parameter (stroke hilite (border-thickness->integer 'medium)
                                                                      'butt 'miter +nan.0 solid-dash 0.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desc-stroke : (->* ()
                           (Stroke #:color (Option Color) #:opacity Real #:width (Option Real)
                                   #:cap (Option Symbol) #:join (U Symbol Real False)
                                   #:dash (U Symbol (Vectorof Nonnegative-Flonum) False) #:offset (Option Real))
                           Stroke)
  (lambda [[baseline (default-stroke)] #:color [color #false] #:opacity [opacity 1.0] #:width [width #false]
                                       #:cap [cap #false] #:join [join #false] #:dash [dash #false] #:offset [offset #false]]
    (define linewidth : Nonnegative-Flonum
      (cond [(not width) (stroke-width baseline)]
            [else (let ([flw (real->double-flonum width)])
                    (cond [(>= flw 0.0) flw]
                          [else (* (abs flw) (stroke-width baseline))]))]))
    (define-values (linejoin miterlimit)
      (cond [(stroke-line-join-option? join) (values join (stroke-miterlimit baseline))]
            [(real? join) (values 'miter (real->double-flonum join))]
            [else (values (stroke-linejoin baseline) (stroke-miterlimit baseline))]))
    (define-values (preoffset dasharray)
      (cond [(stroke-dash-style? dash) (line-dash->array dash linewidth)]
            [(vector? dash) (values (stroke-offset baseline) (dasharray-normalize dash linewidth))]
            [else (values (stroke-offset baseline) (dasharray-normalize (stroke-dash baseline) linewidth (stroke-width baseline)))]))
    (define dashoffset : Flonum (if (not offset) preoffset (real->double-flonum offset)))
    (stroke (rgb* (or color (stroke-color baseline)) opacity) linewidth
            (cond [(stroke-line-cap-option? cap) cap] [(eq? dash 'dot) 'round] [else (stroke-linecap baseline)])
            linejoin miterlimit dasharray dashoffset)))

(define desc-border : (->* () (Stroke #:color (Option Color) #:width (U Real Symbol False) #:style (Option Symbol)) Stroke)
  (lambda [[baseline (default-border)] #:color [color #false] #:width [width #false] #:style [dash #false]]
    (define no-border? : Boolean (or (eq? dash 'none) (eq? dash 'hidden)))
    (define linewidth : Nonnegative-Flonum
      (cond [(and no-border?) 0.0]
            [(real? width) (let ([flw (real->double-flonum width)]) (if (>= flw 0.0) flw (* (abs flw) (stroke-width baseline))))]
            [(css-border-thickness-option? width) (border-thickness->integer width)]
            [else (stroke-width baseline)]))
    (define-values (dashoffset dasharray)
      (case dash
        [(none hidden) (values 0.0 solid-dash)]
        [(dotted) (line-dash->array 'dot linewidth)]
        [(dashed) (line-dash->array 'long-dash linewidth)]
        [else (values (stroke-offset baseline) (dasharray-normalize (stroke-dash baseline) linewidth (stroke-width baseline)))]))
    (stroke (if (not color) (stroke-color baseline) (rgb* color)) linewidth
            (if (eq? dash 'dotted) 'round (stroke-linecap baseline))
            (stroke-linejoin baseline) (stroke-miterlimit baseline)
            dasharray dashoffset)))
