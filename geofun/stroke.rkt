#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [stroke-maybe-rgba stroke-maybe-color]))

(require colorspace/misc)
(require racket/case)

(require "color.rkt")

(require "digitama/base.rkt")
(require "digitama/paint/self.rkt")
(require "digitama/paint/stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-stroke : (Parameterof Pen) (make-parameter (pen black 1.0 'butt 'round +nan.0 solid-dash 0.0 1.0)))
(define default-border : (Parameterof Pen) (make-parameter (pen hilite (border-thickness->integer 'medium)
                                                                'butt 'round +nan.0 solid-dash 0.0 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desc-stroke : (->* ()
                           (Pen #:color (Option Color) #:opacity (Option Real) #:width (Option Real)
                                #:cap (Option Symbol) #:join (U Symbol Real False)
                                #:dash (Option Stroke-Dash-Datum) #:offset (Option Real))
                           Pen)
  (lambda [[baseline (default-stroke)] #:color [color #false] #:opacity [opacity #false] #:width [width #false]
                                       #:cap [cap #false] #:join [join #false] #:dash [dash #false] #:offset [offset #false]]
    (define linewidth : Nonnegative-Flonum
      (cond [(not width) (pen-width baseline)]
            [else (let ([flw (real->double-flonum width)])
                    (cond [(>= flw 0.0) flw]
                          [else (* (abs flw) (pen-width baseline))]))]))
    (define-values (linejoin miterlimit)
      (cond [(stroke-line-join-option? join) (values join (pen-miterlimit baseline))]
            [(real? join) (values 'miter (real->double-flonum join))]
            [else (values (pen-linejoin baseline) (pen-miterlimit baseline))]))
    (define-values (preoffset dasharray)
      (cond [(symbol? dash) (line-dash->array dash linewidth)]
            [(vector? dash) (values (pen-offset baseline) (dasharray-normalize dash linewidth))]
            [else (values (pen-offset baseline) (dasharray-normalize (pen-dash baseline) linewidth (pen-width baseline)))]))
    (define dashoffset : Flonum (if (not offset) preoffset (real->double-flonum offset)))
    (pen (if (not color) (pen-color baseline) (rgb* color))
         linewidth
         (cond [(stroke-line-cap-option? cap) cap]
               [(eq? dash 'dot) 'round]
               [else (pen-linecap baseline)])
         linejoin miterlimit
         dasharray dashoffset
         (if (not opacity) (pen-opacity baseline) (real->alpha opacity)))))

(define desc-border : (->* () (Pen #:color (Option Color) #:opacity (Option Real) #:width (U Real Symbol False) #:style (Option Symbol)) Pen)
  (lambda [[baseline (default-border)] #:color [color #false] #:opacity [alpha #false] #:width [width #false] #:style [dash #false]]
    (define no-border? : Boolean (or (eq? dash 'none) (eq? dash 'hidden)))
    (define linewidth : Nonnegative-Flonum
      (cond [(and no-border?) 0.0]
            [(real? width) (let ([flw (real->double-flonum width)]) (if (>= flw 0.0) flw (* (abs flw) (pen-width baseline))))]
            [(css-border-thickness-option? width) (border-thickness->integer width)]
            [else (pen-width baseline)]))
    (define-values (dashoffset dasharray)
      (case/eq dash
        [(none hidden) (values 0.0 solid-dash)]
        [(dotted) (line-dash->array 'dot linewidth)]
        [(dashed) (line-dash->array 'long-dash linewidth)]
        [else (values (pen-offset baseline) (dasharray-normalize (pen-dash baseline) linewidth (pen-width baseline)))]))
    (pen (if (not color) (pen-color baseline) (rgb* color))
         linewidth
         (if (eq? dash 'dotted) 'round (pen-linecap baseline))
         (pen-linejoin baseline)
         (pen-miterlimit baseline)
         dasharray dashoffset
         (if (not alpha) (pen-opacity baseline) (real->alpha alpha)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke-maybe-rgba : (-> Any (Option FlRGBA))
  (lambda [s]
    (cond [(pen? s) (pen-color s)]
          [(color? s) (rgb* s)]
          [else #false])))

(define stroke-resolve-paint : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Maybe-Stroke-Paint)
  (lambda [self master]
    (cond [(or (pen? self) (not self)) self]
          [(void? self) master]
          [(not (pen? master)) self]
          [else (desc-stroke master #:color self)])))
