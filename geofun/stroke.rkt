#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [stroke-maybe-rgba stroke-maybe-color]))

(require digimon/measure)
(require colorspace/misc)
(require racket/case)

(require "color.rkt")

(require "digitama/base.rkt")
(require "digitama/paint/self.rkt")
(require "digitama/paint/stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the default values follow the SVG specification 
(define default-stroke : (Parameterof Pen)
  (make-parameter (pen black 1.0 (line-cap->integer 'butt) (line-join->integer 'miter) 4.0 solid-dash 0.0 1.0)))

; the default values follow the CSS specification
(define default-border : (Parameterof Pen)
  (make-parameter (pen hilite (border-thickness->integer 'medium)
                       (line-cap->integer 'butt)
                       (line-join->integer 'miter)
                       10.0 ; <- cairo default value, usually useless for borders in the box model
                       solid-dash 0.0 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desc-stroke : (->* ()
                           (Pen #:color (Option Color) #:opacity (Option Real) #:width (Option Length+%)
                                #:cap (Option Symbol) #:join (U Symbol Real False)
                                #:dash (Option Stroke-Dash-Datum) #:offset (Option Real)
                                #:scale (Option Nonnegative-Flonum))
                           Pen)
  (lambda [[base (default-stroke)] #:color [color #false] #:opacity [opacity #false] #:width [width #false]
                                   #:cap [cap #false] #:join [join #false] #:dash [dash #false] #:offset [offset #false]
                                   #:scale [scale #false]]
    (define :color (if (not color) (pen-color base) (rgb* color)))
    (define :opacity (if (not opacity) (pen-opacity base) (real->alpha opacity)))
    (define :linewidth
      (* (if (not width) (pen-width base) (~dimension width (pen-width base)))
         (or scale 1.0)))

    (define :linecap
      (cond [(not cap) (pen-linecap base)]
            [(stroke-line-cap-option? cap) (line-cap->integer cap)]
            [(eq? dash 'dot) (line-cap->integer 'round)]
            [else (pen-linecap base)]))

    (define-values (:linejoin :miterlimit)
      (cond [(not join) (values (pen-linejoin base) (pen-miterlimit base))]
            [(stroke-line-join-option? join) (values (line-join->integer join) (pen-miterlimit base))]
            [(real? join) (values (line-join->integer 'miter) (if (>= join 1.0) (real->double-flonum join) (pen-miterlimit base)))]
            [else (values (pen-linejoin base) (pen-miterlimit base))]))

    (define-values (:preoffset :dasharray)
      (cond [(symbol? dash) (line-dash->array dash :linewidth)]
            [(vector? dash) (values (pen-offset base) (dasharray-normalize dash :linewidth))]
            [else (values (pen-offset base) (dasharray-normalize (pen-dash base) :linewidth (pen-width base)))]))

    (define :dashoffset (if (not offset) :preoffset (real->double-flonum offset)))

    (pen :color :linewidth :linecap :linejoin :miterlimit :dasharray :dashoffset :opacity)))

(define desc-stroke* : (->* ()
                            (Pen #:color (Option Color) #:opacity (Option Real) #:width (Option Length+%)
                                 #:cap (Option Symbol) #:join (U Symbol Real False)
                                 #:dash (Option Stroke-Dash-Datum) #:offset (Option Real)
                                 #:scale (Option Nonnegative-Flonum))
                            Pen)
  (lambda [[base (default-stroke)] #:color [color #false] #:opacity [opacity #false] #:width [width #false]
                                   #:cap [cap #false] #:join [join #false] #:dash [dash #false] #:offset [offset #false]
                                   #:scale [scale #false]]
    (if (or color opacity width cap join dash offset scale)
        (desc-stroke #:color color #:opacity opacity #:width width
                     #:cap cap #:join join #:dash dash #:offset offset
                     #:scale scale
                     base)
        base)))

(define desc-border : (->* () (Pen #:color (Option Color) #:opacity (Option Real) #:width (U Length+% Symbol False) #:style (Option Symbol)) Pen)
  (lambda [[base (default-border)] #:color [color #false] #:opacity [alpha #false] #:width [width #false] #:style [dash #false]]
    (define no-border? (or (eq? dash 'none) (eq? dash 'hidden)))
    (define linewidth
      (cond [(and no-border?) 0.0]
            [(length+%? width) (~dimension width (pen-width base))]
            [(css-border-thickness-option? width) (border-thickness->integer width)]
            [else (pen-width base)]))

    (define-values (dashoffset dasharray)
      (case/eq dash
        [(none hidden) (values 0.0 solid-dash)]
        [(dotted) (line-dash->array 'dot linewidth)]
        [(dashed) (line-dash->array 'long-dash linewidth)]
        [else (values (pen-offset base) (dasharray-normalize (pen-dash base) linewidth (pen-width base)))]))
    
    (pen (if (not color) (pen-color base) (rgb* color))
         linewidth
         (if (eq? dash 'dotted) (line-cap->integer 'round) (pen-linecap base))
         (pen-linejoin base)
         (pen-miterlimit base)
         dasharray dashoffset
         (if (not alpha) (pen-opacity base) (real->alpha alpha)))))

(define desc-border* : (->* () (Pen #:color (Option Color) #:opacity (Option Real) #:width (U Length+% Symbol False) #:style (Option Symbol)) Pen)
  (lambda [[base (default-border)] #:color [color #false] #:opacity [opacity #false] #:width [width #false] #:style [dash #false]]
    (if (or color opacity width dash)
        (desc-border #:color color #:opacity opacity #:width width #:style dash
                     base)
        base)))

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
