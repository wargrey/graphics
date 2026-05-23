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
  (make-parameter (pen black 1.0
                       (line-cap->integer 'butt)
                       (line-join->integer 'miter)
                       4.0
                       solid-dash 0.0 1.0
                       #true)))

; the default values follow the CSS specification
(define default-border : (Parameterof Pen)
  (make-parameter (pen hilite (border-thickness->integer 'medium)
                       (line-cap->integer 'butt)
                       (line-join->integer 'miter)
                       10.0 ; <- cairo default value, usually useless for borders in the box model
                       solid-dash 0.0 1.0
                       #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-halo-stroke : (Parameterof Halo-Pen) (make-parameter (halo-pen null 8.0 #true 1.0 #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desc-stroke
  (lambda [#:color [color : (Option Color) #false]
           #:opacity [opacity : (Option Real) #false]
           #:width [width : (Option Length+%) #false]
           #:cap [cap : (Option Symbol) #false]
           #:join [join : (U Symbol Real False) #false]
           #:dash [dash : (Option Stroke-Dash-Datum) #false]
           #:offset [offset : (Option Real) #false]
           #:scalable? [scalable? : (U Boolean Void) (void)]
           [base : Pen (default-stroke)]] : Pen
    (define :color (if (not color) (pen-color base) (rgb* color)))
    (define :opacity (if (not opacity) (pen-opacity base) (real->alpha opacity)))
    (define :linewidth (if (not width) (pen-width base) (~dimension width (pen-width base))))

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
    (define :scalable? (if (void? scalable?) (pen-scalable? base) scalable?))

    (pen :color :linewidth :linecap :linejoin :miterlimit :dasharray :dashoffset :opacity :scalable?))) 

(define desc-stroke*
  (lambda [#:color [color : (Option Color) #false]
           #:opacity [opacity : (Option Real) #false]
           #:width [width : (Option Length+%) #false]
           #:cap [cap : (Option Symbol) #false]
           #:join [join : (U Symbol Real False) #false]
           #:dash [dash : (Option Stroke-Dash-Datum) #false]
           #:offset [offset : (Option Real) #false]
           #:scalable? [scalable? : (U Boolean Void) (void)]
           [base : Pen (default-stroke)]] : Pen
    (if (or color opacity width cap join dash offset (boolean? scalable?))
        (desc-stroke #:color color #:opacity opacity #:width width
                     #:cap cap #:join join #:dash dash #:offset offset
                     #:scalable? scalable?      
                     base)
        base)))

(define desc-border
  (lambda [#:color [color : (Option Color) #false]
           #:opacity [alpha : (Option Real) #false]
           #:width [width : (U Length+% Symbol False) #false]
           #:style [dash : (Option Symbol) #false]
           [base : Pen (default-border)]] : Pen
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
         (if (not alpha) (pen-opacity base) (real->alpha alpha))
         (pen-scalable? base))))

(define desc-border*
  (lambda [#:color [color : (Option Color) #false]
           #:opacity [opacity : (Option Real) #false]
           #:width [width : (U Length+% Symbol False) #false]
           #:style [dash : (Option Symbol) #false]
           [base : Pen (default-border)]]
    (if (or color opacity width dash)
        (desc-border #:color color #:opacity opacity #:width width #:style dash
                     base)
        base)))

(define desc-halo-stroke
  (lambda [#:colors [colors : (Option (U Color (Listof (U Color (Pairof Color Real))))) #false]
           #:width [width : (Option Length+%) #false]
           #:round? [round? : (U Boolean Void) (void)]
           #:opacity [opacity : (Option Real) #false]
           #:scalable? [scalable? : (U Boolean Void) (void)]
           [base : Halo-Pen (default-halo-stroke)]] : Halo-Pen
    (define :width (if (not width) (halo-pen-width base) (~dimension width (halo-pen-width base))))
    (define :opacity (if (not opacity) (halo-pen-opacity base) (real->alpha opacity)))
    (define :round? (if (void? round?) (halo-pen-round? base) round?))
    (define :scalable? (if (void? scalable?) (halo-pen-scalable? base) scalable?))

    (define :colors
      (cond [(not colors) (halo-pen-colors base)]
            [(null? colors) null]
            [(not (list? colors)) (list (cons (rgb* colors) 1.0))]
            [else (let ([delta (/ -1.0 (length colors))])
                    (for/list : (Listof (Pairof FlRGBA Nonnegative-Flonum)) ([c (in-list colors)]
                                                                             [pos (in-range 1.0 delta delta)])
                      (cond [(pair? c) (cons (rgb* (car c)) (~clamp (cdr c) 0.0 1.0))]
                            [else (cons (rgb* c) (~clamp pos 0.0 1.0))])))]))

    (halo-pen ((inst sort (Pairof FlRGBA Nonnegative-Flonum) Nonnegative-Flonum) :colors >= #:key cdr)
              :width :round? :opacity :scalable?)))

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
