#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [brush-maybe-rgba brush-maybe-color]))

(require colorspace/misc)

(require "color.rkt")

(require "digitama/base.rkt")
(require "digitama/paint/self.rkt")

(require "digitama/unsafe/visual.rkt")
(require "digitama/unsafe/typed/c.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-winding-brush : (Parameterof Brush) (make-parameter (brush transparent #false 1.0 'winding)))
(define default-evenodd-brush : (Parameterof Brush) (make-parameter (brush transparent #false 1.0 'even-odd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desc-brush : (->* ()
                          (Brush #:color (Option Color) #:pattern (U Void Visual-Object<%> False)
                                 #:opacity (Option Real) #:rule (Option Fill-Rule))
                          Brush)
  (lambda [#:color [color #false] #:pattern [pattern (void)] #:opacity [opacity #false] #:rule [rule #false]
           [base (default-winding-brush)]]
    (if (or color opacity rule (not (void? pattern)))
        (brush (if (not color)
                   (brush-color base)
                   (rgb* color))
               (cond [(void? pattern) (brush-pattern base)]
                     [(visual-object<%>? pattern)
                      (let ([maybe-surface (vobject-convert pattern 'cairo-surface #false)])
                        (cond [(cairo-surface? maybe-surface) maybe-surface]
                              [else #false]))]
                     [else #false])
               (if (not opacity)
                   (brush-opacity base)
                   (real->alpha opacity))
               (if (not rule)
                   (brush-rule base)
                   rule))
        base)))

(define try-desc-brush : (->* ((Option Brush))
                              (#:color (Option Color) #:pattern (U Void Visual-Object<%> False)
                               #:opacity (Option Real) #:rule (Option Fill-Rule))
                              (Option Brush))
  (lambda [#:color [color #false] #:pattern [pattern (void)] #:opacity [opacity #false] #:rule [rule #false]
           base]
    (and base (desc-brush base #:color color #:pattern pattern #:opacity opacity #:rule rule))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define brush-maybe-rgba : (-> Any (Option FlRGBA))
  (lambda [s]
    (cond [(brush? s) (brush-color s)]
          [(color? s) (rgb* s)]
          [else #false])))
