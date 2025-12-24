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
           [baseline (default-winding-brush)]]
    (brush (if (not color)
               (brush-color baseline)
               (rgb* color))
           (cond [(void? pattern) (brush-pattern baseline)]
                 [(visual-object<%>? pattern)
                  (let ([maybe-surface (vobject-convert pattern 'cairo-surface #false)])
                    (cond [(cairo-surface? maybe-surface) maybe-surface]
                          [else #false]))]
                 [else #false])
           (if (not opacity)
               (brush-opacity baseline)
               (real->alpha opacity))
           (if (not rule)
               (brush-rule baseline)
               rule))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define brush-maybe-rgba : (-> Any (Option FlRGBA))
  (lambda [s]
    (cond [(brush? s) (brush-color s)]
          [(color? s) (rgb* s)]
          [else #false])))
