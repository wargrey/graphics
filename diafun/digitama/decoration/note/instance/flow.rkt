#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/digitama/self)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/composite)
(require geofun/digitama/geometry/sides)

(require "../self.rkt")
(require "../../../block/dc.rkt")
(require "../../../block/style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-flow-note-build : Dia-Note-Builder
  (lambda [key body style padding direction whatever]
    (define-values (top rgt bot lft) (geo-inset-values padding))
    (define-values (w h) (geo-size body))
    (define-values (width height) (values (+ lft w rgt) (+ top h bot)))
    (define angle (or direction 0.0))
    (define open-side
      (cond [(< (abs angle) pi/4) 'l]
            [(< angle (+ 3pi/4)) 't]
            [(< angle (- 3pi/4)) 'b]
            [else 'r]))

    (create-dia-block #:block dia:block:note
                      #:id key whatever
                      #:with-group style
                      (geo-composite (geo-open-rectangle #:open-sides (list open-side)
                                                         #:stroke (dia-block-resolve-stroke-paint style)
                                                         #:fill (dia-block-resolve-fill-paint style)
                                                         width height)
                                     lft top body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow-note-factory : Dia-Note-Factory (make-dia-note-factory #:builder default-flow-note-build))
