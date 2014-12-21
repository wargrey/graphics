#lang at-exp racket/gui

(require (except-in "../digitama/digimon.rkt" digimon-ark))

(provide digimon-ark)
(provide (all-from-out "../digitama/digimon.rkt"))

(define digimon-ark
  {lambda [monster #:lightness [threshold 0.64] #:rallies [rallies0 null] #:close [close? 0]]
    (define-values {width0 height0} (flomap-size (cdr (digimon-figure monster))))
    (define figure (flomap-copy (cdr (digimon-figure monster)) 0 0 width0 height0))
    (define d-ark (new {class dialog% (super-new [label (format "Analyzer [~a, ~a]" width0 height0)] [style '{close-button}] [min-width width0] [min-height height0])
                         (field [rallies null] [ltxy #false])
                         (field [vark (new canvas% [parent this] [paint-callback {lambda [who-cares painter] (send painter draw-bitmap (flomap->bitmap figure) 0 0)}])])
                         (field [dark (let ([arc (send vark get-dc)])
                                        (send vark set-canvas-background (get-panel-background))
                                        (send arc set-pen (make-pen #:color (get-panel-background)))
                                        arc)])
                         
                         (define {erase x y}
                           (define xy (* 4 (+ x (* y width0))))
                           (when (and (< -1 x width0) (< -1 y height0)
                                      (> (flvector-ref (flomap-values figure) xy) 0.0))
                             (define-values {h s v} (rgb->hsv (flvector-ref (flomap-values figure) (+ 1 xy))
                                                              (flvector-ref (flomap-values figure) (+ 2 xy))
                                                              (flvector-ref (flomap-values figure) (+ 3 xy))))
                             (when (> v threshold)
                               (flvector-set! (flomap-values figure) xy 0.0)
                               (send dark draw-point x y)
                               (erase (sub1 x) y)
                               (erase x (sub1 y))
                               (erase (add1 x) y)
                               (erase x (add1 y)))
                             (list h s v)))
                         
                         (define/override {on-superwindow-show ?}
                           (cond [? (for-each {lambda [rally]
                                                (when (void? (erase (car rally) (cdr rally)))
                                                  (printf "WARNING: (cons ~a ~a) is reduntant~n" (car rally) (cdr rally)))}
                                              (cons (cons 0 0) rallies0))
                                    (send vark refresh-now)
                                    (when (and (real? close?) (>= close? 0))
                                      (sleep close?)
                                      (send this show #false))]
                                 [else (for-each {lambda [sally] (printf "(cons ~a ~a)~n" (car sally) (cdr sally))} rallies)]))

                         (define/override (on-subwindow-event who mouse)
                           (when (equal? vark who)
                             (define-values {x0 y0} (values (send mouse get-x) (send mouse get-y)))
                             (case (send mouse get-event-type)
                               [{left-down} (set! ltxy (cons x0 y0))]
                               [{left-up} (let ([lt (if (cons? ltxy) ltxy (cons x0 y0))])
                                            (when (cons? ltxy)
                                              (set! ltxy #false)
                                              (newline))
                                            (for* ([x (in-range (min (car lt) (add1 x0)) (max (car lt) (add1 x0)))]
                                                   [y (in-range (min (cdr lt) (add1 y0)) (max (cdr lt) (add1 y0)))])
                                              (define v (erase x y))
                                              (send vark refresh-now)
                                              (if (void? v)
                                                  (printf "Ø XY(~a, ~a)~n" x y)
                                                  (let* ([~r4 (curry ~r #:precision '{= 4})]
                                                         [~print (curryr printf x y (~r4 (first v)) (~r4 (second v)) (~r4 (third v)))])
                                                    (if (> (third v) threshold)
                                                        {begin (set! rallies (append rallies (list (cons x y))))
                                                               (~print "√ XY(~a, ~a):: HSV(~a, ~a, ~a)~n")}
                                                        (~print "† XY(~a, ~a):: HSV(~a, ~a, ~a)~n"))))))])))}))
    (send d-ark show #true)
    (bitmap (flomap->bitmap (flomap-trim figure #true)))})
