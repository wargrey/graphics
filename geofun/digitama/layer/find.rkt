#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")
(require "position.rkt")

(require "../convert.rkt")
(require "../dc/composite.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-find : (-> Geo-Pin-Anchor Geo (U Geo Symbol) (Option Float-Complex))
  (lambda [anchor master target]
    (cond [(geo:group? master)
           (let find ([offset : Float-Complex 0.0+0.0i]
                      [layers : (Listof (GLayerof Geo)) (vector-ref (geo:group-layers master) 2)])
             (and (pair? layers)
                  (let* ([self (car layers)]
                         [subg (vector-ref self 0)]
                         [loc (make-rectangular (vector-ref self 1) (vector-ref self 2))])
                    (or (and (geo:group? subg)
                             (find (+ offset loc) (vector-ref (geo:group-layers subg) 2)))
                        (and (geo-found? subg target)
                             (let*-values ([(gw gh) (values (vector-ref self 3) (vector-ref self 4))]
                                           [(ax ay) (geo-superimpose-layer anchor (* gw 2.0) (* gh 2.0) gw gh)])
                               (+ offset loc (make-rectangular ax ay))))
                        (find offset (cdr layers))))))]
          [(geo-found? master target)
           (let*-values ([(w h) (geo-flsize master)]
                         [(x y) (geo-superimpose-layer anchor (* w 2.0) (* h 2.0) w h)])
             (make-rectangular x y))]
          [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-found? : (-> Geo (U Geo Symbol) Boolean)
  (lambda [self target]
    (if (symbol? target)
        (eq? (geo-id self) target)
        (eq? self target))))
