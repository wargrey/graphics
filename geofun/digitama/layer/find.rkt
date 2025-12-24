#lang typed/racket/base

(provide (all-defined-out))

(require "type.rkt")
(require "position.rkt")

(require "../self.rkt")
(require "../dc/composite.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-find : (-> Geo-Pin-Anchor Geo (U Geo Symbol) (Option Float-Complex))
  (lambda [anchor master target]
    (cond [(geo:group? master)
           (let find ([offset : Float-Complex 0.0+0.0i]
                      [layers : (Listof (GLayerof Geo)) (glayer-group-layers (geo:group-selves master))])
             (and (pair? layers)
                  (let* ([self (car layers)]
                         [subg (glayer-master self)]
                         [loc (make-rectangular (glayer-x self) (glayer-y self))])
                    (or (and (geo:group? subg)
                             (find (+ offset loc (geo-group-origin subg))
                                   (glayer-group-layers (geo:group-selves subg))))
                        (and (geo-found? subg target)
                             (let*-values ([(gw gh) (values (glayer-width self) (glayer-height self))]
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
