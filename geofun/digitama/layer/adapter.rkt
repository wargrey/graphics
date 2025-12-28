#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "type.rkt")
(require "combine.rkt")

(require "../self.rkt")
(require "../geometry/spacing.rkt")
(require "../../resize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-fit-layers : (-> Geo (Option Geo) Nonnegative-Flonum Nonnegative-Flonum
                             Flonum Flonum Flonum Flonum Geo-Spacing
                             (GLayer-Groupof Geo))
  (lambda [base maybe-geo hfit% vfit% bx% by% gx% gy% spacing]
    (cond [(not maybe-geo) (geo-own-layers base)]
          [(or (nan? hfit%) (nan? vfit%)) (geo-composite-layers base maybe-geo bx% by% gx% gy%)]
          [else (let ([fit-label (geo-try-fit maybe-geo base hfit% vfit% spacing)])
                  (cond [(not fit-label) (geo-own-layers base)]
                        [else (geo-composite-layers base fit-label bx% by% gx% gy%)]))])))
