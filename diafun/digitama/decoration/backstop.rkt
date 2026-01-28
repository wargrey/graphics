#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/dc/path)

(require "../track/style.rkt")
(require "interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-free-track-build : (Dia-Free-Track-Builder S)
  (lambda [source target tracks style]
    (define-values (stip ttip) (dia-track-resolve-tips style))
    
    (geo-path* #:id (dia-track-id-merge source target #false)
               #:stroke (dia-track-resolve-line-paint style)
               #:source-tip stip #:target-tip ttip
               #:tip-placement 'inside
               tracks)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-free-track-builder-compose : (-> (Option (Dia-Free-Track-Builder S)) (Dia-Free-Track-Builder S))
  (lambda [custom]
    (cond [(not custom) dia-free-track-build]
          [else (λ [source target ctracks style]
                  (define maybe-track (custom source target ctracks style))
                  (if (void? maybe-track)
                      (dia-free-track-build source target ctracks style)
                      maybe-track))])))
