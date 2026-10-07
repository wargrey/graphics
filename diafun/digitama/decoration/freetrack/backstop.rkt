#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [dia-track-annotate dia-free-track-annotate]))

(require geofun/fill)
(require geofun/digitama/path/dc)
(require geofun/digitama/paint/source)

(require "self.rkt")
(require "../../track/style.rkt")
(require "../../track/backstop.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-free-track-build : Dia-Free-Track-Builder
  (lambda [source target tracks style]
    (define-values (stip ttip) (dia-track-resolve-tips style))
    (define cstyle (dia-track-style-spec-custom style))
    
    (geo-path* #:id (dia-track-id-merge source target #false)
               #:type (dia-track-style-type-object style)
               #:stroke (dia-track-resolve-line-paint style)
               #:source-tip stip #:target-tip ttip
               #:tip-placement 'inside
               #:fill (let ([phantom (dia-track-style-phantom-type cstyle)])
                        (and (dia-zone-track-style? phantom)
                             (try-desc-brush* #:opacity (dia-track-style-spec-opacity style)
                                              (fill-paint->source* (dia-zone-track-style-fill-paint phantom)))))
               tracks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-free-track-builder-compose : (-> (Option Dia-Free-Track-Builder) Dia-Free-Track-Builder)
  (lambda [custom]
    (cond [(not custom) dia-free-track-build]
          [else (λ [source target ctracks style]
                  (define maybe-track (custom source target ctracks style))
                  (if (void? maybe-track)
                      (dia-free-track-build source target ctracks style)
                      maybe-track))])))
