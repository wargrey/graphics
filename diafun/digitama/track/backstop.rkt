#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/path)
(require geofun/digitama/path/label)

(require "style.rkt")
(require "interface.rkt")

(require "../block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-build : (Dia-Track-Builder S)
  (lambda [source target tracks style]
    (geo-path* #:id (dia-track-id-merge (geo-id source) (and target (geo-id target)) #true)
               #:stroke (dia-track-resolve-line-paint style)
               #:source-tip (dia-track-resolve-source-tip style)
               #:target-tip (and target (not (dia:block:label? target)) (dia-track-resolve-target-tip style))
               #:tip-placement 'inside
               tracks)))

(define #:forall (S) dia-free-track-build : (Dia-Free-Track-Builder S)
  (lambda [source target tracks style]
    (geo-path* #:id (dia-track-id-merge source target #false)
               #:stroke (dia-track-resolve-line-paint style)
               #:source-tip (dia-track-resolve-source-tip style)
               #:target-tip (dia-track-resolve-target-tip style)
               #:tip-placement 'inside
               tracks)))

(define #:forall (S) dia-track-annotate : (Dia-Track-Annotator S)
  (lambda [idx label base-position style]
    (make-geo-path-labels #:index idx
                          #:font (dia-track-resolve-font style)
                          #:color (dia-track-resolve-font-paint style)
                          #:rotate? (dia-track-resolve-label-rotate? style)
                          #:distance (cond [(dia-track-resolve-label-inline? style) 0.0]
                                           [else (dia-track-resolve-label-distance style)])
                          label base-position)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-builder-compose : (-> (Option (Dia-Track-Builder S)) (Dia-Track-Builder S))
  (lambda [custom]
    (cond [(not custom) dia-track-build]
          [else (λ [source target ctracks style]
                  (define maybe-track (custom source target ctracks style))
                  (if (void? maybe-track)
                      (dia-track-build source target ctracks style)
                      maybe-track))])))

(define #:forall (S) dia-free-track-builder-compose : (-> (Option (Dia-Free-Track-Builder S)) (Dia-Free-Track-Builder S))
  (lambda [custom]
    (cond [(not custom) dia-free-track-build]
          [else (λ [source target ctracks style]
                  (define maybe-track (custom source target ctracks style))
                  (if (void? maybe-track)
                      (dia-free-track-build source target ctracks style)
                      maybe-track))])))

(define #:forall (S) dia-track-annotator-compose : (-> (Option (Dia-Track-Annotator S)) (Dia-Track-Annotator S))
  (lambda [custom]
    (cond [(not custom) dia-track-annotate]
          [else (λ [idx labels position style]
                  (define maybe-track (custom idx labels position style))
                  (if (void? maybe-track)
                      (dia-track-annotate idx labels position style)
                      maybe-track))])))
