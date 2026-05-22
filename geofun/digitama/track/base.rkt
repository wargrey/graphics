#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "metadata.rkt"))
(provide (all-from-out "trail.rkt"))
(provide (all-from-out "anchor.rkt"))
(provide (all-from-out "../nice/box.rkt"))

(provide Geo:Track geo:track?)
(provide Geo-Track-Anchor->Sticker)
(provide Geo-Sticker geo-sticker?)
(provide make-sticker default-track-anchor->sticker)
(provide current-master-track)

(provide (rename-out [make-sticker make-geo-sticker]))

(require "self.rkt")
(require "metadata.rkt")
(require "trail.rkt")
(require "datum.rkt")
(require "sticker.rkt")
(require "anchor.rkt")
(require "trace.rkt")

(require "../self.rkt")
(require "../composite.rkt")
(require "../dc/composite.rkt")
(require "../layer/sticker.rkt")
(require "../geometry/bbox.rkt")
(require "../nice/box.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-stick
  (lambda [#:id [id : (Option Symbol) #false]
           #:desc [desc : (Option String) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:trusted-anchors [trusted-anchors : (Option Geo-Trusted-Anchors) #false]
           #:truncate? [truncate? : Boolean #true]
           #:offset [offset : Float-Complex 0.0+0.0i]
           [self : Geo:Track]
           [anchor->sticker : Geo-Track-Anchor->Sticker default-track-anchor->sticker]] : Geo:Trail
    (geo:track-stick self anchor->sticker trusted-anchors truncate?
                     id (or desc (geo-desc self)) base-op sibs-op offset frame)))

(define geo-track-anchor-position : (->* (Geo:Track Geo-Anchor-Name) (#:translate? Boolean) Float-Complex)
  (lambda [self anchor #:translate? [translate? #false]]
    (define abspos : Float-Complex (geo-trace-ref (geo:track-trace self) anchor))
    
    (cond [(not translate?) abspos]
          [else (- abspos (geo-bbox-position (geo:track-bbox self)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gtmult : (->* (Geo-Track-Multiplicity+Label Geo-Track-Multiplicity+Label) (#:t Nonnegative-Flonum) #:rest Geo-Track-Info-Datum Geo:Track:Info)
  (lambda [start end #:t [t (default-geo-track-multiplicity-base-position)] . extra]
    (define-values (mstart lstart) (if (pair? start) (values (car start) (cdr start)) (values start #false)))
    (define-values (m-end  l-end)  (if (pair? end)   (values (car end)   (cdr end))   (values end #false)))
    
    (geo-track-info/paired-labels (default-geo-track-label-base-position) lstart l-end
                                  (and (or mstart m-end)
                                       (geo:track:multiplicity mstart m-end t))
                                  extra)))

(define gtlabel : (->* (Geo-Track-Datum Geo-Track-Datum) (#:t Nonnegative-Flonum) #:rest Geo-Track-Info-Datum Geo:Track:Info)
  (lambda [start end #:t [t (default-geo-track-label-base-position)] . extra]
    (geo-track-info/paired-labels t (geo-track-normalize start) (geo-track-normalize end) #false extra)))

(define gtlabel* : (->* ((Listof Geo-Track-Datum)) (#:t Nonnegative-Flonum) #:rest Geo-Track-Info-Datum Geo:Track:Info)
  (lambda [labels #:t [t (default-geo-track-label-base-position)] . extra]
    (geo-track-info/labels t (map geo-track-normalize labels) #false extra)))
