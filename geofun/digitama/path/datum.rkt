#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")
(require "../markup.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-empty-info : Geo:Path:Info (geo:path:info #false 0.0 #false null))

(define geo-path-info/paired-labels : (-> Nonnegative-Flonum Geo-Path-Label-Datum Geo-Path-Label-Datum
                                          (Option Geo:Path:Multiplicity) (Listof Geo-Path-Info-Datum)
                                          Geo:Path:Info)
  (lambda [t head tail mult extra]
    (cond [(and head tail) (geo:path:info (cons head tail) t mult extra)]
          [(or  head tail) (geo:path:info (list head tail) t mult extra)]
          [(or  mult (pair? extra))  (geo:path:info #false t mult extra)]
          [else geo-path-empty-info])))

(define geo-path-info/labels : (-> Nonnegative-Flonum (Listof Geo-Path-Label-Datum)
                                   (Option Geo:Path:Multiplicity) (Listof Geo-Path-Info-Datum)
                                   Geo:Path:Info)
  (lambda [t labels mult extra]
    (if (or mult (pair? labels) (pair? extra))
        (geo:path:info labels t mult null)
        geo-path-empty-info)))

(define geo-path-info : (-> Any Geo:Path:Info)
  (lambda [v]
    (cond [(geo:path:info? v) v]
          [(dc-markup-text? v) (geo:path:info v (default-path-label-base-position) #false null)]
          [(list? v)
           (if (andmap dc-markup-maybe-text? v)
               (geo:path:info v (default-path-label-base-position) #false null)
               geo-path-empty-info)]
          [(pair? v)
           (let-values ([(head tail) (values (car v) (cdr v))])
             (or (and (dc-markup-maybe-text? head)
                      (dc-markup-maybe-text? tail)
                      (geo-path-info/paired-labels (default-path-label-base-position) head tail #false null))
                 geo-path-empty-info))]
          [else geo-path-empty-info])))
