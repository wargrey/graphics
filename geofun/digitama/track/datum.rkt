#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)

(require "self.rkt")

(require "../richtext/self.rkt")
(require "../path/label.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Track-Datum (U Geo-Rich-Text Keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-empty-info : Geo:Track:Info (geo:track:info #false 0.0 #false null))

(define geo-track-info/paired-labels : (-> Nonnegative-Flonum Geo-Path-Label-Text Geo-Path-Label-Text
                                           (Option Geo:Track:Multiplicity) (Listof Geo-Track-Info-Datum)
                                           Geo:Track:Info)
  (lambda [t head tail mult extra]
    (cond [(and head tail) (geo:track:info (cons head tail) t mult extra)]
          [(or  head tail) (geo:track:info (list head tail) t mult extra)]
          [(or  mult (pair? extra))  (geo:track:info #false t mult extra)]
          [else geo-track-empty-info])))

(define geo-track-info/labels : (-> Nonnegative-Flonum (Listof Geo-Path-Label-Text)
                                    (Option Geo:Track:Multiplicity) (Listof Geo-Track-Info-Datum)
                                    Geo:Track:Info)
  (lambda [t labels mult extra]
    (if (or mult (pair? labels) (pair? extra))
        (geo:track:info labels t mult null)
        geo-track-empty-info)))

(define geo-track-info : (-> Any Geo:Track:Info)
  (lambda [v]
    (cond [(geo:track:info? v) v]
          [(geo-track-datum? v) (geo:track:info (geo-track-normalize v) (default-geo-track-label-base-position) #false null)]
          [(list? v)
           (if (andmap geo-option-track-datum? v)
               (geo:track:info (map geo-track-normalize v) (default-geo-track-label-base-position) #false null)
               geo-track-empty-info)]
          [(pair? v)
           (let-values ([(head tail) (values (car v) (cdr v))])
             (cond [(and (geo-option-track-datum? head) (geo-option-track-datum? tail))
                    (geo-track-info/paired-labels (default-geo-track-label-base-position)
                                                  (geo-track-normalize head) (geo-track-normalize tail)
                                                  #false null)]
                   [else geo-track-empty-info]))]
          [else geo-track-empty-info])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-datum? : (-> Any Boolean : Geo-Track-Datum)
  (lambda [v]
    (or (geo-rich-text? v)
        (keyword? v))))

(define geo-option-track-datum? : (-> Any Boolean : (Option Geo-Track-Datum))
  (lambda [v]
    (or (geo-track-datum? v)
        (not v))))

(define geo-track-normalize : (case-> [Geo-Track-Datum -> Geo-Rich-Text]
                                      [(Option Geo-Track-Datum) -> Geo-Option-Rich-Text])
  (lambda [v]
    (if (keyword? v)
        (format "«~a»" (keyword->immutable-string v))
        v)))
