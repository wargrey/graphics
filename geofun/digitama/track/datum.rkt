#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)

(require "self.rkt")

(require "../richtext/self.rkt")
(require "../path/label.rkt")

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
          [(geo-rich-text? v) (geo:track:info v (default-geo-track-label-base-position) #false null)]
          [(symbol? v) (geo:track:info (format "[~a]" v) (default-geo-track-label-base-position) #false null)] ; UML guard
          [(keyword? v) (geo:track:info (format "«~a»" (keyword->immutable-string v)) (default-geo-track-label-base-position) #false null)] ; UML stereotype
          [(list? v)
           (if (andmap geo-option-rich-text? v)
               (geo:track:info v (default-geo-track-label-base-position) #false null)
               geo-track-empty-info)]
          [(pair? v)
           (let-values ([(head tail) (values (car v) (cdr v))])
             (or (and (geo-option-rich-text? head)
                      (geo-option-rich-text? tail)
                      (geo-track-info/paired-labels (default-geo-track-label-base-position) head tail #false null))
                 geo-track-empty-info))]
          [else geo-track-empty-info])))
