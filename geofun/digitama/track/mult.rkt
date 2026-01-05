#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")
(require "../path/label.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-multiplicity-map : (-> Geo-Track-Multiplicity-Datum (Option String))
  (lambda [mult]
    (and mult
         (cond [(string? mult) mult]
               [else (format "~a" mult)]))))

(define geo-track-multiplicities-map : (-> Geo-Track-Multiplicity-Datum Geo-Track-Multiplicity-Datum (Option Geo-Path-Labels))
  (lambda [source target]
    (define src (geo-track-multiplicity-map source))
    (define tgt (geo-track-multiplicity-map target))

    (cond [(and src tgt) (cons src tgt)]
          [(or  src tgt) (list src tgt)]
          [else #false])))
