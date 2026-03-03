#lang typed/racket/base

(provide (all-defined-out))

(require "../path/label.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: should it be rich text? or plain text + comment block
(define-type Geo-Track-Multiplicity-Datum (U Integer Symbol Keyword Char String False))
(define-type Geo-Track-Multiplicity+Label (U Geo-Track-Multiplicity-Datum (Pairof Geo-Track-Multiplicity-Datum Geo-Path-Label-Text)))
(define-type Geo-Track-Info-Datum (U Number Symbol Keyword Char Geo:Track:Datum))

(define default-geo-track-label-base-position : (Parameterof Nonnegative-Flonum) (make-parameter 0.25))
(define default-geo-track-multiplicity-base-position : (Parameterof Nonnegative-Flonum) (make-parameter 0.15))

(struct geo:track:datum ()
  #:type-name Geo:Track:Datum
  #:transparent)

(struct geo:track:info
  ([labels : Geo-Path-Label-Datum]
   [base-position : Nonnegative-Flonum]
   [multiplicity : (Option Geo:Track:Multiplicity)]
   [extra : (Listof Geo-Track-Info-Datum)])
  #:type-name Geo:Track:Info
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:track:multiplicity geo:track:datum
  ([source : Geo-Track-Multiplicity-Datum]
   [target : Geo-Track-Multiplicity-Datum]
   [base-position : Nonnegative-Flonum])
  #:type-name Geo:Track:Multiplicity
  #:transparent)
