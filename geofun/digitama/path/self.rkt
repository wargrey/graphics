#lang typed/racket/base

(provide (all-defined-out))

(require "../markup.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Label-Datum (Option DC-Markup-Text))
(define-type Geo-Path-Labels (U Geo-Path-Label-Datum (Pairof DC-Markup-Text DC-Markup-Text) (Listof Geo-Path-Label-Datum)))
(define-type Geo-Path-Info-Datum (U Number Symbol Keyword Char Geo:Path:Datum))
(define-type Geo-Path-Multiplicity-Datum (U Integer String Symbol Keyword Char False))
(define-type Geo-Path-Multiplicity+Label (U Geo-Path-Multiplicity-Datum (Pairof Geo-Path-Multiplicity-Datum Geo-Path-Label-Datum)))

(define default-path-label-base-position : (Parameterof Nonnegative-Flonum) (make-parameter 0.25))
(define default-path-multiplicity-base-position : (Parameterof Nonnegative-Flonum) (make-parameter 0.15))

(struct geo:path:datum ()
  #:type-name Geo:Path:Datum
  #:transparent)

(struct geo:path:info
  ([labels : Geo-Path-Labels]
   [base-position : Nonnegative-Flonum]
   [multiplicity : (Option Geo:Path:Multiplicity)]
   [extra : (Listof Geo-Path-Info-Datum)])
  #:type-name Geo:Path:Info
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:path:multiplicity geo:path:datum
  ([source : Geo-Path-Multiplicity-Datum]
   [target : Geo-Path-Multiplicity-Datum]
   [base-position : Nonnegative-Flonum])
  #:type-name Geo:Path:Multiplicity
  #:transparent)
