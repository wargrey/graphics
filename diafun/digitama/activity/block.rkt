#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require racket/symbol)

(require "../block/dc.rkt")
(require "../block/interface.rkt")

(require "../block/dc/node.rkt")
(require "../block/dc/symbol.rkt")

(require "parameter.rkt")

(require geofun/digitama/dc/gadget)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; controls
(define #:forall (S) act-block-final : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction tag]
    (dia-symbol-bullseye block-key style width height (&% 72) 'Activity)))

(define #:forall (S) act-block-flow-final : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (dia-symbol-circle block-key style width height (list pi/4 3pi/4) 'Flow)))

(define #:forall (S) act-block-fork : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction tag]
    (define width-coeffcient : Nonnegative-Flonum
      (cond [(eq? tag 'Compact) 0.5]
            [else (let* ([s (symbol->immutable-string block-key)]
                         [leads (regexp-match #px"^[-]+" s)]
                         [tails (regexp-match #px"[=]+$" s)])
                    (cond [(and leads tails)
                           (+ (* (real->double-flonum (string-length (car leads))) 0.5)
                              (* (real->double-flonum (string-length (car tails))) 1.0))]
                          [else 1.0]))]))
    (dia-block-rectangle/cr:2nd block-key #false style (* width width-coeffcient) height direction tag)))

(define #:forall (S) act-block-join : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction tag]
    (define width-coeffcient : Nonnegative-Flonum
      (cond [(eq? tag 'Compact) 0.5]
            [else (let* ([s (symbol->immutable-string block-key)]
                         [leads (regexp-match #px"^[=]+" s)]
                         [tails (regexp-match #px"[-]+$" s)])
                    (cond [(and leads tails)
                           (+ (* (real->double-flonum (string-length (car leads))) 1.0)
                              (* (real->double-flonum (string-length (car tails))) 0.5))]
                          [else 1.0]))]))
    (dia-block-rectangle/cr:2nd block-key #false style (* width width-coeffcient) height direction tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actions
(define #:forall (S) act-block-time-event : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignore-width height direction stereotype]
    (create-dia-block #:id block-key '(Event Time)
                      #:fit-region 1.00 0.42 0.00 0.00
                      #:create-with style [geo-sandglass (* height 0.618)]
                      caption)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objects
(define #:forall (S) act-block-object : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (dia-block-rectangle block-key caption style width height direction stereotype
                         stereotype (default-act-stereotype-font))))

(define #:forall (S) act-block-central-buffer : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction stereotype]
    (act-block-object block-key caption style width height direction
                      (if (or stereotype) 'datastore 'centralBuffer))))
