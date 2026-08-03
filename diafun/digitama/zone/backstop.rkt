#lang typed/racket/base

(provide (all-defined-out))

(require "interface.rkt")
(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-zone-typeset : (Dia-Zone-Typesetter S)
  (lambda [id type text style width height]
    (dia-zone-text->title #:id id #:type type
                          text style width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-zone-typesetter-compose : (case-> [(Option (Dia-Zone-Typesetter S)) -> (Dia-Zone-Typesetter S)]
                                                           [(Option (Dia-Zone-Typesetter S)) (Dia-Zone-Typesetter S) -> (Dia-Zone-Typesetter S)])
  (case-lambda
    [(custom) (or custom dia-zone-typeset)]
    [(custom fallback) (or custom fallback)]))

(define #:forall (S M) dia-zone-builder-compose : (-> (Option (Dia-Zone-Builder S)) (Dia-Zone-Builder S) (Dia-Zone-Builder S))
  (lambda [custom fallback]
    (cond [(not custom) fallback]
          [else (λ [id type caption style width height properties mask]
                  (define maybe-track (custom id type caption style width height properties mask))
                  
                  (if (void? maybe-track)
                      (fallback id type caption style width height properties mask)
                      maybe-track))])))

