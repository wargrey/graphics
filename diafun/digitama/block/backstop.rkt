#lang typed/racket/base

(provide (all-defined-out))

(require "style.rkt")
(require "interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-typeset : (Dia-Block-Typesetter S)
  (lambda [id text style]
    (dia-block-text->caption text style #:id id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-typesetter-compose : (case-> [(Option (Dia-Block-Typesetter S)) -> (Dia-Block-Typesetter S)]
                                                            [(Option (Dia-Block-Typesetter S)) (Dia-Block-Typesetter S) -> (Dia-Block-Typesetter S)])
  (case-lambda
    [(custom) (or custom dia-block-typeset)]
    [(custom fallback) (or custom fallback)]))

(define #:forall (S M) dia-block-builder-compose : (-> (Option (Dia-Block-Builder S M)) (Dia-Block-Builder S M) (Dia-Block-Builder S M))
  (lambda [custom fallback]
    (cond [(not custom) fallback]
          [else (λ [id caption style width height direction properties]
                  (define maybe-track (custom id caption style width height direction properties))
                  
                  (if (void? maybe-track)
                      (fallback id caption style width height direction properties)
                      maybe-track))])))

