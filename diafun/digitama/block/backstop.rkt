#lang typed/racket/base

(provide (all-defined-out))

(require "style.rkt")
(require "interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-typeset : (Dia-Block-Typesetter S)
  (lambda [id text style]
    (dia-block-text->caption text style #:id id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-typesetter-compose : (-> (Option (Dia-Block-Typesetter S)) (Dia-Block-Typesetter S))
  (lambda [custom]
    (cond [(or custom) custom]
          [else dia-block-typeset])))

(define #:forall (S M) dia-block-builder-compose : (-> (Option (Dia-Block-Builder S M)) (Dia-Block-Builder S M) (Dia-Block-Builder S M))
  (lambda [custom fallback]
    (cond [(not custom) fallback]
          [else (λ [id caption style width height direction properties]
                  (define maybe-track (custom id caption style width height direction properties))
                  
                  (if (void? maybe-track)
                      (fallback id caption style width height direction properties)
                      maybe-track))])))
