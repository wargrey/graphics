#lang typed/racket/base

(provide (all-defined-out))
(provide dia:block? Dia:Block)

(require "dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia:block-same-type? : (-> Dia:Block (Option Dia:Block) Boolean)
  (lambda [lgt rgt]
    (and rgt
         (eq? (object-name (dia:block-phantom-type lgt))
              (object-name (dia:block-phantom-type rgt))))))

(define dia:block-diff-type? : (-> Dia:Block (Option Dia:Block) Boolean)
  (lambda [lgt rgt]
    (if rgt
        (not (eq? (object-name (dia:block-phantom-type lgt))
                  (object-name (dia:block-phantom-type rgt))))
        #true)))

(define dia:block-typeof? : (-> Dia:Block (-> Any Boolean) Boolean)
  (lambda [self phantom-type?]
    (phantom-type? (dia:block-phantom-type self))))

(define dia:block*-typeof? : (-> (Option Dia:Block) (-> Any Boolean) Boolean : #:+ Dia:Block)
  (lambda [self phantom-type?]
    (and self (phantom-type? (dia:block-phantom-type self)))))

(define dia:block-has-tag? : (-> Dia:Block (U Keyword Symbol) Boolean)
  (lambda [self tag]
    (and (memq tag (dia:block-tags self)) #true)))

(define dia:block*-has-tag? : (-> (Option Dia:Block) (U Keyword Symbol) Boolean : #:+ Dia:Block)
  (lambda [self tag]
    (and self (dia:block-has-tag? self tag))))
