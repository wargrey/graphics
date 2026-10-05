#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/richtext/self)

(require "slot/style.rkt")
(require "slot/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Expr-Slot-Create Style Property)
  (-> Symbol (Option Geo) (Expr-Slot-Style-Spec Style)
      Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) Property
      Expr:Slot))

(define-type (Expr-Datum->Term Datum Style Property) (-> Symbol Datum Geo-Rich-Text (Expr-Slot-Style-Spec Style) Property (Option Geo)))
(define-type (Expr-Datum->Slot Datum Style Property)
  (-> Symbol Datum (Option Geo) (Expr-Slot-Style-Spec Style) Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) Property
      (U Void  ; user says: use engine's fallback
         False ; user says: it should be denied
         Expr:Slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Expr-Slot-Describe Datum) (-> Datum String Geo-Maybe-Rich-Text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (D S P) default-expr-datum->term : (Expr-Datum->Term D S P)
  (lambda [id datum desc style property]
    (expr-slot-text-term #:id id desc style)))
