#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)

(require "types.rkt")
(require "style.rkt")

(require "../slot/dc.rkt")
(require "../slot/style.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Mtx-Block-Type (U 'entry 'hole 'mask 'rhdr 'chdr 'cnr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-mtx-header-style : (-> Symbol Mtx-Block-Type Mtx-Hdr-Index (Expr-Slot-Style Mtx-Slot-Style))
  (lambda [id type indices]
    (cond [(eq? type 'rhdr) (expr-slot-theme-adjust (default-mtx-row-header-style) id (void) (default-mtx-row-header-theme-adjuster) indices)]
          [(eq? type 'chdr) (expr-slot-theme-adjust (default-mtx-col-header-style) id (void) (default-mtx-col-header-theme-adjuster) indices)]
          [else             (expr-slot-theme-adjust (default-mtx-corner-style) id (void) (default-mtx-corner-theme-adjuster) indices)])))

(define #:forall (M) dia-mtx-style-make : (-> Symbol M Mtx-Block-Type Mtx-Indices (Expr-Slot-Style Mtx-Slot-Style))
  (lambda [id self type indices]
    (cond [(eq? type 'entry) (expr-slot-theme-adjust (default-mtx-entry-style) id self (default-mtx-entry-theme-adjuster) indices)]
          [(eq? type  'hole) (expr-slot-theme-adjust (default-mtx-hole-style) id self (default-mtx-hole-theme-adjuster) indices)]
          [else              (expr-slot-theme-adjust (default-mtx-mask-style) id self (default-mtx-mask-theme-adjuster) indices)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T Idx) make-mtx-slot : (-> Symbol T (Expr-Slot-Style-Spec Mtx-Slot-Style) Idx (Option Geo)
                                             Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                             (Option (Expr-Datum->Slot T Mtx-Slot-Style Idx))
                                             (Expr-Datum->Slot T Mtx-Slot-Style Idx)
                                             (Option Expr:Slot))
  (lambda [id self style indices term width height direction make-slot fallback-slot]
    (define slot : (U Expr:Slot Void False)
      (cond [(not make-slot) (void)]
            [else (make-slot id self term style width height direction indices)]))
    
    (if (void? slot)
        (let ([fallback-slot (fallback-slot id self term style width height direction indices)])
          (and (expr:slot? fallback-slot)
               fallback-slot))
        slot)))
