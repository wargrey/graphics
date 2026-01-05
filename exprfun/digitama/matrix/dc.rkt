#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)
(require geofun/digitama/self)

(require "types.rkt")
(require "style.rkt")

(require "../slot/dc.rkt")
(require "../slot/style.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Mtx-Block-Type (U 'entry 'hole 'mask 'rhdr 'chdr 'cnr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-mtx-header-style-make : (-> Symbol Mtx-Block-Type Mtx-Hdr-Index Expr-Slot-Style)
  (lambda [id type indices]
    (case/eq type
      [(rhdr) (expr-slot-style-construct id (void) (default-mtx-row-header-style-make) make-mtx-row-header-style indices)]
      [(chdr) (expr-slot-style-construct id (void) (default-mtx-col-header-style-make) make-mtx-col-header-style indices)]
      [else (expr-slot-style-construct id (void) (default-mtx-corner-style-make) make-mtx-corner-style indices)])))

(define #:forall (M) dia-mtx-style-make : (-> Symbol M Mtx-Block-Type Mtx-Indices Expr-Slot-Style)
  (lambda [id self type indices]
    (case/eq type
      [(entry) (expr-slot-style-construct id self (default-mtx-entry-style-make) make-mtx-entry-style indices)]
      [(hole) (expr-slot-style-construct id self (default-mtx-hole-style-make) make-mtx-hole-style indices)]
      [else (expr-slot-style-construct id self (default-mtx-mask-style-make) make-mtx-mask-style indices)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T Idx) dia-mtx-slot-make : (-> Symbol T Expr-Slot-Style-Layers Idx (Option Geo)
                                             Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                             (Option (Expr-Datum->Slot T Idx)) (Expr-Datum->Slot T Idx)
                                             (Option Expr:Slot))
  (lambda [id self style indices term width height direction make-slot fallback-slot ]
    (define slot : (U Expr:Slot Void False)
      (cond [(not make-slot) (void)]
            [else (make-slot id self term style width height direction indices)]))
    
    (if (void? slot)
        (let ([fallback-slot (fallback-slot id self term style width height direction indices)])
          (and (expr:slot? fallback-slot)
               fallback-slot))
        slot)))
