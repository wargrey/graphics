#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Pin-Operator geo-pin-operators)
(provide geo-composite geo-pin* geo:group? Geo:Group)
(provide geo:table? Geo:Table)
(provide geo:path:group? Geo:Path:Group)
(provide geo-path-group geo-path-group*)

(provide (rename-out [geo-pin-over geo-pin])
         (rename-out [geo-ct-superimpose geo-n-superimpose] [geo-ct-superimpose* geo-n-superimpose*]
                     [geo-cb-superimpose geo-s-superimpose] [geo-cb-superimpose* geo-s-superimpose*]
                     [geo-lc-superimpose geo-w-superimpose] [geo-lc-superimpose* geo-w-superimpose*]
                     [geo-rc-superimpose geo-e-superimpose] [geo-rc-superimpose* geo-e-superimpose*])
         (rename-out [geo-lt-superimpose geo-wn-superimpose] [geo-lt-superimpose* geo-wn-superimpose*]
                     [geo-rt-superimpose geo-en-superimpose] [geo-rt-superimpose* geo-en-superimpose*]
                     [geo-lb-superimpose geo-ws-superimpose] [geo-lb-superimpose* geo-ws-superimpose*]
                     [geo-rb-superimpose geo-es-superimpose] [geo-rb-superimpose* geo-es-superimpose*])
         (rename-out [geo-h?-append geo-hx-append] [geo-h?-append* geo-hx-append*]
                     [geo-v?-append geo-vx-append] [geo-v?-append* geo-vx-append*]
                     [geo-l?-superimpose geo-lx-superimpose] [geo-l?-superimpose* geo-lx-superimpose*]
                     [geo-c?-superimpose geo-cx-superimpose] [geo-c?-superimpose* geo-cx-superimpose*]
                     [geo-r?-superimpose geo-rx-superimpose] [geo-r?-superimpose* geo-rx-superimpose*]
                     [geo-?t-superimpose geo-xt-superimpose] [geo-?t-superimpose* geo-xt-superimpose*]
                     [geo-?c-superimpose geo-xc-superimpose] [geo-?c-superimpose* geo-xc-superimpose*]
                     [geo-?b-superimpose geo-xb-superimpose] [geo-?b-superimpose* geo-xb-superimpose*]
                     [geo-??-superimpose geo-xx-superimpose] [geo-??-superimpose* geo-xx-superimpose*]))

(require digimon/digitama/unsafe/release/ops)

(require "digitama/self.rkt")
(require "digitama/composite.rkt")
(require "digitama/geometry/spacing.rkt")

(require "digitama/layer/type.rkt")
(require "digitama/layer/void.rkt")
(require "digitama/layer/find.rkt")
(require "digitama/layer/combine.rkt")
(require "digitama/layer/pyramid.rkt")

(require "digitama/dc/composite.rkt")
(require "digitama/dc/plain.rkt")

(require "digitama/paint/self.rkt")
(require "digitama/path/group.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pin geo-pin-over  #:-> Geo Geo:Group #:as geo-composite #:with 'over #:id)
(define-pin geo-pin-under #:-> Geo Geo:Group #:as geo-composite #:with 'dest-over #:id)

(define-combiner "geo-~a-append*" #:-> Geo
  #:with [geobjs #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                 #:gapsize [delta : Real 0.0] #:id [id : (Option Symbol) #false]]
  #:empty the-void-geo
  #:short-path #:for alignment base sibling #:if (zero? delta)
  ([(vl) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.0 1.0 0.0 0.0))]
   [(vc) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.5 1.0 0.5 0.0))]
   [(vr) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 1.0 1.0 0.0))]
   [(v?) (make-geo:group id base-op sibs-op (geo-append-layers 'v? base (cdr geobjs) (real->double-flonum delta)))]
   [(ht) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 0.0 0.0 0.0))]
   [(hc) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 0.5 0.0 0.5))]
   [(hb) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 1.0 0.0 1.0))]
   [(h?) (make-geo:group id base-op sibs-op (geo-append-layers 'h? base (cdr geobjs) (real->double-flonum delta)))])
  #:do (make-geo:group id base-op sibs-op (geo-append-layers alignment base (cdr geobjs) (real->double-flonum delta))))

(define geo-vl-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vl-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-vc-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vc-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-vr-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vr-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-v?-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-v?-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-ht-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-ht-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-hc-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-hc-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-hb-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-hb-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-h?-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-h?-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define-combiner "geo-~a-superimpose*" #:-> Geo
  #:with [geobjs #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                 #:gapsize [delta : Real 0.0] #:id [id : (Option Symbol) #false]]
  #:empty the-void-geo
  #:short-path #:for anchor base sibling #:if #true
  ([(lt)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.0 0.0 0.0 0.0))]
   [(lc)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.0 0.5 0.0 0.5))]
   [(lb)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.0 1.0 0.0 1.0))]
   [(ct)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.5 0.0 0.5 0.0))]
   [(cc)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.5 0.5 0.5 0.5))]
   [(cb)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.5 1.0 0.5 1.0))]
   [(rt)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 0.0 1.0 0.0))]
   [(rc)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 0.5 1.0 0.5))]
   [(rb)  (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 1.0 1.0 1.0))]
   [(l?)  (make-geo:group id base-op sibs-op (geo-superimpose-layers 'l? base (cdr geobjs)))]
   [(c?)  (make-geo:group id base-op sibs-op (geo-superimpose-layers 'c? base (cdr geobjs)))]
   [(r?)  (make-geo:group id base-op sibs-op (geo-superimpose-layers 'r? base (cdr geobjs)))]
   [(?t)  (make-geo:group id base-op sibs-op (geo-superimpose-layers '?t base (cdr geobjs)))]
   [(?c)  (make-geo:group id base-op sibs-op (geo-superimpose-layers '?c base (cdr geobjs)))]
   [(?b)  (make-geo:group id base-op sibs-op (geo-superimpose-layers '?b base (cdr geobjs)))]
   [(??)  (make-geo:group id base-op sibs-op (geo-superimpose-layers '?? base (cdr geobjs)))])
  #:do (make-geo:group id base-op sibs-op (geo-superimpose-layers anchor base (cdr geobjs))))

(define geo-lt-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-lt-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-ct-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-ct-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-rt-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-rt-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-lc-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-lc-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-cc-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-cc-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-rc-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-rc-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-lb-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-lb-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-cb-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-cb-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-rb-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-rb-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-l?-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-l?-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-c?-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-c?-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-r?-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-r?-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-?t-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-?t-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-?c-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-?c-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-?b-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-?b-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

(define geo-??-superimpose : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] . geobjs]
    (geo-??-superimpose* #:id id #:base-operator base-op #:operator sibs-op geobjs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-lt-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'lt master target)))
(define geo-ct-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'ct master target)))
(define geo-rt-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'rt master target)))
(define geo-lc-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'lc master target)))
(define geo-cc-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'cc master target)))
(define geo-rc-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'rc master target)))
(define geo-lb-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'lb master target)))
(define geo-cb-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'cb master target)))
(define geo-rb-find : (-> Geo Geo (Option Float-Complex)) (λ [master target] (geo-find 'rb master target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-frame : (-> Geo
                        [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)]
                        [#:border Maybe-Stroke-Paint] [#:background Maybe-Fill-Paint] [#:margin Geo-Spacing] [#:padding Geo-Spacing]
                        Geo:Group)
  (lambda [#:id [id #false] #:base-operator [base-op #false] #:operator [operator #false]
           #:margin [margin 0.0] #:padding [inset 0.0] #:border [border (void)] #:background [bg-fill (void)]
           geo]
    (make-geo:group id base-op operator (geo-own-layers geo) margin inset border bg-fill)))

;;; NOTE
; As a lower-level compositor, 
; Don't waste time to support vectors as input,
; since the vector is intentionally used as a mutable data structure,
; in which case it requires strictly type checking.
; as a result, you somehow have to produce immutable vectors.
; so, why not just use lists? given that
; there almost is no way to flexibly create an immutable vector at runtime.
(define geo-table : (->* (Integer (Listof Geo))
                         (#:id (Option Symbol) #:base-operator (Option Geo-Pin-Operator) #:operator (Option Geo-Pin-Operator)
                          #:border Maybe-Stroke-Paint #:background Maybe-Fill-Paint #:margin (Option Geo-Spacing) #:padding (Option Geo-Spacing)
                          (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real) (Geo-Config-Argof Real))
                         Geo:Table)
  (lambda [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false]
           #:border [bdr #false] #:background [bg #false] #:margin [margin #false] #:padding [padding #false]
           ncols siblings [col-anchors null] [row-anchors null] [col-gaps null] [row-gaps null]]
    (define size : Index (length siblings))
    
    (or (and (> size 0)
             (> ncols 0) (index? ncols)
             (let-values ([(maybe-nrows extra-ncols) (quotient/remainder size ncols)])
               (define nrows : Nonnegative-Fixnum (+ maybe-nrows (if (= extra-ncols 0) 0 1)))
               (and (> nrows 0) (index? nrows)
                    (create-geometry-table geo:table id base-op sibs-op
                                           #:border bdr #:background bg #:margin margin #:padding padding
                                           (geo-siblings->table siblings (* nrows ncols) the-void-layer)
                                           ncols nrows col-anchors row-anchors col-gaps row-gaps))))
        (create-geometry-table geo:table id base-op sibs-op
                               #:border bdr #:background bg #:margin margin #:padding padding
                               (geo-siblings->table (list the-void-geo) 1 the-void-layer)
                               1 1 col-anchors row-anchors col-gaps row-gaps))))

(define geo-table* : (->* ((Listof (Listof (Option Geo))))
                          (#:id (Option Symbol) #:base-operator (Option Geo-Pin-Operator) #:operator (Option Geo-Pin-Operator)
                           #:border Maybe-Stroke-Paint #:background Maybe-Fill-Paint #:margin (Option Geo-Spacing) #:padding (Option Geo-Spacing)
                           (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real) (Geo-Config-Argof Real))
                          Geo:Table)
  (lambda [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false]
           #:border [bdr #false] #:background [bg #false] #:margin [margin #false] #:padding [padding #false]
           siblings [col-anchors null] [row-anchors null] [col-gaps null] [row-gaps null]]
    (define ncols : Index (apply max 0 ((inst map Index (Listof (Option Geo))) length siblings)))
    (define nrows : Index (length siblings))
    
    (if (and (> ncols 0) (> nrows 0))
        (create-geometry-table geo:table id base-op sibs-op
                               #:border bdr #:background bg #:margin margin #:padding padding
                               (geo-siblings*->table siblings ncols the-void-layer)
                               ncols nrows col-anchors row-anchors col-gaps row-gaps)
        (create-geometry-table geo:table id base-op sibs-op
                               #:border bdr #:background bg #:margin margin #:padding padding
                               (geo-siblings*->table (list (list the-void-geo)) 1 the-void-layer)
                               1 1 col-anchors row-anchors col-gaps row-gaps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-pyramid : (->* ((Listof Geo))
                           (#:id (Option Symbol) #:base-operator (Option Geo-Pin-Operator) #:operator (Option Geo-Pin-Operator) #:extra (Option Geo)
                            Real (Option Real) (Geo-Config-Argof Geo-Pin-Anchor))
                           Geo)
  (lambda [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:extra [defobjs #false]
           siblings [sub-gaps 0] [sibling-gaps #false] [aligns null]]
    (cond [(null? siblings) the-void-geo]
          [(null? (cdr siblings)) (car siblings)]
          [else (let ([n (length siblings)]
                      [layer-gap (real->double-flonum (or sibling-gaps sub-gaps))])
                  (define-values (layers width height extra) (geo-list->pyramid (car siblings) (cdr siblings) defobjs))
                  (define anchors : (Vectorof Geo-Pin-Anchor) (geo-config-expand aligns n 'cc))
                  (define cell : Geo (geo-blank width height))
                  (geo-vc-append* #:gapsize sub-gaps #:id id #:base-operator base-op #:operator sibs-op
                                  (let compose : (Listof Geo) ([rest : (Listof (Listof Geo)) layers]
                                                               [idx : Index 0]
                                                               [sreyal : (Listof Geo) null])
                                    (if (pair? rest)
                                        (let-values ([(layer tail) (values (car rest) (cdr rest))])
                                          (compose tail (unsafe-idx+ idx (length layer))
                                                   (cons (geo-hc-append* #:gapsize layer-gap
                                                                         (for/list : (Listof Geo) ([g (in-list layer)]
                                                                                                   [i (in-naturals idx)])
                                                                           (define-values (w h) (geo-flsize g))
                                                                           (if (or (< w width) (< h height))
                                                                               (if (< i n)
                                                                                   (make-geo:group id base-op sibs-op
                                                                                                   (geo-superimpose-layers (vector-ref anchors i)
                                                                                                                           cell (list g)))
                                                                                   cell)
                                                                               g)))
                                                         sreyal)))
                                        (reverse sreyal)))))])))
