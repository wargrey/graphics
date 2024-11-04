#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Pin-Operator geo-pin-operators)
(provide geo-composite geo-pin* geo:group? Geo:Group)
(provide geo-blank geo-ghost geo:blank? Geo:Blank)
(provide geo:table? Geo:Table)

(provide (rename-out [geo-pin-over geo-pin]))

(require racket/list)
(require digimon/function)

(require "digitama/layer/type.rkt")
(require "digitama/layer/combine.rkt")
(require "digitama/layer/find.rkt")

(require "digitama/dc/composite.rkt")
(require "digitama/dc/plain.rkt")

(require "digitama/composite.rkt")
(require "digitama/convert.rkt")
(require "paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pin geo-pin-over  #:-> Geo Geo:Group #:as geo-composite #:with 'over #:id)
(define-pin geo-pin-under #:-> Geo Geo:Group #:as geo-composite #:with 'dest-over #:id)

(define-combiner "geo-~a-append*" #:-> Geo
  #:with [geobjs #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                 #:gapsize [delta : Real 0.0] #:id [id : (Option Symbol) #false]]
  #:empty (geo-blank)
  #:short-path #:for alignment base sibling #:if (zero? delta)
  ([(vl) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.0 1.0 0.0 0.0))]
   [(vc) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.5 1.0 0.5 0.0))]
   [(vr) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 1.0 1.0 0.0))]
   [(ht) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 0.0 0.0 0.0))]
   [(hc) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 0.5 0.0 0.5))]
   [(hb) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 1.0 0.0 1.0))])
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

(define geo-ht-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-ht-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-hc-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-hc-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define geo-hb-append : (-> [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false] #:gapsize [delta 0.0] . siblings]
    (geo-hb-append* #:id id #:base-operator base-op #:operator sibs-op #:gapsize delta siblings)))

(define-combiner "geo-~a-superimpose*" #:-> Geo
  #:with [geobjs #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                 #:gapsize [delta : Real 0.0] #:id [id : (Option Symbol) #false]]
  #:empty (geo-blank)
  #:short-path #:for anchor base sibling #:if #true
  ([(lt) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.0 0.0 0.0 0.0))]
   [(lc) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.0 0.5 0.0 0.5))]
   [(lb) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.0 1.0 0.0 1.0))]
   [(ct) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.5 0.0 0.5 0.0))]
   [(cc) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.5 0.5 0.5 0.5))]
   [(cb) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 0.5 1.0 0.5 1.0))]
   [(rt) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 0.0 1.0 0.0))]
   [(rc) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 0.5 1.0 0.5))]
   [(rb) (make-geo:group id base-op sibs-op (geo-composite-layers base sibling 1.0 1.0 1.0 1.0))])
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
(define geo-frame : (-> Geo [#:id (Option Symbol)] [#:base-operator (Option Geo-Pin-Operator)] [#:operator (Option Geo-Pin-Operator)]
                        [#:border Maybe-Stroke-Paint] [#:background Maybe-Fill-Paint]
                        [#:margin (U Nonnegative-Real (Listof Nonnegative-Real))]
                        [#:padding (U Nonnegative-Real (Listof Nonnegative-Real))]
                        Geo:Group)
  (lambda [#:id [id #false] #:base-operator [base-op #false] #:operator [operator #false]
           #:margin [margin 0.0] #:padding [inset 0.0] #:border [border (void)] #:background [bg-fill (void)]
           geo]
    (make-geo:group id base-op operator (geo-own-layers geo) margin inset border bg-fill)))

(define geo-table : (->* (Integer (Listof Geo))
                         (#:id (Option Symbol) #:base-operator (Option Geo-Pin-Operator) #:operator (Option Geo-Pin-Operator)
                          (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real) (Geo-Config-Argof Real))
                         Geo)
  (lambda [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false]
           ncols siblings [col-anchors null] [row-anchors null] [col-gaps null] [row-gaps null]]
    (define size : Index (length siblings))
    (define cont : Geo (geo-blank))
    (or (and (> size 0)
             (> ncols 0) (index? ncols)
             (let-values ([(maybe-nrows extra-ncols) (quotient/remainder size ncols)])
               (define nrows : Nonnegative-Fixnum (+ maybe-nrows (if (= extra-ncols 0) 0 1)))
               (and (> nrows 0) (index? nrows)
                    (make-geo:table id base-op sibs-op siblings ncols nrows col-anchors row-anchors col-gaps row-gaps cont))))
        cont)))

(define geo-table* : (->* ((Listof (Listof (Option Geo))))
                          (#:id (Option Symbol) #:base-operator (Option Geo-Pin-Operator) #:operator (Option Geo-Pin-Operator)
                           (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real) (Geo-Config-Argof Real))
                          Geo)
  (lambda [#:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false]
           siblings [col-anchors null] [row-anchors null] [col-gaps null] [row-gaps null]]
    (define ncols : Index (apply max 0 ((inst map Index (Listof (Option Geo))) length siblings)))
    (define nrows : Index (length siblings))
    (define cont : Geo (geo-blank))
    (define fill-row ((inst λoption Geo Geo) values cont))

    (or (and (> ncols 0) (> nrows 0)
             (make-geo:table id base-op sibs-op
                             (for/fold ([cells : (Listof Geo) null])
                                       ([rows : (Listof (Option Geo)) (in-list siblings)])
                               (define rsize : Index (length rows))
                               (cond [(= ncols rsize) (append cells (map fill-row rows))]
                                     [else (append cells (map fill-row rows) (make-list (- ncols rsize) cont))]))
                             ncols nrows col-anchors row-anchors col-gaps row-gaps cont))
        cont)))
