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
(require digimon/measure)

(require "digitama/self.rkt")
(require "digitama/composite.rkt")
(require "digitama/geometry/sides.rkt")

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
                 #:gapsize [delta : Real-Length 0.0] #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]]
  #:empty the-void-geo
  #:short-path #:for alignment base sibling #:if (&zero? delta)
  ([(vl) (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 0.0 1.0 0.0 0.0))]
   [(vc) (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 0.5 1.0 0.5 0.0))]
   [(vr) (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 1.0 1.0 1.0 0.0))]
   [(v?) (make-geo:group id base-op sibs-op desc (geo-append-layers 'v? base (cdr geobjs) (~distance delta)))]
   [(ht) (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 1.0 0.0 0.0 0.0))]
   [(hc) (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 1.0 0.5 0.0 0.5))]
   [(hb) (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 1.0 1.0 0.0 1.0))]
   [(h?) (make-geo:group id base-op sibs-op desc (geo-append-layers 'h? base (cdr geobjs) (~distance delta)))])
  #:do (make-geo:group id base-op sibs-op desc (geo-append-layers alignment base (cdr geobjs) (~distance delta))))

(define (geo-vl-append #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false] #:gapsize [delta : Real-Length 0.0]
                       #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                       . [siblings : Geo *]) : Geo
  (geo-vl-append* #:id id #:desc desc #:base-operator base-op #:operator sibs-op #:gapsize delta siblings))

(define (geo-vc-append #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false] #:gapsize [delta : Real-Length 0.0]
                       #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                       . [siblings : Geo *]) : Geo
  (geo-vc-append* #:id id #:desc desc #:base-operator base-op #:operator sibs-op #:gapsize delta siblings))

(define (geo-vr-append #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false] #:gapsize [delta : Real-Length 0.0]
                       #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                       . [siblings : Geo *]) : Geo
  (geo-vr-append* #:id id #:desc desc #:base-operator base-op #:operator sibs-op #:gapsize delta siblings))

(define (geo-v?-append #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false] #:gapsize [delta : Real-Length 0.0]
                       #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                       . [siblings : Geo *]) : Geo
  (geo-v?-append* #:id id #:desc desc #:base-operator base-op #:operator sibs-op #:gapsize delta siblings))

(define (geo-ht-append #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false] #:gapsize [delta : Real-Length 0.0]
                       #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                       . [siblings : Geo *]) : Geo
  (geo-ht-append* #:id id #:desc desc #:base-operator base-op #:operator sibs-op #:gapsize delta siblings))

(define (geo-hc-append #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false] #:gapsize [delta : Real-Length 0.0]
                       #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                       . [siblings : Geo *]) : Geo
  (geo-hc-append* #:id id #:desc desc #:base-operator base-op #:operator sibs-op #:gapsize delta siblings))

(define (geo-hb-append #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false] #:gapsize [delta : Real-Length 0.0]
                       #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                       . [siblings : Geo *]) : Geo
  (geo-hb-append* #:id id #:desc desc #:base-operator base-op #:operator sibs-op #:gapsize delta siblings))

(define (geo-h?-append #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false] #:gapsize [delta : Real-Length 0.0]
                       #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                       . [siblings : Geo *]) : Geo
  (geo-h?-append* #:id id #:desc desc #:base-operator base-op #:operator sibs-op #:gapsize delta siblings))

(define-combiner "geo-~a-superimpose*" #:-> Geo
  #:with [geobjs #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                 #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]]
  #:empty the-void-geo
  #:short-path #:for anchor base sibling #:if #true
  ([(lt)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 0.0 0.0 0.0 0.0))]
   [(lc)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 0.0 0.5 0.0 0.5))]
   [(lb)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 0.0 1.0 0.0 1.0))]
   [(ct)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 0.5 0.0 0.5 0.0))]
   [(cc)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 0.5 0.5 0.5 0.5))]
   [(cb)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 0.5 1.0 0.5 1.0))]
   [(rt)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 1.0 0.0 1.0 0.0))]
   [(rc)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 1.0 0.5 1.0 0.5))]
   [(rb)  (make-geo:group id base-op sibs-op desc (geo-composite-layers base sibling 1.0 1.0 1.0 1.0))]
   [(l?)  (make-geo:group id base-op sibs-op desc (geo-superimpose-layers 'l? base (cdr geobjs)))]
   [(c?)  (make-geo:group id base-op sibs-op desc (geo-superimpose-layers 'c? base (cdr geobjs)))]
   [(r?)  (make-geo:group id base-op sibs-op desc (geo-superimpose-layers 'r? base (cdr geobjs)))]
   [(?t)  (make-geo:group id base-op sibs-op desc (geo-superimpose-layers '?t base (cdr geobjs)))]
   [(?c)  (make-geo:group id base-op sibs-op desc (geo-superimpose-layers '?c base (cdr geobjs)))]
   [(?b)  (make-geo:group id base-op sibs-op desc (geo-superimpose-layers '?b base (cdr geobjs)))]
   [(??)  (make-geo:group id base-op sibs-op desc (geo-superimpose-layers '?? base (cdr geobjs)))])
  #:do (make-geo:group id base-op sibs-op desc (geo-superimpose-layers anchor base (cdr geobjs))))

(define (geo-lt-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-lt-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-ct-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-ct-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-rt-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-rt-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-lc-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-lc-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-cc-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-cc-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-rc-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-rc-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-lb-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-lb-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-cb-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-cb-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-rb-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-rb-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-l?-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-l?-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-c?-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-c?-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-r?-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-r?-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-?t-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-?t-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-?c-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-?c-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-?b-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-?b-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

(define (geo-??-superimpose #:id [id : (Option Symbol) #false] #:desc [desc : (Option String) #false]
                            #:base-operator [base-op : (Option Geo-Pin-Operator) #false] #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
                            . [geobjs : Geo *]) : Geo
  (geo-??-superimpose* #:id id #:desc desc #:base-operator base-op #:operator sibs-op geobjs))

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
(define geo-frame
  (lambda [#:id [id : (Option Symbol) #false]
           #:desc [desc : (Option String) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [operator : (Option Geo-Pin-Operator) #false]
           #:margin [margin : Geo-Insets-Datum 0.0]
           #:padding [inset : Geo-Insets-Datum 0.0]
           #:border [border : Maybe-Stroke-Paint (void)]
           #:background [bg-fill : Maybe-Fill-Paint (void)]
           #:open-sides [open-sides : (Option Geo-Open-Sides) #false]
           [geo : Geo]] : Geo:Group
    (make-geo:group id base-op operator desc (geo-own-layers geo) margin inset border bg-fill open-sides)))

;;; NOTE
; As a lower-level compositor, 
; Don't waste time to support vectors as input,
; since the vector is intentionally used as a mutable data structure,
; in which case it requires strictly type checking.
; as a result, you somehow have to produce immutable vectors.
; so, why not just use lists? given that
; there almost is no way to flexibly create an immutable vector at runtime.
(define geo-table
  (lambda [#:id [id : (Option Symbol) #false]
           #:desc [desc : (Option String) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Insets-Datum) #false]
           #:padding [padding : (Option Geo-Insets-Datum) #false]
           #:open-sides [open-sides : (Option Geo-Open-Sides) #false]
           [ncols : Integer]
           [siblings : (Listof Geo)]
           [col-anchors : (Geo-Config-Argof Geo-Pin-Anchor) null]
           [row-anchors : (Geo-Config-Argof Geo-Pin-Anchor) null]
           [col-gaps : (Geo-Config-Argof Real) null]
           [row-gaps : (Geo-Config-Argof Real) null]] : Geo:Table
    (define size : Index (length siblings))
    
    (or (and (> size 0)
             (> ncols 0) (index? ncols)
             (let-values ([(maybe-nrows extra-ncols) (quotient/remainder size ncols)])
               (define nrows : Nonnegative-Fixnum (+ maybe-nrows (if (= extra-ncols 0) 0 1)))
               (and (> nrows 0) (index? nrows)
                    (create-geometry-table geo:table id base-op sibs-op
                                           #:desc desc #:open-sides open-sides
                                           #:border bdr #:background bg #:margin margin #:padding padding
                                           (geo-siblings->table siblings (* nrows ncols) the-void-layer)
                                           ncols nrows col-anchors row-anchors col-gaps row-gaps))))
        (create-geometry-table geo:table id base-op sibs-op
                               #:desc desc #:open-sides open-sides
                               #:border bdr #:background bg #:margin margin #:padding padding
                               (geo-siblings->table (list the-void-geo) 1 the-void-layer)
                               1 1 col-anchors row-anchors col-gaps row-gaps))))

(define geo-table*
  (lambda [#:id [id : (Option Symbol) #false]
           #:desc [desc : (Option String) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Insets-Datum) #false]
           #:padding [padding : (Option Geo-Insets-Datum) #false]
           #:open-sides [open-sides : (Option Geo-Open-Sides) #false]
           [siblings : (Listof (Listof (Option Geo)))]
           [col-anchors : (Geo-Config-Argof Geo-Pin-Anchor) null]
           [row-anchors : (Geo-Config-Argof Geo-Pin-Anchor) null]
           [col-gaps : (Geo-Config-Argof Real-Length) null]
           [row-gaps : (Geo-Config-Argof Real-Length) null]] : Geo:Table
    (define ncols : Index (apply max 0 ((inst map Index (Listof (Option Geo))) length siblings)))
    (define nrows : Index (length siblings))
    
    (if (and (> ncols 0) (> nrows 0))
        (create-geometry-table geo:table id base-op sibs-op
                               #:desc desc #:open-sides open-sides
                               #:border bdr #:background bg #:margin margin #:padding padding
                               (geo-siblings*->table siblings ncols the-void-layer)
                               ncols nrows col-anchors row-anchors col-gaps row-gaps)
        (create-geometry-table geo:table id base-op sibs-op
                               #:desc desc #:open-sides open-sides
                               #:border bdr #:background bg #:margin margin #:padding padding
                               (geo-siblings*->table (list (list the-void-geo)) 1 the-void-layer)
                               1 1 col-anchors row-anchors col-gaps row-gaps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-pyramid
  (lambda [#:id [id : (Option Symbol) #false]
           #:desc [desc : (Option String) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [sibs-op : (Option Geo-Pin-Operator) #false]
           #:extra [defobjs : (Option Geo) #false]
           [siblings : (Listof Geo)]
           [sub-gaps : Real-Length 0]
           [sibling-gaps : (Option Real-Length) #false]
           [aligns : (Geo-Config-Argof Geo-Pin-Anchor) null]] : Geo
    (cond [(null? siblings) the-void-geo]
          [(null? (cdr siblings)) (car siblings)]
          [else (let ([n (length siblings)]
                      [layer-gap (~distance (or sibling-gaps sub-gaps))])
                  (define-values (layers width height extra) (geo-list->pyramid (car siblings) (cdr siblings) defobjs))
                  (define anchors : (Vectorof Geo-Pin-Anchor) (geo-config-expand aligns n 'cc))
                  (define cell : Geo (geo-blank width height))
                  (geo-vc-append* #:gapsize sub-gaps #:id id #:desc desc #:base-operator base-op #:operator sibs-op
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
                                                                                   (make-geo:group id base-op sibs-op #false
                                                                                                   (geo-superimpose-layers (vector-ref anchors i)
                                                                                                                           cell (list g)))
                                                                                   cell)
                                                                               g)))
                                                         sreyal)))
                                        (reverse sreyal)))))])))
