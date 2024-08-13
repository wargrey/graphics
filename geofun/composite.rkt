#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Pin-Operator geo-pin-operators)
(provide geo-composite geo-pin* geo:group? Geo:Group)
(provide geo-blank geo-ghost geo:blank? Geo:Blank)
(provide geo-frame geo:frame? Geo:Frame)

(provide (rename-out [geo-pin-over geo-pin]))

(require "digitama/dc/composite.rkt")
(require "digitama/dc/plain.rkt")
(require "digitama/unsafe/composite.rkt")

(require "digitama/composite.rkt")
(require "digitama/convert.rkt")
(require "paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pin geo-pin-over  #:-> Geo<%> Geo:Group #:as geo-composite #:with 'over #:id)
(define-pin geo-pin-under #:-> Geo<%> Geo:Group #:as geo-composite #:with 'dest-over #:id)

(define-combiner "geo-~a-append*" #:-> (Geo<%> [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] [#:id (Option Symbol)])
  #:with alignment geobjs [#:operator [op #false] #:gapsize [delta 0.0] #:id [id #false]]
  #:empty (geo-blank)
  #:short-path #:for base sibling #:if (zero? delta)
  ([(vl) (make-geo:group id op (geo-composite-layers base sibling 0.0 1.0 0.0 0.0))]
   [(vc) (make-geo:group id op (geo-composite-layers base sibling 0.5 1.0 0.5 0.0))]
   [(vr) (make-geo:group id op (geo-composite-layers base sibling 1.0 1.0 1.0 0.0))]
   [(ht) (make-geo:group id op (geo-composite-layers base sibling 1.0 0.0 0.0 0.0))]
   [(hc) (make-geo:group id op (geo-composite-layers base sibling 1.0 0.5 0.0 0.5))]
   [(hb) (make-geo:group id op (geo-composite-layers base sibling 1.0 1.0 0.0 1.0))])
  #:do (make-geo:group id op (geo-append-layers alignment (car geobjs) (cdr geobjs) (real->double-flonum delta))))

(define geo-vl-append : (-> [#:id (Option Symbol)] [#:operator Geo-Pin-Operator] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . siblings]
    (geo-vl-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-vc-append : (-> [#:id (Option Symbol)] [#:operator Geo-Pin-Operator] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . siblings]
    (geo-vc-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-vr-append : (-> [#:id (Option Symbol)] [#:operator Geo-Pin-Operator] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . siblings]
    (geo-vr-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-ht-append : (-> [#:id (Option Symbol)] [#:operator Geo-Pin-Operator] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . siblings]
    (geo-ht-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-hc-append : (-> [#:id (Option Symbol)] [#:operator Geo-Pin-Operator] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . siblings]
    (geo-hc-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-hb-append : (-> [#:id (Option Symbol)] [#:operator Geo-Pin-Operator] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op (default-pin-operator)] #:gapsize [delta 0.0] . siblings]
    (geo-hb-append* #:id id #:operator op #:gapsize delta siblings)))

