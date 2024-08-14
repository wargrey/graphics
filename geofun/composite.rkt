#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Pin-Operator geo-pin-operators)
(provide geo-composite geo-pin* geo:group? Geo:Group)
(provide geo-blank geo-ghost geo:blank? Geo:Blank)
(provide geo-frame geo:frame? Geo:Frame)

(provide (rename-out [geo-pin-over geo-pin]))
(provide (rename-out [geo-ct-superimpose geo-n-superimpose] [geo-ct-superimpose* geo-n-superimpose*]
                     [geo-cb-superimpose geo-s-superimpose] [geo-cb-superimpose* geo-s-superimpose*]
                     [geo-lc-superimpose geo-w-superimpose] [geo-lc-superimpose* geo-w-superimpose*]
                     [geo-rc-superimpose geo-e-superimpose] [geo-rc-superimpose* geo-e-superimpose*])
         (rename-out [geo-lt-superimpose geo-wn-superimpose] [geo-lt-superimpose* geo-wn-superimpose*]
                     [geo-rt-superimpose geo-en-superimpose] [geo-rt-superimpose* geo-en-superimpose*]
                     [geo-lb-superimpose geo-ws-superimpose] [geo-lb-superimpose* geo-ws-superimpose*]
                     [geo-rb-superimpose geo-es-superimpose] [geo-rb-superimpose* geo-es-superimpose*]))

(require "digitama/dc/composite.rkt")
(require "digitama/dc/plain.rkt")
(require "digitama/layer/combine.rkt")
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

(define geo-vl-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vl-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-vc-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vc-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-vr-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vr-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-ht-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-ht-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-hc-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-hc-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-hb-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-hb-append* #:id id #:operator op #:gapsize delta siblings)))

(define-combiner "geo-~a-superimpose*" #:-> (Geo<%> [#:operator (Option Geo-Pin-Operator)] [#:id (Option Symbol)])
  #:with port geobjs [#:operator [op #false] #:gapsize [delta 0.0] #:id [id #false]]
  #:empty (geo-blank)
  #:short-path #:for base sibling #:if #true
  ([(lt) (make-geo:group id op (geo-composite-layers base sibling 0.0 0.0 0.0 0.0))]
   [(lc) (make-geo:group id op (geo-composite-layers base sibling 0.0 0.5 0.0 0.5))]
   [(lb) (make-geo:group id op (geo-composite-layers base sibling 0.0 1.0 0.0 1.0))]
   [(ct) (make-geo:group id op (geo-composite-layers base sibling 0.5 0.0 0.5 0.0))]
   [(cc) (make-geo:group id op (geo-composite-layers base sibling 0.5 0.5 0.5 0.5))]
   [(cb) (make-geo:group id op (geo-composite-layers base sibling 0.5 1.0 0.5 1.0))]
   [(rt) (make-geo:group id op (geo-composite-layers base sibling 1.0 0.0 1.0 0.0))]
   [(rc) (make-geo:group id op (geo-composite-layers base sibling 1.0 0.5 1.0 0.5))]
   [(rb) (make-geo:group id op (geo-composite-layers base sibling 1.0 1.0 1.0 1.0))])
  #:do (make-geo:group id op (geo-superimpose-layers port (car geobjs) (cdr geobjs))))

(define geo-lt-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-lt-superimpose* #:id id #:operator op geobjs)))

(define geo-ct-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-ct-superimpose* #:id id #:operator op geobjs)))

(define geo-rt-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-rt-superimpose* #:id id #:operator op geobjs)))

(define geo-lc-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-lc-superimpose* #:id id #:operator op geobjs)))

(define geo-cc-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-cc-superimpose* #:id id #:operator op geobjs)))

(define geo-rc-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-rc-superimpose* #:id id #:operator op geobjs)))

(define geo-lb-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-lb-superimpose* #:id id #:operator op geobjs)))

(define geo-cb-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-cb-superimpose* #:id id #:operator op geobjs)))

(define geo-rb-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo<%> * Geo<%>)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-rb-superimpose* #:id id #:operator op geobjs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
