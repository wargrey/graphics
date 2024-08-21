#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Pin-Operator geo-pin-operators)
(provide geo-composite geo-pin* geo:group? Geo:Group)
(provide geo-blank geo-ghost geo:blank? Geo:Blank)
(provide geo-frame geo:frame? Geo:Frame)
(provide geo:table? Geo:Table)

(provide (rename-out [geo-pin-over geo-pin]))

(require racket/list)
(require digimon/function)

(require "digitama/layer/type.rkt")
(require "digitama/layer/combine.rkt")

(require "digitama/dc/composite.rkt")
(require "digitama/dc/plain.rkt")

(require "digitama/composite.rkt")
(require "digitama/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-pin geo-pin-over  #:-> Geo Geo:Group #:as geo-composite #:with 'over #:id)
(define-pin geo-pin-under #:-> Geo Geo:Group #:as geo-composite #:with 'dest-over #:id)

(define-combiner "geo-~a-append*" #:-> (Geo [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] [#:id (Option Symbol)])
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

(define geo-vl-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vl-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-vc-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vc-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-vr-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-vr-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-ht-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-ht-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-hc-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-hc-append* #:id id #:operator op #:gapsize delta siblings)))

(define geo-hb-append : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] [#:gapsize Real] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] #:gapsize [delta 0.0] . siblings]
    (geo-hb-append* #:id id #:operator op #:gapsize delta siblings)))

(define-combiner "geo-~a-superimpose*" #:-> (Geo [#:operator (Option Geo-Pin-Operator)] [#:id (Option Symbol)])
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

(define geo-lt-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-lt-superimpose* #:id id #:operator op geobjs)))

(define geo-ct-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-ct-superimpose* #:id id #:operator op geobjs)))

(define geo-rt-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-rt-superimpose* #:id id #:operator op geobjs)))

(define geo-lc-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-lc-superimpose* #:id id #:operator op geobjs)))

(define geo-cc-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-cc-superimpose* #:id id #:operator op geobjs)))

(define geo-rc-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-rc-superimpose* #:id id #:operator op geobjs)))

(define geo-lb-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-lb-superimpose* #:id id #:operator op geobjs)))

(define geo-cb-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-cb-superimpose* #:id id #:operator op geobjs)))

(define geo-rb-superimpose : (-> [#:id (Option Symbol)] [#:operator (Option Geo-Pin-Operator)] Geo * Geo)
  (λ [#:id [id #false] #:operator [op #false] . geobjs]
    (geo-rb-superimpose* #:id id #:operator op geobjs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-table : (->* (Integer (Listof Geo))
                         (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator)
                          (Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Real) (Geo-Config-Argof Real))
                         Geo)
  (lambda [#:id [id #false] #:operator [op #false] ncols siblings [col-ports null] [row-ports null] [col-gaps null] [row-gaps null]]
    (define size : Index (length siblings))
    (define cont : Geo (geo-blank))
    (or (and (> size 0)
             (> ncols 0) (index? ncols)
             (let-values ([(maybe-nrows extra-ncols) (quotient/remainder size ncols)])
               (define nrows : Nonnegative-Fixnum (+ maybe-nrows (if (= extra-ncols 0) 0 1)))
               (and (> nrows 0) (index? nrows)
                    (make-geo:table id op siblings ncols nrows col-ports row-ports col-gaps row-gaps cont))))
        cont)))

(define geo-table* : (->* ((Listof (Listof (Option Geo))))
                          (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator)
                           (Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Real) (Geo-Config-Argof Real))
                          Geo)
  (lambda [#:id [id #false] #:operator [op #false] siblings [col-ports null] [row-ports null] [col-gaps null] [row-gaps null]]
    (define ncols : Index (apply max 0 ((inst map Index (Listof (Option Geo))) length siblings)))
    (define nrows : Index (length siblings))
    (define cont : Geo (geo-blank))
    (define fill-row ((inst λoption Geo Geo) values cont))

    (or (and (> ncols 0) (> nrows 0)
             (make-geo:table id op
                             (for/fold ([cells : (Listof Geo) null])
                                       ([rows : (Listof (Option Geo)) (in-list siblings)])
                               (define rsize : Index (length rows))
                               (cond [(= ncols rsize) (append cells (map fill-row rows))]
                                     [else (append cells (map fill-row rows) (make-list (- ncols rsize) cont))]))
                             ncols nrows col-ports row-ports col-gaps row-gaps cont))
        cont)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
