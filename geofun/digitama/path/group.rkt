#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../dc/plain.rkt")
(require "../dc/path.rkt")
(require "../dc/composite.rkt")

(require "../convert.rkt")
(require "../layer/type.rkt")
(require "../layer/combine.rkt")
(require "../layer/merge.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:path:group geo:group
  ([translate : Float-Complex])
  #:type-name Geo:Path:Group
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-group : (->* () (#:offset Float-Complex #:id (Option Symbol)) #:rest Geo
                              (U Geo:Path:Group Geo:Blank))
  (lambda [#:offset [offset 0.0+0.0i] #:id [name #false] . selves]
    (geo-path-group* selves #:offset offset #:id name)))

(define geo-path-group* : (->* ((Listof Geo)) (#:offset Float-Complex #:id (Option Symbol))
                               (U Geo:Path:Group Geo:Blank))
  (lambda [selves #:offset [offset 0.0+0.0i] #:id [name #false]]
    (define layers : (Listof (GLayerof Geo))
      (for/fold ([layers : (Listof (GLayerof Geo)) null]
                 #:result (reverse layers))
                ([self (in-list selves)])
        (cond [(geo:path? self) (cons (geo-path-self-pin-layer self offset) layers)]
              [(geo:path:self? self) (cons (geo-path-self-pin-layer self offset) layers)]
              [(geo:group? self)
               (let ([t (if (geo:path:group? self) (+ offset (geo:path:group-translate self)) offset)])
                 (let flatten ([sub : (Listof (GLayerof Geo)) (glayer-group-layers (geo:group-selves self))]
                               [sreyal : (Listof (GLayerof Geo)) layers])
                   (if (pair? sub)
                       (let-values ([(subself rest) (values (glayer-master (car sub)) (cdr sub))])
                         (cond [(geo:path:self? subself) (flatten rest (cons (geo-path-self-pin-layer subself t) sreyal))]
                               [(geo:path? subself) (flatten rest (cons (geo-path-self-pin-layer subself t) sreyal))]
                               [(geo:group? subself) (flatten (append (glayer-group-layers (geo:group-selves self)) sub) sreyal)]
                               [else (flatten rest sreyal)]))
                       sreyal)))]
              [else layers])))
    
    (if (pair? layers)
        (create-geometry-group geo:path:group name #false #false
                               (geo-layers-merge layers) offset)
        (geo-blank))))
