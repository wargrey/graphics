#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)
(require geofun/digitama/path/self)

(require geofun/digitama/edge/label)
(require geofun/digitama/dc/edge)

(require "../node/style.rkt")
(require "../node/dc.rkt")
(require "../edge/style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Path-Block-Datum (List String Dia-Node-Style (Option Symbol)))

(define-type Dia-Path-Block-Identifier (-> Geo-Anchor-Name (Option Dia-Path-Block-Datum)))
(define-type Dia-Path-Arrow-Identifier (-> Dia:Node (Option Dia:Node) (Listof Geo-Edge-Label-Datum) (Listof Geo-Path-Info-Datum) (Option Dia-Edge-Style)))
(define-type Dia-Path-Block-Create (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) (Option Symbol) Dia:Node))
(define-type Dia-Path-Id->Label-String (U (HashTable Geo-Anchor-Name String) (-> Geo-Anchor-Name String (U String Void False))))

(define-type Dia-Path-Id->Node-Label
  (-> Symbol String Dia-Node-Style (Option Symbol)
      (Option Geo)))

(define-type Dia-Path-Id->Node-Shape
  (-> Symbol (Option Geo) Dia-Node-Style Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) (Option Symbol)
      (U Void  ; use default
         False ; invisible node
         Dia:Node)))

(define-type Dia-Path-Arrow->Edge
  (-> Dia:Node (Option Dia:Node) Dia-Edge-Style
      Geo-Path-Clean-Prints+ (Listof Geo-Edge-Label)
      (U Geo:Edge Geo:Labeled-Edge Void False)))

(define-type Dia-Path-Arrow->Edge-Label
  (-> Dia:Node (Option Dia:Node) Dia-Edge-Style
      Float-Complex Float-Complex Geo-Edge-Label-Datum Nonnegative-Flonum
      (U Geo-Edge-Label (Listof Geo-Edge-Label) Void False)))

(define-type Dia-Path-Free-Track->Edge
  (-> Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint Dia-Edge-Style
      Geo-Path-Clean-Prints+ (Listof Geo-Edge-Label)
      (U Geo:Edge Geo:Labeled-Edge Void False)))

(define-type Dia-Path-Free-Track->Edge-Label
  (-> Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint Dia-Edge-Style
      Float-Complex Float-Complex Geo-Edge-Label-Datum Nonnegative-Flonum
      (U Geo-Edge-Label (Listof Geo-Edge-Label) Void False)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-dia-path-node-label-construct : Dia-Path-Id->Node-Label
  (lambda [id label style hint]
    (dia-node-text-label id label style)))

(define default-dia-path-edge-construct : Dia-Path-Arrow->Edge
  (lambda [source target style tracks labels]
    (geo-edge-attach-label
     (geo-edge* #:id (dia-edge-id-merge (geo-id source) (and target (geo-id target)) #true)
                #:stroke (dia-edge-select-line-paint style)
                #:source-marker (dia-edge-select-source-marker style)
                #:target-marker (and target (not (dia:node:label? target)) (dia-edge-select-target-marker style))
                tracks)
     labels)))

(define default-dia-path-edge-label-construct : Dia-Path-Arrow->Edge-Label
  (lambda [source target style start end label base-position]
    (make-geo-edge-labels #:font (dia-edge-select-font style)
                          #:font-paint (dia-edge-select-font-paint style)
                          #:rotate? (dia-edge-select-label-rotate? style)
                          #:distance (and (dia-edge-select-label-inline? style) 0.0)
                          start end label base-position)))

(define default-dia-path-free-edge-construct : Dia-Path-Free-Track->Edge
  (lambda [source target style tracks labels]
    (geo-edge-attach-label
     (geo-edge* #:id (dia-edge-id-merge source target #false)
                #:stroke (dia-edge-select-line-paint style)
                #:source-marker (dia-edge-select-source-marker style)
                #:target-marker (dia-edge-select-target-marker style)
                tracks)
     labels)))

(define default-dia-path-free-edge-label-construct : Dia-Path-Free-Track->Edge-Label
  (lambda [source target style start end label base-position]
    (make-geo-edge-labels #:font (dia-edge-select-font style)
                          #:font-paint (dia-edge-select-font-paint style)
                          #:rotate? (dia-edge-select-label-rotate? style)
                          #:distance (if (dia-edge-select-label-inline? style) 0.0 (dia-edge-select-label-distance style))
                          start end label base-position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-path-block-style-construct : (->* (Geo-Anchor-Name String
                                                                            (Option (Dia-Node-Style-Make* (∩ S Dia-Node-Style) (Option Symbol)))
                                                                            (-> (∩ S Dia-Node-Style)))
                                                           ((Option Symbol))
                                                           Dia-Path-Block-Datum)
  (lambda [anchor text mk-style mk-fallback-style [hint #false]]
    (list text (dia-node-style-construct anchor mk-style mk-fallback-style hint) hint)))
