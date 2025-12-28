#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)
(require geofun/digitama/track/self)

(require geofun/digitama/path/label)
(require geofun/digitama/dc/path)

(require "track/style.rkt")
(require "block/style.rkt")
(require "block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Dia-Block-Info* Brief Urgent) (List Brief Dia-Block-Style Urgent))
(define-type (Dia-Block-Identifier* Brief Urgent) (-> Geo-Anchor-Name (Option (Dia-Block-Info* Brief Urgent))))
(define-type (Dia-Block-Create* Urgent) (-> Symbol (Option Geo) Dia-Block-Style-Layers Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) Urgent Dia:Block))

; designed mainly for track based diagrams
(define-type (Dia-Anchor->Brief* Urgent) (-> Geo-Anchor-Name Dia-Block-Brief-Datum Dia-Block-Style-Layers Urgent (Option Geo)))
(define-type (Dia-Anchor->Block* T Urgent)
  (-> T (Option Geo) Dia-Block-Style-Layers Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) Urgent
      (U Void  ; user says: use engine's fallback
         False ; user says: it should be denied
         Dia:Block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Block-Info (Dia-Block-Info* String (Option Symbol)))

(define-type Dia-Block-Identifier (Dia-Block-Identifier* String (Option Symbol)))
(define-type Dia-Track-Identifier (-> Dia:Block (Option Dia:Block) (Listof Geo-Path-Labels) (Listof Geo-Track-Info-Datum) (Option Dia-Track-Style)))
(define-type Dia-Block-Describe (U (Immutable-HashTable Geo-Anchor-Name Dia-Block-Option-Brief) (-> Geo-Anchor-Name String Dia-Block-Maybe-Brief)))
(define-type Dia-Anchor->Brief (Dia-Anchor->Brief* (Option Symbol)))
(define-type Dia-Block-Create (Dia-Block-Create* (Option Symbol)))
(define-type Dia-Anchor->Block (Dia-Anchor->Block* Symbol (Option Symbol)))

(define-type Dia-Track->Path
  (-> Dia:Block (Option Dia:Block) Dia-Track-Style-Layers
      Geo-Path-Clean-Prints* (Listof Geo:Path:Label)
      (U Geo:Path Void False)))

(define-type Dia-Track->Label
  (-> Dia:Block (Option Dia:Block) Dia-Track-Style-Layers
      Index Geo-Path-Labels Nonnegative-Flonum
      (U Geo:Path:Label (Listof Geo:Path:Label) Void False)))

(define-type Dia-Free-Track->Path
  (-> Dia-Free-Track-Endpoint Dia-Free-Track-Endpoint Dia-Track-Style-Layers
      Geo-Path-Clean-Prints* (Listof Geo:Path:Label)
      (U Geo:Path Void False)))

(define-type Dia-Free-Track->Label
  (-> Dia-Free-Track-Endpoint Dia-Free-Track-Endpoint Dia-Track-Style-Layers
      Index Geo-Path-Labels Nonnegative-Flonum
      (U Geo:Path:Label (Listof Geo:Path:Label) Void False)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (D) default-dia-anchor->brief : (Dia-Anchor->Brief* D)
  (lambda [id desc style datum]
    (dia-block-text-brief #:id id desc style)))

(define default-dia-track->path : Dia-Track->Path
  (lambda [source target style tracks labels]
    (geo-path-attach-label
     (geo-path* #:id (dia-track-id-merge (geo-id source) (and target (geo-id target)) #true)
                #:stroke (dia-track-resolve-line-paint style)
                #:source-tip (dia-track-resolve-source-tip style)
                #:target-tip (and target (not (dia:block:label? target)) (dia-track-resolve-target-tip style))
                #:tip-placement 'inside
                tracks)
     labels)))

(define default-dia-track->label : Dia-Track->Label
  (lambda [source target style idx label base-position]
    (make-geo-path-labels #:font (dia-track-resolve-font style)
                          #:color (dia-track-resolve-font-paint style)
                          #:rotate? (dia-track-resolve-label-rotate? style)
                          #:distance (and (dia-track-resolve-label-inline? style) 0.0)
                          #:index idx
                          label base-position)))

(define default-dia-free-track->path : Dia-Free-Track->Path
  (lambda [source target style tracks labels]
    (geo-path-attach-label
     (geo-path* #:id (dia-track-id-merge source target #false)
                #:stroke (dia-track-resolve-line-paint style)
                #:source-tip (dia-track-resolve-source-tip style)
                #:target-tip (dia-track-resolve-target-tip style)
                #:tip-placement 'inside
                tracks)
     labels)))

(define default-dia-free-track->label : Dia-Free-Track->Label
  (lambda [source target style idx label base-position]
    (make-geo-path-labels #:font (dia-track-resolve-font style)
                          #:color (dia-track-resolve-font-paint style)
                          #:rotate? (dia-track-resolve-label-rotate? style)
                          #:distance (cond [(dia-track-resolve-label-inline? style) 0.0]
                                           [else (dia-track-resolve-label-distance style)])
                          #:index idx
                          label base-position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T S B U) dia-block-info* : (-> T B
                                                 (Option (Dia-Block-Style-Make* T (∩ S Dia-Block-Style) U))
                                                 (-> (∩ S Dia-Block-Style))
                                                 U
                                                 (Dia-Block-Info* B U))
  (lambda [anchor text mk-style mk-fallback-style datum]
    (list text (dia-block-style-construct anchor mk-style mk-fallback-style datum) datum)))

(define #:forall (S) dia-block-info : (->* (Geo-Anchor-Name String
                                                            (Option (Dia-Block-Style-Make* Geo-Anchor-Name (∩ S Dia-Block-Style) (Option Symbol)))
                                                            (-> (∩ S Dia-Block-Style)))
                                           ((Option Symbol))
                                           Dia-Block-Info)
  (lambda [anchor text mk-style mk-fallback-style [datum #false]]
    (dia-block-info* anchor text mk-style mk-fallback-style datum)))
