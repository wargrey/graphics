#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/richtext/self)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)
(require geofun/digitama/track/self)

(require geofun/digitama/path/label)
(require geofun/digitama/dc/path)

(require "track/style.rkt")
(require "block/style.rkt")
(require "block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Block-Info (List String Dia-Block-Style (Option Symbol)))

(define-type Dia-Block-Identifier (-> Geo-Anchor-Name (Option Dia-Block-Info)))
(define-type Dia-Track-Identifier (-> Dia:Block (Option Dia:Block) (Listof Geo-Path-Labels) (Listof Geo-Track-Info-Datum) (Option Dia-Track-Style)))
(define-type Dia-Block-Describe (U (Immutable-HashTable Geo-Anchor-Name Geo-Maybe-Rich-Text) (-> Geo-Anchor-Name String Geo-Maybe-Rich-Text)))
(define-type Dia-Anchor->Caption (-> Geo-Anchor-Name Geo-Rich-Text Dia-Block-Style-Layers (Option Symbol) (Option Geo)))
(define-type Dia-Block-Create (-> Symbol (Option Geo) Dia-Block-Style-Layers Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) (Option Symbol) Dia:Block))

(define-type Dia-Anchor->Block
  (-> Symbol (Option Geo) Dia-Block-Style-Layers Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) (Option Symbol)
      (U Void  ; user says: use engine's fallback
         False ; user says: it should be denied
         Dia:Block)))

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
(define default-dia-anchor->caption : Dia-Anchor->Caption
  (lambda [id desc style property]
    (dia-block-text-caption #:id id desc style)))

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
(define #:forall (S) dia-block-info : (->* (Geo-Anchor-Name String
                                                            (Option (Dia-Block-Style-Make (∩ S Dia-Block-Style)))
                                                            (-> (∩ S Dia-Block-Style)))
                                           ((Option Symbol))
                                          Dia-Block-Info)
  (lambda [anchor text mk-style mk-fallback-style [datum #false]]
    (list text (dia-block-style-construct anchor mk-style mk-fallback-style datum) datum)))
