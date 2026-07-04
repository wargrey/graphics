#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

(require "block.rkt")
(require "../image/enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PSD-Layer-Mask-Parameter (Immutable-Vector (Option Byte) (Option Flonum) (Option Byte) (Option Flonum)))
(define-type PSD-Layer-Channel-Id (U Index Symbol))
(define-type PSD-Layer-Channel-Info (Pairof PSD-Layer-Channel-Id Index))
(define-type PSD-Blending-Range (Immutable-Vector Byte Byte Byte Byte))
(define-type PSD-Blending-Ranges (Pairof (Pairof PSD-Blending-Range PSD-Blending-Range)
                                         (Listof (Pairof PSD-Blending-Range PSD-Blending-Range))))

(struct psd-layer-object
  ([id : (U Index Symbol)]
   [name : String] ; unicode name
   [channels : (Listof PSD-Layer-Channel)]
   [has-transparency-data? : Boolean]
   [record : PSD-Layer-Record])
  #:type-name PSD-Layer-Object
  #:transparent)

(struct psd-layer psd-layer-object
  ([blocks : PSD-Layer-Tagged-Blocks]
   [image : (Option Bytes)])
  #:type-name PSD-Layer)

(struct psd:layer:folder psd-layer () #:type-name PSD:Layer:Folder #:transparent)
(struct psd:layer:open psd:layer:folder () #:type-name PSD:Layer:Open #:transparent)
(struct psd:layer:closed psd:layer:folder () #:type-name PSD:Layer:Closed #:transparent)
(struct psd:layer:divider psd-layer () #:type-name PSD:Layer:Divider #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct psd-layer-header
  ([x : Fixnum]
   [y : Fixnum]
   [width : Index]
   [height : Index]
   [blend : PSD-Blend-Mode]
   [opacity : Byte]
   [clipping : Byte]
   [flags : (Listof Symbol)])
  #:type-name PSD-Layer-Header
  #:transparent)

(struct psd-layer-record PSD-Layer-Header
  ([mask : (Option PSD:Layer:Mask)]
   [blending-ranges : PSD-Blending-Ranges]
   [name : String] #|pascal name, rarely useful|#)
  #:type-name PSD-Layer-Record)

(struct psd-layer-channel
  ([id : PSD-Layer-Channel-Id]
   [compress-method : PSD-Compression-Method]
   [start : Index]
   [size : Index])
  #:type-name PSD-Layer-Channel
  #:transparent)

(struct psd:layer:mask
  ([x : Fixnum]
   [y : Fixnum]
   [width : Index]
   [height : Index]
   [default-color : Byte]
   [flags : (Listof Symbol)]
   [parameter : PSD-Layer-Mask-Parameter])
  #:type-name PSD:Layer:Mask
  #:transparent)

(struct psd:layer:mask:user PSD:Layer:Mask
  ([flags : (Listof Symbol)]
   [background : Byte]
   [x : Fixnum]
   [y : Fixnum]
   [width : Index]
   [height : Index])
  #:type-name PSD:Layer:Mask:User
  #:transparent)

(struct psd-global-mask-info
  ([overlay-colorspace : Fixnum]
   [colors : (List Index Index Index Index)]
   [opacity : PSD-Mask-Opacity]
   [kind : PSD-Mask-Kind])
  #:type-name PSD-Global-Mask-Info
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* psd-mask-opacity #:+> PSD-Mask-Opacity
  mask-opacity->integer integer->mask-opacity
  #:range [transparent 1] [opaque 100])

(define-enumeration* psd-mask-kind #:+> PSD-Mask-Kind
  mask-kind->integer integer->mask-kind
  #:range [selected 0] [protected 1] [shadowed 128])
