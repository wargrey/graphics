#lang typed/racket/base

(provide (all-defined-out) unsafe-provide)
(provide (struct-out psd-resource) PSD-Resource)

(require typed/racket/unsafe)
(require digimon/enumeration)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct psd:res:version:info psd-resource ; 1057
  ([version : Index]
   [has-real-merged-data? : Boolean]
   [writer : String]
   [reader : String]
   [file-version : Index])
  #:type-name PSD:Res:Version:Info
  #:transparent)

(struct psd:res:thumbnail psd-resource    ; 1036
  ([format : PSD-Thumbnail-Format]
   [width : Positive-Fixnum]
   [height : Positive-Fixnum]
   [widthbytes : Positive-Fixnum]
   [size : Positive-Fixnum]
   [compressed-size : Positive-Fixnum]
   [depth : Byte]
   [planes : Index]
   [image : False])
  #:type-name PSD:Res:Thumbnail
  #:transparent)

(struct psd:res:file:info psd-resource    ; 1028 1060
  ([raw : Any])
  #:type-name PSD:Res:File:Info
  #:transparent)

(struct psd:res:grid+guides psd-resource  ; 1032
  ([version : Fixnum]
   [horizontal : Fixnum]
   [vertical : Fixnum]
   [guides : (Listof (Pairof Fixnum PSD-Guide-Direction))])
  #:type-name PSD:Res:Grid+Guides
  #:transparent)

(struct psd:res:print:flags psd-resource  ; 1011
  ([labels : Boolean]
   [crop-marks : Boolean]
   [color-bars : Boolean]
   [registration-marks : Boolean]
   [negative : Boolean]
   [flip : Boolean]
   [interpolate : Boolean]
   [caption : Boolean]
   [print-flags : Boolean])
  #:type-name PSD:Res:Print:Flags
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* psd-thumbnail-format #:+> PSD-Thumbnail-Format ; order matters
  thumbnail-format->integer integer->thumbnail-format
  [0 kJpegRGB kRawRGB])

(define-enumeration* psd-guide-direction #:+> PSD-Guide-Direction ; order matters
  vhselect->integer integer->vhselect
  [0 vertical horizontal])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /psd/res : Path (collection-file-path "resource" "psd" "digitama"))
