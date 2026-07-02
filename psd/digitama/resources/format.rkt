#lang typed/racket/base

(provide (all-defined-out) unsafe-provide)
(provide (struct-out PSD-Resource))

(require typed/racket/unsafe)
(require digimon/enumeration)

(require "../resource.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct PSD-Version-Info PSD-Resource ; 1057
  ([version : Index]
   [has-real-merged-data? : Boolean]
   [writer : String]
   [reader : String]
   [file-version : Index])
  #:transparent)

(struct PSD-Thumbnail PSD-Resource    ; 1036
  ([format : PSD-Thumbnail-Format]
   [width : Positive-Fixnum]
   [height : Positive-Fixnum]
   [widthbytes : Positive-Fixnum]
   [size : Positive-Fixnum]
   [compressed-size : Positive-Fixnum]
   [depth : Byte]
   [planes : Index]
   [image : False])
  #:transparent)

(struct PSD-File-Info PSD-Resource    ; 1028 1060
  ([raw : Any])
  #:transparent)

(struct PSD-Grid+Guides PSD-Resource  ; 1032
  ([version : Fixnum]
   [horizontal : Fixnum]
   [vertical : Fixnum]
   [guides : (Listof (Pairof Fixnum PSD-Guide-Direction))])
  #:transparent)

(struct PSD-Print-Flags PSD-Resource  ; 1011
  ([labels : Boolean]
   [crop-marks : Boolean]
   [color-bars : Boolean]
   [registration-marks : Boolean]
   [negative : Boolean]
   [flip : Boolean]
   [interpolate : Boolean]
   [caption : Boolean]
   [print-flags : Boolean])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* psd-thumbnail-format #:+> PSD-Thumbnail-Format ; order matters
  thumbnail-format->integer integer->thumbnail-format
  [0 kJpegRGB kRawRGB])

(define-enumeration* psd-guide-direction #:+> PSD-Guide-Direction ; order matters
  vhselect->integer integer->vhselect
  [0 vertical horizontal])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /psd/resources : Path (collection-file-path "resources" "psd" "digitama"))
