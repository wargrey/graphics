#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../self.rkt")
(require "../convert.rkt")
(require "../paint.rkt")

(require "../../stroke.rkt")
(require "../paint/self.rkt")

(require "../unsafe/dc/dingbat.rkt")
(require "../unsafe/typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:stadium geo
  ([length : Nonnegative-Flonum]
   [radius : Nonnegative-Flonum])
  #:type-name Geo:Stadium
  #:transparent)

(struct geo:file geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [x-fold-size : Nonnegative-Flonum]
   [y-fold-size : Nonnegative-Flonum]
   [corner : Geo-Corner-Choice]
   [lines : (Listof Nonnegative-Flonum)]
   [line-span : Nonnegative-Flonum]
   [line-pos% : Nonnegative-Flonum])
  #:type-name Geo:File
  #:transparent)

(struct geo:document geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [wave-height : Nonnegative-Flonum]
   [gapsize : Nonnegative-Flonum]
   [extra-n : Index])
  #:type-name Geo:Document
  #:transparent)

(struct geo:database geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [bradius : Nonnegative-Flonum]
   [gapsize : Nonnegative-Flonum]
   [extra-n : Index])
  #:type-name Geo:Database
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-stadium
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           [length : Real-Length]
           [radius : Length+%]] : Geo:Stadium
    (define flength : Nonnegative-Flonum (~dimension length))
    (define flradius : Nonnegative-Flonum (~dimension radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:stadium
                            #:with [id (geo-draw-stadium stroke pattern)
                                       (geo-shape-extent (+ d flength) d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flength flradius)))

(define geo-file
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:line-stroke [line-stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:dog-ear-size [fold-size : Length+% (&% 25)]
           #:dog-ear-corner [corner : Geo-Corner-Choice 'rt]
           #:dog-ear-angle [angle : (Option Real) #false]
           #:lines [lines : (Listof Length+%) null]
           #:line-span [span : Length+% (&% 72)]
           #:line-pos% [pos : Real 0.5]
           [width : Real-Length]
           [height : Length+% (&% 161.8)]] : Geo:File
    (define-values (flwidth flheight) (~extent width height))
    (define short-side-len (min flwidth flheight))
    (define xdgr-size (~clamp (~dimension fold-size short-side-len) (* short-side-len 0.5)))
    (define ydgr-size (if (not angle) xdgr-size (~clamp (* xdgr-size (tan angle)) 0.0 (* flheight 0.5))))
    (define ctxt-zone-height (max (- flheight ydgr-size) 0.0))
    
    (create-geometry-object geo:file
                            #:with [id (geo-draw-file stroke line-stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight xdgr-size ydgr-size corner
                            (for/list ([l (in-list lines)]) (~placement l ctxt-zone-height))
                            (~clamp (~dimension span flwidth) 0.0 flwidth)
                            (~clamp pos 0.0 1.0))))

(define geo-document
  (lambda [#:extra-n [extra-n : Index 0]
           #:id [id : (Option Symbol) #false]
           #:gapsize [gapsize : Length+% (&% 61.8)]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           [width : Real-Length]
           [height : Length+%]
           [wave : Length+% (&% 16.18)]]
    (define-values (flwidth flheight) (~extent width height))
    (define flwave : Nonnegative-Flonum (~dimension wave flheight))
    
    (create-geometry-object geo:document
                            #:with [id (geo-draw-document stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight flwave (~dimension gapsize flwave) extra-n)))

(define geo-database
  (lambda [#:extra-n [extra-n : Index 2]
           #:id [id : (Option Symbol) #false]
           #:gapsize [gapsize : Length+% (&% 61.8)]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           [width : Real-Length]
           [height : Length+%]
           [bradius : Length+% (&% 16.18)]] : Geo:Database
    (define-values (flwidth flheight) (~extent width height))
    (define flb : Nonnegative-Flonum (~dimension bradius flheight))
    
    (create-geometry-object geo:database
                            #:with [id (geo-draw-database stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight flb (~dimension gapsize flb) extra-n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-stadium : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:stadium? self)
        (dc_stadium cr x0 y0 width height
                    (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))

(define geo-draw-file : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke line-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:file? self)
        (define border-pen (geo-select-stroke-paint alt-stroke))
        
        (dc_file cr x0 y0 width height
                 (geo:file-x-fold-size self) (geo:file-y-fold-size self) (geo:file-corner self)
                 border-pen (geo-select-fill-source alt-fill) (geo-select-stroke-paint line-stroke border-pen)
                 (geo:file-lines self) (geo:file-line-span self) (geo:file-line-pos% self))))))

(define geo-draw-document : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:document? self)
        (dc_document cr x0 y0 width height (geo:document-wave-height self)
                     (geo:document-gapsize self) (geo:document-extra-n self)
                     (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))

(define geo-draw-database : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:database? self)
        (dc_database cr x0 y0 width height (geo:database-bradius self)
                     (geo:database-gapsize self) (geo:database-extra-n self)
                     (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))
