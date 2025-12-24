#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)
(require geofun/digitama/self)
(require geofun/digitama/layer/sticker)

(require colorspace/palette)

(require "self.rkt")
(require "interface.rkt")
(require "reference.rkt")

(require "../axis/self.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (plot-realize stx)
  (syntax-case stx []
    [(_ self idx total xview yview args ...)
     (syntax/loc stx
       (let-values ([(xmin xmax) (plot-range-select (plot-visualizer-xrng self) xview)]
                    [(ymin ymax) (plot-range-select (plot-visualizer-yrng self) yview)])
         ((plot-visualizer-realize self) idx total xmin xmax ymin ymax args ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-realize-all : (-> (Listof Plot-Visualizer) (Pairof Real Real) (Pairof Real Real)
                               Plot-Position-Transform Palette-Index->Pen+Brush-Colors (Option FlRGBA)
                               (Listof Geo-Visualizer))
  (lambda [visualizers xview yview origin-dot->pos palette bg-color]
    (define color-pool ((inst make-hasheq Symbol FlRGBA)))
    (define polyline-pool ((inst make-hasheq Symbol Plot-Visualizer)))

    (define (update-pool! [self : Plot-Visualizer] [vself : Geo-Visualizer]) : Void
      (define vid (geo-id (if (geo? vself) vself (geo-sticker-self (car vself)))))
      (unless (symbol-interned? vid)
        (unless (plot-visualizer-skip-palette? self)
          (hash-set! color-pool vid (geo:visualizer-color vself)))
        (when (geo:line:visualizer? vself)
          (hash-set! polyline-pool vid self))))
    
    (parameterize ([default-plot-palette palette]
                   [current-visualizer-color-pool color-pool]
                   [current-visualizer-polyline-pool polyline-pool])
      (let ([N (length visualizers)])
        (if (> N 0)
            (let realize ([visualizers : (Listof Plot-Visualizer) visualizers]
                          [idx : Nonnegative-Fixnum 0]
                          [sreyalv : (Listof Geo-Visualizer) null])
              (if (and (pair? visualizers) (<= idx N))
                  (let-values ([(self rest) (values (car visualizers) (cdr visualizers))])
                    (define vself (plot-realize self idx N xview yview origin-dot->pos bg-color))

                    (if (or vself)
                        (let ([idx++ (if (plot-visualizer-skip-palette? self) idx (+ idx 1))])
                          (update-pool! self vself)
                          (realize rest idx++ (cons vself sreyalv)))
                        (realize rest idx sreyalv)))
                  (let 2nd-stage-realize ([sreyalv : (Listof Geo-Visualizer) sreyalv]
                                          [vlayers : (Listof Geo-Visualizer) null])
                    (if (pair? sreyalv)
                        (let* ([self (car sreyalv)]
                               [nested-realize (geo:visualizer-realize self)])
                          (2nd-stage-realize (cdr sreyalv)
                                             (cons (if (not nested-realize) self (nested-realize))
                                                   vlayers)))
                        vlayers))))
            null)))))
