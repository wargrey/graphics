#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)
(require geofun/digitama/convert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Renderer-Tree (Listof (U Plot:Renderer Plot-Renderer-Tree)))

(define-type Plot-Renderer-Range (Pairof (Option Real) (Option Real)))
(define-type Plot-Renderer-Realize (-> Real Real Flonum Flonum Flonum Flonum (-> Flonum Flonum Float-Complex) Geo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:renderer
  ([realize : Plot-Renderer-Realize]
   [label : Any]
   [xrng : Plot-Renderer-Range]
   [yrng : Plot-Renderer-Range]
   [color : FlRGBA])
  #:property prop:procedure (struct-field-index realize)
  #:type-name Plot:Renderer
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-renderer-tree-flatten : (-> Plot-Renderer-Tree (Values (Listof Plot:Renderer) (Pairof Real Real)))
  (lambda [tree]
    (define-values (sreredner xmin xmax)
      (let flatten : (Values (Listof Plot:Renderer) Real Real)
        ([ts : Plot-Renderer-Tree tree]
         [xmin : Real +inf.0]
         [xmax : Real -inf.0]
         [sreredner : (Listof Plot:Renderer) null])
        (if (pair? ts)
            (let-values ([(self rest) (values (car ts) (cdr ts))])
              (if (list? self)
                  (let-values ([(sbus xsmin xsmax) (flatten self xmin xmax sreredner)])
                    (flatten rest xsmin xsmax sbus))
                  (let ([xrng (plot:renderer-xrng self)]
                        [yrng (plot:renderer-yrng self)])
                    (flatten rest
                             (min xmin (or (car xrng) +inf.0))
                             (max xmax (or (cdr xrng) -inf.0))
                             (cons self sreredner)))))
            (values sreredner xmin xmax))))
    
    (values (reverse sreredner) (cons xmin xmax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-renderer-real-filter : (-> (Option Real) (Option Real))
  (lambda [v]
    (and (rational? v)
         v)))

(define plot-range-normalize : (-> (Option Real) (Option Real) (Option Real) (Option Real)
                                   (Values Plot-Renderer-Range Plot-Renderer-Range))
  (lambda [xmin xmax ymin ymax]
    (values (cons (plot-renderer-real-filter xmin)
                  (plot-renderer-real-filter xmax))
            (cons (plot-renderer-real-filter ymin)
                  (plot-renderer-real-filter ymax)))))
