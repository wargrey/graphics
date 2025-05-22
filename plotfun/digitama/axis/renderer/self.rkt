#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)
(require geofun/digitama/convert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Renderer-Tree (Listof (U Plot:Renderer Plot-Renderer-Tree)))

(define-type Plot-Renderer-Tick-Range (Pairof (Option Real) (Option Real)))
(define-type Plot-Renderer-Data-Range (-> Real Real (Pairof Real Real)))
(define-type Plot-Renderer-Realize (-> (Pairof Real Real) (Pairof Real Real) (-> Flonum Flonum Float-Complex) (Values Geo Float-Complex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:renderer
  ([realize : Plot-Renderer-Realize]
   [label : Any]
   [xrng : Plot-Renderer-Tick-Range]
   [yrng : Plot-Renderer-Tick-Range]
   [color : FlRGBA]
   [range : Plot-Renderer-Data-Range])
  #:type-name Plot:Renderer
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-renderer-tree-flatten : (-> Plot-Renderer-Tree (Values (Listof Plot:Renderer) Plot-Renderer-Tick-Range Plot-Renderer-Tick-Range))
  (lambda [tree]
    (define-values (sreredner xmin xmax ymin ymax)
      (let flatten : (Values (Listof Plot:Renderer) Real Real Real Real)
        ([ts : Plot-Renderer-Tree tree]
         [xmin : Real +inf.0] [xmax : Real -inf.0]
         [ymin : Real +inf.0] [ymax : Real -inf.0]
         [sreredner : (Listof Plot:Renderer) null])
        (if (pair? ts)
            (let-values ([(self rest) (values (car ts) (cdr ts))])
              (if (list? self)
                  (let-values ([(sbus xsmin xsmax ysmin ysmax) (flatten self xmin xmax ymin ymax sreredner)])
                    (flatten rest xsmin xsmax ysmin ysmax sbus))
                  (let ([xrng (plot:renderer-xrng self)]
                        [yrng (plot:renderer-yrng self)])
                    (flatten rest
                             (min xmin (or (car xrng) +inf.0)) (max xmax (or (cdr xrng) -inf.0))
                             (min ymin (or (car yrng) +inf.0)) (max ymax (or (cdr yrng) -inf.0))
                             (cons self sreredner)))))
            (values sreredner xmin xmax ymin ymax))))
    
    (values (reverse sreredner)
            (plot-range-normalize xmin xmax)
            (plot-range-normalize ymin ymax))))

(define plot-renderer-ranges : (-> (Listof Plot:Renderer) (Option (Pairof Real Real)) (Option (Pairof Real Real))
                                   Plot-Renderer-Tick-Range Plot-Renderer-Tick-Range (Pairof Real Real)
                                   (Values (Pairof Real Real) (Pairof Real Real)))
  (lambda [selves xtick-rng ytick-rng maybe-xrng maybe-yrng fallback-dom]
    (if (not (and xtick-rng ytick-rng))
        (let*-values ([(xmin xmax) (if (or xtick-rng) (plot-range-values xtick-rng) (plot-range-values maybe-xrng))]
                      [(ymin ymax) (if (or ytick-rng) (plot-range-values ytick-rng) (plot-range-values maybe-yrng))]
                      [(left rght) (values (or xmin (car fallback-dom)) (or xmax (cdr fallback-dom)))])
          (if (not (and ymin ymax))
              (let bounds ([top : Real (or ymin +inf.0)]
                           [btm : Real (or ymax -inf.0)]
                           [rs : (Listof Plot:Renderer) selves])
                (cond [(null? rs) (values (cons left rght) (cons top btm))]
                      [else (let* ([vrng ((plot:renderer-range (car rs)) left rght)])
                              (bounds (min top (car vrng)) (max btm (cdr vrng)) (cdr rs)))]))
              (values (cons left rght) (cons ymin ymax))))
        (values xtick-rng ytick-rng))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-renderer-real-filter : (-> (Option Real) (Option Real))
  (lambda [v]
    (and (rational? v)
         v)))

(define plot-range-normalize : (case-> [(Option Real) (Option Real) -> Plot-Renderer-Tick-Range]
                                       [(Option Real) (Option Real) (Option Real) (Option Real) -> (Values Plot-Renderer-Tick-Range Plot-Renderer-Tick-Range)])
  (case-lambda
    [(rmin rmax)
     (cons (plot-renderer-real-filter rmin)
           (plot-renderer-real-filter rmax))]
    [(xmin xmax ymin ymax)
     (values (plot-range-normalize xmin xmax)
             (plot-range-normalize ymin ymax))]))

(define plot-range-values : (case-> [(Pairof Real Real) -> (Values Real Real)]
                                    [Plot-Renderer-Tick-Range -> (Values (Option Real) (Option Real))])
  (lambda [rng]
    (values (car rng) (cdr rng))))
