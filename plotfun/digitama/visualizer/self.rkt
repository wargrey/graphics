#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)
(require geofun/digitama/convert)
(require geofun/digitama/markup)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Visualizer-Tree (Listof (U Plot-Visualizer Plot-Visualizer-Tree)))

(define-type Plot-Visualizer-Tick-Range (Pairof (Option Real) (Option Real)))
(define-type Plot-Visualizer-Data-Range (-> Real Real (Pairof Real Real)))
(define-type Plot-Visualizer-Realize (-> Index (Pairof Real Real) Real Real (-> Flonum Flonum Float-Complex) (Option FlRGBA) Geo:Visualizer))

(define-syntax (plot-realize stx)
  (syntax-case stx []
    [(_ self idx xview yview args ...)
     (syntax/loc stx
       (let-values ([(xrng) (plot-range-select (plot-visualizer-xrng self) xview)]
                    [(ymin ymax) (plot-range-values (plot-range-select (plot-visualizer-yrng self) yview))])
         ((plot-visualizer-realize self) idx xrng ymin ymax args ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot-visualizer
  ([realize : Plot-Visualizer-Realize]
   [xrng : Plot-Visualizer-Tick-Range]
   [yrng : Plot-Visualizer-Tick-Range]
   [λrange : Plot-Visualizer-Data-Range]
   [skip-palette? : Boolean])
  #:type-name Plot-Visualizer
  #:transparent)

(struct geo:visualizer geo
  ([position : Float-Complex]
   [legend : (Option (Pairof Geo DC-Markup-Text))])
  #:type-name Geo:Visualizer
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-visualizer-tree-flatten : (-> Plot-Visualizer-Tree (Values (Listof Plot-Visualizer) Plot-Visualizer-Tick-Range Plot-Visualizer-Tick-Range))
  (lambda [tree]
    (define-values (sreredner xmin xmax ymin ymax)
      (let flatten : (Values (Listof Plot-Visualizer) Real Real Real Real)
        ([ts : Plot-Visualizer-Tree tree]
         [xmin : Real +inf.0] [xmax : Real -inf.0]
         [ymin : Real +inf.0] [ymax : Real -inf.0]
         [sreredner : (Listof Plot-Visualizer) null])
        (if (pair? ts)
            (let-values ([(self rest) (values (car ts) (cdr ts))])
              (if (list? self)
                  (let-values ([(sbus xsmin xsmax ysmin ysmax) (flatten self xmin xmax ymin ymax sreredner)])
                    (flatten rest xsmin xsmax ysmin ysmax sbus))
                  (let*-values ([(sxrng syrng) (values (plot-visualizer-xrng self) (plot-visualizer-yrng self))]
                                [(sxmin sxmax) (values (car sxrng) (cdr sxrng))]
                                [(symin symax) (values (car syrng) (cdr syrng))])
                    (flatten rest
                             (if (rational? sxmin) (min xmin sxmin) xmin) (if (rational? sxmax) (max xmax sxmax) xmax)
                             (if (rational? symin) (min ymin symin) ymin) (if (rational? symax) (max ymax symax) ymax)
                             (cons self sreredner)))))
            (values sreredner xmin xmax ymin ymax))))
    
    (values (reverse sreredner)
            (plot-range-normalize xmin xmax)
            (plot-range-normalize ymin ymax))))

(define plot-visualizer-ranges : (-> (Listof Plot-Visualizer) (Option (Pairof Real Real)) (Option (Pairof Real Real))
                                     Plot-Visualizer-Tick-Range Plot-Visualizer-Tick-Range (Pairof Real Real)
                                     (Values (Pairof Real Real) (Pairof Real Real)))
  (lambda [selves xtick-rng ytick-rng maybe-xrng maybe-yrng fallback-dom]
    (if (not (and xtick-rng ytick-rng))
        (let*-values ([(xmin xmax) (if (or xtick-rng) (plot-range-values xtick-rng) (plot-range-values maybe-xrng))]
                      [(ymin ymax) (if (or ytick-rng) (plot-range-values ytick-rng) (plot-range-values maybe-yrng))]
                      [(left rght) (values (or xmin (car fallback-dom)) (or xmax (cdr fallback-dom)))])
          (if (not (and ymin ymax))
              (let bounds ([top : Real (or ymin +inf.0)]
                           [btm : Real (or ymax -inf.0)]
                           [rs : (Listof Plot-Visualizer) selves])
                (cond [(null? rs) (values (cons left rght) (cons top btm))]
                      [else (let* ([self (car rs)]
                                   [xrng (plot-visualizer-xrng self)]
                                   [yrng ((plot-visualizer-λrange self) (or (car xrng) left) (or (cdr xrng) rght))]
                                   [ymin (car yrng)]
                                   [ymax (cdr yrng)])
                              (bounds (if (rational? ymin) (min top ymin) top)
                                      (if (rational? ymax) (max btm ymax) btm)
                                      (cdr rs)))]))
              (values (cons left rght) (cons ymin ymax))))
        (values xtick-rng ytick-rng))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-visualizer-real-filter : (-> (Option Real) (Option Real))
  (lambda [v]
    (and (rational? v)
         v)))

(define plot-range-normalize : (case-> [(Option Real) (Option Real) -> Plot-Visualizer-Tick-Range]
                                       [(Option Real) (Option Real) (Option Real) (Option Real) -> (Values Plot-Visualizer-Tick-Range Plot-Visualizer-Tick-Range)])
  (case-lambda
    [(rmin rmax)
     (cons (plot-visualizer-real-filter rmin)
           (plot-visualizer-real-filter rmax))]
    [(xmin xmax ymin ymax)
     (values (plot-range-normalize xmin xmax)
             (plot-range-normalize ymin ymax))]))

(define plot-range-values : (case-> [(Pairof Real Real) -> (Values Real Real)]
                                    [Plot-Visualizer-Tick-Range -> (Values (Option Real) (Option Real))])
  (lambda [rng]
    (values (car rng) (cdr rng))))

(define plot-range-select : (-> Plot-Visualizer-Tick-Range (Pairof Real Real) (Pairof Real Real))
  (lambda [rng view]
    (cons (if (car rng) (max (car view) (car rng)) (car view))
          (if (cdr rng) (min (cdr view) (cdr rng)) (cdr view)))))
