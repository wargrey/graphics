#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/base)
(require geofun/digitama/convert)

(require geofun/digitama/layer/sticker)

(require "vaid/self.rkt")
(require "../marker/self.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/symbol))
(require (for-syntax racket/sequence))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Visualizer-Tree (Listof (U Plot-Visualizer False Void Plot-Visualizer-Tree)))

(define-type Plot-Visualizer-Tick-Range (Pairof (Option Real) (Option Real)))
(define-type Plot-Visualizer-Data-Range (-> Real Real (Pairof Real Real)))
(define-type Plot-Visualizer-Realize (-> Index Positive-Index Real Real Real Real
                                         (case-> [Flonum Flonum -> Float-Complex]
                                                 [Float-Complex -> Float-Complex])
                                         (Option FlRGBA)
                                         (Option Geo-Visualizer)))

(define-type Plot-Visualizer-View-Range (U (Pairof Real Real) (-> (Pairof Real Real) (Pairof Real Real))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-visualizer stx)
  (syntax-parse stx #:literals [:]
    [(_ id : ID #:for GID ([field : FieldType defval ...] ...))
     (with-syntax* ([ID<%> (format-id #'ID "~a<%>" (syntax-e #'ID))]
                    [id<%> (format-id #'id "~a<%>" (syntax-e #'id))]
                    [id-abs (format-id #'id "~a-interface" (syntax-e #'id))]
                    [desc-id (format-id #'id "desc-~a" (syntax-e #'id))]
                    [(kw-args ...)
                     (for/fold ([args null])
                               ([<field> (in-syntax #'(field ...))]
                                [<Argument> (in-syntax #'([field : FieldType defval ...] ...))])
                       (cons (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>))))
                             (cons <Argument> args)))]
                    [([id-field ...] [id<%>-field ...])
                     (for/fold ([id-fields null] [abs-fields null] #:result (list id-fields abs-fields))
                               ([<field> (in-list (reverse (syntax->list #'(field ...))))])
                       (define <id-field> (datum->syntax <field> (string->symbol (format "~a-~a" (syntax-e #'id) (syntax-e <field>)))))
                       (define <abs-field> (datum->syntax <field> (string->symbol (format "~a-~a" (syntax-e #'id<%>) (syntax-e <field>)))))
                       (values (cons <id-field> id-fields)
                               (cons <abs-field> abs-fields)))])
       (syntax/loc stx
         (begin (define-type GID (U ID (Pairof Geo-Sticker ID<%>)))

                (struct id<%> ([field : FieldType] ...) #:transparent #:type-name ID<%>)
                (struct id geo ([interface : ID<%>]) #:type-name ID)

                (define (desc-id kw-args ...) : ID<%>
                  (id<%> field ...))

                (define (id-field [self : GID]) : FieldType
                  (id<%>-field (if (pair? self) (cdr self) (id-abs self))))
                ...)))]))

(define-syntax (create-visualizer stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo
        (~seq #:with [name draw!:expr extent:expr insets:expr
                           desc ...])
        argl ...)
     (syntax/loc stx
       (create-geometry-object Geo #:with [name draw! extent insets]
                               (desc-geo:visualizer desc ...)
                               argl ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot-visualizer
  ([realize : Plot-Visualizer-Realize]
   [xrng : Plot-Visualizer-Tick-Range]
   [yrng : Plot-Visualizer-Tick-Range]
   [λrange : Plot-Visualizer-Data-Range]
   [skip-palette? : Boolean])
  #:type-name Plot-Visualizer
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-visualizer geo:visualizer : Geo:Visualizer #:for Geo-Visualizer
  ([realize : (Option (-> Geo-Visualizer)) #false]
   [position : Float-Complex]
   [color : FlRGBA]
   [label : (Option Plot:Mark) #false]
   [legend : (Option Geo) #false]
   [pin-angle : (Option (-> Real (Option Real))) #false]
   [gap-angle : (Option (-> Real (Option Real))) #false]
   [visual-aids : (Listof Plot-Visual-Aid) null]))

(struct geo:line:visualizer geo:visualizer
  ([dots : (Listof Float-Complex)])
  #:type-name Geo:Line:Visualizer
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
              (cond [(plot-visualizer? self)
                     (let*-values ([(sxrng syrng) (values (plot-visualizer-xrng self) (plot-visualizer-yrng self))]
                                   [(sxmin sxmax) (values (car sxrng) (cdr sxrng))]
                                   [(symin symax) (values (car syrng) (cdr syrng))])
                       (flatten rest
                                (if (rational? sxmin) (min xmin sxmin) xmin)
                                (if (rational? sxmax) (max xmax sxmax) xmax)
                                (if (rational? symin) (min ymin symin) ymin)
                                (if (rational? symax) (max ymax symax) ymax)
                                (cons self sreredner)))]
                    [(list? self)
                     (let-values ([(sbus xsmin xsmax ysmin ysmax) (flatten self xmin xmax ymin ymax sreredner)])
                       (flatten rest xsmin xsmax ysmin ysmax sbus))]
                    [else (flatten rest xmin xmax ymin ymax sreredner)]))
            (values sreredner xmin xmax ymin ymax))))
    
    (values (reverse sreredner)
            (plot-range-normalize xmin xmax)
            (plot-range-normalize ymin ymax))))

(define plot-visualizer-ranges : (-> (Listof Plot-Visualizer)
                                     (Option Plot-Visualizer-View-Range) (Option Plot-Visualizer-View-Range)
                                     Plot-Visualizer-Tick-Range Plot-Visualizer-Tick-Range (Pairof Real Real)
                                     (Values (Pairof Real Real) (Pairof Real Real)))
  (lambda [selves xtick-rng ytick-rng maybe-xrng maybe-yrng fallback-dom]
    (cond [(and (pair? xtick-rng) (pair? ytick-rng)) (values xtick-rng ytick-rng)]
          [else (let*-values ([(xmin xmax) (if (pair? xtick-rng) (plot-range-values xtick-rng) (plot-range-values maybe-xrng))]
                              [(ymin ymax) (if (pair? ytick-rng) (plot-range-values ytick-rng) (plot-range-values maybe-yrng))]
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
                      (values ((if (procedure? xtick-rng) xtick-rng values) (cons left rght))
                              ((if (procedure? ytick-rng) ytick-rng values) (cons ymin ymax)))))])))

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

(define plot-range-select : (-> Plot-Visualizer-Tick-Range (Pairof Real Real) (Values Real Real))
  (lambda [rng view]
    (values (if (car rng) (max (car view) (car rng)) (car view))
            (if (cdr rng) (min (cdr view) (cdr rng)) (cdr view)))))
