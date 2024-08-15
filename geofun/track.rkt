#lang typed/racket/base

(provide (all-defined-out))
(provide Track Dryland-Wani)
(provide track? dryland-wani?)
(provide track-close)

(provide
 (rename-out [dryland-wani-step-up-right! dryland-wani-step-right-up!]
             [dryland-wani-step-right-down! dryland-wani-step-down-right!]
             [dryland-wani-step-down-left! dryland-wani-step-left-down!]
             [dryland-wani-step-left-up! dryland-wani-step-up-left!])

 (rename-out [dryland-wani-jump-up-right! dryland-wani-jump-right-up!]
             [dryland-wani-jump-right-down! dryland-wani-jump-down-right!]
             [dryland-wani-jump-down-left! dryland-wani-jump-left-down!]
             [dryland-wani-jump-left-up! dryland-wani-jump-up-left!])

 (rename-out [track-close dryland-wani-close!]))

(require racket/keyword)
(require racket/symbol)

(require "digitama/bbox.rkt")
(require "digitama/anchor.rkt")
(require "digitama/convert.rkt")
(require "digitama/dot.rkt")
(require "digitama/composite.rkt")

(require "digitama/dc/text.rkt")
(require "digitama/dc/track.rkt")
(require "digitama/dc/composite.rkt")

(require "digitama/layer/type.rkt")
(require "digitama/layer/position.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-dryland-wani! stx)
  (syntax-case stx []
    [(_ name [args ...] #:- move-expr ...)
     (syntax/loc stx
       (define name : Dryland-Wani
         (with-dryland-wani! (make-dryland-wani args ...)
           move-expr ...)))]))

(define-syntax (with-dryland-wani! stx)
  (syntax-case stx []
    [(_ wani (move argl ...) ...)
     (with-syntax* ([(dryland-wani-move ...)
                     (for/list ([<move> (in-list (syntax->list #'(move ...)))])
                       (format-id <move> "dryland-wani-~a!" (syntax->datum <move>)))])
       (syntax/loc stx
         (let ([self wani])
           (dryland-wani-move self argl ...)
           ...
           self)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dryland-wani : (->* (Real)
                                 (Real #:turn-scale Track-Print-Datum #:U-scale Track-Print-Datum
                                       #:anchor Keyword #:at Track-Print-Datum #:id (Option Symbol))
                                 Dryland-Wani)
  (lambda [#:turn-scale [t-scale +nan.0] #:U-scale [u-scale +nan.0]
           #:anchor [anchor '#:home] #:at [home 0] #:id [name #false]
           xstepsize [ystepsize 0.0]]
    (define xstep : Nonnegative-Flonum
      (cond [(<= xstepsize 0.0) 1.0]
            [else (max (real->double-flonum xstepsize) 0.0)]))
    
    (define ystep : Nonnegative-Flonum
      (cond [(= ystepsize 0.0) xstep]
            [(< ystepsize 0.0) (* xstep (- (real->double-flonum ystepsize)))]
            [else (max (real->double-flonum ystepsize) 0.0)]))
    
    (define home-pos : Float-Complex (~point2d home))
    (define-values (tsx tsy) (track-turn-scales t-scale 0.5))
    (define-values (usx usy) (track-turn-scales u-scale 0.25))
    
    (create-geometry-object dryland-wani #:with [track-surface (geo-path-bbox-wrapper track-bounding-box)] #:id name
                            (list (cons start-of-track home-pos)) (make-geo-path home-pos anchor)
                            (make-geo-bbox home-pos) home-pos home-pos
                            xstep ystep (* tsx xstep) (* tsy ystep) (* usx xstep) (* usy ystep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani-line-move! left            #:-> -1.0)
(define-dryland-wani-line-move! right           #:->  1.0)
(define-dryland-wani-line-move! up              #:!> -1.0)
(define-dryland-wani-line-move! down            #:!>  1.0)
(define-dryland-wani-line-move! up-right        #:+>  1.0-1.0i)
(define-dryland-wani-line-move! right-down      #:+>  1.0+1.0i)
(define-dryland-wani-line-move! down-left       #:+> -1.0+1.0i)
(define-dryland-wani-line-move! left-up         #:+> -1.0-1.0i)

(define-dryland-wani-turn-move! up-right-down   #:+> [180.0 360.0  1.0  0.0  2.0  0.0] #:boundary-guard -1.0i)
(define-dryland-wani-turn-move! up-left-down    #:-> [360.0 180.0 -1.0  0.0 -2.0  0.0] #:boundary-guard -1.0i)
(define-dryland-wani-turn-move! right-down-left #:+> [-90.0  90.0  0.0  1.0  0.0  2.0] #:boundary-guard 1.0)
(define-dryland-wani-turn-move! right-up-left   #:-> [ 90.0 -90.0  0.0 -1.0  0.0 -2.0] #:boundary-guard 1.0)
(define-dryland-wani-turn-move! down-left-up    #:+> [  0.0 180.0 -1.0  0.0 -2.0  0.0] #:boundary-guard +1.0i)
(define-dryland-wani-turn-move! down-right-up   #:-> [180.0   0.0  1.0  0.0  2.0  0.0] #:boundary-guard +1.0i)
(define-dryland-wani-turn-move! left-up-right   #:+> [ 90.0 270.0  0.0 -1.0  0.0 -2.0] #:boundary-guard -1.0)
(define-dryland-wani-turn-move! left-down-right #:-> [270.0  90.0  0.0  1.0  0.0  2.0] #:boundary-guard -1.0)

(define-dryland-wani-turn-move! up right
  #:+> [180.0 270.0 1.0  0.0 1.0 -1.0]
  #:-> [ 90.0   0.0 0.0 -1.0 1.0 -1.0])

(define-dryland-wani-turn-move! right down
  #:+> [270.0 360.0 0.0 1.0 1.0 1.0]
  #:-> [180.0  90.0 1.0 0.0 1.0 1.0])

(define-dryland-wani-turn-move! down left
  #:+> [  0.0  90.0 -1.0 0.0 -1.0 1.0]
  #:-> [270.0 180.0  0.0 1.0 -1.0 1.0])

(define-dryland-wani-turn-move! left up
  #:+> [ 90.0 180.0  0.0 -1.0 -1.0 -1.0]
  #:-> [360.0 270.0 -1.0  0.0 -1.0 -1.0])

(define dryland-wani-drift! : (->* (Dryland-Wani Track-Bezier-Datum (Listof Track-Bezier-Datum)) ((Option Geo-Path-Anchor-Name)) Void)
  (lambda [wani end-step ctrl-steps [anchor #false]]
    (define xsize : Flonum (dryland-wani-xstepsize wani))
    (define ysize : Flonum (dryland-wani-ystepsize wani))
    (define endpt : Float-Complex (track-bezier-point wani end-step xsize ysize))
    (define ctrls : (Listof Float-Complex)
      (for/list : (Listof Float-Complex) ([ctrl (in-list ctrl-steps)])
        (track-bezier-point wani ctrl xsize ysize)))

    (cond [(null? ctrls) (track-linear-bezier wani endpt anchor)]
          [(null? (cdr ctrls)) (track-quadratic-bezier wani endpt (car ctrls) anchor)]
          [else (track-cubic-bezier wani endpt (car ctrls) (cadr ctrls) anchor)])))

(define dryland-wani-step-to! : (-> Dryland-Wani Geo-Path-Anchor-Name Void)
  (lambda [wani target]
    (track-connect-to wani target)))

(define dryland-wani-jump-back! : (->* (Dryland-Wani) ((Option Geo-Path-Anchor-Name)) Void)
  (lambda [wani [target #false]]
    (track-jump-to wani target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Track-Sticker (U Geo<%> (Pairof Geo<%> (U Geo-Pin-Port (Pairof Geo-Pin-Port Float-Complex)))))
(define-type Track-Anchor->Sticker (-> Track Geo-Path-Anchor-Name Float-Complex Nonnegative-Flonum Nonnegative-Flonum
                                       (U Track-Sticker (Listof Track-Sticker))))

(define default-track-anchor->sticker : Track-Anchor->Sticker
  (lambda [self anchor pos Width Height]
    (if (symbol? anchor)
        (geo-text (symbol->immutable-string anchor) #:color 'RoyalBlue)
        (geo-text (keyword->immutable-string anchor) #:color 'Gray))))

(define track-stick : (->* (Track)
                           (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator) #:trusted-anchors (Option (Listof Geo-Path-Anchor-Name))
                            #:truncate? Boolean Track-Anchor->Sticker)
                           (U Geo:Group Track))
  (lambda [#:trusted-anchors [trusted-anchors #false] #:id [id #false] #:operator [op #false] #:truncate? [truncate? #true]
           self [anchor->sticker default-track-anchor->sticker]]
    (define gpath : Geo-Path (track-path self))
    (define srohcna : (Listof Geo-Path-Anchor-Name) (geo-path-ranchors gpath))
    (define origin : Float-Complex (geo-bbox-position (track-bbox self)))
    (define-values (Width Height) (geo-flsize self))

    (let stick ([srohcna : (Listof Geo-Path-Anchor-Name) srohcna]
                [stickers : (Listof (GLayerof Geo<%>)) null])
      (cond [(pair? srohcna)
             (let-values ([(anchor rest) (values (car srohcna) (cdr srohcna))])
               (if (and trusted-anchors (not (memq anchor trusted-anchors)))
                   (stick rest stickers)

                   (let* ([pos (- (geo-path-ref gpath anchor) origin)]
                          [ones (anchor->sticker self anchor pos Width Height)])
                     (stick rest (for/fold ([stickers : (Listof (GLayerof Geo<%>)) stickers])
                                           ([one (if (list? ones) (in-list ones) (in-value ones))])
                                   (define-values (sticker modifier) (if (pair? one) (values (car one) (cdr one)) (values one 'cc)))
                                   (define-values (width height) (geo-flsize sticker))
                                   (define layer : (GLayerof Geo<%>)
                                     (if (pair? modifier)
                                         (geo-stick-layer (car modifier) pos sticker width height (cdr modifier))
                                         (geo-stick-layer modifier pos sticker width height 0.0+0.0i)))
                                   (append stickers (list layer)))))))]
            [(pair? stickers)
             (let-values ([(self-layer) (vector-immutable self 0.0 0.0 Width Height)])
               (or (and (not truncate?)
                        (let-values ([(lx ty rx by) (geo-group-boundary stickers Width Height)])
                          (and (or (<= lx 0.0) (<= ty 0.0))
                               (let ([xoff (if (< lx 0.0) (abs lx) 0.0)]
                                     [yoff (if (< ty 0.0) (abs ty) 0.0)])
                                 (make-geo:group id op
                                                 (vector-immutable (+ rx xoff) (+ by yoff)
                                                                   (cons (geo-layer-translate self-layer xoff yoff)
                                                                         (for/list : (Listof (GLayerof Geo<%>)) ([sticker (in-list stickers)])
                                                                           (geo-layer-translate sticker xoff yoff)))))))))
                   (make-geo:group id op (vector-immutable Width Height (cons self-layer stickers)))))]
            [else self]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-anchor-position : (->* (Track Geo-Path-Anchor-Name) (#:translate? Boolean) Float-Complex)
  (lambda [self anchor #:translate? [translate? #false]]
    (define abspos : Float-Complex (geo-path-ref (track-path self) anchor))
    
    (cond [(not translate?) abspos]
          [else (- abspos (geo-bbox-position (track-bbox self)))])))

