#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)
(require racket/list)

(require "digicore.rkt")
(require "draw.rkt")
(require "misc.rkt")

(require "unsafe/draw.rkt")

(require (for-syntax racket/base))

(define-syntax (define-combiner stx)
  (syntax-case stx []
    [(_ [make frmt (tips ...)] ...)
     (with-syntax ([([(bitmap-combiner make-combiner) ...] ...)
                    (for/list ([<tips> (in-list (syntax->list #'([tips ...] ...)))]
                               [<mkcb> (in-list (syntax->list #'(make ...)))]
                               [<frmt> (in-list (syntax->list #'(frmt ...)))])
                      (define frmt (syntax-e <frmt>))
                      (for/list ([<tip> (in-list (syntax->list <tips>))])
                        (list (datum->syntax <tip> (string->symbol (format frmt (syntax-e <tip>))))
                              <mkcb>)))])
       #'(begin (define-values (bitmap-combiner ...) (values (make-combiner 'tips) ...))
                ...))]))

(define-enumeration* bitmap-blend-mode #:+> Bitmap-Blend-Mode ; order matters
  bitmap-operator->integer integer->bitmap-operator
  [0 clear source over in out atop dest dest-over dest-in dest-out dest-atop xor add saturate
     multiply screen overlay darken lighten color-dodge color-burn hard-light soft-light difference exclusion
     hsl-hue hsl-saturation hsl-color hsl-liminosity])

(define-type Bitmap-Layer->XY (-> Flonum Flonum (Values Flonum Flonum)))
(define-type Bitmap-Layers (Listof (Pairof Bitmap Bitmap-Layer->XY)))
(define-type Superimpose-Alignment (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))

(define superimpose : (-> Symbol (Listof Bitmap) (Values Flonum Flonum Bitmap-Layers))
  (lambda [alignment bitmaps]
    (for/fold ([width : Flonum 0.0] [height : Flonum 0.0] [layers : (Listof (Pairof Bitmap Bitmap-Layer->XY)) null])
              ([bmp : Bitmap (in-list bitmaps)])
      (define-values (w h) (bitmap-flsize bmp))
      (values (flmax width w) (flmax height h) (cons (cons bmp (make-layer alignment w h)) layers)))))

(define make-layer : (-> Symbol Flonum Flonum Bitmap-Layer->XY)
  (lambda [alignment w h]
    (λ [[W : Flonum] [H : Flonum]] (superimpose-xy alignment W H w h))))

(define superimpose-xy : (-> Symbol Flonum Flonum Flonum Flonum (Values Flonum Flonum))
  (lambda [alignment width height w h]
    (define-values (rx by) (values (fl- width w) (fl- height h)))
    (define-values (cx cy) (values (fl/ rx 2.0) (fl/ by 2.0)))
    (case alignment
      [(lt) (values 0.0 0.0)] [(lc) (values 0.0 cy)] [(lb) (values 0.0 by)]
      [(ct) (values  cx 0.0)] [(cc) (values  cx cy)] [(cb) (values  cx by)]
      [(rt) (values  rx 0.0)] [(rc) (values  rx cy)] [(rb) (values  rx by)]
      [else #|unreachable|# (values 0.0 0.0)])))

(define-type Bitmap-Cell (List Bitmap Flonum Flonum))
(define-type Bitmap-Tables (Vectorof Bitmap-Cell))
(define-type Pseudo-Bitmap* (List Flonum Flonum (Listof (List Bitmap-Cell Flonum Flonum))))

(define superimpose* : (-> Symbol (Listof Bitmap-Cell) Pseudo-Bitmap*)
  (lambda [alignment cells]
    (define-values (width height)
      (for/fold ([width : Flonum 0.0] [height : Flonum 0.0])
                ([cell : Bitmap-Cell (in-list cells)])
        (values (flmax width (cadr cell)) (flmax height (caddr cell)))))
    (list width height
          (for/list ([cell (in-list cells)])
            (define-values (w h) (values (cadr cell) (caddr cell)))
            (define-values (x y) (superimpose-xy alignment width height w h))
            (list cell x y)))))

(define find-xy : (All (a) (-> a Pseudo-Bitmap* (Values Flonum Flonum)))
  (lambda [bmp pbmps]
    (let find ([rest (caddr pbmps)])
      (cond [(null? rest) (values 0.0 0.0)]
            [(eq? bmp (caar rest)) (values (cadar rest) (caddar rest))]
            [else (find (cdr rest))]))))

(define list->table : (-> (Listof Bitmap) Natural Natural Bitmap-Tables)
  (lambda [bitmaps nrows ncols]
    (define cells : Bitmap-Tables
      (for/vector : Bitmap-Tables ([bmp (in-list bitmaps)])
        (define-values (w h) (bitmap-flsize bmp))
        (list bmp w h)))
    (define diff : Integer (fx- (fx* nrows ncols) (vector-length cells)))
    (cond [(fx<= diff 0) cells]
          [else (let ([filling : Bitmap-Cell (list the-invalid-image 1.0 1.0)])
                  (vector-append cells ((inst make-vector Bitmap-Cell) diff filling)))])))

(define list->n:vector : (All (a) (-> (Listof a) Integer a (Vectorof a)))
  (lambda [src total defval] ; NOTE: (length src) is usually much smaller then the demand. 
    (define diff : Integer (fx- total (length src)))
    (cond [(= diff total) (make-vector total defval)]
          [(> diff 0) (list->vector (append src (make-list diff (last src))))]
          [else (list->vector src)])))
