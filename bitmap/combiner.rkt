#lang typed/racket/base

(provide (except-out (all-defined-out) make-pin make-append make-superimpose))

(require "digitama/bitmap.rkt")
(require "constructor.rkt")
(require "resize.rkt")
(require "misc.rkt")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-pin : (-> Symbol (->* (Bitmap Real Real Bitmap) (Real Real) Bitmap))
  (lambda [order]
    (λ [bmp1 x1 y1 bmp2 [x2 0] [y2 0]]
      (define dx : Integer (exact-round (- x1 x2)))
      (define dy : Integer (exact-round (- y1 y2)))
      (define dx1 : Natural (fxmax 0 (fx- 0 dx)))
      (define dy1 : Natural (fxmax 0 (fx- 0 dy)))
      (define dx2 : Natural (fxmax 0 dx))
      (define dy2 : Natural (fxmax 0 dy))
      (define dc : (Instance Bitmap-DC%)
        (make-object bitmap-dc%
          (bitmap-blank (fxmax (fx+ dx1 (send bmp1 get-width)) (fx+ dx2 (send bmp2 get-width)))
                        (fxmax (fx+ dy1 (send bmp1 get-height)) (fx+ dy2 (send bmp2 get-height))))))
      (send dc set-smoothing 'aligned)
      (case order
        [(over)  (void (send dc draw-bitmap bmp1 dx1 dy1)
                       (send dc draw-bitmap bmp2 dx2 dy2))]
        [(under) (void (send dc draw-bitmap bmp2 dx2 dy2)
                       (send dc draw-bitmap bmp1 dx1 dy1))])
      (or (send dc get-bitmap) (bitmap-blank)))))

(define make-append : (-> Symbol (-> [#:gapsize Real] Bitmap * Bitmap))
  (lambda [alignment]
    (λ [#:gapsize [delta 0.0] . bitmaps]
      (cond [(null? bitmaps) (bitmap-blank)]
            [(null? (cdr bitmaps)) (car bitmaps)]
            [else (let*-values ([(base others) (values (car bitmaps) (cdr bitmaps))]
                                [(min-width min-height) (bitmap-size base)])
                    (define-values (width0 height0)
                      (for/fold ([width : Real (send base get-width)]
                                 [height : Real (send base get-height)])
                                ([child : Bitmap (in-list others)])
                        (define w : Positive-Integer (send child get-width))
                        (define h : Positive-Integer (send child get-height))
                        (case alignment
                          [(vl vc vr) (values (max width w) (+ height h delta))]
                          [(ht hc hb) (values (+ width w delta) (max height h))]
                          [else #|unreachable|# (values (max width w) (max height h))])))
                    
                    (define width : Positive-Real (max min-width width0))
                    (define height : Positive-Real (max min-height height0))
                    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
                    (send dc set-smoothing 'aligned)
                    
                    (let append-bitmap : Void ([bmps : (Listof Bitmap) bitmaps]
                                               [xoff : Real (- delta)]
                                               [yoff : Real (- delta)])
                      (unless (null? bmps)
                        (define bmp : Bitmap (car bmps))
                        (define-values (w h) (bitmap-size bmp))
                        (define this-x-if-use : Real (+ xoff delta))
                        (define this-y-if-use : Real (+ yoff delta))
                        (define-values (x y)
                          (case alignment
                            [(vl) (values 0                 this-y-if-use)]
                            [(vc) (values (/ (- width w) 2) this-y-if-use)]
                            [(vr) (values (- width w)       this-y-if-use)]
                            [(ht) (values this-x-if-use     0)]
                            [(hc) (values this-x-if-use     (/ (- height h) 2))]
                            [(hb) (values this-x-if-use     (- height h))]
                            [else #|unreachable|# (values this-x-if-use this-y-if-use)]))
                        (send dc draw-bitmap bmp x y)
                        (append-bitmap (cdr bmps) (+ this-x-if-use w) (+ this-y-if-use h))))
                    (or (send dc get-bitmap) (bitmap-blank)))]))))

(define make-superimpose : (-> Symbol (-> Bitmap * Bitmap))
  (lambda [alignment]
    (λ bitmaps
      (cond [(null? bitmaps) (bitmap-blank)]
            [(null? (cdr bitmaps)) (car bitmaps)]
            [(null? (cddr bitmaps))
             (let-values ([(base bmp) (values (car bitmaps) (cadr bitmaps))])
               (case alignment
                 [(lt) (bitmap-pin  0   0   0   0  base bmp)]
                 [(lc) (bitmap-pin  0  1/2  0  1/2 base bmp)]
                 [(lb) (bitmap-pin  0   1   0   1  base bmp)]
                 [(ct) (bitmap-pin 1/2  0  1/2  0  base bmp)]
                 [(cc) (bitmap-pin 1/2 1/2 1/2 1/2 base bmp)]
                 [(cb) (bitmap-pin 1/2  1  1/2  1  base bmp)]
                 [(rt) (bitmap-pin  1   0   1   0  base bmp)]
                 [(rc) (bitmap-pin  1  1/2  1  1/2 base bmp)]
                 [(rb) (bitmap-pin  1   1   1   1  base bmp)]
                 [else base]))]
            [else (let ([base (car bitmaps)])
                    (define-values (width height)
                      (for/fold ([width : Positive-Integer (send base get-width)]
                                 [height : Positive-Integer (send base get-height)])
                                ([bmp : Bitmap (in-list (cdr bitmaps))])
                        (values (max width (send bmp get-width)) (max height (send bmp get-height)))))
                    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank width height)))
                    (send dc set-smoothing 'aligned)
                
                    (for ([bmp : Bitmap (in-list bitmaps)])
                      (define w : Positive-Integer (send bmp get-width))
                      (define h : Positive-Integer (send bmp get-height))
                      (define-values (rx by) (values (- width w) (- height h)))
                      (define-values (cx cy) (values (/ rx 2) (/ by 2)))
                      (define-values (x y)
                        (case alignment
                          [(lt) (values  0 0)] [(lc) (values  0 cy)] [(lb) (values  0 by)]
                          [(ct) (values cx 0)] [(cc) (values cx cy)] [(cb) (values cx by)]
                          [(rt) (values rx 0)] [(rc) (values rx cy)] [(rb) (values rx by)]
                          [else #|unreachable|# (values 0 0)]))
                      (send dc draw-bitmap bmp x y))
                    (or (send dc get-bitmap) (bitmap-blank)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-combiner
  [make-pin         "bitmap-pin-~a"         (over under)]
  [make-append      "bitmap-~a-append"      (vl vc vr ht hc hb)]
  [make-superimpose "bitmap-~a-superimpose" (lt lc lb ct cc cb rt rc rb)])

(define bitmap-pin : (-> Real Real Real Real Bitmap Bitmap * Bitmap)
  (lambda [x1% y1% x2% y2% bmp0 . bmps]
    (define-values (bmp _who _cares)
      (for/fold ([bmp : Bitmap  bmp0]
                 [x : Exact-Rational  0]
                 [y : Exact-Rational  0])
                ([bmp1 (in-list (cons bmp0 bmps))]
                 [bmp2 (in-list bmps)])
        (define-values (w1 h1) (bitmap-size bmp1))
        (define-values (w2 h2) (bitmap-size bmp2))
        (define x1 (+ x (- (inexact->exact (* x1% w1)) (inexact->exact (* x2% w2)))))
        (define y1 (+ y (- (inexact->exact (* y1% h1)) (inexact->exact (* y2% h2)))))
        (values (bitmap-pin-over bmp x1 y1 bmp2) (max 0 x1) (max 0 y1))))
    bmp))
