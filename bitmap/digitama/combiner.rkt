#lang typed/racket

(provide (all-defined-out))

(require "digicore.rkt")

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

(define-type Bitmap-Layer->XY (-> Integer Integer (Values Real Real)))
(define-type Pseudo-Bitmap (List Integer Integer (Listof (Pairof Bitmap Bitmap-Layer->XY))))
(define-type Superimpose-Alignment (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))

(define superimpose : (-> Symbol (Listof Bitmap) Pseudo-Bitmap)
  (lambda [alignment bitmaps]
    (define-values (width height sreyal)
      (for/fold ([width : Integer 0] [height : Integer 0] [layers : (Listof (Pairof Bitmap Bitmap-Layer->XY)) null])
                ([bmp : Bitmap (in-list bitmaps)])
        (define-values (w h) (values (send bmp get-width) (send bmp get-height)))
        (values (fxmax width w) (fxmax height h) (cons (cons bmp (make-layer alignment w h)) layers))))
    (list width height (reverse sreyal))))

(define make-layer : (-> Symbol Integer Integer Bitmap-Layer->XY)
  (lambda [alignment w h]
    (λ [[W : Integer] [H : Integer]] (superimpose-xy alignment W H w h))))

(define superimpose-xy : (-> Symbol Integer Integer Integer Integer (Values Real Real))
  (lambda [alignment width height w h]
    (define-values (rx by) (values (fx- width w) (fx- height h)))
    (define-values (cx cy) (values (/ rx 2) (/ by 2)))
    (case alignment
      [(lt) (values 0.0 0.0)] [(lc) (values 0.0 cy)] [(lb) (values 0.0 by)]
      [(ct) (values  cx 0.0)] [(cc) (values  cx cy)] [(cb) (values  cx by)]
      [(rt) (values  rx 0.0)] [(rc) (values  rx cy)] [(rb) (values  rx by)]
      [else #|unreachable|# (values 0.0 0.0)])))

(define-type Bitmap-Cell (List Bitmap Positive-Integer Positive-Integer))
(define-type Bitmap-Tables (Vectorof (Vectorof Bitmap-Cell)))
(define-type Pseudo-Bitmap* (List Integer Integer (Listof (List Bitmap-Cell Real Real))))

(define superimpose* : (-> Symbol (Listof Bitmap-Cell) Pseudo-Bitmap*)
  (lambda [alignment cells]
    (define-values (width height)
      (for/fold ([width : Integer 0] [height : Integer 0])
                ([cell : Bitmap-Cell (in-list cells)])
        (values (max width (cadr cell)) (max height (caddr cell)))))
    (list width height
          (for/list ([cell (in-list cells)])
            (define-values (w h) (values (cadr cell) (caddr cell)))
            (define-values (x y) (superimpose-xy alignment width height w h))
            (list cell x y)))))

(define find-xy : (All (a) (-> a Pseudo-Bitmap* (Values Real Real)))
  (lambda [bmp pbmps]
    (let find ([rest (caddr pbmps)])
      (cond [(null? rest) (values 0.0 0.0)]
            [(eq? bmp (caar rest)) (values (cadar rest) (caddar rest))]
            [else (find (cdr rest))]))))

(define list->table : (-> (Listof Bitmap) Natural Natural Bitmap-Tables)
  (lambda [bitmaps nrows ncols]
    (let row-fold ([row : Integer nrows] [src : (Listof Bitmap) bitmaps] [row++ : (Listof (Vectorof Bitmap-Cell)) null])
      (cond [(zero? row) (list->vector (reverse row++))]
            [else (let col-fold ([col : Integer ncols] [src : (Listof Bitmap) src] [col++ : (Listof Bitmap-Cell) null])
                    (cond [(zero? col) (row-fold (sub1 row) src (cons ((inst list->vector Bitmap-Cell) (reverse col++)) row++))]
                          [(null? src) (col-fold (sub1 col) null (cons (list the-invalid-image 1 1) col++))]
                          [else (let* ([this.bmp (car src)]
                                       [w (send this.bmp get-width)]
                                       [h (send this.bmp get-height)])
                                  (col-fold (sub1 col) (cdr src) (cons (list this.bmp w h) col++)))]))]))))

(define list->n:vector : (All (a) (-> (Listof a) Integer a (Vectorof a)))
  (lambda [src total defval]
    (cond [(null? src) (make-vector total defval)]
          [else (let fold ([dest : (List* a (Listof a)) (list (car src))]
                           [rest : (Listof a) (cdr src)]
                           [count : Integer 1])
                  (cond [(= total count) (list->vector (reverse dest))]
                        [(pair? rest) (fold (cons (car rest) dest) (cdr rest) (add1 count))]
                        [else (list->vector (append (reverse dest) (make-list (- total count) (car dest))))]))])))

(define nmap : (All (a) (-> Integer (-> Integer a) (Listof a)))
  (lambda [total f]
    (let fold ([n : Integer total] [acc : (Listof a) null])
      (if (zero? n) acc (fold (sub1 n) (cons (f (sub1 n)) acc))))))
