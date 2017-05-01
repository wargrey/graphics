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

(define-type Pseudo-Bitmap (List Natural Natural (Listof (List Bitmap Flonum Flonum))))
(define-type (Pseudo-Bitmap* a) (List Natural Natural (Listof (List a Flonum Flonum))))
(define-type Superimpose-Alignment (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))

(define superimpose : (-> Symbol (Listof Bitmap) Pseudo-Bitmap)
  (lambda [alignment bitmaps]
    (cond [(null? bitmaps) (list 0 0 null)]
          [else (let-values ([(width height) (superimpose-size (car bitmaps) (cdr bitmaps))])
                  (list width height
                        (for/list ([bmp (in-list bitmaps)])
                          (define-values (x y) (superimpose-xy bmp width height alignment))
                          (list bmp x y))))])))

(define superimpose-size : (-> Bitmap (Listof Bitmap) (Values Positive-Integer Positive-Integer))
  (lambda [base bitmaps]
    (for/fold ([width : Positive-Integer (send base get-width)]
               [height : Positive-Integer (send base get-height)])
              ([bmp : Bitmap (in-list bitmaps)])
      (values (max width (send bmp get-width))
              (max height (send bmp get-height))))))

(define superimpose-xy : (-> Bitmap Positive-Integer Positive-Integer Symbol (Values Flonum Flonum))
  (lambda [bmp width height alignment]
    (define-values (w h) (values (send bmp get-width) (send bmp get-height)))
    (define-values (rx by) (values (fx->fl (fx- width w)) (fx->fl (fx- height h))))
    (define-values (cx cy) (values (fl/ rx 2.0) (fl/ by 2.0)))
    (case alignment
      [(lt) (values 0.0 0.0)] [(lc) (values 0.0 cy)] [(lb) (values 0.0 by)]
      [(ct) (values  cx 0.0)] [(cc) (values  cx cy)] [(cb) (values  cx by)]
      [(rt) (values  rx 0.0)] [(rc) (values  rx cy)] [(rb) (values  rx by)]
      [else #|unreachable|# (values 0.0 0.0)])))

(define superimpose* : (All (a) (-> Symbol (Listof a) (-> a Bitmap) (Pseudo-Bitmap* a)))
  (lambda [alignment &bitmaps unbmp]
    (cond [(null? &bitmaps) (list 0 0 null)]
          [else (let*-values ([(bitmaps) (map unbmp &bitmaps)]
                              [(width height) (superimpose-size (car bitmaps) (cdr bitmaps))])
                  (list width height
                        (for/list ([&bmp (in-list &bitmaps)]
                                   [bmp (in-list bitmaps)])
                          (define-values (x y) (superimpose-xy bmp width height alignment))
                          (list &bmp x y))))])))

(define find-xy : (All (a) (-> a (Pseudo-Bitmap* a) (Values Flonum Flonum)))
  (lambda [bmp pbmps]
    (let find ([rest (caddr pbmps)])
      (cond [(null? rest) (values 0.0 0.0)]
            [(eq? bmp (caar rest)) (values (cadar rest) (caddar rest))]
            [else (find (cdr rest))]))))

(define list->n:vector : (All (a) (-> (Listof a) Integer a (Vectorof a)))
  (lambda [src total defval]
    (cond [(null? src) (make-vector total defval)]
          [else (let fold ([dest : (List* a (Listof a)) (list (car src))]
                           [rest : (Listof a) (cdr src)]
                           [count : Integer 1])
                  (cond [(= total count) (list->vector (reverse dest))]
                        [(pair? rest) (fold (cons (car rest) dest) (cdr rest) (add1 count))]
                        [else (list->vector (append (reverse dest) (make-list (- total count) (car dest))))]))])))

(define nmap : (All (a) (-> (-> Integer a) Integer (Listof a)))
  (lambda [f total]
    (let fold ([n : Integer total] [acc : (Listof a) null])
      (if (zero? n) acc (fold (sub1 n) (cons (f (sub1 n)) acc))))))
