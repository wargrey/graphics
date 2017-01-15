#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "bitmap.rkt")

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

(define-type (Pseudo-Bitmap a) (List Natural Natural (Listof (List a Nonnegative-Real Nonnegative-Real))))
(define-type Superimpose-Alignment (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))

(define superimpose : (All (a) (case-> [Symbol (Listof Bitmap) -> (Pseudo-Bitmap Bitmap)]
                                       [Symbol (Listof a) (-> a Bitmap) -> (Pseudo-Bitmap a)]))
  (case-lambda
    [(alignment &bitmaps) (superimpose alignment &bitmaps (λ [[bmp : Bitmap]] bmp))]
    [(alignment &bitmaps unbmp)
     (cond [(null? &bitmaps) (list 0 0 null)]
           [else (let ([base (unbmp (car &bitmaps))])
                   (define-values (width height)
                     (for/fold ([width : Positive-Integer (send base get-width)]
                                [height : Positive-Integer (send base get-height)])
                               ([&bmp : a (in-list (cdr &bitmaps))])
                       (define bmp : Bitmap (unbmp &bmp))
                       (values (max width (send bmp get-width)) (max height (send bmp get-height)))))
                   
                   (list width height
                         (for/list : (Listof (List a Nonnegative-Real Nonnegative-Real)) ([&bmp : a (in-list &bitmaps)])
                           (define bmp : Bitmap (unbmp &bmp))
                           (define-values (w h) (values (send bmp get-width) (send bmp get-height)))
                           (define-values (rx by) (values (max (- width w) 0) (max (- height h) 0)))
                           (define-values (cx cy) (values (/ rx 2) (/ by 2)))
                           (define-values (x y)
                             (case alignment
                               [(lt) (values  0 0)] [(lc) (values  0 cy)] [(lb) (values  0 by)]
                               [(ct) (values cx 0)] [(cc) (values cx cy)] [(cb) (values cx by)]
                               [(rt) (values rx 0)] [(rc) (values rx cy)] [(rb) (values rx by)]
                               [else #|unreachable|# (values 0 0)]))
                           (list &bmp x y))))])]))

(define find-xy : (All (a) (-> a (Pseudo-Bitmap a) (Values Nonnegative-Real Nonnegative-Real)))
  (lambda [bmp pbmps]
    (let find ([rest : (Listof (List a Nonnegative-Real Nonnegative-Real)) (caddr pbmps)])
      (cond [(null? rest) (values 0.0 0.0)]
            [(eq? bmp (caar rest)) (values (cadar rest) (caddar rest))]
            [else (find (cdr rest))]))))

(define list->n:vector : (All (a) (-> (Listof a) Integer a (Vectorof a)))
  (lambda [src total defval]
    (cond [(null? src) (make-vector total defval)]
          [else (let ([filled : a (last src)])
                  (let fold ([dest : (Listof a) null]
                             [count : Integer 0]
                             [rest : (Listof a) src])
                  (cond [(= total count) (list->vector (reverse dest))]
                        [(null? rest) (fold (cons filled dest) (add1 count) null)]
                        [else (fold (cons (car rest) dest) (add1 count) (cdr rest))])))])))

(define nmap : (All (a) (-> (-> Integer a) Integer (Listof a)))
  (lambda [f total]
    (let fold ([n : Integer total] [acc : (Listof a) null])
      (if (zero? n) acc (fold (sub1 n) (cons (f (sub1 n)) acc))))))
