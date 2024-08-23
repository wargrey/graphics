#lang typed/racket

(require bitmap/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define r : Index 64)

(bitmap-polygon #:stroke 'green #:fill 'burlywood '(0+0i -10+20i 60 -10-20i) #:window -1.0-1.0i)

(bitmap-polygon #:fill 'silver '((0 . 0) (100 . 0)))
(bitmap-polygon #:fill 'silver '((0 . 0) (100 . 0) (99.5 . 0.5) (0.5 . 0.5)))
(bitmap-polygon #:fill 'silver '((0 . 0) (100 . 0) (95 . 5) (5 . 5)))

(for/list : (Listof Bitmap) ([n (in-range 17)])
  (bitmap-cc-superimpose (bitmap-regular-polygon n r #:fill 'azure)
                         (bitmap-circle r)
                         (bitmap-text n)))

(for/list : (Listof Bitmap) ([n (in-range 17)])
  (bitmap-cc-superimpose (bitmap-regular-polygon n r #:inscribed? #true #:fill 'azure)
                         (bitmap-circle r #:fill 'ghostwhite)
                         (bitmap-text n)))
