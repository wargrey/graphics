#lang typed/racket

(require "../base.rkt")

(bitmap-frame (bitmap-polygon #:color "burlywood" #:border 'green '((0 . 0) (-10 . 20) (60 . 0) (-10 . -20))))
(bitmap-frame (bitmap-polygon #:color "burlywood" #:border 'green '((0 . 0) (-10 . 20) (60 . 0) (-10 . -20)) 10 20))
(bitmap-frame (bitmap-polygon #:border (cons "darkslategray" 10) '((0 . 0) (50 . 0) (0 . 50) (50 . 50))))
(bitmap-frame (bitmap-polygon #:color 'plum #:border 'green '((0 . 0) (0 . 40) (20 . 40) (20 . 60) (40 . 60) (40 . 20) (20 . 20) (20 . 0))))
(bitmap-frame (bitmap-polygon #:color "green" #:border (cons "snow" 8) '((40 . 20))))

(bitmap-polygon #:color "silver" '((0 . 0) (100 . 0)))
(bitmap-polygon #:color "silver" '((0 . 0) (100 . 0) (99.5 . 0.5) (0.5 . 0.5)))
(bitmap-polygon #:color "silver" '((0 . 0) (100 . 0) (95 . 5) (5 . 5)))

(define benchmark : (-> (-> (Pairof (Pairof Real Real) (Listof (Pairof Real Real))) Bitmap) Void)
  (lambda [polygon]
    (printf "~a " polygon)

    (define times 100000)
    (collect-garbage)
    (time (for ([idx (in-range times)])
            (polygon '((0 . 0) (0 . 40) (20 . 40) (20 . 60) (40 . 60) (40 . 20) (20 . 20) (20 . 0)))))))
