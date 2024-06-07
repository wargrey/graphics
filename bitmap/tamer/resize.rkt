#lang typed/racket/base

(require "../resize.rkt")
(require "../constructor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xy->argb : XYWH->ARGB
  (lambda [x y w h]
    (define w+h (+ w h))
    (define w-x (- w x))
    (define h-y (- h y))
    (values (real->double-flonum (/ (+ x y)     w+h))
            (real->double-flonum (/ (+ w-x y)   w+h))
            (real->double-flonum (/ (+ w-x h-y) w+h))
            (real->double-flonum (/ (+ x h-y)   w+h)))))

(printf "====== ~a =====~n" '(density 1.0))
(time (bitmap-rectangular 100 100 xy->argb #:density 1.00))
(printf "====== ~a =====~n" '(density 1.75))
(time (bitmap-rectangular 100 100 xy->argb #:density 1.75))
(printf "====== ~a =====~n" '(density 2.0))
(time (bitmap-rectangular 100 100 xy->argb #:density 2.00))

(define plane (time (bitmap-rectangular 100 100 xy->argb #:density 2.00)))
(printf "====== ~a =====~n" 'COPY)
(bitmap-copy plane)
(printf "====== ~a =====~n" 'INSET)
(bitmap-inset plane 16.0 16.0 -16.0 -16.0)
(printf "====== ~a =====~n" 'SCALE)
(bitmap-scale plane 2.0 1.0)
(printf "====== ~a =====~n" 'RB-CROP)
(bitmap-rb-crop plane 64 64)

(define text (bitmap-text (string-append "memory: " (number->string (current-memory-use)))))
(define trimed-text (time (bitmap-trim text #false)))
(bitmap-frame text)
(bitmap-bounding-box text)
(bitmap-frame trimed-text)
(bitmap-bounding-box trimed-text)
