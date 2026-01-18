#lang typed/racket/base

(require bitmap)
(require geofun/vector)
(require geofun/tamer/flomap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(bitmap-scale plane -2.0 -1.0)
(printf "====== ~a =====~n" 'LB-CROP)
(bitmap-lb-crop plane 64 64)

(define text (geo-freeze (geo-text (string-append "memory: " (number->string (current-memory-use))) #:color 'RoyalBlue)))
(define trimed-text (time (bitmap-trim text #false)))
(bitmap-frame text)
(bitmap-bounding-box text)
(bitmap-frame trimed-text)
(bitmap-bounding-box trimed-text)
(bitmap-skew text 20 10 'deg)
