#lang typed/racket/base

(require geofun/vector)
(require geofun/tamer/flomap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plane (geo-rectangular 200 200 xy->argb))

plane
(printf "====== ~a =====~n" 'COPY)
(geo-copy plane)
(printf "====== ~a =====~n" 'INSET)
(geo-inset plane 32.0 32.0 -32.0 -32.0)
(printf "====== ~a =====~n" 'SCALE)
(geo-scale plane 2.0 1.0)
(geo-scale plane -2.0 -1.0)
(geo-scale (geo-circle 32.0) 1.618 0.618)
(printf "====== ~a =====~n" 'LB-CROP)
(geo-lb-crop plane 128 128)

(define text (geo-text (string-append "memory: " (number->string (current-memory-use))) #:color plane))
;(define trimed-text (time (geo-trim text #false)))
;(geo-frame text)
(geo-extent text)
;(geo-frame trimed-text)
;(geo-bounding-box trimed-text)
