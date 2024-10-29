#lang typed/racket/base

(require geofun/vector)
(require geofun/tamer/flomap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plane (geo-rectangular 200 200 xy->argb))
(define end (geo-solid 'Green 8))

plane
(geo-vl-append (geo-text "====== COPY =====")
               (geo-copy plane))

(geo-vl-append (geo-text "====== INSET =====")
               (geo-inset plane 32.0 32.0 -32.0 -32.0))

(geo-vl-append #:gapsize 8.0
               (geo-text "====== SCALE =====")
               (geo-scale plane 2.0 1.0)
               (geo-scale plane -2.0 -1.0)
               (geo-scale (geo-circle 32.0) 1.618 0.618))

(geo-vl-append (geo-text "====== LB-CROPT =====")
               (geo-lb-crop plane 128 128)
               (geo-ghost end)
               end)

(define text (geo-text (string-append "memory: " (number->string (current-memory-use))) #:color plane))
(define trimed-text (geo-trim text))
(geo-frame text)
(geo-bounding-box text)
(geo-frame trimed-text)
(geo-bounding-box trimed-text)
