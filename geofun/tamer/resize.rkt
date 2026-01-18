#lang typed/racket/base

(require geofun/vector)
(require geofun/tamer/flomap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plane (geo-rectangular 200 200 xy->argb))
(define end (geo-solid 'Green 8))

(define plane-text : (-> String Geo:Text)
  (lambda [str]
    (geo-text str #:color plane)))

(define plane-boxed-text : (-> String Geo)
  (lambda [str]
    (define txt (plane-text str))
    (define-values (w h) (geo-size txt))

    (geo-cc-superimpose (geo-rectangle (+ w 16.0) (+ h 16.0))
                        txt)))





(module+ main
  plane

  (geo-text "====== COPY =====")
  (geo-copy plane)
  
  (geo-text "====== INSET =====")
  (geo-inset plane 32.0 32.0 -32.0 -32.0)
  
  (geo-vl-append #:gapsize 8.0
                 (geo-text "====== SCALE =====")
                 (geo-scale plane 2.0 1.0)
                 (geo-scale plane -2.0 -1.0)
                 (geo-scale (geo-circle 32.0) 1.618 0.618))
  
  (geo-text "====== LB-CROPT =====")
  (geo-lb-crop plane 128 128)
  (geo-ghost end)
  
  (geo-text "====== SHEAR =====")
  (geo-shear plane 0 -0.3)
  (geo-shear plane 0.5 0.0)
  (geo-shear (geo-circle 32.0) -0.5 0.3)
  (geo-shear (plane-boxed-text "pseudo-3d rotation via shearing") 0.0 -0.3)
  (geo-skew (plane-boxed-text "pseudo-3d rotation via skewing") 20.0 10.0 'deg)
  
  (define text (plane-text (string-append "memory: " (number->string (current-memory-use)))))
  (define trimed-text (geo-trim text))
  (define sheared-text (geo-shear text 0.0 -0.3))
  (geo-frame text)
  (geo-bounding-box text)
  (geo-frame trimed-text)
  (geo-bounding-box trimed-text))
