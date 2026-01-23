#lang typed/racket/base

(require geofun/vector)

(require "../flomap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (width height bradius) (values 100.0 64.0 8))
(define icon (geo-rectangular 128 128 xy->argb))
(define weird (geo-solid 'DodgerBlue 128))
(define text (geo-text "Adapter"))
(define database (geo-database #:stroke 'RoyalBlue #:fill 'Azure width height bradius))
(define bucket (geo-bucket #:stroke 'RoyalBlue #:fill 'Azure width height bradius))
(define-values (hfit% dbvfit% bktvfit%) (values 1.0 (- 1.0 (/ (* bradius 3.0) height)) (- 1.0 (/ (* bradius 2.0) height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-lc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% dbvfit% #:top% 1.0
                database icon)
  
  (geo-cc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% dbvfit% #:top% 1.0
                database icon)
  
  (geo-rc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% dbvfit% #:top% 1.0
                database weird)
  
  (geo-ct-stamp #:padding 4.0 #:hfit% hfit% #:vfit% bktvfit% #:top% 1.0
                bucket text)
  
  (geo-cc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% bktvfit% #:top% 1.0
                bucket text)
  
  (geo-cb-stamp #:padding 4.0 #:hfit% hfit% #:vfit% bktvfit% #:top% 1.0
                bucket text))
