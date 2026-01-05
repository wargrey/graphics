#lang typed/racket/base

(require geofun/vector)

(require "../flomap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (width height aradius) (values 100.0 42.0 8))
(define icon (geo-rectangular 128 128 xy->argb))
(define text (geo-text "Adapter"))
(define storage (geo-storage #:stroke 'RoyalBlue #:fill 'Azure width height aradius))
(define hfit% (- 1.0 (/ (* aradius 2.0) width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-lc-stamp #:margin 4.0 #:hfit% hfit% #:vfit% 1.0
                storage icon)
  
  (geo-cc-stamp #:margin 4.0 #:hfit% hfit% #:vfit% 1.0
                storage icon)
  
  (geo-rc-stamp #:margin 4.0 #:hfit% hfit% #:vfit% 1.0
                storage icon)
  
  (geo-ct-stamp #:margin 4.0 #:hfit% hfit% #:vfit% 1.0
                storage text)
  
  (geo-cc-stamp #:margin 4.0 #:hfit% hfit% #:vfit% 1.0
                storage text)
  
  (geo-cb-stamp #:margin 4.0 #:hfit% hfit% #:vfit% 1.0
                storage text))
