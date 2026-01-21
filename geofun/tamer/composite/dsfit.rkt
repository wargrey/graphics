#lang typed/racket/base

(require geofun/vector)

(require "../flomap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (width height bradius) (values 100.0 64.0 8))
(define icon (geo-rectangular 128 128 xy->argb))
(define weird (geo-solid 'DodgerBlue 128))
(define text (geo-text "Adapter"))
(define storage (geo-database #:stroke 'RoyalBlue #:fill 'Azure width height bradius))
(define-values (hfit% vfit%) (values 1.0 (- 1.0 (/ (* bradius 3.0) height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-lc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% 1.0
                storage icon)
  
  (geo-cc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% 1.0
                storage icon)
  
  (geo-rc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% 1.0
                storage weird)
  
  (geo-ct-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% 1.0
                storage text)
  
  (geo-cc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% 1.0
                storage text)
  
  (geo-cb-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% 1.0
                storage text))
