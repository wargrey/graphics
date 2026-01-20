#lang typed/racket/base

(require geofun/vector)

(require "../flomap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (width height bradius) (values 100.0 64.0 8))
(define icon (geo-rectangular 128 128 xy->argb))
(define text (geo-text "Adapter"))
(define storage (geo-database #:stroke 'RoyalBlue #:fill 'Azure width height bradius))
(define-values (hfit% top%) (values 1.0 (/ (* bradius 3.0) height)))
(define vfit% (- 1.0 top%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-lc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% top%
                storage icon)
  
  (geo-cc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% top%
                storage icon)
  
  (geo-rc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% top%
                storage icon)
  
  (geo-ct-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% top%
                storage text)
  
  (geo-cc-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% top%
                storage text)
  
  (geo-cb-stamp #:padding 4.0 #:hfit% hfit% #:vfit% vfit% #:top% top%
                storage text))
