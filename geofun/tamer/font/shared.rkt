#lang typed/racket

(provide (all-defined-out))

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(default-border (desc-stroke #:color 'gray #:width 1 #:dash 'long-dash))

(define geo-text* : (->* (String Font) (Real) Geo)
  (lambda [text font [size -2.0]]
    (define content (geo-text #:ascent 'magenta #:descent 'blue #:capline 'orange #:meanline 'green #:baseline 'red
                              text (desc-font (desc-font font #:size 'xx-large) #:size size)))
    (geo-frame content)))
