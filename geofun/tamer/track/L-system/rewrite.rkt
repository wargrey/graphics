#lang typed/racket/base

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-renamon-rule! K #:= K + K - - K + K #:- F)
(define-renamon-generator! koch-snowflake #:closed? #true #:angle pi/3 #:- K - - K - - K)

(define-renamon-rule! X #:= X + Y F +)
(define-renamon-rule! Y #:= - F X - Y)
(define-renamon-generator! dragon-curve #:angle pi/2 #:- X F)

(define-renamon-rule! Fl #:= Fl + Fr + + Fr - Fl - - Fl Fl - Fr + #:- F)
(define-renamon-rule! Fr #:= - Fl + Fr Fr + + Fr + Fl - - Fl - Fr #:- F)
(define-renamon-generator! hexagonal-gosper-curve #:angle pi/3 #:- Fl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (default-track-halo-stroke (desc-halo-stroke #:colors 'Lime #:width 10.0 #:opacity 0.1))
  (default-stroke-paint (desc-stroke #:color 'DarkOrange #:dash 'long-dash #:width 1.5))
  (default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.1)))
  
  (define white-halo (desc-halo-stroke #:colors (list (rgb* 'GhostWhite 0.8) (rgb* 'White 0.9)) #:width 12.0))
  (define snowflake-pen (desc-stroke #:width 2.0 #:color 'Silver #:scalable? #false))
  
  (define renamon-label : (-> Geo Geo)
    (lambda [rena]
      (geo-vc-append #:gapsize 4.0
                     (geo-frame rena #:background 'MintCream)
                     (geo-text rena))))

  (geo-table 3 (for/list : (Listof Geo) ([order (in-range 0 6)])
                 (define snowflake
                   (koch-snowflake*! #:order (assert order byte?)
                                     (make-renamon #:halo-stroke white-halo #:stroke snowflake-pen #:fill 'Snow
                                                   10 0.0)))
                 (renamon-label (geo-scale snowflake (/ 256 (geo-height snowflake)))))
             'cc 'cc 4.0 4.0)

  (geo-hb-append #:gapsize 4.0
                 (renamon-label (hexagonal-gosper-curve*! (make-renamon 8 #:halo-stroke #false) #:order 4))
                 (renamon-label (dragon-curve*! (make-renamon 10 #:stroke 'Green) #:order 10))))
