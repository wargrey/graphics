#lang typed/racket/base

(require geofun/digitama/track/renamon)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hradius 5.5)

(define head1 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'ForestGreen #:opacity 0.5) #:fill (desc-brush #:color 'MediumSeaGreen #:opacity 0.5)))
(define head2 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'DarkOrange #:opacity 0.5) #:fill (desc-brush #:color 'Orange #:opacity 0.5)))
(define head3 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'Crimson #:opacity 0.5) #:fill (desc-brush #:color 'Red #:opacity 0.5)))

(define-renamon! rena [100 pi/4 pi/2 #:avatar head1 #:desc "缘起"] #:-
  [#:fork (F @)
   [=> (+ @ head2) (F @)]
   [=> (- @ head2)
       [#:fork (F @)
        [=> (- @ head3) (F @)]]
       (F @)]]

  [#:fork (F @)
   [=> (+ @ head2) (F @)]
   [=> (- @ head2) (F @)]])

(define-renamon-primitive! K #:= K + K - - K + K #:- F)
(define-renamon-generator! koch-snowflake #:- K - - K - - K close)

(define-renamon-primitive! X #:= X + Y F +)
(define-renamon-primitive! Y #:= - F X - Y)
(define-renamon-generator! dragon-curve #:- X F)

(define-renamon-primitive! Fl #:= Fl + Fr + + Fr - Fl - - Fl Fl - Fr + #:- F)
(define-renamon-primitive! Fr #:= - Fl + Fr Fr + + Fr + Fl - - Fl - Fr #:- F)
(define-renamon-generator! hexagonal-osper-curve #:- Fl)

(define-renamon-primitive! W #:= W W - [=> - W + W + W] + [=> + W - W - W] #:- F)
(define-renamon-generator! tree-in-the-wind #:- W)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require geofun/digitama/track/self)
  (require geofun/vector)
  (require bitmap)

  (default-halo-paints (desc-stroke #:color (rgb* 'Lime 0.1) #:width 10.0 #:scalable? #false))
  (default-stroke-paint (desc-stroke #:color 'Orange #:dash 'long-dash))
  (default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.1)))
  
  (define white-halos
    (list (desc-stroke #:color (rgb* 'GhostWhite 0.8) #:width 12.0 #:scalable? #false)
          (desc-stroke #:color (rgb* 'White 0.9) #:width 6.0  #:scalable? #false)))

  (define snowflake-pen (desc-stroke #:color 'Silver #:scalable? #false))
  
  (define renamon-label : (-> Geo Geo)
    (lambda [rena]
      (geo-vc-append #:gapsize 4.0
                     (geo-frame rena #:background 'WhiteSmoke)
                     (geo-text (geo->string rena)))))

  (geo-hb-append #:gapsize 4.0
                 (renamon-label (geo-track-stick rena))
                 (renamon-label (tree-in-the-wind*! (make-renamon 10 pi/6 pi/2 #:halo-strokes white-halos) #:order 4)))
  
  (geo-hb-append* #:gapsize 4.0
                  (for/list : (Listof Geo) ([order (in-range 0 6)])
                    (define snowflake
                      (koch-snowflake*! #:order (assert order byte?)
                                        (make-renamon #:halo-strokes white-halos #:stroke snowflake-pen #:fill 'Snow
                                                      10 pi/3 0.0)))
                    (renamon-label (geo-scale snowflake (/ 256 (geo-height snowflake))))))

  (geo-hb-append #:gapsize 4.0
                 (renamon-label (dragon-curve*! (make-renamon 10 pi/2 #:stroke 'Green) #:order 10))
                 (renamon-label (hexagonal-osper-curve*! (make-renamon 8 pi/3 #:halo-strokes null) #:order 4))))
