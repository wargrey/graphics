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
(define-renamon-generator! koch-snowflake #:- K - - K - - K)

(define-renamon-primitive! X #:= X + Y F +)
(define-renamon-primitive! Y #:= - F X - Y)
(define-renamon-generator! dragon-curve #:- X F)

(define-renamon-primitive! Fl #:= Fl + Fr + + Fr - Fl - - Fl Fl - Fr + #:- F)
(define-renamon-primitive! Fr #:= - Fl + Fr Fr + + Fr + Fl - - Fl - Fr #:- F)
(define-renamon-generator! hexagonal-osper-curve #:- Fl)

(define-renamon-primitive! W #:= W W - [=> - W + W + W] + [=> + W - W - W] #:- F)
(define-renamon-generator! tree-in-wind #:desc "风中之树" #:- W)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require geofun/digitama/track/self)
  (require geofun/vector)
  (require bitmap)

  (default-halo-paints (list (desc-stroke #:color (rgb* 'Lime 0.1) #:width 8.0)))
  (default-stroke-paint (desc-stroke #:color 'Orange #:dash 'long-dash))
  (default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.618) #:width 2.0))

  (default-renamon-description-format "~a(~a阶)")

  (define renamon-label : (-> Geo Geo)
    (lambda [rena]
      (geo-vc-append #:gapsize 4.0
                     (geo-frame rena)
                     (geo-text (geo->string rena)))))

  (geo-hb-append #:gapsize 4.0
                 (renamon-label (geo-track-stick rena))
                 (renamon-label (tree-in-wind*! (make-renamon 10 pi/6 pi/2 #:halo-strokes null) #:order 4)))
  
  (geo-hb-append* #:gapsize 4.0
                  (for/list : (Listof Geo) ([order (in-range 5)])
                    (renamon-label (koch-snowflake*! #:desc "科赫雪花" #:order (assert order byte?)
                                                     (make-renamon (/ 100 (expt (add1 order) 2)) pi/3 0.0 #:stroke 'SeaGreen)))))

  (geo-hb-append #:gapsize 4.0
                 (renamon-label (dragon-curve*! (make-renamon 10 pi/2 #:stroke 'ForestGreen) #:order 10 #:desc "龙分形"))
                 (renamon-label (hexagonal-osper-curve*! (make-renamon 10 pi/3 #:halo-strokes null #:fill 'Snow) #:order 4 #:desc "高斯帕曲线"))))
