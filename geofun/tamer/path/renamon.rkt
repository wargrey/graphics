#lang typed/racket/base

(require geofun/digitama/track/renamon)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hradius 5.5)

(define head1 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'ForestGreen #:opacity 0.5) #:fill (desc-brush #:color 'MediumSeaGreen #:opacity 0.5)))
(define head2 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'DarkOrange #:opacity 0.5) #:fill (desc-brush #:color 'Orange #:opacity 0.5)))
(define head3 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'Crimson #:opacity 0.5) #:fill (desc-brush #:color 'Red #:opacity 0.5)))

(define-renamon! rena [100 pi/4 pi/2 #:avatar head1] #:-
  [#:fork (F @)
   [=> (+ @ head2)
       (F @)]
   [=> (- @ head2)
       [#:fork (F @)
        [=> (- @ head3)
            (F @)]]
       (F @)]]

  [#:fork (F @)
   [=> (+ @)
       (F @)]
   [=> (- @)
       (F @)]])

(define-renamon-primitive! Fl #:= (Fl) (+) (Fr) (+) (+) (Fr) (-) (Fl) (-) (-) (Fl) (Fl) (-) (Fr) (+) #:- (F))
(define-renamon-primitive! Fr #:= (-) (Fl) (+) (Fr) (Fr) (+) (+) (Fr) (+) (Fl) (-) (-) (Fl) (-) (Fr) #:- (F))

(define dragon (make-renamon 10 pi/3))
(define-renamon-script! dragon-curve! #:- (Fl))
(dragon-curve! dragon #:order 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require geofun/digitama/track/self)
  (require geofun/vector)
  (require bitmap)

  (default-halo-paints (list (desc-stroke #:color (rgb* 'Lime 0.1) #:width 10.0)))
  (default-stroke-paint (desc-stroke #:color 'Orange #:dash 'long-dash))
  (default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.618)))
  
  (reverse (geo:track-footprints rena))
  (geo:track-bbox rena)
  (geo-frame (geo-track-stick rena void))
  (geo-frame (geo-track-stick dragon void)))
