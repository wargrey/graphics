#lang typed/racket/base

(require geofun/digitama/track/renamon)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hradius 5.5)

(define head1 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'ForestGreen #:opacity 0.5) #:fill (desc-brush #:color 'MediumSeaGreen #:opacity 0.5)))
(define head2 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'DarkOrange #:opacity 0.5) #:fill (desc-brush #:color 'Orange #:opacity 0.5)))
(define head3 (geo-arrow hradius 0.0 #:stroke (desc-stroke #:color 'Crimson #:opacity 0.5) #:fill (desc-brush #:color 'Red #:opacity 0.5)))

(define halo-pen (desc-stroke #:color (rgb* 'Lime 0.1) #:width 10.0 #:cap 'round #:join 'round))

(define-renamon-primitive! Fl #:- (Fl) (+) (Fr) (+) (+) (Fr) (-) (Fl) (-) (-) (Fl) (-) (Fr) (+))
(define-renamon-primitive! Fr #:- (-) (Fl) (+) (Fr) (Fr) (+) (+) (Fr) (+) (Fl) (-) (-) (Fl) (-) (Fr))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require geofun/digitama/track/self)
  (require geofun/vector)
  (require bitmap)

  (default-halo-paints (list halo-pen))
  (default-stroke-paint (desc-stroke #:color 'Orange #:dash 'long-dash))
  (default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.618)))

  (define make-anchor-sticker : Geo-Track-Anchor->Sticker
    (lambda [anchor pos Width Height]
      (define sticker : Geo
        (if (keyword? anchor)
            (geo-text (geo-anchor->string anchor) #:color 'Gray)
            (geo-text (geo-anchor->string anchor) #:id anchor #:color 'Green)))

      (case anchor
        [(#:end #:moved) (make-sticker sticker 'lt 4 0)]
        [(#:teleport #:slided) (make-sticker sticker 'lt 8 0)]
        [(#:moving #:sliding) (make-sticker sticker 'lb 4 0)]
        [(#:home) (make-sticker sticker 'rc -4 0)]
        [else (make-sticker sticker 'cc)])))
  
  (reverse (geo:track-footprints rena))
  (geo:track-bbox rena)
  (geo-frame (geo-track-stick rena void)))
