#lang typed/racket/base

(require geofun/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! drywani [108 108] #:-
  (move-left)
  (move-right)
  (move-down 2 '#:r)
  (move-left 1 '#:l)
  (move-up 1 'E)
  (jump-back)
  (move-left)
  (jump-back)
  (move-down)

  (jump-back 'E)
  (jump-up 2 '#:z)
  (move-left)
  (move-down)
  (move-left 1 'lx)
  (close)
  
  (jump-right-down 2 1 '#:diamond)
  (move-up-right)
  (move-right-down)
  (move-down-left)
  (move-left-up)
  
  (jump-down 3 'Dart)
  (turn-up-right)
  (turn-right-up)
  (turn-right-down)
  (turn-down-right)
  (turn-down-left)
  (turn-left-down)
  (turn-left-up)
  (turn-up-left)
  (close)
  
  (jump-left 3 'Flower)
  (turn-up-right-down)
  (turn-right-down-left)
  (turn-down-left-up)
  (turn-left-up-right)
  (close)
  
  (jump-down-right 2)
  (turn-up-left-down)
  (turn-left-down-right)
  (turn-down-right-up)
  (turn-right-up-left)
  (close)
  
  (jump-back)
  (drift '#:home '(-0.5+i -0.5-i))
  
  (jump-back)
  (drift '#:home '())
  
  (jump-back)
  (drift 'leftmost '(-2+i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require geofun/digitama/dc/path)
  (require geofun/vector)
  (require geofun/bitmap)

  (default-stroke-paint (desc-stroke #:color 'Crimson #:width 4.0))
  (default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.618)))

  (define make-anchor-sticker : Geo-Anchor->Sticker
    (lambda [self anchor pos Width Height]
      (define sticker : Geo
        (if (keyword? anchor)
            (geo-art-text (geo-anchor->string anchor) #:stroke #false #:fill 'Gray)
            (geo-art-text (geo-anchor->string anchor) #:stroke #false #:fill 'Green)))

      (case anchor
        [(#:home) sticker]
        [(#:z #:l) (make-sticker sticker 'ct 0 6)]
        [(#:r #:diamond) (make-sticker sticker 'lc 8.0)]
        [(E) (make-sticker sticker 'cb 0.0 -2.0)]
        [(lx) (make-sticker sticker 'lt 2.0 2.0)]
        [(Dart Flower) (make-sticker sticker 'rb -2.0 -2.0)]
        [else (make-sticker sticker 'cc)])))
  
  (reverse (geo:path-footprints drywani))
  (geo-frame (geo-path-stick drywani make-anchor-sticker #:truncate? #false))
  (geo-freeze drywani #:stroke 'ForestGreen #:fill (rgb* 'RoyalBlue 0.618))

  (let ([bmp (bitmap-square 512)])
    (geo-freeze! bmp drywani -32 -32 #:stroke 'Orange)
    bmp)
  
  (geo-anchor-position drywani '#:home)
  (geo-anchor-position drywani '#:home #:translate? #true))
