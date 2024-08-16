#lang typed/racket/base

(require geofun/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! drywani [108 108] #:-
  (step-left)
  (step-right)
  (step-down 2 '#:r)
  (step-left 1 '#:l)
  (step-up 1 'E)
  (jump-back)
  (step-left)
  (jump-back)
  (step-down)

  (jump-back 'E)
  (jump-up 2 '#:z)
  (step-left)
  (step-down)
  (step-left 1 'lx)
  (close)
  
  (jump-right-down 2 1 '#:diamond)
  (step-up-right)
  (step-right-down)
  (step-down-left)
  (step-left-up)
  
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
  
  (jump-down-right 2)
  (turn-up-left-down)
  (turn-left-down-right)
  (turn-down-right-up)
  (turn-right-up-left)
  
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

  (default-stroke-paint (desc-stroke #:color 'crimson #:width 2.0))
  #;(default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.618)))

  (define make-anchor-sticker : Geo-Anchor->Sticker
    (lambda [self anchor pos Width Height]
      (define sticker : Geo<%> (geo-text (format "~a" anchor) #:color (if (keyword? anchor) 'Gray 'Green)))
      (case anchor
        [(#:z #:l) (cons sticker (cons 'ct +6i))]
        [(#:r #:diamond #:home) (cons sticker (cons 'lc 8))]
        [(E) (cons sticker (cons 'cb -2i))]
        [(lx) (cons sticker (cons 'lt 2+2i))]
        [(Dart Flower) (cons sticker (cons 'rb -2-2i))]
        [else (cons sticker 'cc)])))
  
  (reverse (geo-path-footprints drywani))
  (geo-frame (geo-path-stick drywani make-anchor-sticker #:truncate? #false))
  (geo-freeze drywani #:stroke 'ForestGreen)

  (let ([bmp (bitmap-square 512)])
    (geo-freeze! bmp drywani -32 -32 #:stroke 'Orange)
    bmp)
  
  (geo-anchor-position drywani '#:home)
  (geo-anchor-position drywani '#:home #:translate? #true))
