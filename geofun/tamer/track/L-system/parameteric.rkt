#lang typed/racket/base

(require geofun/vector)
(require geofun/palette)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define next-arrow-color : (-> FlRGBA) (palette-generator-create #:for 'stroke))
(define next-trunk-color : (-> FlRGBA) (palette-generator-create #:for 'fill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-growth-arrow : (-> Real Geo:Arrow)
  (lambda [w]
    (define c (next-arrow-color))
    (geo-arrow #:stroke (desc-stroke #:color c #:width (* w 0.1))
               #:fill (desc-brush #:color c #:opacity 0.618)
               5.5 0.0)))

(define make-branch : (-> Renamon Real Real Float-Complex Float-Complex Geo)
  (lambda [self s w start end]
    (geo-hline #:stroke (desc-stroke #:color (next-trunk-color) #:width w #:cap 'round #:opacity 0.32)
               (magnitude (- end start)) 1.0)))

(define θ : (-> Flonum)
  (lambda []
    (+ (random) 0.5)))

(define-renamon-rule! R #:with ([s : Real] [w : Real])
  #:= (let ([w-- (* w 0.707)])
        [~> [100 #:= (let ([s-- (* s 0.8)]) [=> (+ (θ)) (A s-- w--)] (F s-- ~) [=> (- (θ)) (A s-- w--)] (A s-- w--))]
            [100 #:= (let ([s-- (* s 0.7)]) [=> (+ (θ)) (A s-- w--)] (A s-- w--))]
            [100 #:= (let ([s-- (* s 0.7)]) [=> (- (θ)) (A s-- w--)] (A s-- w--))]]))

(define-renamon-rule! A #:with ([s : Real] [w : Real])
  #:make-ribbon make-branch
  [(>= w 9.00) #:= (F s #:@ (make-growth-arrow w) #:~) (R s w)]
  [(>= w 4.00) #:= (F s #:~@ (make-growth-arrow w)) (R s w)]
  [(>= s 0.05) #:= (F s ~) (R s w)]
  #:- (F s))

(define-renamon-generator! parameteric-tree #:with [[s : Real 1.0] [w : Real 10.0]] #:- (A s w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (default-stroke-paint (desc-stroke #:color 'ForestGreen #:dash 'long-dash #:width 1.5))
  
  (geo-table 4 (for/list : (Listof Geo) ([order (in-range 1 13)])
                 (parameteric-tree*! (make-renamon 100 #:angle pi/7) #:order order))
             'cb 'cb 8.0 8.0))
