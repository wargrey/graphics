#lang typed/racket/base

(require geofun/digitama/track/renamon)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-growth-arrow : (-> Color Geo:Arrow)
  (lambda [c]
    (geo-arrow #:stroke c #:fill (desc-brush #:color c #:opacity 0.618)
               5.5 0.0)))

(define internode (make-growth-arrow 'Peru))
(define apex (make-growth-arrow 'ForestGreen))

(define lateral-step 0.618)
(define apex-step 0.5)

(define spore
  (geo-scale (geo-bullet #:stroke 'Goldenrod #:fill (desc-brush #:color 'Goldenrod #:opacity 0.75)
                         10.0 3.0)
             -1.0 1.0))

(define-renamon! beginner [64 pi/2 #:avatar internode #:angle pi/4 #:desc "An Axial Tree (by beginner)"] #:-
  [#:fork (F @) 
   [=> +
       [#:fork (F lateral-step @)
        [=> + (F apex-step #:@ apex)]
        [=> (F apex-step #:@ apex)]]]]

  [#:fork (F @)
   [=> -
       [#:fork (F lateral-step @)
        [=> (- 1.75)
            [#:fork (F lateral-step @)
             [=> + (F apex-step #:@ apex)]
             [=> - (F apex-step #:@ apex)]]]]
       [=> [#:fork (F lateral-step @)
            [=> (- 1.75) (F apex-step #:@ apex)]
            [=> (F apex-step #:@ apex)]]]
       [=> (+ 0.75) (F apex-step #:@ apex)]]]

  [#:fork (F @)
   [=> + (F apex-step #:@ apex)]
   [=> (F apex-step #:@ apex)]])

(define-renamon! expert [64 pi/2 #:avatar internode #:angle pi/4 #:desc "An Axial Tree (by expert)"] #:-
  (F @) 
  [=> + (F lateral-step @)
      [=> + (F apex-step #:@ apex)]
      [=> (F apex-step #:@ apex)]]

  (F @)
  [=> - (F lateral-step @)
      [=> (- 1.75) (F lateral-step @)
          [=> + (F apex-step #:@ apex)]
          [=> - (F apex-step #:@ apex)]]
      [=> (F lateral-step @)
          [=> (- 1.75) (F apex-step #:@ apex)]
          [=> (F apex-step #:@ apex)]]
      [=> (+ 0.75) (F apex-step #:@ apex)]]

  (F @)
  [=> + (F apex-step #:@ apex)]
  [=> (F apex-step #:@ apex)])

(define-renamon! what-a-mess [64 pi/2 #:avatar internode #:angle pi/4 #:desc "An Axial Tree (what a mess)"] #:-
  [#:seq
   [(F @) #:=> + [#:seq
                  [(F lateral-step @) #:=> + (F apex-step #:@ apex)]
                  [(F apex-step #:@ apex)]]]

   [(F @) #:=> - [#:seq
                  [(F lateral-step @) #:=>
                                      [=> (- 1.75) [#:tree (F lateral-step @)
                                                    [=> + (F apex-step #:@ apex)]
                                                    [=> - (F apex-step #:@ apex)]]]
                                      [=> (+ 0.75) (F apex-step #:@ apex)]]
                  [(F lateral-step @) #:=> (- 1.75) (F apex-step #:@ apex)]
                  [(F apex-step #:@ apex)]]]

   [(F @) #:=> + (F apex-step #:@ apex)]
   [(F apex-step #:@ apex)]])

(define-renamon! true-dichotomous-branching [#:avatar apex #:angle pi/4 #:desc "True Dichotomous Branching"
                                             #:stroke (desc-stroke #:color 'SeaGreen #:width 1.0 #:dash 'short-dash)
                                             #:halo-stroke (desc-halo-stroke #:width 5.0 #:opacity 0.42 #:scalable? #true
                                                                             #:colors '(OliveDrab OliveDrab OliveDrab))
                                             #:anchor '_
                                             100 pi/3] #:-
  (drift 0.25-0.10i (list 0.125))
  [=> (drift 0.50-0.20i (list 0.20))
      [=> (drift 1.0-0.20i (list 0.9-0.30i))
          (drift 0.6+0.35i (list 0.1))
          [=> (drift 0.50+0.18i (list 0.30+0.15i 0.40+0.15i))
              [=> (drift 1.0+0.20i (list 0.30+0.20i) #:@ spore)]
              [=> (drift 1.0-0.10i (list 0.30-0.25i) #:@ spore)]]
          [=> (drift 0.50-0.25i (list 0.30-0.25i) #:@)]]
      [=> (drift 1.5-0.10i (list 1.40-0.15i))
          (drift 0.5+0.50i (list 0.15) #:@)]]
  [=> - (drift 0.75-0.15i (list 0.2+0.15i 0.65-0.20i))
      (drift 0.35+0.4i (list 0.10))
      [=> (drift 0.75+0.15i (list 0.5))
          [=> (drift 0.50+0.15i (list 0.30+0.15i))
              [=> (drift 0.40+0.05i (list 0.20+0.05i) #:@ spore)]
              [=> (drift 0.40-0.10i (list 0.20-0.10i) #:@ spore)]]
          [=> (drift 0.50-0.15i (list 0.30-0.15i))
              [=> (drift 0.40+0.05i (list 0.20+0.05i) #:@ spore)]
              [=> (drift 0.40-0.10i (list 0.20-0.10i) #:@ spore)]]]
      [=> (drift 0.50-0.25i (list 0.4))
          (drift 1.00-0.50i (list 0.20))
          (drift 0.15+0.05i (list 0.10) #:@)]])

(define-renamon-rule! W #:= W W - [=> - W + W + W] + [=> + W - W - W] #:- F)
(define-renamon-generator! tree-in-the-wind #:angle pi/6 #:- F F W)

(define-renamon-rule! X #:= D + [=> [=> X] - X] - D [=> - D X] + X #:- F)
(define-renamon-rule! D #:= D D #:- F)
(define-renamon-generator! fractal-tree #:angle (~rad 25 'deg) #:- - X)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (default-track-halo-stroke (desc-halo-stroke #:colors 'Lime #:width 10.0 #:opacity 0.1))
  (default-stroke-paint (desc-stroke #:color 'DarkOrange #:dash 'long-dash #:width 1.5))
  (default-border-paint (desc-border #:color (rgb* 'RoyalBlue 0.1)))
  
  (define renamon-label : (-> Geo Geo)
    (lambda [rena]
      (geo-vc-append #:gapsize 4.0
                     (geo-frame rena)
                     (geo-text rena))))

  (geo-hb-append #:gapsize 4.0
                 (renamon-label (geo-track-stick true-dichotomous-branching void))
                 (renamon-label (geo-track-stick beginner void))
                 (renamon-label (geo-track-stick expert void))
                 (renamon-label (geo-track-stick what-a-mess void)))

  (geo-hb-append #:gapsize 4.0
                 (renamon-label (tree-in-the-wind*! (make-renamon 8 pi/2 #:stroke 'OliveDrab #:halo-stroke #false) #:order 4))
                 (renamon-label (fractal-tree*! (make-renamon 3 pi/2 #:stroke 'Goldenrod #:halo-stroke #false) #:order 6))))
