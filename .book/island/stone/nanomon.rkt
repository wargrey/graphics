#lang at-exp racket/gui

(require images/icons/style)
(require pict)
(require plot)

(require "composition.rkt")

(plot-decorations? #false)

(plot-foreground-alpha 1.00)
(plot-background-alpha 0.00)
(plot-font-size 16)
(plot-width 320)
(plot-height (exact-round (/ (plot-width) 0.618)))
(plot3d-angle 8)
(plot3d-altitude 60)

(default-icon-material glass-icon-material)
(default-icon-height 48)

(define rallies (list (cons 241 147)
                      (cons 200 156)
                      (cons 56 135)))

(define nanomon (let ([nanomon (recv-digimon 'Nanomon)])
                  (if (digimon? nanomon)
                      (let ([figure (digimon-ark (digimon-figure nanomon) #:lightness 0.84 #:rallies rallies)]
                            [background (digimon-visualize (digimon-figure nanomon))]
                            [foreground (profile nanomon '機械の修理にかけてはピカイチのマシーン型デジモンだ '{プラグボム ナノクラッシュ カウンタートラップ})])
                        (cc-superimpose background foreground figure))
                      nanomon)))
