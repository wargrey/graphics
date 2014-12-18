#lang at-exp racket

(require images/icons/control)
(require images/icons/misc)
(require (only-in plot/utils color-seq*))

;;; These two lines can fool makefile.rkt, which is the expected dependency.
;(require "../../../../village/sakuyamon/digitama/digimon.rkt")
(require (file "../../d-ark.rkt"))

;;; This line is fixed automatically when building.
(wikimon-dir "../../wikimon")

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

(define visualize
  {lambda [monster]
    (define figure (flomap-flip-vertical (cdr (digimon-figure monster))))
    (define get-pixel {lambda [x y] (let ([flv (flomap-ref* figure x y)]) (rgb->hsv (flvector-ref flv 1) (flvector-ref flv 2) (flvector-ref flv 3)))})
    (define prop (/ (plot-height) (plot-width)))
    (define count 5)
    (plot3d-pict (list (contour-intervals3d {lambda [x y] (let-values ([{v who cares} (get-pixel x y)]) (* v prop))}
                                            0 (flomap-width figure) 0 (flomap-height figure)
                                            #:colors (color-seq* (list dark-metal-icon-color metal-icon-color light-metal-icon-color) count)
                                            #:alphas (make-list count 0.04))
                       (contour-intervals3d {lambda [x y] (let-values ([{who v cares} (get-pixel x y)]) (* v prop))}
                                            0 (flomap-width figure) 0 (flomap-height figure)
                                            ;#:colors (color-seq* (list dark-metal-icon-color metal-icon-color light-metal-icon-color) count)
                                            #:alphas (make-list count 0.08))
                       (contour-intervals3d {lambda [x y] (let-values ([{who cares v} (get-pixel x y)]) (* v prop))}
                                            0 (flomap-width figure) 0 (flomap-height figure)
                                            ;#:colors (color-seq* (list dark-metal-icon-color metal-icon-color light-metal-icon-color) count)
                                            #:alphas (make-list count 0.08))))})

(define profile
  {lambda [monster details skills]
    (define fsize (plot-font-size))
    (define flcolor (color->flvector metal-icon-color))
    (define hbar (ghost (bitmap (bar-icon #:height fsize #:color metal-icon-color))))
    (define {fdigimoji txt [color light-metal-icon-color]} (digimoji (~a txt) #:height fsize #:color color))
    (define {fbg fg [wdth #false]} (let ([width (if wdth wdth (exact-round (+ fsize (pict-width fg))))]
                                         [height (exact-round (+ fsize (pict-height fg)))])
                                     (cc-superimpose (cellophane (bitmap (flomap->bitmap (flomap-shadow (make-flomap* width height flcolor) fsize flcolor))) 1.00) fg)))
    (define head (apply vr-append
                        (fbg (vc-append (pict-width hbar) (fdigimoji "Digital Monster") (fdigimoji (digimon-kana monster))) (plot-width))
                        (map {lambda [emblem] (let ([% (/ (default-icon-height) (flomap-width (cdr emblem)))])
                                                (cellophane (bitmap (flomap->bitmap (flomap-scale (cdr emblem) %))) %))}
                             (digimon-fields monster))))
    (define body (let ([skill (apply vl-append (pict-width hbar) (map {lambda [skill] (hc-append (bitmap (bomb-icon #:height fsize)) hbar (fdigimoji skill))} skills))])
                   (fbg (vl-append (desc details (pict-width skill) #:ftext text #:height fsize #:color light-metal-icon-color) hbar skill))))
    (define watermark (vr-append (fdigimoji "wargrey" metal-icon-color) (rotate hbar (/ pi 2))))
    (rb-superimpose (rt-superimpose (lb-superimpose (blank (plot-width) (plot-height)) body) head) watermark)})

(let ([nanomon (recv-digimon 'Nanomon)])
  (pict->bitmap (if (digimon? nanomon)
                    (let* ([figure (digimon-ark nanomon #:lightness 0.84 #:rallies rallies)]
                           [background (visualize nanomon)]
                           [foreground (profile nanomon '機械の修理にかけてはピカイチのマシーン型デジモンだ '{プラグボム ナノクラッシュ カウンタートラップ})])
                      (cc-superimpose background foreground figure))
                    nanomon)))