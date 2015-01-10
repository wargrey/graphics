#lang at-exp racket/gui

(require images/icons/control)
(require images/icons/misc)
(require (only-in plot/utils color-seq*))

(require "../../d-ark.rkt")

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

(define rallies (list (cons 80 100) (cons 94 188) (cons 100 192) (cons 102 193) (cons 117 165) (cons 143 143) (cons 162 200)
                      (cons 168 182) (cons 232 291) (cons 235 266) (cons 269 250) (cons 245 252) (cons 216 227) (cons 228 220)
                      (cons 230 242) (cons 232 208) (cons 225 209) (cons 205 240) (cons 188 226) (cons 191 232) (cons 211 211)
                      (cons 221 202) (cons 217 208)))

(define miko-mode
  {lambda [sakuyamon]
    (define figure 'Sakuyamon_miko.jpg)
    (digimon-ark (struct-copy digimon sakuyamon [figure (cons figure (bitmap->flomap (wikimon-image figure)))])
                 #:lightness 0.85 #:rallies (list (cons 111 150) (cons 124 161) (cons 125 160) (cons 127 160)))})

(define profile
  {lambda [monster]
    (define fsize (plot-font-size))
    (define flcolor (color->flvector metal-icon-color))
    (define hbar (ghost (bitmap (bar-icon #:height fsize #:color metal-icon-color))))
    (define {fdigimoji txt [color light-metal-icon-color]} (digimoji (~a txt) #:height fsize #:color color))
    (define {fbg fg [wdth #false]} (let ([width (if wdth wdth (exact-round (+ fsize (pict-width fg))))]
                                         [height (exact-round (+ fsize (pict-height fg)))])
                                     (cc-superimpose (cellophane (bitmap (flomap->bitmap (flomap-shadow (make-flomap* width height flcolor) fsize flcolor))) 1.00) fg)))
    (define head (apply vr-append
                        (fbg (vc-append (pict-width hbar) (fdigimoji "Digital Monster") (fdigimoji (digimon-kana monster))) (plot-width))
                        (map {lambda [emblem] (let ([% (/ (default-icon-height) (flomap-width emblem))])
                                                (cellophane (bitmap (flomap->bitmap (flomap-scale emblem %))) %))}
                             (digimon-fields monster))))
    (define body (let ([skill (apply vl-append (pict-width hbar) (map {lambda [attack] (hc-append (bitmap (bomb-icon #:height fsize)) hbar (fdigimoji attack))} (digimon-attacks monster)))])
                   (fbg (vl-append (desc (substring (digimon-profile monster) 0 23) (pict-width skill) #:ftext text #:height fsize #:color light-metal-icon-color) hbar skill))))
    (define watermark (vr-append (fdigimoji "wargrey" metal-icon-color) (rotate hbar (/ pi 2))))
    (rb-superimpose (rt-superimpose (lb-superimpose (blank (plot-width) (plot-height)) body) head) watermark)})

(let ([sakuya (recv-digimon 'Sakuyamon)])
  (cond [(digimon? sakuya) (let* ([figure (digimon-ark sakuya #:lightness 0.64 #:rallies rallies)]
                                  [background (miko-mode sakuya)]
                                  [foreground (profile sakuya)])
                             (cc-superimpose background foreground figure))]
        [else sakuya]))