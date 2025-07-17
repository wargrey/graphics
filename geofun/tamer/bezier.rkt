#lang typed/racket/base

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pen (desc-stroke #:color 'RoyalBlue #:dash 'long-dash))
(define aux (desc-stroke #:color 'Gray #:dash 'long-dash))
(define otl (desc-stroke #:color 'ForestGreen #:dash 'long-dash))

(define cubic-bezier-curve : (-> Complex Complex Complex Complex Geo)
  (lambda [spt ctrl1 ctrl2 ept]
    (geo-cc-superimpose
     (geo-polybezier #:stroke aux #:close? #t ctrl2 ctrl1 spt (list ctrl1 ctrl2 ept))
     (geo-polybezier #:stroke pen #:close? #f spt (list ctrl1 ctrl2 ept))
     (geo-trim (geo-bezier #:stroke otl #:close? #f spt ctrl1 ctrl2 ept)))))

(define linear-bezier
  (geo-hc-append
   (geo-bezier #:stroke pen 100+75i 70+155i)
   (geo-polybezier #:stroke pen (list 100+75i 70+155i))
   (geo-polybezier #:stroke pen +nan.0 (list 100+75i 70+155i))))

(define quadratic-bezier
  (geo-cc-superimpose
   (geo-polybezier #:stroke aux #:close? #t 20+110i 220+60i (list 20+110i 70+250i))
   (geo-polybezier #:stroke pen #:close? #f (list 220+60i 20+110i 70+250i))))

(define cubic-bezier (cubic-bezier-curve 110+150i 25+190i 210+250i 210+30i))

(define nth-bezier
  (geo-cc-superimpose
   (geo-polybezier #:stroke aux
                    198+18i   34+57i 18+156i 221+90i
                   186+177i   14+82i 12+236i 45+290i
                   218+294i 248+188i +nan.0
                   (list 198+18i   34+57i 18+156i 221+90i
                         186+177i   14+82i 12+236i 45+290i
                         218+294i 248+188i))
   (geo-bezier #:stroke pen
               198+18i   34+57i 18+156i 221+90i
               186+177i   14+82i 12+236i 45+290i
               218+294i 248+188i)))

(define flipped-bezier
  (geo-cc-superimpose
   (geo-polybezier #:stroke pen #:scale +2 25+25i 100+75i (list 20+110i +nan.0 70+155i))
   (geo-polybezier #:stroke otl #:scale -2 25+25i 100+75i (list 20+110i +nan.0 70+155i))))

(module+ main
  linear-bezier
  quadratic-bezier
  cubic-bezier
  nth-bezier
  flipped-bezier

  (cubic-bezier-curve 60+105i 75+30i 215+115i 140+160i)
  (cubic-bezier-curve 25+128i 102.4+230.4i 153.6+25.6i 230.4+128i))
