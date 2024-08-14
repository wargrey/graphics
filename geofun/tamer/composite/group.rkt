#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ring-thickness : Flonum 32.0)
(define ring-colors : (Listof Symbol) '(royalblue crimson lime purple chocolate khaki))
(define monospace : Font (desc-font #:family 'monospace #:size 16.0))

(define colors : (Listof Geo<%>)
  (build-list 16 (λ [[i : Index]]
                  (let ([rc (random #xFFFFFF)])
                    (geo-text #:color (rgb* rc)
                              (string-upcase (format "~a: ~x" (add1 i) rc))
                              monospace)))))

(geo-hb-append #:gapsize 8.0
 (geo-vl-append* #:gapsize -32.0 colors)
 (geo-vl-append* colors))

(geo-cc-superimpose
 
 (geo-cc-superimpose*
  (build-list 4 (λ [[i : Index]]
                  (let ([rc (random #xFFFFFF)]
                        [fs (* 8.0 (add1 i))])
                    (geo-text #:color (rgb* rc)
                              (string-upcase (format "~a" fs))
                              (desc-font #:family 'monospace #:size fs))))))

 (geo-cc-superimpose*
  (build-list (length ring-colors)
              (λ [[i : Index]]
                (let ([arclength (/ 360.0 (length ring-colors))])
                  (geo-arc #:stroke (desc-stroke #:width ring-thickness #:color (list-ref ring-colors i))
                           #:radian? #false
                           (* ring-thickness 4.0)
                           (* i arclength)
                           (* (+ i 1) arclength)))))))

(geo-cc-superimpose*
 (build-list (length ring-colors)
             (λ [[i : Index]]
                 (let ([arclength (/ 360.0 (length ring-colors))])
                   (geo-sector #:fill (list-ref ring-colors i) #:border 'ghostwhite
                               #:radian? #false
                               (* ring-thickness 4.0)
                               (* i arclength)
                               (* (+ i 1) arclength))))))
