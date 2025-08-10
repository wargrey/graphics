#lang typed/racket

(require bitmap)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ring-thickness : Flonum 32.0)
(define ring-colors : (Listof Symbol) '(royalblue crimson lime purple chocolate khaki))
(define monospace : Font (desc-font #:family 'monospace #:size 16.0))

(define colors : (Listof Bitmap)
  (build-list 16 (λ [[i : Index]]
                  (let ([rc (random #xFFFFFF)])
                    (geo-freeze
                     (geo-text #:color (rgb* rc)
                               (string-upcase (format "~a: ~x" (add1 i) rc))
                               monospace))))))

(bitmap-hb-append #:gapsize 8.0
                  (bitmap-vl-append* #:gapsize -32.0 colors)
                  (bitmap-vl-append* colors))

(bitmap-cc-superimpose
 
 (bitmap-cc-superimpose*
  (build-list 4 (λ [[i : Index]]
                  (let ([rc (random #xFFFFFF)]
                        [fs (* 8.0 (add1 i))])
                    (geo-freeze
                     (geo-text #:color (rgb* rc)
                               (string-upcase (format "~a" fs))
                               (desc-font #:family 'monospace #:size fs)))))))

 (bitmap-cc-superimpose*
  (build-list (length ring-colors)
              (λ [[i : Index]]
                (let ([arclength (degrees->radians (/ 360.0 (length ring-colors)))])
                  (geo-freeze
                   (geo-arc #:stroke (desc-stroke #:width ring-thickness #:color (list-ref ring-colors i))
                            (* ring-thickness 4.0)
                            (* i arclength)
                            (* (+ i 1) arclength))))))))

(bitmap-cc-superimpose*
 (build-list (length ring-colors)
             (λ [[i : Index]]
                 (let ([arclength (degrees->radians (/ 360.0 (length ring-colors)))])
                   (geo-freeze
                    (geo-sector #:fill (list-ref ring-colors i) #:stroke 'ghostwhite
                                (* ring-thickness 4.0)
                                (* i arclength)
                                (* (+ i 1) arclength)))))))
