#lang typed/racket

(require bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ring-thickness : Flonum 32.0)
(define ring-colors : (Listof Symbol) '(royalblue crimson lime purple chocolate khaki))
(define monospace : Font (desc-font #:family 'monospace #:size 16.0))

(bitmap-vl-append* #:gapsize -8.0
 (build-list 16 (λ [[i : Index]]
                  (let ([rc (random #xFFFFFF)])
                    (bitmap-text #:color (rgb* rc)
                                 (string-upcase (format "~a: ~x" (add1 i) rc))
                                 monospace)))))

(bitmap-cc-superimpose
 
 (bitmap-cc-superimpose*
  (build-list 4 (λ [[i : Index]]
                  (let ([rc (random #xFFFFFF)]
                        [fs (* 8.0 (add1 i))])
                    (bitmap-text #:color (rgb* rc)
                                 (string-upcase (format "~a" fs))
                                 (desc-font #:family 'monospace #:size fs))))))

 (bitmap-cc-superimpose*
  (build-list (length ring-colors)
              (λ [[i : Index]]
                (let ([arclength (/ 360.0 (length ring-colors))])
                  (bitmap-arc #:stroke (desc-stroke #:width ring-thickness #:color (list-ref ring-colors i))
                              #:radian? #false
                              (* ring-thickness 4.0)
                              (* i arclength)
                              (* (+ i 1) arclength)))))))

(bitmap-cc-superimpose*
 (build-list (length ring-colors)
             (λ [[i : Index]]
                 (let ([arclength (/ 360.0 (length ring-colors))])
                   (bitmap-sector #:fill (list-ref ring-colors i) #:border 'ghostwhite
                                  #:radian? #false
                                  (* ring-thickness 4.0)
                                  (* i arclength)
                                  (* (+ i 1) arclength))))))
