#lang typed/racket

(require "../../constructor.rkt")
(require "../../composite.rkt")
(require "../../font.rkt")
(require "../../color.rkt")

(require "../../digitama/unsafe/convert.rkt")

(define monospace : Font (desc-font #:family 'monospace #:size 16.0))

(bitmap-vl-append* #:gapsize -8.0
 (build-list 16 (λ [[i : Index]]
                  (let ([rc (random #xFFFFFF)])
                    (bitmap-text #:color (rgb* rc)
                                 (string-upcase (format "~a: ~x" (add1 i) rc))
                                 monospace)))))

(bitmap-cc-superimpose*
 (build-list 4 (λ [[i : Index]]
                  (let ([rc (random #xFFFFFF)]
                        [fs (* 8.0 (add1 i))])
                    (bitmap-text #:color (rgb* rc)
                                 (string-upcase (format "~a" fs))
                                 (desc-font #:family 'monospace #:size fs))))))
