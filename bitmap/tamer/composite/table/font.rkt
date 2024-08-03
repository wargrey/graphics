#lang racket

(provide font-style-table)

(require bitmap)
(require pangocairo/digitama/font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exgap (bitmap-blank))

(define (font-style-table exfont ex-chars options update-font)
  (define hgapsize (font-size exfont))
  
  (bitmap-table*
   (for/list ([property (in-list (cons '|| options))])
     (cons (bitmap-text (~a property) exfont #:color 'green)
           (apply append
                  (for/list ([exchar (in-list ex-chars)])
                    (cons exgap
                          (for/list ([style (in-list css-font-style-options)])
                            (cond [(eq? property '||) (bitmap-text (~a style) exfont)]
                                  [else (bitmap-text (~a " " exchar " ")
                                                     (update-font exfont property style))])))))))
   'cc 'cc hgapsize (* hgapsize 0.618)))

(module+ main
  (define exfont (desc-font #:family 'fantasy))
  
  (font-style-table exfont
                    (list "a" "N") css-font-stretch-options
                    (λ [f p s] (desc-font f #:stretch p #:style s)))
  
  (font-style-table exfont
                    (list "A" "永") css-font-weight-options
                    (λ [f p s] (desc-font f #:weight p #:style s))))
