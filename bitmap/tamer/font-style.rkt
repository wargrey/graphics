#lang racket

(require bitmap)

(require "../digitama/font.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exfont (desc-font #:family 'fantasy))

(define (font-style-table ex-chars options update-font hgapsize)
  (bitmap-table*
   (for/list ([property (in-list (cons '|| options))])
     (cons (bitmap-text (~a property) exfont #:color 'green)
           (apply append
                  (for/list ([exchar (in-list ex-chars)])
                    (cons (bitmap-blank)
                          (for/list ([style (in-list css-font-style-options)])
                            (cond [(eq? property '||) (bitmap-text (~a style) exfont)]
                                  [else (bitmap-text (~a " " exchar " ")
                                                     (update-font exfont property style))])))))))
   'cc 'cc hgapsize (* hgapsize 0.618)))

(font-style-table (list "a" "N") css-font-stretch-options
                  (λ [f p s] (desc-font f #:stretch p #:style s))
                  (font-size exfont))

(font-style-table (list "A" "永") css-font-weight-options
                  (λ [f p s] (desc-font f #:weight p #:style s))
                  (font-size exfont))
