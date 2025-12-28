#lang racket/base

(provide font-style-table)

(require geofun/vector)
(require geofun/digitama/font)

(require racket/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (font-style-table exfont ex-chars options update-font)
  (define hgapsize (font-size exfont))
  
  (geo-table*
   (for/list ([property (in-list (cons '|| options))])
     (cons (geo-text (~a property) exfont #:color 'green)
           (apply append
                  (for/list ([exchar (in-list ex-chars)])
                    (cons the-void-geo
                          (for/list ([style (in-list css-font-style-options)])
                            (cond [(eq? property '||) (geo-text (~a style) exfont)]
                                  [else (geo-text (~a " " exchar " ")
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
