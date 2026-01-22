#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)

(require racket/format)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define label-font (desc-font #:family 'monospace #:size 'large))
(define border-pen (desc-border #:width 4.0 #:color 'RoyalBlue #:opacity 0.5))
(define open-sides (make-geo-open-sides #:top-span (&% 90) #:right-span (&% 90)
                                        #:bottom-span (&% 90) #:left-span (&% 90)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define named-color-list : (->* () ((-> Symbol Symbol Boolean)) Geo)
  (lambda [[< symbol<?]]
    (define all-names (sort (list-color-names) <))
    (define-values (size mod) (quotient/remainder (length all-names) 2))
      
    (geo-table*
     (append (for/list : (Listof (Listof Geo)) ([lname (in-list (take all-names size))]
                                                   [rname (in-list (take-right all-names size))])
               (append (name->row lname) (name->row rname)))
             (if (= mod 0) null (list (name->row (list-ref all-names (add1 size))))))
     'lc 'cc 16.0 4.0)))

(define xterm-color-list : (->* () (Color) Geo)
  (lambda [[bgcolor 'black]]
    (geo-table #:border border-pen #:open-sides open-sides
               #:background bgcolor #:padding 8.0 #:margin 8.0
               16
               (for/list : (Listof Geo) ([xidx (in-range 256)])
                 (geo-text #:color (xterm (assert xidx byte?))
                           (number->string xidx) label-font))
               'cc 'cc 8.0 8.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (name->row [name : Symbol]) : (List Geo Geo Geo)
  (define c (rgb* name))
  (list (geo-square (font-size label-font) #:fill c)
        (geo-text (symbol->string name) label-font)
        (geo-text (~a #\# (string-upcase (~r (flcolor->hex c) #:base 16 #:min-width 6 #:pad-string "0"))) label-font)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (named-color-list)
  (xterm-color-list 'white))
