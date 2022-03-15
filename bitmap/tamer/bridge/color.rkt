#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/base)

(require racket/format)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define label-font (desc-font #:family 'monospace #:size 'large))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define named-color-list : (-> Bitmap)
  (lambda []
    (define all-names (sort (list-color-names) symbol<?))
    (define-values (size mod) (quotient/remainder (length all-names) 2))
      
    (bitmap-table*
     (append (for/list : (Listof (Listof Bitmap)) ([lname (in-list (take all-names size))]
                                                   [rname (in-list (take-right all-names size))])
               (append (name->row lname) (name->row rname)))
             (if (= mod 0) null (list (name->row (list-ref all-names (add1 size))))))
     'lc 'cc 16.0 4.0)))

(define xterm-color-list : (->* () (Color) Bitmap)
  (lambda [[bgcolor 'black]]
    (bitmap-frame #:fill bgcolor #:padding 4.0
                  (bitmap-table 16
                                (for/list ([xidx (in-range 256)])
                                  (bitmap-text #:color (xterm (assert xidx byte?))
                                               (number->string xidx) label-font))
                                'cc 'cc 8.0 8.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (name->row [name : Symbol]) : (List Bitmap Bitmap Bitmap)
  (define c (rgb* name))
  (list (bitmap-square (font-size label-font) #:fill c)
        (bitmap-text (symbol->string name) label-font)
        (bitmap-text (~a #\# (string-upcase (~r (flcolor->hex c) #:base 16 #:min-width 6 #:pad-string "0"))) label-font)))
