#lang typed/racket/base

(require diafun/matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx
  ((inst dia-matrix* Byte) #:row-header-desc #("1" "2" "Row Three") #:col-header-desc #("1" "2" "Column Three" "4" "5")
                           #:col-header-rotate -1.57
                           #:mask? < #:hole? odd?
                           #[#[ 1  2  3  4]
                             #[ 5  6  7  8]
                             #[ 9 10 11 12]
                             #[13 14 15 16]]
                           32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  mtx)
