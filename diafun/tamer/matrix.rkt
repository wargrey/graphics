#lang typed/racket/base

(require diafun/matrix)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx
  ((inst dia-array Byte) #:row-header-desc #("1" "2" "Row Three") #:col-header-desc #("1" "2" "Column Three" "4" "5")
                         #:col-header-rotate -1.57
                         #:mask? < #:hole? odd?
                         #:ncols 4
                         (range 1 17)
                         32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  mtx)
