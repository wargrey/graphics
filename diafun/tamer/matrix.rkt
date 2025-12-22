#lang typed/racket/base

(require diafun/matrix)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx
  ((inst dia-matrix Byte) #:row-header-desc #("1" "2" "Row Three") #:col-header-desc #("1" "2" "Column Three" "4" "5")
                          #:col-header-rotate -1.57
                          #:mask? < #:hole? odd?
                          4 (range 1 17)
                          32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  mtx)
