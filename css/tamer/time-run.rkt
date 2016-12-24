#lang typed/racket

(provide (all-defined-out))

(require digimon/format)

(define-syntax (time-run stx)
  (syntax-case stx []
    [(_ sexp ...)
     #'(let ([momery0 : Natural (current-memory-use)])
         (define-values (result cpu real gc) (time-apply (thunk sexp ...) null))
         (printf "memory: ~a cpu time: ~a real time: ~a gc time: ~a~n"
                 (~size (- (current-memory-use) momery0) 'Bytes)
                 cpu real gc)
         (car result))]))
