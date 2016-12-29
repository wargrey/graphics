#lang typed/racket/gui

(provide (all-defined-out))

(require digimon/format)
(require racket/runtime-path)

(require "../syntax.rkt")

(define-syntax (time-run stx)
  (syntax-case stx []
    [(_ sexp ...)
     #'(let ([momery0 : Natural (current-memory-use)])
         (define-values (result cpu real gc) (time-apply (thunk sexp ...) null))
         (printf "memory: ~a cpu time: ~a real time: ~a gc time: ~a~n"
                 (~size (- (current-memory-use) momery0) 'Bytes)
                 cpu real gc)
         (car result))]))

(define DrRacket? : Boolean (regexp-match? #px"DrRacket$" (find-system-path 'run-file)))
(define-runtime-path tamer/tamer.css "tamer.css")
(define-runtime-path tamer/bitmap.css "bitmap.css")

(define css-configure-@media : (-> Void)
  (lambda []
    (define-values (width height) (get-display-size))
    (css-deprecate-media-type #true)
    (default-css-media-type 'screen)
    (default-css-media-preferences
      ((inst make-hash Symbol CSS-Media-Datum)
       (list (cons 'orientation 'landscape)
             (cons 'width (or width 0))
             (cons 'height (or height 0)))))))
