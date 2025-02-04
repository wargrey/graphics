#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require digimon/digitama/unsafe/release/ops)

(require "type.rkt")
(require "position.rkt")
(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-list->pyramid : (->* (Geo (Listof Geo)) ((Option Geo))
                                 (Values (Listof (Listof Geo)) Nonnegative-Flonum Nonnegative-Flonum Index))
  (lambda [base siblings [defobj #false]]
    (define-values (min-width min-height) (geo-flsize base))
    (let compose ([width : Nonnegative-Flonum min-width]
                  [height : Nonnegative-Flonum min-height]
                  [gnilbis : (Listof Geo) null]
                  [idx : Nonnegative-Fixnum 0]
                  [count : Index 2]
                  [siblings : (Listof Geo) siblings]
                  [sreyal : (Listof (Listof Geo)) (list (list base))])
      (cond [(pair? siblings)
             (let*-values ([(self rest) (values (car siblings) (cdr siblings))]
                           [(w h) (geo-flsize self)])
               (if (< idx count)
                   (compose (max width w) (max height h) (cons self gnilbis) (+ idx 1) count rest sreyal)
                   (compose (max width w) (max height h) (list self) 1 (unsafe-idx+ count 1) rest (cons gnilbis sreyal))))]
            [(pair? gnilbis)
             (let ([diff (- count idx)])
               (values (reverse (cons (if (and defobj (> diff 0))
                                          (append (reverse gnilbis)
                                                  (make-list diff defobj))
                                          (reverse gnilbis))
                                      sreyal))
                       width height (assert diff index?)))]
            [else (values (reverse sreyal) width height 0)]))))
