#lang typed/racket/base

(provide prefab-bitmap prefab-bitmap-list)

(require (for-syntax racket/base))
(require (for-syntax (submod "digitama/unsafe/prefab.rkt" unsafe)))

(require "draw.rkt")
(require "digitama/unsafe/prefab.rkt")

(begin-for-syntax
  (define (make-3d-bitmap ctxt bm)
    (with-syntax ([(raw width height density) (datum->syntax ctxt (bitmap-save bm))])
      (syntax/loc ctxt (bitmap-restore raw width height density)))))

(define-syntax (prefab-bitmap stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (let-syntax ([maker (λ (inner-stx)
                             (define bm expr)
                             (unless (is-a? bm bitmap%)
                               (raise-syntax-error
                                'prefab-bitmap
                                (format "expected argument of type <bitmap%>; given ~e" bm)
                                #'expr))
                             (make-3d-bitmap inner-stx bm))])
         (maker)))]))

(define-syntax (prefab-bitmap-list stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (let-syntax ([maker (λ (inner-stx)
                             (define bms expr)
                             (unless (and (list? bms) (andmap (λ (bm) (is-a? bm bitmap%)) bms))
                               (raise-syntax-error
                                'prefab-bitmap-list
                                (format "expected argument of type <list of bitmap%>; given ~e" bms)
                                #'expr))
                             (with-syntax ([(bm (... ...)) (map (λ (e) (make-3d-bitmap inner-stx e)) bms)])
                               #'(list bm (... ...))))])
         (maker)))]))
