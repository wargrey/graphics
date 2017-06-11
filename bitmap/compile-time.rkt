#lang typed/racket

(require (for-syntax racket/base (submod "digitama/draw.rkt" untyped)))

(require "digitama/draw.rkt")

(provide compiled-bitmap compiled-bitmap-list)

(begin-for-syntax
  (define (save-png bm)
    (define p (open-output-bytes))
    (send bm save-file p 'png #:unscaled? #t)
    (define bs (get-output-bytes p))
    bs)
  
  (define (make-3d-bitmap ctxt bm)
    (with-syntax ([bs  (datum->syntax ctxt (save-png bm))]
                  [scale (send bm get-backing-scale)])
      (syntax/loc ctxt (load-png bs scale)))))

(define (load-png [bs : Bytes] [scale : Positive-Real]) : (Instance Bitmap%)
  (read-bitmap (open-input-bytes bs) 'png/alpha #:backing-scale scale))

(define-syntax (compiled-bitmap stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (let-syntax ([maker (λ (inner-stx)
                             (define bm expr)
                             (unless (is-a? bm bitmap%)
                               (raise-syntax-error
                                'compiled-bitmap
                                (format "expected argument of type <bitmap%>; given ~e" bm)
                                #'expr))
                             (make-3d-bitmap inner-stx bm))])
         (maker)))]))

(define-syntax (compiled-bitmap-list stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (let-syntax ([maker (λ (inner-stx)
                             (define bms expr)
                             (unless (and (list? bms) (andmap (λ (bm) (is-a? bm bitmap%)) bms))
                               (raise-syntax-error
                                'compiled-bitmap-list
                                (format "expected argument of type <list of bitmap%>; given ~e" bms)
                                #'expr))
                             (with-syntax ([(bm (... ...)) (map (λ (e) (make-3d-bitmap inner-stx e)) bms)])
                               #'(list bm (... ...))))])
         (maker)))]))
