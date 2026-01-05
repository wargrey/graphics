#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-cropper stx)
  (syntax-case stx [: ->]
    [(_ geo-crop : (-> Args ... Geo) (#:lambda λargs λmain ...)
        #:with (frmt [tips left% top%] ...))
     (with-syntax ([(geo-cropper ...)
                    (let ([geo-~a-crop (syntax-e #'frmt)])
                      (for/list ([<tip> (in-list (syntax->list #'(tips ...)))])
                        (datum->syntax <tip> (string->symbol (format geo-~a-crop (syntax-e <tip>))))))])
       (syntax/loc stx
         (begin (define geo-crop : (-> Args ... Nonnegative-Flonum Nonnegative-Flonum Geo)
                  (lambda λargs λmain ...))
                (define geo-cropper : (-> Args ... Geo)
                  (lambda [self w h] (geo-crop self w h left% top%))) ...)))]))
