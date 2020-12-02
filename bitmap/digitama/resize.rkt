#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

(define-syntax (define-cropper stx)
  (syntax-case stx [: ->]
    [(_ bitmap-crop : (-> Args ... Bitmap) (#:lambda λargs λmain ...)
        #:with (frmt [tips left% top%] ...))
     (with-syntax ([(bitmap-cropper ...)
                    (let ([bitmap-~a-crop (syntax-e #'frmt)])
                      (for/list ([<tip> (in-list (syntax->list #'(tips ...)))])
                        (datum->syntax <tip> (string->symbol (format bitmap-~a-crop (syntax-e <tip>))))))])
       (syntax/loc stx
         (begin (define bitmap-crop : (-> Args ... Nonnegative-Flonum Nonnegative-Flonum Bitmap)
                  (lambda λargs λmain ...))
                (define bitmap-cropper : (-> Args ... Bitmap)
                  (lambda [bmp w h] (bitmap-crop bmp w h left% top%))) ...)))]))

#;(define-enumeration* filter-option #:+> Filter-Algorithm ; order matters
  filter-algorithm->integer integer->filter-algorithm
  [0 fast good best nearest bilinear gaussian])
