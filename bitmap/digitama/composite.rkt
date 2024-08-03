#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

(require "convert.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-combiner stx)
  (syntax-case stx []
    [(_ frmt #:-> (Extra-Type ...) #:with alignment bitmaps [extra-args ...]
        #:blend-mode [blend blend-expr] #:empty blank-expr
        #:short-path #:for base bmp #:if short-path-condition ([(tip) short-path-expr] ...) #:do sexp ...)
     (with-syntax ([(bitmap-combiner ...)
                    (for/list ([<tip> (in-list (syntax->list #'(tip ...)))])
                      (datum->syntax <tip> (string->symbol (format (syntax-e #'frmt) (syntax-e <tip>)))))])
       (syntax/loc stx
         (begin (define bitmap-combiner : (-> (Listof Bitmap) Extra-Type ... Bitmap)
                  (let ([alignment 'tip])
                    (λ [bitmaps extra-args ...]
                      (let ([blend blend-expr])
                        (cond [(null? bitmaps) blank-expr]
                              [(null? (cdr bitmaps)) (car bitmaps)]
                              [(and short-path-condition (null? (cddr bitmaps)))
                               (let-values ([(base bmp) (values (car bitmaps) (cadr bitmaps))])
                                 short-path-expr)]
                              [else sexp ...])))))
                ...)))]))

(define-syntax (bitmap-expand-args  stx)
  (syntax-case stx []
    [(_ in type? default)
     (syntax/loc stx
       (cond [(null? in) (list default)]
             [(type? in) (list in)]
             [else in]))]
    [(_ in type? default in->out)
     (syntax/loc stx
       (cond [(null? in) (list default)]
             [(type? in) (list (in->out in))]
             [else (map in->out in)]))]))

(define-type Superimpose-Alignment (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))

(define-type Bitmap-Pin
  (case-> [Bitmap Complex Bitmap -> Bitmap]
          [Bitmap Real Real Bitmap -> Bitmap]
          [Bitmap Complex Bitmap Complex -> Bitmap]
          [Bitmap Real Real Bitmap Real Real -> Bitmap]))

(define-enumeration* bitmap-composition-operator #:+> Bitmap-Composition-Operator ; order matters
  bitmap-operator->integer integer->bitmap-operator
  [0 clear source over in out atop dest dest-over dest-in dest-out dest-atop xor add saturate
     multiply screen overlay darken lighten color-dodge color-burn hard-light soft-light difference exclusion
     hsl-hue hsl-saturation hsl-color hsl-liminosity])
