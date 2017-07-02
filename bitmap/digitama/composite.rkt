#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)
(require racket/list)

(require "digicore.rkt")
(require "draw.rkt")
(require "misc.rkt")

(require "unsafe/draw.rkt")

(require (for-syntax racket/base))

(define-syntax (define-combiner stx)
  (syntax-case stx []
    [(_ [make frmt (tips ...)] ...)
     (with-syntax ([([(bitmap-combiner make-combiner) ...] ...)
                    (for/list ([<tips> (in-list (syntax->list #'([tips ...] ...)))]
                               [<mkcb> (in-list (syntax->list #'(make ...)))]
                               [<frmt> (in-list (syntax->list #'(frmt ...)))])
                      (define frmt (syntax-e <frmt>))
                      (for/list ([<tip> (in-list (syntax->list <tips>))])
                        (list (datum->syntax <tip> (string->symbol (format frmt (syntax-e <tip>))))
                              <mkcb>)))])
       #'(begin (define-values (bitmap-combiner ...) (values (make-combiner 'tips) ...))
                ...))]))

(define-type Superimpose-Alignment (U 'lt 'lc 'lb 'ct 'cc 'cb 'rt 'rc 'rb))

(define-enumeration* bitmap-blend-mode #:+> Bitmap-Blend-Mode ; order matters
  bitmap-operator->integer integer->bitmap-operator
  [0 clear source over in out atop dest dest-over dest-in dest-out dest-atop xor add saturate
     multiply screen overlay darken lighten color-dodge color-burn hard-light soft-light difference exclusion
     hsl-hue hsl-saturation hsl-color hsl-liminosity])

(define list->n:vector : (All (a) (-> (Listof a) Integer a (Vectorof a)))
  (lambda [src total defval] ; NOTE: (length src) is usually small. 
    (define diff : Integer (fx- total (length src)))
    (cond [(= diff total) (make-vector total defval)]
          [(> diff 0) (list->vector (append src (make-list diff (last src))))]
          [else (list->vector src)])))
