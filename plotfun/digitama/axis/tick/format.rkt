#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require geofun/digitama/richtext/self)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-tick->label-text : Plot-Tick-Format
  (lambda [tval para]
    (cond [(exact? tval) (number->string tval)]
          [(integer? tval) (number->string (inexact->exact tval))]
          [(>= para 0) (~r tval #:precision para)]
          [else (~a tval)])))

(define plot-symbol-tick->label-text : (-> Geo-Rich-Text (HashTable Real Real) Plot-Tick-Format)
  (lambda [sym db]
    (define (symbol-tick->text [tval0 : Real] [para : Integer]) : Geo-Rich-Text
      (define tval (hash-ref db tval0 (λ [] tval0)))
      
      (cond [(zero? tval) (number->string tval)]
            [(integer? tval)
             (cond [(= tval 1) sym]
                   [(= tval -1) (~a #\- sym)]
                   [else (~a (inexact->exact tval) sym)])]
            [(exact? tval) (~a (symbol-tick->text (numerator tval) para) #\/ (denominator tval))]
            [(>= para 0) (~r tval #:precision para)]
            [else (~a tval sym)]))

    symbol-tick->text))
