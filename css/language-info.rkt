#lang racket/base

(provide (all-defined-out))

(define css-read
  (lambda [in]
    (displayln (object-name in))
    (displayln (read-line in))
    (read in)))
  
(define css-read-syntax
  (lambda [src in]
    (read-syntax src in)))
  
(define css-language-info
  (lambda [argument]
    (λ [key default]
      (case key
        [else default]))))

(define css-info
  (lambda [key default use-default]
    (case key
      [else (use-default key default)])))
