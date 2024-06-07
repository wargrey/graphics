#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-pdf-title : (Parameterof (Option String)) (make-parameter #false))
(define default-pdf-author : (Parameterof (Option String)) (make-parameter #false))
(define default-pdf-subject : (Parameterof (Option String)) (make-parameter #false))
(define default-pdf-keywords : (Parameterof (Option String)) (make-parameter #false))
(define default-pdf-producer : (Parameterof (Option String)) (make-parameter #false))
