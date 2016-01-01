#lang at-exp typed/racket

(provide (all-defined-out))

@require{digicore.rkt}

(define-syntax (define-sysno stx)
  (syntax-case stx [:]
    [(_ platform : Type [E v1 v2 v3] ...)
     #'(begin (define E : Type
                (case platform [(illumos) v1] [(macosx) v2] [else v3]))
              ...)]))

(define-sysno (digimon-system) : Natural
  [ECONNRESET   131 54 104]
  [ECONNREFUSED 146 61 111])
