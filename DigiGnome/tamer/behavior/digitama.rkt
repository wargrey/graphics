#lang racket

(require rackunit)

(require racket/runtime-path)

(require "../../digitama/runtimepath.rkt")

(provide (all-from-out racket "../../digitama/runtimepath.rkt"))
(provide (all-from-out rackunit))

(provide (except-out (all-defined-out) compiled-syntax-source-directory))

(define-runtime-path compiled-syntax-source-directory ".")

(digimon-setenv digimon-gnome)

(define-values {digimon-makefile tamer-makefile}
  (values `(file ,(path->string (build-path (getenv "digimon-zone") "makefile.rkt")))
          `(submod (file ,(path->string (build-path (path-only (simplify-path compiled-syntax-source-directory)) "makefile.rkt"))) main)))
