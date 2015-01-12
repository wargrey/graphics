#lang racket

(require rackunit)

(require "../../digitama/runtimepath.rkt")

(provide (all-from-out racket "../../digitama/runtimepath.rkt"))
(provide (all-from-out rackunit))

(provide (all-defined-out))

(digimon-setenv digimon-gnome)

(define-values {digimon-makefile tamer-makefile}
  (values `(file ,(path->string (build-path (getenv "digimon-zone") "makefile.rkt")))
          `(submod (file ,(path->string (build-path (path-only (syntax-source #'digitama)) "makefile.rkt"))) main)))
