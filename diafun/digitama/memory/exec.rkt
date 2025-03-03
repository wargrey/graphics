#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require digimon/cc)
(require digimon/wisemon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-name : Symbol 'dia-memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-build : (-> Path Boolean Path)
  (lambda [src.c optimize?]
    (define c.o : Path (c-optimized-path (assert (c-source->object-file src.c)) optimize?))
    (define c.so : Path (c-optimized-path (assert (c-source->shared-object-file src.c #false #:lib-prefixed? #false)) optimize?))
    (define cpp? : Boolean (not (regexp-match? #px"\\.c$" src.c)))
    (define verbose? : Boolean #false)

    (define specs : Wisemon-Specification
      (list (wisemon-spec c.o #:^ (cons src.c (c-include-headers src.c #:topic the-name))
                          #:- (c-compile #:standard 2017 #:cpp? cpp? #:verbose? verbose? #:optimize? optimize?
                                         #:macros (list '__racket__)
                                         src.c c.o))
            
            (wisemon-spec c.so #:^ (list c.o)
                          #:- (c-link #:cpp? cpp? #:verbose? verbose?
                                      #:subsystem #false #:entry #false
                                      c.o c.so))))

    (wisemon-make specs (list c.so) #:name the-name)
    c.so))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-optimized-path : (-> Path Boolean Path)
  (lambda [p optimize?]
    (cond [(and optimize?) p]
          [else (build-path (assert (path-only p))
                            "O0"
                            (assert (file-name-from-path p)))])))
