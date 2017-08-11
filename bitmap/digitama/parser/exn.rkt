#lang typed/racket/base

(provide (except-out (all-defined-out) exn-args->message))

(struct exn:fail:read:signature exn:fail:read () #:extra-constructor-name make-exn:fail:read:signature)

(define throw-eof-error : (-> Input-Port Nothing)
  (lambda [/dev/stdin]
    (raise (make-exn:fail:read:eof "unexpected end of file!"
                                   (continuation-marks #false)
                                   null))))

(define throw-read-error : (-> Input-Port Symbol Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:fail:read (format "~a: ~a" (exn-args->message src args) (object-name /dev/stdin))
                               (continuation-marks #false)
                               null))))

(define throw-signature-error : (-> Input-Port Symbol Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:fail:read:signature (format "~a: ~a" (exn-args->message src args) (object-name /dev/stdin))
                                         (continuation-marks #false)
                                         null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exn-args->message : (-> Symbol (Listof Any) String)
  (lambda [src args]
    (cond [(null? args) (symbol->string src)]
          [(string? (car args)) (apply format (string-append "~a: " (car args)) src (cdr args))]
          [else (format "~a: ~s" src args)])))
