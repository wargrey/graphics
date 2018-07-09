#lang typed/racket/base

(provide (except-out (all-defined-out) exn-args->message exn-src+args->message))

(require racket/string)
(require racket/format)
(require racket/path)

(struct exn:fail:read:signature exn:fail:read () #:extra-constructor-name make-exn:read:signature)
(struct exn:fail:syntax:check exn:fail:syntax () #:extra-constructor-name make-exn:syntax:check)
(struct exn:fail:syntax:range exn:fail:syntax () #:extra-constructor-name make-exn:syntax:range)

(define throw-impossible-error : (-> Input-Port Symbol Nothing)
  (lambda [/dev/stdin fname]
    (raise (make-exn:fail:read:eof (format "~a: ~a: [I have a bug report]!" (port-name /dev/stdin) fname)
                                   (continuation-marks #false)
                                   null))))

(define throw-eof-error : (-> Input-Port Nothing)
  (lambda [/dev/stdin]
    (raise (make-exn:fail:read:eof (format "~a: unexpected end of file!" (port-name /dev/stdin))
                                   (continuation-marks #false)
                                   null))))

(define throw-read-error : (-> Input-Port Symbol Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:fail:read (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                               (continuation-marks #false)
                               null))))

(define throw-signature-error : (-> Input-Port Symbol Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:read:signature (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                                    (continuation-marks #false)
                                    null))))

(define throw-syntax-error : (-> Input-Port Symbol Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:fail:syntax (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                                 (continuation-marks #false)
                                 null))))

(define throw-check-error : (-> Input-Port Symbol Any * Nothing)
  (lambda [/dev/stdin src . args]
    (raise (make-exn:syntax:check (format "~a: ~a" (port-name /dev/stdin) (exn-src+args->message src args))
                                  (continuation-marks #false)
                                  null))))

(define throw-range-error : (-> Symbol (U Procedure String (Pairof Real Real) (Listof Any)) Any Any * Nothing)
  (lambda [src constraint given . args]
    (define message : String (exn-args->message args "out of range"))
    (define expected : String
      (cond [(string? constraint) constraint]
            [(procedure? constraint) (~a (object-name constraint))]
            [(pair? constraint) (format "[~a, ~a]" (car constraint) (cdr constraint))]
            [else (string-join #:before-first "(" ((inst map String Any) ~s constraint) ", " #:after-last ")")]))
    (raise (make-exn:syntax:range (format "~a: ~a~n expected: ~a~n given: ~s" src message expected given)
                                  (continuation-marks #false)
                                  null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define port-name : (-> Input-Port String)
  (lambda [/dev/stdin]
    (define portname : Any (object-name /dev/stdin))
    (cond [(path? portname) (format "~a" (file-name-from-path portname))]
          [(string? portname) portname]
          [else (format "~a" portname)])))

(define exn-args->message : (-> (Listof Any) String String)
  (lambda [args defmsg]
    (cond [(null? args) defmsg]
          [(string? (car args)) (apply format (car args) (cdr args))]
          [else (~s args)])))

(define exn-src+args->message : (-> Symbol (Listof Any) String)
  (lambda [src args]
    (cond [(null? args) (symbol->string src)]
          [(string? (car args)) (apply format (string-append "~a: " (car args)) src (cdr args))]
          [else (format "~a: ~s" src args)])))
