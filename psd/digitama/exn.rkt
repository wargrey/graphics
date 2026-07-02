#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/digitama/ioexn))

(require racket/format)
(require digimon/digitama/ioexn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define throw-obsolete-error : (-> Integer Any * Nothing)
  (lambda [id . args]
    (define-values (idstr message) (psd-args->message id args))
    
    (raise (exn:fail:unsupported (format "obsolete resource: ~a" message)
                                 (current-continuation-marks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-warn-broken-resource : (-> exn False)
  (lambda [e]
    (log-message (current-logger) 'warning 'exn:psd (exn-message e) e)
    #false))

(define psd-warn-broken-information : (-> exn False)
  (lambda [e]
    (log-message (current-logger) 'warning 'exn:psd (exn-message e) e)
    #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-id->string : (-> Integer String)
  (lambda [id]
    (define ID : String (~r id #:base 16 #:min-width 4 #:pad-string "0"))
    (format "0x~a(~a)" (string-upcase ID) id)))

(define psd-args->message : (-> Integer (Listof Any) (Values String String))
  (lambda [id args]
    (define idstr : String (psd-id->string id))
    (define message : String
      (cond [(null? args) idstr]
            [(string? (car args)) (apply format (string-append idstr ": " (car args)) (cdr args))]
            [else (format "~a: ~s" idstr args)]))
    (values idstr message)))

