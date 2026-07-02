#lang typed/racket/base

;;; Documentation/IPTC/iimv4.pdf

(require "format.rkt")
(require "../parser.rkt")
(require "../exn.rkt")

(unsafe-provide 0x404)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x404 : (-> Integer String Bytes Fixnum Index Null PSD-File-Info)
  (lambda [id name iptc-naa idx size argl]
    (define max-idx : Fixnum (unsafe-fx+ idx size))
    (let parse ([start : Integer idx]
                [entries : (Listof (Pairof Complex Bytes)) null])
      (cond [(and (< start max-idx) (eq? (bytes-ref iptc-naa start) #x1C #|ASCII Code of File Separator|#))
             (define-values (record: dataset data-size data) (parse-dataset iptc-naa start))
             (parse (unsafe-fx+ (unsafe-fx+ start 5) data-size)
                    (cons (cons (make-rectangular record: dataset) data) entries))]
            [else (PSD-File-Info id name (reverse entries))]))))

(define parse-dataset : (-> Bytes Integer (Values Byte Byte Natural Bytes))
  (lambda [iptc-naa start]
    (define-values (record-number: dataset-number)
      ; IIM identifies dataset tags with shape `rn:dsn`
      (values (parse-uint8 iptc-naa (unsafe-fx+ start 1))
              (parse-uint8 iptc-naa (unsafe-fx+ start 2))))
    (define data-size : Fixnum (parse-int16 iptc-naa (unsafe-fx+ start 3)))
    (if (> data-size 0)
        (values record-number: dataset-number data-size
                (parse-iimv4 record-number: dataset-number iptc-naa (unsafe-fx+ start 5) data-size))
        (throw-unsupported-error (current-ioexn-input-port)
                                 'parse-dataset
                                 "confronted an extended dataset in resource: ~a"
                                 (psd-id->string 1028)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-iimv4 : (-> Byte Byte Bytes Fixnum Fixnum Bytes)
  (lambda [record dataset src start size]
    (case record
      [(1) (parse-object-envelope-record dataset src start size)]
      [(2) (parse-application-record dataset src start size)]
      [(3) (parse-digital-newsphoto-parameter-record dataset src start size)]
      [(4 5) (parse-not-allocated-record dataset src start size)]
      [(6) (parse-abstract-relationship-record dataset src start size)]
      [(7) (parse-pre-object-data-descriptor-record dataset src start size)]
      [(8) (parse-object-data-descriptor-record dataset src start size)]
      [(9) (parse-post-object-data-descriptor-record dataset src start size)]
      [else (parse-nbytes src start size)])))

(define parse-object-envelope-record : (-> Byte Bytes Fixnum Fixnum Bytes)
  (lambda [dataset src start size]
    (parse-nbytes src start size)))

(define parse-application-record : (-> Byte Bytes Fixnum Fixnum Bytes)
  (lambda [dataset src start size]
    (parse-nbytes src start size)))

(define parse-digital-newsphoto-parameter-record : (-> Byte Bytes Fixnum Fixnum Bytes)
  (lambda [dataset src start size]
    (parse-nbytes src start size)))

(define parse-not-allocated-record : (-> Byte Bytes Fixnum Fixnum Bytes)
  (lambda [dataset src start size]
    (parse-nbytes src start size)))

(define parse-abstract-relationship-record : (-> Byte Bytes Fixnum Fixnum Bytes)
  (lambda [dataset src start size]
    (parse-nbytes src start size)))

(define parse-pre-object-data-descriptor-record : (-> Byte Bytes Fixnum Fixnum Bytes)
  (lambda [dataset src start size]
    (parse-nbytes src start size)))

(define parse-object-data-descriptor-record : (-> Byte Bytes Fixnum Fixnum Bytes)
  (lambda [dataset src start size]
    (parse-nbytes src start size)))

(define parse-post-object-data-descriptor-record : (-> Byte Bytes Fixnum Fixnum Bytes)
  (lambda [dataset src start size]
    (parse-nbytes src start size)))
