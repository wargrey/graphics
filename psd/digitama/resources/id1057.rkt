#lang typed/racket/base

(require "format.rkt")
(require "../parser.rkt")

(unsafe-provide 0x421)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x421 : (-> Integer String Bytes Fixnum Index Null PSD-Version-Info)
  (lambda [id name iptc-naa idx size argl]
    (define-values (writer reader-idx) (parse-utf16-string iptc-naa (unsafe-fx+ idx 5)))
    (define-values (reader version-idx) (parse-utf16-string iptc-naa reader-idx))
    
    (PSD-Version-Info id name
                      (parse-uint32 iptc-naa idx index?)
                      (> (parse-uint8 iptc-naa (unsafe-fx+ idx 4)) 0)
                      writer reader
                      (parse-uint32 iptc-naa version-idx index?))))
