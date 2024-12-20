#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type C-Variable-Datum (U C-Variable C-Vector C-Pad))
(define-type C-Variables (Listof C-Variable-Datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct c-placeholder
  ([addr : Index]
   [raw : Bytes])
  #:type-name C-Placeholder
  #:prefab)

(struct c-variable c-placeholder
  ([datum : (Boxof Any)]
   [name : (U Keyword Symbol)]
   [decltype : (Option Symbol)]
   [addr1 : Index]
   [segment : Symbol])
  #:type-name C-Variable
  #:constructor-name make-variable
  #:prefab)

(struct c-vector c-placeholder
  ([data : (Vectorof Any)]
   [name : (U Keyword Symbol)]
   [decltype : (Option Symbol)]
   [addr1 : Index]
   [type-size : Byte]
   [segment : Symbol])
  #:type-name C-Vector
  #:constructor-name make-array
  #:prefab)

(struct c-padding c-placeholder ()
  #:type-name C-Pad
  #:constructor-name make-pad
  #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-padding*? : (-> Any Boolean : C-Pad)
  (lambda [v]
    (and (c-padding? v)
         (index? (c-placeholder-addr v))
         (bytes? (c-placeholder-raw v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-variable-display : (->* (C-Placeholder) (Output-Port) Void)
  (lambda [self [/dev/stdout (current-output-port)]]
    (fprintf /dev/stdout "#(~a" (object-name self))
    
    (when (c-variable? self)
      (fprintf /dev/stdout " ~a @" (c-variable-name self)))
    (write-bytes #" 0x" /dev/stdout)
    (write-string (~r (c-placeholder-addr self) #:base 16) /dev/stdout)
    
    (write-bytes #" {" /dev/stdout)
    (let ([raw (c-placeholder-raw self)])
      (write-string (~r (bytes-ref raw 0) #:base 16 #:min-width 2 #:pad-string "0") /dev/stdout)
      (for ([idx (in-bytes raw 1)])
        (write-bytes #", ")
        (write-string (~r idx #:base 16 #:min-width 2 #:pad-string "0") /dev/stdout)))
    (write-bytes #"}" /dev/stdout)
    
    (when (and (c-variable? self) (box? (c-variable-datum self)))
      (if (c-variable-decltype self)
          (fprintf /dev/stdout " : ~a(~a)" (c-variable-decltype self) (unbox (c-variable-datum self)))
          (fprintf /dev/stdout " => ~a" (unbox (c-variable-datum self)))))
    (write-char #\) /dev/stdout)      
    (void)))

(define c-variable-displayln : (->* (C-Placeholder) (Output-Port) Void)
  (lambda [self [/dev/stdout (current-output-port)]]
    (c-variable-display self /dev/stdout)
    (newline /dev/stdout)))
