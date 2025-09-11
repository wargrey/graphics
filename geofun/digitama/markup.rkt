#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [pexpr pml]))

(require digimon/symbol)
(require racket/format)

(require "../color.rkt")

;;; WARNING
; Meanwhile Racket's Pango is of version 1.42, which is too low.
;   So that `size` related attributes are in 1024ths of a point,
;   and other dimensional values are unavaliable.
; Thus,
;   * floating numbers would be interpreted as relative to 1024 for sizes
;   * byte integer numbers would be interpreted as percentage for alphas
;   * keywords would be interpreted as hexadecimal colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PExpr-Attribute-Value (U String Symbol (Listof Symbol) Flonum Byte Keyword FlColor))
(define-type PExpr-Attribute (U (Pairof Symbol PExpr-Attribute-Value) (List Symbol PExpr-Attribute-Value)))
(define-type PExpr-AttList (Listof PExpr-Attribute))
(define-type PExpr-Datum (U String Index Char Symbol))
(define-type PExpr (U PExpr-Datum PExpr-Element))

(struct pexpr-element
  ([name : Symbol]
   [attrs : PExpr-AttList]
   [children : (Listof PExpr)])
  #:type-name PExpr-Element)

(define pexpr : (case-> [Symbol -> PExpr-Element]
                        [Symbol (U (Listof PExpr) PExpr-Datum) -> PExpr-Element]
                        [Symbol PExpr-AttList (U (Listof PExpr) PExpr-Datum) -> PExpr-Element])
  (let ([db : (HashTable Symbol PExpr-Element) (make-hasheq)])
    (case-lambda
      [(name) (hash-ref! db name (λ [] (pexpr-element name null null)))]
      [(name children)
       (if (null? children)
           (pexpr name)
           (pexpr-element name null (if (list? children) children (list children))))]
      [(name attlist children)
       (if (and (null? attlist) (null? children))
           (pexpr name)
           (pexpr-element name attlist (if (list? children) children (list children))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc-markup-datum->text : (-> Any Bytes)
  (lambda [src]
    (cond [(string? src) (pexpr->bytes src)]
          [(bytes? src) src]
          [(pexpr-element? src) (pexpr->bytes src)]
          [else (pexpr->bytes (format "~a" src))])))

(define dc-markup-datum->maybe-text : (-> Any (Option Bytes))
  (lambda [src]
    (and src
         (dc-markup-datum->text src))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DC-Markup-Text (U String PExpr-Element Bytes))

(define dc-markup-text? : (-> Any Boolean : DC-Markup-Text)
  (lambda [info]
    (or (string? info)
        (pexpr-element? info)
        (bytes? info))))

(define dc-markup-maybe-text? : (-> Any Boolean : (Option DC-Markup-Text))
  (lambda [info]
    (or (dc-markup-text? info)
        (not info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pexpr? : (-> Any Boolean : PExpr)
  (lambda [v]
    (or (pexpr-datum? v)
        (pexpr-element? v))))

(define write-pexpr : (->* (PExpr) (Output-Port) Void)
  (lambda [x [/dev/pmlout (current-output-port)]]
    (cond [(string? x)
           (write-pexpr-string x /dev/pmlout)]
          [(pexpr-element? x)
           (write-pexpr-element (pexpr-element-name x) (pexpr-element-attrs x) (pexpr-element-children x) /dev/pmlout)]
          [(symbol? x)
           (write-char #\& /dev/pmlout)
           (write x /dev/pmlout)
           (write-char #\; /dev/pmlout)]
          [(index? x)
           (write-bytes #"&#x" /dev/pmlout)
           (write-string (number->string x 16) /dev/pmlout)
           (write-char #\; /dev/pmlout)]
          [(char? x)
           (write-bytes #"&#x" /dev/pmlout)
           (write-string (number->string (char->integer x) 16) /dev/pmlout)
           (write-char #\; /dev/pmlout)])
    (void)))

(define pexpr->bytes : (-> PExpr Bytes)
  (lambda [x]
    (define /dev/pmlout (open-output-bytes '/dev/pmlout))

    (write-pexpr x /dev/pmlout)
    (get-output-bytes /dev/pmlout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define write-pexpr-element : (->* (Symbol PExpr-AttList (Listof PExpr)) (Output-Port) Void)
  (lambda [tagname attrlist children [/dev/pmlout (current-output-port)]]
    (write-char #\< /dev/pmlout)
    (write tagname /dev/pmlout)
    (write-pexpr-attrlist attrlist /dev/pmlout)
    (write-char #\> /dev/pmlout)
    (write-pexpr-children children /dev/pmlout)
    (write-bytes #"</" /dev/pmlout)
    (write tagname /dev/pmlout)
    (write-char #\> /dev/pmlout)))

(define write-pexpr-attrlist : (->* (PExpr-AttList) (Output-Port) Void)
  (lambda [attrlist [/dev/pmlout (current-output-port)]]
    (for ([attr (in-list attrlist)])
      (write-char #\space /dev/pmlout)
      (write (car attr) /dev/pmlout)
      (write-char #\= /dev/pmlout)
      
      (write-char #\" /dev/pmlout)
      (write-pexpr-attribute-value (cdr attr) /dev/pmlout)
      (write-char #\" /dev/pmlout))))

(define write-pexpr-children : (->* ((Listof PExpr)) (Output-Port) Void)
  (lambda [children [/dev/pmlout (current-output-port)]]
    (for ([child (in-list children)])
      (write-pexpr child /dev/pmlout))))

(define write-pexpr-attribute-value : (->* ((U PExpr-Attribute-Value (List PExpr-Attribute-Value))) (Output-Port) Void)
  (lambda [v [/dev/pmlout (current-output-port)]]
    (cond [(string? v) (write-pexpr-string v /dev/pmlout)]
          [(symbol? v) (write-pexpr-string (symbol->immutable-string v) /dev/pmlout)]
          [(keyword? v) (write-pexpr-color v /dev/pmlout)]
          [(flcolor? v) (write-pexpr-color v /dev/pmlout)]
          [(flonum? v) (write-pexpr-size v /dev/pmlout)]
          [(byte? v) (write-pexpr-alpha v /dev/pmlout)]
          [(symbol-list? v) (write-pexpr-string (symbol-join v) /dev/pmlout)]
          [else (write-pexpr-attribute-value (car v) /dev/pmlout)])))

(define write-pexpr-string : (->* (String) (Output-Port) Void)
  (lambda [s [/dev/pmlout (current-output-port)]]
    (for ([ch (in-string s)])
      (case ch
        [(#\") (display "&quot;" /dev/pmlout)]
        [(#\') (display "&apos;" /dev/pmlout)]
        [(#\&) (display "&amp;" /dev/pmlout)]
        [(#\<) (display "&lt;" /dev/pmlout)]
        [(#\>) (display "&gt;" /dev/pmlout)]
        [else (write-char ch /dev/pmlout)]))))

(define write-pexpr-size : (->* (Flonum) (Output-Port) Void)
  ; `size` related attributes in 1024ths of point
  (lambda [c [/dev/pmlout (current-output-port)]]
    (write (inexact->exact (round (* c 1024.0))) /dev/pmlout)
    (void)))

(define write-pexpr-alpha : (->* (Byte) (Output-Port) Void)
  ; `size` related attributes in 1024ths of point
  (lambda [alpha [/dev/pmlout (current-output-port)]]
    (write (quotient (* alpha 65535) 100) /dev/pmlout)
    (void)))

(define write-pexpr-color : (->* (Color) (Output-Port) Void)
  (lambda [c [/dev/pmlout (current-output-port)]]
    (write-char #\# /dev/pmlout)
    (write-string (~r (flcolor->hex c) #:base 16 #:min-width 6 #:pad-string "0") /dev/pmlout)
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pexpr-datum? : (-> Any Boolean : PExpr-Datum)
  (lambda [v]
    (or (string? v)
        (symbol? v)
        (index? v)
        (char? v))))

(define pexpr-attribute-value? : (-> Any Boolean : PExpr-Attribute-Value)
  (lambda [v]
    (or (string? v)
        (symbol? v)
        (flcolor? v)
        (keyword? v)
        (byte? v)
        (flonum? v)
        (symbol-list? v))))

(define pexpr-attribute? : (-> Any Boolean : PExpr-Attribute)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (or (pexpr-attribute-value? (cdr e))
             (and (pair? (cdr e))
                  (pexpr-attribute-value? (cadr e))
                  (null? (cddr e)))))))

(define pexpr-attlist? : (-> Any Boolean : PExpr-AttList)
  (lambda [a]
    (and (list? a)
         (andmap pexpr-attribute? a))))

(define pexpr-list? : (-> Any Boolean : (Listof PExpr))
  (lambda [a]
    (and (list? a)
         (andmap pexpr? a))))
