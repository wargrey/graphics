#lang typed/racket/base

(provide (all-defined-out))

(require digimon/symbol)
(require racket/format)

(require "../color.rkt")

;;; WARNING
; Meanwhile Racket's Pango is of version 1.16, which is too low.
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

(define-type PExpr-Full-Element (Rec elem (U (List Symbol PExpr-AttList (Listof (U PExpr-Datum PExpr-Element elem))))))
(define-type PExpr-Children-Element (Rec elem (List Symbol (Listof (U PExpr-Datum PExpr-Element elem)))))
(define-type PExpr-Empty-Element (U (List Symbol) (List Symbol (Listof PExpr-Attribute))))

(define-type PExpr-Element (U PExpr-Full-Element PExpr-Children-Element PExpr-Empty-Element))
(define-type PExpr (U PExpr-Datum PExpr-Element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc-markup-datum->text : (-> Any Bytes)
  (lambda [src]
    (cond [(string? src) (string->bytes/utf-8 src)]
          [(bytes? src) src]
          [(pexpr-element? src) (pexpr->bytes src)]
          [else (string->bytes/utf-8 (format "~a" src))])))

(define dc-markup-datum->maybe-text : (-> Any (Option Bytes))
  (lambda [src]
    (and src
         (dc-markup-datum->text src))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pexpr? : (-> Any Boolean : PExpr)
  (lambda [v]
    (or (pexpr-datum? v)
        (pexpr-element? v))))

(define write-pexpr : (->* (PExpr) (Output-Port) Void)
  (lambda [x [/dev/pmlout (current-output-port)]]
    (cond [(string? x)
           (write-pexpr-string x /dev/pmlout)]
          [(pair? x)
           (cond [(pexpr-children-element? x)
                  (write-pexpr-element (car x) null (cadr x) /dev/pmlout)]
                 [(pexpr-full-element? x)
                  (write-pexpr-element (car x) (cadr x) (caddr x) /dev/pmlout)]
                 [(pexpr-empty-element? x)
                  (write-char #\< /dev/pmlout)
                  (write (car x) /dev/pmlout)
                  (let ([maybe-attrs (cdr x)])
                    (when (pair? maybe-attrs)
                      (write-pexpr-attrlist (car maybe-attrs) /dev/pmlout)))
                  (write-bytes #"/>" /dev/pmlout)])]
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

(define pexpr-element? : (-> Any Boolean : PExpr-Element)
  (lambda [e]
    (and (pair? e)
         (or (pexpr-children-element? e)
             (pexpr-full-element? e)
             (pexpr-empty-element? e)))))

(define pexpr-full-element? : (-> (Pairof Any Any) Boolean : PExpr-Full-Element)
  (lambda [e]
    (and (symbol? (car e))
         (pair? (cdr e))
         (pexpr-attlist? (cadr e))
         (pair? (cddr e))
         (null? (cdddr e))
         (pexpr-list? (caddr e)))))

(define pexpr-children-element? : (-> (Pairof Any Any) Boolean : PExpr-Children-Element)
  (lambda [e]
    (and (symbol? (car e))
         (pair? (cdr e))
         (null? (cddr e))
         (pexpr-list? (cadr e)))))

(define pexpr-empty-element? : (-> (Pairof Any Any) Boolean : PExpr-Empty-Element)
  (lambda [e]
    (and (symbol? (car e))
         (or (null? (cdr e))
             (and (pair? (cdr e))
                  (pexpr-attlist? (cadr e))
                  (null? (cddr e)))))))

