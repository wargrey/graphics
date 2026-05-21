#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require "self.rkt")
(require "markup.rkt")
(require "unit.rkt")

(require "../self.rkt")
(require "../dc/string.rkt")
(require "../dc/composite.rkt")

(require "../layer/type.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; This function works for scenarios,
;  in which users might want to check if there are some important hints in plain text
;  so resulting strings don't have to deal with detailed ghosts.
(define geo-rich-text->plain-text : (case-> [(U String Symbol Complex Geo:Rich:Point) -> String]
                                            [(U False Symbol Geo-Rich-Text) -> (Option String)])
  (lambda [v]
    (cond [(string? v) v]
          [(geo-markup-datum? v) (hash-ref! maybedb v (λ [] (geo-markup-extract-plain-text v)))]
          [(geo? v)
           (let geo-cat ([this : Geo v])
             (cond [(geo:string? this) (geo:string-body this)]
                   [(geo:group? this)
                    (or (geo-desc this)
                        (let ([g (geo:group-selves this)])
                          (hash-ref! maybedb g
                                     (λ [] (let cat : (Option String) ([selves : (Listof (GLayerof Geo)) (glayer-group-layers g)]
                                                                       [snialp : (Listof String) null])
                                             (if (pair? selves)
                                                 (let-values ([(self rest) (values (glayer-master (car selves)) (cdr selves))])
                                                   (define maybe-plain : (Option String) (geo-cat self))
                                                   (cat rest
                                                        (cond [(not maybe-plain) snialp]
                                                              [else (cons maybe-plain snialp)])))
                                                 (and (pair? snialp)
                                                      (string-join #:before-first "{" #:after-last "}"
                                                                   (reverse snialp) ", "))))))))]
                   [else (geo-desc this)]))]
          [(complex? v) (number->string v)]
          [(geo:rich:dimension? v)
           (hash-ref! plaindb v
                      (λ [] (let* ([datum (geo:rich:dimension-value v)]
                                   [d (geo-rich-text->plain-text datum)]
                                   [d (if (real? datum) d (string-append "(" d ")"))]
                                   [u (geo-rich-text->plain-text (geo:rich:dimension-unit v))])
                              ; we don't check if the `1`, `0` or `-1` could be omitted here
                              (cond [(not u) d]
                                    [(geo-rich-unit-needs-gap-space? u) (string-append d " " u)]
                                    [else (string-append d u)]))))]
          [(geo:rich:point? v)
           (hash-ref! plaindb v
                      (λ [] (let ([pt (geo:rich:point-coordinates v)])
                              (string-join #:before-first "(" #:after-last ")"
                                           (map geo-rich-text->plain-text
                                                (cond [(real? pt) (list pt)]
                                                      [(complex? pt) (list (real-part pt) (imag-part pt))]
                                                      [else pt]))
                                           ", "))))]
          [else (and (symbol? v) (symbol->immutable-string v))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rich-text-match? : (-> Geo-Rich-Text (U Regexp Byte-Regexp) Boolean)
  (lambda [self pattern]
    (cond [(string? self) (regexp-match? pattern self)]
          [else (let ([str (geo-rich-text->plain-text self)])
                  (and str (regexp-match? pattern str)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plaindb : (HashTable Geo-Rich-Text String) (make-weak-hash))
(define maybedb : (HashTable (U Geo-Markup-Datum (GLayer-Groupof Geo)) (Option String)) (make-weak-hash))
