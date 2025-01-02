#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require racket/list)

(require geofun/font)
(require geofun/constructor)
(require geofun/composite)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define math-positional-digits
  (lambda [#:base [base : Byte 10]
           #:nslots [min-slots : Index 0]
           #:font [font : Font (default-font)]
           #:line-height [line-height : Real -1.618]
           #:gapsize [gapsize : Real 2.0]
           [n : Integer] [shift : Integer 0]] : Geo
    (define slot-size : Nonnegative-Flonum (~length line-height (font-metrics-ref font 'em)))
    (define empty-slot : Geo (geo-square slot-size))
    (define weight : Integer (expt 10 (abs shift)))
    (define digits : String (number->string ((if (>= shift 0) * quotient) (abs n) weight) base))
    (define nslots : Index (string-length digits))

    (let make-slots ([stols : (Listof Geo) null]
                     [idx : Nonnegative-Fixnum 0])
      (cond [(< idx nslots) (make-slots (cons (geo-cc-superimpose empty-slot (geo-text (string-ref digits idx) font)) stols)
                                        (+ idx 1))]
            [(< idx min-slots) (make-slots (append stols (make-list (- min-slots idx) empty-slot)) min-slots)]
            [else (geo-hc-append* (reverse stols) #:gapsize gapsize)]))))
