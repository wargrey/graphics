#lang typed/racket

;;; https://drafts.csswg.org/css-device-adapt

(provide (all-defined-out))

(require "misc.rkt")
(require "digicore.rkt")
(require "condition.rkt")
(require "../recognizer.rkt")

;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
(define css-viewport-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name !]
    (define-css-disjoint-filter viewport-length-filter #:-> (U Symbol Nonnegative-Inexact-Real)
      (<css-keyword> 'auto)
      (<css:percentage> nonnegative-single-flonum?)
      (<css+length> #true))
    (case suitcased-name
      [(width) (make-css-pair-parser (viewport-length-filter) 'min-width 'max-width)]
      [(height) (make-css-pair-parser (viewport-length-filter) 'min-height 'max-height)]
      [(zoom min-zoom max-zoom) (CSS<^> (CSS:<+> (<css-keyword> 'auto) (CSS:<~> (<css+%real>) real->double-flonum)))]
      [(min-width max-width min-height max-height) (CSS<^> (viewport-length-filter))]
      [(orientation) (CSS<^> (<css-keyword> '(auto portrait landscape)))]
      [(user-zoom) (CSS<^> (<css-keyword> '(zoom fixed)))]
      [else #false])))

(define css-viewport-filter : (CSS-Cascaded-Value-Filter (HashTable Symbol CSS-Media-Datum))
  ;;; https://drafts.csswg.org/css-device-adapt/#constraining
  ;;; https://drafts.csswg.org/css-device-adapt/#handling-auto-zoom
  ;;; https://drafts.csswg.org/css-device-adapt/#media-queries
  ;;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
  (lambda [cascaded-values inherited-viewport]
    ; Notes: There is no need to check the `initial-viewport` to specific the `specified values` since
    ;          @viewport is a controversial @rule which is easy to be used incorrectly,
    ;          @viewport is rarely used in desktop applications, and
    ;          this behavior is indeed specified if I understand the specification correctly.
    (define-values (initial-width initial-height)
      (let ([w (hash-ref (default-css-media-features) 'width (const #false))]
            [h (hash-ref (default-css-media-features) 'height (const #false))])
        (values (if (and (real? w) (positive? w)) (flmax (real->double-flonum w) 1.0) 1.0)
                (if (and (real? h) (positive? h)) (flmax (real->double-flonum h) 1.0) 1.0))))
    (define defzoom : Nonnegative-Flonum (default-css-viewport-auto-zoom))
    (define (smart [maix : (-> Flonum Flonum Flonum)] [v1 : (U Flonum Symbol)] [v2 : (U Flonum Symbol)]) : (U Flonum Symbol)
      (cond [(and (symbol? v1) (symbol? v2)) 'auto]
            [(symbol? v1) v2]
            [(symbol? v2) v1]
            [else (maix v1 v2)]))
    (define css->width (make-css->size 'auto #:100% initial-width))
    (define css->height (make-css->size 'auto #:100% initial-height))
    (define min-zoom : Flonum (css-ref cascaded-values #false 'min-zoom nonnegative-flonum? 0.0))
    (define max-zoom : Flonum (flmax min-zoom (css-ref cascaded-values #false 'max-zoom nonnegative-flonum? +inf.0)))
    (define min-width : (U Flonum Symbol) (css-ref cascaded-values #false 'min-width css->width))
    (define max-width : (U Flonum Symbol) (css-ref cascaded-values #false 'max-width css->width))
    (define min-height : (U Flonum Symbol) (css-ref cascaded-values #false 'min-height css->height))
    (define max-height : (U Flonum Symbol) (css-ref cascaded-values #false 'max-height css->height))
    (define-values (width height)
      (let* ([width (smart flmax min-width (smart flmin max-width initial-width))]
             [height (smart flmax min-height (smart flmin max-height initial-height))]
             [width (cond [(and (symbol? width) (symbol? height)) initial-width]
                          [(symbol? width) (if (zero? initial-height) initial-width (fl* height (fl/ initial-width initial-height)))]
                          [else width])])
        (values width (cond [(flonum? height) height]
                            [(zero? initial-width) initial-height]
                            [else (fl* width (fl/ initial-height initial-width))]))))
    (define actual-viewport (hash-copy (default-css-media-features)))
    (for ([name (in-list      '(min-zoom max-zoom width height))]
          [value (in-list (list min-zoom max-zoom width height))])
      (hash-set! actual-viewport name value))
    (hash-set! actual-viewport 'orientation (css-ref cascaded-values #false 'orientation symbol? 'auto))
    (hash-set! actual-viewport 'user-zoom (css-ref cascaded-values #false 'user-zoom symbol? 'zoom))
    (hash-set! actual-viewport 'zoom (max min-zoom (min max-zoom (css-ref cascaded-values #false 'zoom nonnegative-flonum? defzoom))))
    actual-viewport))

(define-values (default-css-viewport-parsers default-css-viewport-filter default-css-viewport-auto-zoom)
  (values (make-parameter css-viewport-parsers)
          (make-parameter css-viewport-filter)
          (ann (make-parameter 1.0) (Parameterof Nonnegative-Flonum))))
