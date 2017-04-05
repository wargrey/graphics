#lang typed/racket

;;; https://drafts.csswg.org/css-conditional
;;; https://drafts.csswg.org/mediaqueries

(provide (all-defined-out))

(require "misc.rkt")
(require "digicore.rkt")
(require "../recognizer.rkt")

;; https://drafts.csswg.org/css-conditional/#at-supports
;; https://drafts.csswg.org/mediaqueries/#media-types
;; https://drafts.csswg.org/mediaqueries/#mq-syntax
;; https://drafts.csswg.org/mediaqueries/#mq-features
(define-type CSS-Media-Query (U CSS-Media-Type CSS-Feature-Query (Pairof CSS-Media-Type CSS-Feature-Query)))
(define-type CSS-Feature-Query (U CSS-Not CSS-And CSS-Or CSS-Media-Feature CSS-Declaration Symbol CSS-Syntax-Error))
(define-type CSS-Condition (U CSS-Media-Query CSS-Feature-Query))

(struct: css-media-type : CSS-Media-Type ([name : Symbol] [only? : Boolean]))
(struct: css-media-feature : CSS-Media-Feature ([name : Symbol] [value : CSS-Media-Datum] [operator : Char]))
(struct: css-not : CSS-Not ([condition : CSS-Condition]))
(struct: css-and : CSS-And ([conditions : (Listof CSS-Condition)]))
(struct: css-or : CSS-Or ([conditions : (Listof CSS-Condition)]))

;; https://drafts.csswg.org/mediaqueries/#media-descriptor-table
;; https://drafts.csswg.org/mediaqueries/#mf-deprecated
(define-type CSS-@Supports-Okay? (-> Symbol Any Boolean))
(define-type CSS-@Media-Filters (-> Symbol Boolean (-> Void) (U Void (CSS:Filter CSS-Media-Datum))))

(define css-media-feature-filters : CSS-@Media-Filters
  (lambda [downcased-name min/max? deprecated!]
    (case downcased-name
      [(width height device-width device-height)
       (when (or (eq? downcased-name 'device-width) (eq? downcased-name 'device-height)) (deprecated!))
       (<css+length> #true)]
      [(aspect-ratio device-aspect-ratio)
       (when (eq? downcased-name 'device-aspect-ratio) (deprecated!))
       (CSS:<~> (<css:ratio>) real->double-flonum)]
      [(resolution) (CSS:<+> (CSS:<=> (<css-keyword> 'infinite) +inf.0) (<css+resolution>))]
      [(color color-index monochrome) (<css:integer> exact-nonnegative-integer?)]
      [(grid) #|legacy descriptor|# (when (false? min/max?) (<css-boolean>))]
      [(orientation) (<css-keyword> '(portrait landscape))]
      [(scan) (<css-keyword> '(interlace progressive))]
      [(update) (<css-keyword> '(none slow fast))]
      [(overflow-block) (<css-keyword> '(none scroll optional-paged paged))]
      [(overflow-inline) (<css-keyword> '(none scroll))]
      [(color-gamut) (<css-keyword> '(srgb p3 rec2020))]
      [(pointer any-pointer) (<css-keyword> '(none coarse fine))]
      [(havor any-havor) (<css-keyword> '(none havor))]
      [(scripting) (<css-keyword> '(none initial-only enabled))])))

(define css-deprecate-media-type : (Parameterof Boolean) (make-parameter #false))
(define default-css-media-type : (Parameterof Symbol) (make-parameter 'all))
  
(define-values (default-css-media-features default-css-media-feature-filters default-css-feature-support?)
  (values (make-parameter ((inst make-hasheq Symbol CSS-Media-Datum)))
          (make-parameter css-media-feature-filters)
          (make-parameter (ann (const #true) CSS-@Supports-Okay?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-@media-okay? : (-> (Listof CSS-Media-Query) CSS-Media-Features Boolean)
  ;;; https://drafts.csswg.org/mediaqueries/#evaluating
  ;;; https://drafts.csswg.org/mediaqueries/#media-types
  ;;; https://drafts.csswg.org/mediaqueries/#boolean-context
  (lambda [queries features]
    (define (okay? [query : CSS-Condition]) : Boolean
      (cond [(css-media-feature? query)
             (define downcased-name : Symbol (css-media-feature-name query))
             (define datum : CSS-Media-Datum (css-media-feature-value query))
             (define metadata : (U CSS-Media-Datum EOF) (hash-ref features downcased-name (λ _ eof)))
             (cond [(symbol? datum) (and (symbol? metadata) (eq? datum metadata))]
                   [(real? metadata) (case (css-media-feature-operator query)
                                       [(#\>) (> metadata datum)] [(#\≥) (>= metadata datum)]
                                       [(#\<) (< metadata datum)] [(#\≤) (<= metadata datum)]
                                       [else (= metadata datum)])]
                   [else #false])]
            [(symbol? query)
             (define metadata : CSS-Media-Datum (hash-ref features query (λ _ 'none)))
             (not (if (symbol? metadata) (eq? metadata 'none) (zero? metadata)))]
            [(css-media-type? query)
             (define result (memq (css-media-type-name query) (list (default-css-media-type) 'all)))
             (if (css-media-type-only? query) (and result #true) (not result))]
            [else (and (pair? query)
                       (css-condition-okay? (car query) okay?)
                       (css-condition-okay? (cdr query) okay?))]))
    (or (null? queries)
        (for/or ([query (in-list queries)])
          (css-condition-okay? query okay?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-condition-okay? : (-> CSS-Media-Query (-> CSS-Condition Boolean) Boolean)
  (lambda [query okay?]
    (cond [(css-not? query) (not (css-condition-okay? (css-not-condition query) okay?))]
          [(css-and? query) (andmap (λ [[q : CSS-Condition]] (css-condition-okay? q okay?)) (css-and-conditions query))]
          [(css-or? query) (ormap (λ [[q : CSS-Condition]] (css-condition-okay? q okay?)) (css-or-conditions query))]
          [else (okay? query)])))
