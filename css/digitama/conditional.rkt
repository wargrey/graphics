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

(struct: css-media-type : CSS-Media-Type ([name : Symbol] [only? : Boolean]))
(struct: css-media-feature : CSS-Media-Feature ([name : Symbol] [value : CSS-Media-Datum] [operator : Char]))
(struct: css-not : CSS-Not ([condition : CSS-Feature-Query]))
(struct: css-and : CSS-And ([conditions : (Listof CSS-Feature-Query)]))
(struct: css-or : CSS-Or ([conditions : (Listof CSS-Feature-Query)]))

;; https://drafts.csswg.org/mediaqueries/#media-descriptor-table
;; https://drafts.csswg.org/mediaqueries/#mf-deprecated
(define-type CSS-Feature-Support? (-> Symbol (Listof+ CSS-Token) Boolean))
(define-type CSS-Media-Feature-Filter (-> Symbol Boolean (-> Void) (U Void (CSS:Filter CSS-Media-Datum))))

(define css-media-feature-filter : CSS-Media-Feature-Filter
  (lambda [downcased-name min/max? deprecated!]
    (case downcased-name
      [(width height device-width device-height resolution)
       (when (or (eq? downcased-name 'device-width) (eq? downcased-name 'device-height)) (deprecated!))
       (<css+length> #true)]
      [(aspect-ratio device-aspect-ratio)
       (when (eq? downcased-name 'device-aspect-ratio) (deprecated!))
       (CSS:<~> (<css:ratio>) real->double-flonum)]
      [(resolution) (CSS:<+> (CSS:<=> (<css-keyword> 'infinite) +inf.0) (<css:resolution>))]
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
  
(define-values (default-css-media-preferences default-css-media-feature-filter default-css-feature-support?)
  (values (make-parameter ((inst make-hasheq Symbol CSS-Media-Datum)))
          (make-parameter css-media-feature-filter)
          (make-parameter (const #false))))
