#lang typed/racket

;;; https://drafts.csswg.org/css-cascade
;;; https://drafts.csswg.org/css-values

(provide (all-defined-out))

(require "misc.rkt")
(require "digicore.rkt")
(require "selector.rkt")
(require "variables.rkt")
(require "conditional.rkt")

(define-type CSS-StyleSheet-Pool (HashTable Natural CSS-StyleSheet))
(define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule CSS-Media-Rule))
(define-type CSS-Media-Rule (Pairof (Listof CSS-Grammar-Rule) CSS-Media-Preferences))

(struct: css-style-rule : CSS-Style-Rule ([selectors : (Listof+ CSS-Complex-Selector)] [properties : CSS-Declarations]))

(struct: css-stylesheet : CSS-StyleSheet
  ([pool : CSS-StyleSheet-Pool]
   [location : (U String Symbol)]
   [timestamp : Integer]
   [preferences : CSS-Media-Preferences]
   [imports : (Listof Positive-Integer)]
   [namespaces : CSS-Namespace]
   [rules : (Listof CSS-Grammar-Rule)]))

(define css-stylesheet-placeholder : CSS-StyleSheet
  (make-css-stylesheet (make-hasheq) '/dev/null 0 (make-hasheq) null (make-hasheq) null))

(define css-cascade : (All (Preference) (-> (Listof CSS-StyleSheet) (Listof CSS-Subject)
                                            CSS-Declaration-Parsers (CSS-Cascaded-Value-Filter Preference)
                                            (Option CSS-Values) [#:quirk? Boolean]
                                            (Values Preference CSS-Values)))
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  ;;; https://drafts.csswg.org/css-variables/#cycles
  (lambda [stylesheets stcejbus desc-filter value-filter inherited-values #:quirk? [quirk? #false]]
    (define declared-values : CSS-Values (make-css-values))
    (let cascade-stylesheets ([batch : (Listof CSS-StyleSheet) stylesheets])
      (for ([stylesheet (in-list batch)])
        (define child-identities : (Listof Positive-Integer) (css-stylesheet-imports stylesheet))
        (cascade-stylesheets (for/list : (Listof CSS-StyleSheet) ([import (in-list child-identities)])
                               (hash-ref (css-stylesheet-pool stylesheet) import)))
        (css-cascade-rules (css-stylesheet-rules stylesheet) stcejbus desc-filter quirk?
                           (css-stylesheet-preferences stylesheet) declared-values)))
    (css-resolve-variables declared-values inherited-values)
    ;;; TODO: should we copy the inherited values after invoking (value-filter)?
    (values (value-filter declared-values inherited-values) declared-values)))

(define css-cascade-rules : (->* ((Listof CSS-Grammar-Rule) (Listof CSS-Subject) CSS-Declaration-Parsers)
                                 (Boolean CSS-Media-Preferences CSS-Values) CSS-Values)
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
  ;;; https://drafts.csswg.org/selectors/#data-model
  (lambda [rules stcejbus desc-filter [quirk? #false] [top-preferences (default-css-media-preferences)] [descbase (make-css-values)]]
    (define-type Style-Metadata (Vector Nonnegative-Fixnum CSS-Declarations CSS-Media-Preferences))
    (define-values (selected-rules-style single-preference?)
      (let cascade-rule : (Values (Listof Style-Metadata) Boolean) ([preferences : CSS-Media-Preferences top-preferences]
                                                                    [grammars : (Listof CSS-Grammar-Rule) rules]
                                                                    [stylebase : (Listof Style-Metadata) null]
                                                                    [single? : Boolean #true])
        (for/fold ([styles stylebase]
                   [single-preference? single?])
                  ([style (in-list grammars)])
          (cond [(css-@rule? style) (values styles single-preference?)]
                [(pair? style) (cascade-rule (cdr style) (car style) styles #false)]
                [else (let ([selectors : (Listof+ CSS-Complex-Selector) (css-style-rule-selectors style)])
                        (define specificity : Nonnegative-Fixnum
                          (for/fold ([max-specificity : Nonnegative-Fixnum 0]) ([selector (in-list selectors)])
                            (define matched-specificity : Nonnegative-Fixnum (or (css-selector-match selector stcejbus quirk?) 0))
                            (fxmax matched-specificity max-specificity)))
                        (cond [(zero? specificity) (values styles single-preference?)]
                              [else (let ([sm : Style-Metadata (vector specificity (css-style-rule-properties style) preferences)])
                                      (values (cons sm styles) single-preference?))]))]))))
    (unless (null? selected-rules-style)
      (define ordered-sources : (Listof Style-Metadata)
        (sort (reverse selected-rules-style)
              (λ [[sm1 : Style-Metadata] [sm2 : Style-Metadata]]
                (fx< (vector-ref sm1 0) (vector-ref sm2 0)))))
      (call-with-css-size-from-media #:preferences top-preferences
        (if (and single-preference?)
            (let ([source-ref (λ [[src : Style-Metadata]] : CSS-Declarations (vector-ref src 1))])
              (css-cascade-declarations desc-filter (map source-ref ordered-sources) descbase))
            (for ([src (in-list ordered-sources)])
              (define alter-preferences : CSS-Media-Preferences (vector-ref src 2))
              (if (eq? alter-preferences top-preferences)
                  (css-cascade-declarations desc-filter (vector-ref src 1) descbase)
                  (call-with-css-size-from-media #:preferences alter-preferences
                    (css-cascade-declarations desc-filter (vector-ref src 1) descbase)))))))
    descbase))

(define css-cascade-declarations : (->* (CSS-Declaration-Parsers CSS-Cascading-Declarations) (CSS-Values) CSS-Values)
  ;;; https://drafts.csswg.org/css-cascade/#shorthand
  ;;; https://drafts.csswg.org/css-cascade/#importance
  ;;; https://drafts.csswg.org/css-variables/#syntax
  ;;; https://drafts.csswg.org/css-variables/#using-variables
  (lambda [desc-parser properties [valuebase (make-css-values)]]
    (define varbase : CSS-Variable-Values (css-values-variables valuebase))
    (define descbase : (HashTable Symbol (-> CSS-Datum)) (css-values-descriptors valuebase))
    (define importants : (HashTable Symbol Boolean) (css-values-importants valuebase))
    (define (desc-more-important? [desc-name : Symbol] [important? : Boolean]) : Boolean
      (or important? (not (hash-has-key? importants desc-name))))
    (define (desc-set! [desc-name : Symbol] [important? : Boolean] [declared-value : (-> CSS-Datum)]) : Void
      (when important? (hash-set! importants desc-name #true))
      (when (eq? desc-name 'all)
        (for ([desc-key (in-list (remq* (default-css-all-exceptions) (hash-keys descbase)))])
          (when (desc-more-important? desc-key important?)
            (hash-set! descbase desc-key declared-value))))
      (hash-set! descbase desc-name declared-value))
    (define #:forall (a) (do-parse [parse : (CSS-Parser a)] [initial : a]
                                   [declared-values : (Listof CSS-Token)] [<desc-name> : CSS:Ident]) : (Option a)
      (define-values (desc-value+exn tail) (parse initial declared-values))
      (cond [(false? desc-value+exn) ((if (null? tail) make+exn:css:missing-value make+exn:css:type) tail <desc-name>) #false]
            [(exn:css? desc-value+exn) (css-log-syntax-error desc-value+exn <desc-name>) #false]
            [(pair? tail) (make+exn:css:overconsumption tail <desc-name>) #false]
            [else desc-value+exn]))
    (define (parse-long [info : (Pairof CSS-Shorthand-Parser (Listof Symbol))]
                        [<desc-name> : CSS:Ident] [declared-values : (Listof+ CSS-Token)]
                        [important? : Boolean] [lazy? : Boolean]) : Void
      (cond [(and lazy?)
             (define pending-thunk : (-> (Option CSS-Longhand-Values))
               (thunk (let ([flat-values (css-variable-substitute <desc-name> declared-values varbase null)])
                        (and (css-pair? flat-values) (do-parse (car info) css-longhand flat-values <desc-name>)))))
             (define &pending-longhand : (Boxof (-> (Option CSS-Longhand-Values))) (box pending-thunk))
             (for ([name (in-list (cdr info))] #:when (desc-more-important? name important?))
               (desc-set! name important?
                          (thunk (let ([longhand ((unbox &pending-longhand))])
                                   (box-cas! &pending-longhand pending-thunk (thunk longhand))
                                   (define desc-value : CSS-Datum
                                     (cond [(not (hash? longhand)) css:unset]
                                           [else (hash-ref longhand name (thunk css:initial))]))
                                   (hash-set! descbase name (thunk desc-value))
                                   desc-value))))]
            [(do-parse (car info) css-longhand declared-values <desc-name>)
             => (λ [longhand] (for ([(name desc-value) (in-hash longhand)])
                                (desc-set! name important? (thunk desc-value))))]))
    (define (parse-desc [info : (CSS-Parser (Listof CSS-Datum))]
                        [<desc-name> : CSS:Ident] [desc-name : Symbol] [declared-values : (Listof+ CSS-Token)]
                        [important? : Boolean] [lazy? : Boolean]) : Void
      (define (normalize [desc-values : (Option (Listof CSS-Datum))]) : (Option CSS-Datum)
        (cond [(pair? desc-values) (if (null? (cdr desc-values)) (car desc-values) (reverse desc-values))]
              [else (and (null? (cdr declared-values)) (css-wide-keywords-ormap (car declared-values)))]))
      (cond [(and lazy?)
             (desc-set! desc-name important?
                        (thunk (let ([flat-values (css-variable-substitute <desc-name> declared-values varbase null)])
                                 (define desc-value : (Option CSS-Datum)
                                   (and (css-pair? flat-values)
                                        (normalize (do-parse info null flat-values <desc-name>))))
                                 (unless (false? desc-value) (hash-set! descbase desc-name (thunk desc-value)))
                                 (or desc-value css:unset))))]
            [(normalize (do-parse info null declared-values <desc-name>))
             => (λ [desc-value] (desc-set! desc-name important? (thunk desc-value)))]))
    (let cascade ([subproperties : CSS-Cascading-Declarations properties])
      (for ([property (in-list subproperties)])
        (cond [(list? property) (cascade property)]
              [else (let* ([<desc-name> : CSS:Ident (css-declaration-name property)]
                           [desc-name : Symbol (css:ident-norm <desc-name>)])
                      (cond [(symbol-unreadable? desc-name) (hash-set! varbase desc-name property)]
                            [else (let ([important? : Boolean (css-declaration-important? property)])
                                    (when (desc-more-important? desc-name important?)
                                      (define declared-values : (Listof+ CSS-Token) (css-declaration-values property))
                                      (define lazy? : Boolean (css-declaration-lazy? property))
                                      (define info : CSS-Declaration-Parser
                                        (desc-parser desc-name (thunk (void (make+exn:css:deprecated <desc-name>)))))
                                      (cond [(or (false? info) (void? info)) (make+exn:css:unrecognized <desc-name>)]
                                            [(pair? info) (parse-long info <desc-name> declared-values important? lazy?)]
                                            [else (parse-desc info <desc-name> desc-name declared-values important? lazy?)])))]))])))
    valuebase))
