#lang typed/racket

;;; https://drafts.csswg.org/css-cascade
;;; https://drafts.csswg.org/css-values

(provide (except-out (all-defined-out) CSS-Style-Metadata))

(require "misc.rkt")
(require "digicore.rkt")
(require "selector.rkt")
(require "variables.rkt")
(require "conditional.rkt")
(require "../recognizer.rkt")

(define-type CSS-StyleSheet-Pool (HashTable Natural CSS-StyleSheet))
(define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule CSS-Media-Rule))
(define-type CSS-Media-Rule (Pairof (Listof CSS-Grammar-Rule) CSS-Media-Preferences))

(define-type CSS-Style-Metadata (Vector Nonnegative-Fixnum CSS-Declarations CSS-Media-Preferences))

(define css-warning-unknown-property? : (Parameterof Boolean) (make-parameter #true))
(define css-select-quirk-mode? : (Parameterof Boolean) (make-parameter #false))
(define css-property-case-sensitive? : (Parameterof Boolean) (make-parameter #false))

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

(define css-cascade : (All (Preference Env) (case-> [-> (Listof CSS-StyleSheet) (Listof CSS-Subject) CSS-Declaration-Parsers
                                                        (CSS-Cascaded-Value-Filter Preference) (Option CSS-Values)
                                                        (Values Preference CSS-Values)]
                                                    [-> (Listof CSS-StyleSheet) (Listof CSS-Subject) CSS-Declaration-Parsers
                                                        (CSS-Cascaded-Value+Filter Preference Env)
                                                        (Option CSS-Values) Env
                                                        (Values Preference CSS-Values)]))
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  (let ()
    (define do-cascade : (-> (Listof CSS-StyleSheet) (Listof CSS-Subject) CSS-Declaration-Parsers (Option CSS-Values) CSS-Values)
      (lambda [stylesheets stcejbus desc-parsers inherited-values]
        (define declared-values : CSS-Values (make-css-values))
        (let cascade-stylesheets ([batch : (Listof CSS-StyleSheet) stylesheets])
          (for ([stylesheet (in-list batch)])
            (define imported-identities : (Listof Positive-Integer) (css-stylesheet-imports stylesheet))
            (cascade-stylesheets (for/list : (Listof CSS-StyleSheet) ([import (in-list imported-identities)])
                                   (hash-ref (css-stylesheet-pool stylesheet) import)))
            (css-cascade-rules (css-stylesheet-rules stylesheet) stcejbus desc-parsers (css-select-quirk-mode?)
                               (css-stylesheet-preferences stylesheet) declared-values)))
        (css-resolve-variables declared-values inherited-values)
        ; TODO: should we copy the inherited values after invoking (value-filter)?
        declared-values))
    (case-lambda
      [(stylesheets stcejbus desc-parsers value-filter inherited-values)
       (define declared-values : CSS-Values (do-cascade stylesheets stcejbus desc-parsers inherited-values))
       (values (value-filter declared-values inherited-values) declared-values)]
      [(stylesheets stcejbus desc-parsers value-filter inherited-values env)
       (define declared-values : CSS-Values (do-cascade stylesheets stcejbus desc-parsers inherited-values))
       (values (value-filter declared-values inherited-values env) declared-values)])))

(define css-cascade-rules : (->* ((Listof CSS-Grammar-Rule) (Listof CSS-Subject) CSS-Declaration-Parsers)
                                 (Boolean CSS-Media-Preferences CSS-Values) CSS-Values)
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  (lambda [rules stcejbus desc-parsers [quirk? #false] [top-preferences (default-css-media-preferences)] [descbase (make-css-values)]]
    (define-values (ordered-srcs single?) (css-select-rules rules stcejbus quirk? top-preferences))
    (call-with-css-size-from-media #:preferences top-preferences
      (if (and single?)
          (let ([source-ref (λ [[src : CSS-Style-Metadata]] : CSS-Declarations (vector-ref src 1))])
            (css-cascade-declarations desc-parsers (map source-ref ordered-srcs) descbase))
          (for ([src (in-list ordered-srcs)])
            (define alter-preferences : CSS-Media-Preferences (vector-ref src 2))
            (if (eq? alter-preferences top-preferences)
                (css-cascade-declarations desc-parsers (vector-ref src 1) descbase)
                (call-with-css-size-from-media #:preferences alter-preferences
                  (css-cascade-declarations desc-parsers (vector-ref src 1) descbase))))))
    descbase))

(define css-cascade-declarations : (->* (CSS-Declaration-Parsers CSS-Cascading-Declarations) (CSS-Values) CSS-Values)
  ;;; https://drafts.csswg.org/css-cascade/#shorthand
  ;;; https://drafts.csswg.org/css-cascade/#importance
  ;;; https://drafts.csswg.org/css-variables/#syntax
  ;;; https://drafts.csswg.org/css-variables/#using-variables
  (lambda [desc-parsers properties [descbase (make-css-values)]]
    ;;; TODO: find a better way to deal with !important
    (define importants : (HashTable Symbol Boolean) (hash-ref! !imptbase descbase (thunk ((inst make-hasheq Symbol Boolean)))))
    (define case-sensitive? : Boolean (css-property-case-sensitive?))
    (define (desc-more-important? [desc-name : Symbol] [important? : Boolean]) : Boolean
      (or important? (not (hash-has-key? importants desc-name))))
    (define (desc-set! [desc-name : Symbol] [important? : Boolean] [declared-value : (-> Any)]) : Void
      (when important? (hash-set! importants desc-name #true))
      (when (eq? desc-name 'all)
        (for ([desc-key (in-list (remq* (default-css-all-exceptions) (hash-keys descbase)))])
          (when (desc-more-important? desc-key important?)
            (hash-set! descbase desc-key declared-value))))
      (hash-set! descbase desc-name declared-value))
    (define #:forall (a) (do-parse [<desc-name> : CSS:Ident] [parse : (CSS-Parser a)] [initial : a]
                                   [declared-values : (Listof CSS-Token)]) : (Option a)
      (define-values (desc-value+exn tail) (parse initial declared-values))
      (cond [(false? desc-value+exn) ((if (null? tail) make+exn:css:missing-value make+exn:css:type) tail <desc-name>) #false]
            [(exn:css? desc-value+exn) (css-log-syntax-error desc-value+exn <desc-name>) #false]
            [(pair? tail) (make+exn:css:overconsumption tail <desc-name>) #false]
            [else desc-value+exn]))
    (define (parse-long [parser : (Pairof CSS-Shorthand-Parser (Listof Symbol))]
                        [<desc-name> : CSS:Ident] [declared-values : (Listof+ CSS-Token)]
                        [important? : Boolean] [lazy? : Boolean]) : Void
      (cond [(and lazy?)
             (define pending-thunk : (-> (Option CSS-Longhand-Values))
               (thunk (let* ([varbase (css-varbase-ref descbase)]
                             [flat-values (css-variable-substitute <desc-name> declared-values varbase null)])
                        (and (css-pair? flat-values)
                             (do-parse <desc-name> (car parser) css-longhand flat-values)))))
             (define &pending-longhand : (Boxof (-> (Option CSS-Longhand-Values))) (box pending-thunk))
             (for ([name (in-list (cdr parser))] #:when (desc-more-important? name important?))
               (desc-set! name important?
                          (thunk (let ([longhand ((unbox &pending-longhand))])
                                   (box-cas! &pending-longhand pending-thunk (thunk longhand))
                                   (let ([desc-value (if (hash? longhand) (hash-ref longhand name (thunk css:initial)) css:unset)])
                                     (hash-set! descbase name (thunk desc-value))
                                     desc-value)))))]
            [(do-parse <desc-name> (car parser) css-longhand declared-values)
             => (λ [longhand] (for ([(name desc-value) (in-hash longhand)])
                                (desc-set! name important? (thunk desc-value))))]))
    (define (parse-desc [parser : (CSS-Parser (Listof Any))]
                        [<desc-name> : CSS:Ident] [desc-name : Symbol] [declared-values : (Listof+ CSS-Token)]
                        [important? : Boolean] [lazy? : Boolean]) : Void
      (define css-parse : (CSS-Parser (Listof Any)) (CSS<+> parser (CSS<^> (<css-wide-keywords>))))
      (define (normalize [desc-values : (Listof Any)]) : Any
        (cond [(and (pair? desc-values) (null? (cdr desc-values))) (car desc-values)]
              [else (reverse desc-values)]))
      (cond [(and lazy?)
             (desc-set! desc-name important?
                        (thunk (let* ([varbase (css-varbase-ref descbase)]
                                      [flat-values (css-variable-substitute <desc-name> declared-values varbase null)])
                                 (define desc-value : Any
                                   (normalize (or (and (css-pair? flat-values) (do-parse <desc-name> css-parse null flat-values))
                                                  (list css:unset))))
                                 (hash-set! descbase desc-name (thunk desc-value)) ; self replace
                                 desc-value)))]
            [(do-parse <desc-name> css-parse null declared-values)
             => (λ [desc-value] (desc-set! desc-name important? (thunk (normalize desc-value))))]))
    (let cascade ([subproperties : CSS-Cascading-Declarations properties])
      (for ([property (in-list subproperties)])
        (cond [(list? property) (cascade property)]
              [else (let* ([<desc-name> : CSS:Ident (css-declaration-name property)]
                           [desc-name : Symbol (if case-sensitive? (css:ident-datum <desc-name>) (css:ident-norm <desc-name>))])
                      (cond [(symbol-unreadable? desc-name) (varbase-set! descbase desc-name property)]
                            [else (let ([important? : Boolean (css-declaration-important? property)])
                                    (when (desc-more-important? desc-name important?)
                                      (define declared-values : (Listof+ CSS-Token) (css-declaration-values property))
                                      (define lazy? : Boolean (css-declaration-lazy? property))
                                      (define info : CSS-Declaration-Parser
                                        (desc-parsers desc-name (thunk (void (make+exn:css:deprecated <desc-name>)))))
                                      (cond [(false? info) (when (css-warning-unknown-property?) (make+exn:css:unrecognized <desc-name>))]
                                            [(void? info) (when (css-warning-unknown-property?) (make+exn:css:unrecognized <desc-name>))]
                                            [(pair? info) (parse-long info <desc-name> declared-values important? lazy?)]
                                            [else (parse-desc info <desc-name> desc-name declared-values important? lazy?)])))]))])))
    descbase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-cascade* : (All (Preference Env) (case-> [-> (Listof CSS-StyleSheet) (Listof CSS-Subject) CSS-Declaration-Parsers
                                                         (CSS-Cascaded-Value-Filter Preference) (Option CSS-Values)
                                                         (Values (Listof Preference) (Listof CSS-Values))]
                                                     [-> (Listof CSS-StyleSheet) (Listof CSS-Subject) CSS-Declaration-Parsers
                                                         (CSS-Cascaded-Value+Filter Preference Env)
                                                         (Option CSS-Values) Env
                                                         (Values (Listof Preference) (Listof CSS-Values))]))
  (let ()
    (define do-cascade* : (All (Preference) (-> (Listof CSS-StyleSheet) (Listof CSS-Subject) CSS-Declaration-Parsers
                                                (Option CSS-Values) (-> CSS-Values Preference)
                                                (Values (Listof Preference) (Listof CSS-Values))))
      (lambda [stylesheets stcejbus desc-parsers inherited-values do-value-filter]
        (define-values (secnereferp seulav)
          (let cascade-stylesheets : (values (Listof Preference) (Listof CSS-Values)) ([batch : (Listof CSS-StyleSheet) stylesheets]
                                                                                       [all-secnereferp : (Listof Preference) null]
                                                                                       [all-seulav : (Listof CSS-Values) null])
            (for/fold ([accu-secnereferp : (Listof Preference) all-secnereferp] [accu-values : (Listof CSS-Values) all-seulav])
                      ([stylesheet (in-list batch)])
              (define imported-identities : (Listof Positive-Integer) (css-stylesheet-imports stylesheet))
              (define-values (sub-secnereferp sub-seulav)
                (cascade-stylesheets (for/list : (Listof CSS-StyleSheet) ([import (in-list imported-identities)])
                                       (hash-ref (css-stylesheet-pool stylesheet) import))
                                     accu-secnereferp accu-values))
              (define this-values : (Listof CSS-Values)
                (css-cascade-rules* (css-stylesheet-rules stylesheet) stcejbus desc-parsers (css-select-quirk-mode?)
                                    (css-stylesheet-preferences stylesheet)))
              (for/fold ([this-secnereferp : (Listof Preference) sub-secnereferp]
                         [this-seulav : (Listof CSS-Values) sub-seulav])
                        ([declared-values : CSS-Values (in-list this-values)])
                (css-resolve-variables declared-values inherited-values)
                (values (cons (do-value-filter declared-values) this-secnereferp)
                        (cons declared-values this-seulav))))))
        (values (reverse secnereferp) (reverse seulav))))
    (case-lambda
      [(stylesheets stcejbus desc-parsers value-filter inherited-values)
       (do-cascade* stylesheets stcejbus desc-parsers inherited-values
                    (λ [[declared-values : CSS-Values]] (value-filter declared-values inherited-values)))]
      [(stylesheets stcejbus desc-parsers value-filter inherited-values env)
       (do-cascade* stylesheets stcejbus desc-parsers inherited-values
                    (λ [[declared-values : CSS-Values]] (value-filter declared-values inherited-values env)))])))

(define css-cascade-rules* : (->* ((Listof CSS-Grammar-Rule) (Listof CSS-Subject) CSS-Declaration-Parsers)
                                  (Boolean CSS-Media-Preferences) (Listof CSS-Values))
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  (lambda [rules stcejbus desc-parsers [quirk? #false] [top-preferences (default-css-media-preferences)]]
    (define-values (ordered-srcs single?) (css-select-rules rules stcejbus quirk? top-preferences))
    (call-with-css-size-from-media #:preferences top-preferences
      (cond [(and single?) (map (λ [[src : CSS-Style-Metadata]] (css-cascade-declarations desc-parsers (vector-ref src 1))) ordered-srcs)]
            [else (for/list : (Listof CSS-Values) ([src (in-list ordered-srcs)])
                    (define alter-preferences : CSS-Media-Preferences (vector-ref src 2))
                    (if (eq? alter-preferences top-preferences)
                        (css-cascade-declarations desc-parsers (vector-ref src 1))
                        (call-with-css-size-from-media #:preferences alter-preferences
                          (css-cascade-declarations desc-parsers (vector-ref src 1)))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define !imptbase : (HashTable CSS-Values (HashTable Symbol Boolean)) ((inst make-hasheq CSS-Values (HashTable Symbol Boolean))))

(define css-select-rules : (->* ((Listof CSS-Grammar-Rule) (Listof CSS-Subject)) (Boolean CSS-Media-Preferences)
                                (Values (Listof CSS-Style-Metadata) Boolean))
  ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
  ;;; https://drafts.csswg.org/selectors/#data-model
  (lambda [rules stcejbus [quirk? #false] [top-preferences (default-css-media-preferences)]]
    (define-values (selected-styles single-preference?)
      (let cascade-rule : (Values (Listof CSS-Style-Metadata) Boolean) ([preferences : CSS-Media-Preferences top-preferences]
                                                                        [grammars : (Listof CSS-Grammar-Rule) rules]
                                                                        [stylebase : (Listof CSS-Style-Metadata) null]
                                                                        [single? : Boolean #true])
        (for/fold ([styles stylebase] [single-preference? single?])
                  ([style (in-list grammars)])
          (cond [(css-@rule? style) (values styles single-preference?)]
                [(pair? style) (cascade-rule (cdr style) (car style) styles #false)]
                [else (let ([selectors : (Listof+ CSS-Complex-Selector) (css-style-rule-selectors style)])
                        (define specificity : Nonnegative-Fixnum
                          (for/fold ([max-specificity : Nonnegative-Fixnum 0]) ([selector (in-list selectors)])
                            (define matched-specificity : Nonnegative-Fixnum (or (css-selector-match selector stcejbus quirk?) 0))
                            (fxmax matched-specificity max-specificity)))
                        (cond [(zero? specificity) (values styles single-preference?)]
                              [else (let ([sm : CSS-Style-Metadata (vector specificity (css-style-rule-properties style) preferences)])
                                      (values (cons sm styles) single-preference?))]))]))))
    (values (sort (reverse selected-styles) #| `reverse` guarantees orginal order |#
                  (λ [[sm1 : CSS-Style-Metadata] [sm2 : CSS-Style-Metadata]] (fx< (vector-ref sm1 0) (vector-ref sm2 0))))
            single-preference?)))
