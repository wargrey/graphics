#lang typed/racket

;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet

(provide (all-defined-out))
  
(require "digicore.rkt")
(require "tokenizer.rkt")
(require "parser.rkt")
(require "conditional.rkt")
(require "device-adapt.rkt")
(require "selector.rkt")
(require "misc.rkt")

(require (for-syntax syntax/parse))

;; https://drafts.csswg.org/css-syntax/#css-stylesheets
(define-type CSS-Media-Rule (Pairof (Listof CSS-Grammar-Rule) CSS-Media-Preferences))
(define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule CSS-Media-Rule))
(define-type CSS-StyleSheet-Pool (HashTable Natural CSS-StyleSheet))

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
  
(define-css-parser-entry read-css-stylesheet :-> CSS-StyleSheet
  ;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet
  ;;; https://drafts.csswg.org/css-syntax/#charset-rule
  ;;; https://drafts.csswg.org/css-namespaces
  ;;; https://drafts.csswg.org/css-cascade/#at-import
  ;;; https://drafts.csswg.org/css-conditional/#processing
  ;;; https://drafts.csswg.org/css-conditional/#contents-of
  ;;; https://drafts.csswg.org/css-device-adapt/#media-queries
  (lambda [/dev/cssin [pool : CSS-StyleSheet-Pool ((inst make-hasheq Natural CSS-StyleSheet))]]
    (define location : (U String Symbol) (let ([p (object-name /dev/cssin)]) (if (symbol? p) p (~a p))))
    (define identity : Natural (if (string? location) (css-stylesheet-path->identity location) 0))
    (define init-viewport : CSS-Media-Preferences (default-css-media-preferences))
    (or (and (string? location) (hash-has-key? pool identity)
             (let ([stylesheet (hash-ref pool identity)])
               (and (not (css-stylesheet-outdated? stylesheet))
                    (css-update-imported-stylesheets stylesheet)
                    stylesheet)))
        (let ([rules (css-consume-stylesheet /dev/cssin)])
          (when (positive? identity) (hash-set! pool identity css-stylesheet-placeholder))
          (define namespaces : CSS-Namespace (make-hasheq))
          (define-values (viewport imports grammars)
            (css-syntax-rules->grammar-rules location rules namespaces #true #true init-viewport pool))
          (define timestamp : Integer (if (string? location) (file-or-directory-modify-seconds location) (current-seconds)))
          (define stylesheet : CSS-StyleSheet (make-css-stylesheet pool location timestamp viewport imports namespaces grammars))
          (when (positive? identity) (hash-set! pool identity stylesheet))
          stylesheet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-syntax-rules->grammar-rules : (->* ((U String Symbol) (Listof CSS-Syntax-Rule) CSS-Namespace Boolean Boolean)
                                               (CSS-Media-Preferences CSS-StyleSheet-Pool)
                                               (Values CSS-Media-Preferences (Listof Positive-Integer) (Listof CSS-Grammar-Rule)))
  (lambda [src syntaxes0 namespaces can-import0? allow-namespace0?
               [init-viewport (default-css-media-preferences)]
               [pool ((inst make-hasheq Natural CSS-StyleSheet))]]
    (define-values (viewport-srotpircsed !viewport-sexatnys)
      (for/fold ([viewport-srotpircsed : (Listof CSS-Declarations) null]
                 [normal-sexatnys : (Listof CSS-Syntax-Rule) null])
                ([stx : CSS-Syntax-Rule (in-list syntaxes0)])
        (define ?descriptor : (U CSS-Declarations CSS-Syntax-Error Void)
          (when (and (css-@rule? stx) (css:@keyword-norm=:=? (css-@rule-name stx) '#:@viewport))
            (define prelude : (Listof CSS-Token) (css-@rule-prelude stx))
            (define ?block : (Option CSS:Block) (css-@rule-block stx))
            (cond [(css-pair? prelude) (make+exn:css:overconsumption prelude)]
                  [?block (css-components->declarations (css:block-components ?block))]
                  [else (make+exn:css:missing-block (css-@rule-name stx))])))
        (cond [(void? ?descriptor) (values viewport-srotpircsed (cons stx normal-sexatnys))]
              [(exn? ?descriptor) (values viewport-srotpircsed normal-sexatnys)]
              [else (values (cons ?descriptor viewport-srotpircsed) normal-sexatnys)])))
    (define viewport : CSS-Media-Preferences
      (cond [(null? viewport-srotpircsed) init-viewport]
            [else (css-cascade-viewport init-viewport (reverse viewport-srotpircsed)
                                        (default-css-viewport-parsers) (default-css-viewport-filter))]))
    (let syntax->grammar : (Values CSS-Media-Preferences (Listof Positive-Integer) (Listof CSS-Grammar-Rule))
      ([seititnedi : (Listof Positive-Integer) null]
       [srammarg : (Listof CSS-Grammar-Rule) null]
       [!viewport-syntaxes : (Listof CSS-Syntax-Rule) (reverse !viewport-sexatnys)]
       [can-import? : Boolean can-import0?]
       [allow-namespace? : Boolean allow-namespace0?])
      (cond [(null? !viewport-syntaxes) (values viewport (reverse seititnedi) (reverse srammarg))]
            [else (let-values ([(stx rest) (values (car !viewport-syntaxes) (cdr !viewport-syntaxes))])
                    (if (css-qualified-rule? stx)
                        (let ([?rule : (U CSS-Style-Rule CSS-Syntax-Error) (css-qualified-rule->style-rule stx namespaces)])
                          (define ++srammarg : (Listof CSS-Grammar-Rule)
                            (cond [(or (exn? ?rule) (null? (css-style-rule-properties ?rule))) srammarg]
                                  [else (cons ?rule srammarg)]))
                          (syntax->grammar seititnedi ++srammarg rest #false #false))
                        (case (css:@keyword-norm (css-@rule-name stx))
                          [(#:@charset)
                           (make+exn:css:misplaced (css-@rule-name stx))
                           (syntax->grammar seititnedi srammarg rest can-import? allow-namespace?)]
                          [(#:@import)
                           (define css-id : (U Positive-Integer False CSS-Syntax-Error)
                             (cond [(false? can-import?) (make+exn:css:misplaced (css-@rule-name stx))]
                                   [else (css-@import->stylesheet-identity stx src viewport pool)]))
                           (syntax->grammar (if (integer? css-id) (cons css-id seititnedi) seititnedi)
                                            srammarg rest can-import? allow-namespace?)]
                          [(#:@namespace)
                           (define css-ns : (U (Pairof Symbol String) CSS-Syntax-Error)
                             (cond [allow-namespace? (css-@namespace->namespace stx)]
                                   [else (make+exn:css:misplaced (css-@rule-name stx))]))
                           (when (pair? css-ns) (hash-set! namespaces (car css-ns) (cdr css-ns)))
                           (syntax->grammar seititnedi srammarg rest #false allow-namespace?)]
                          [(#:@media)
                           (define media-rules (css-@media->media-rule stx viewport namespaces pool))
                           (define ++srammarg : (Listof CSS-Grammar-Rule)
                             (cond [(list? media-rules) (append (reverse media-rules) srammarg)]
                                   [(pair? media-rules) (cons media-rules srammarg)]
                                   [else srammarg]))
                           (syntax->grammar seititnedi ++srammarg rest #false #false)]
                          [(#:@support)
                           (define expand-stxes (css-@support->syntax-rules stx))
                           (define delta : (Listof CSS-Syntax-Rule) (if (pair? expand-stxes) (append expand-stxes rest) rest))
                           (syntax->grammar seititnedi srammarg delta #false #false)]
                          [else (syntax->grammar seititnedi (cons stx srammarg) rest #false #false)])))]))))
  
(define css-@import->stylesheet-identity : (-> CSS-@Rule Any CSS-Media-Preferences CSS-StyleSheet-Pool
                                               (U Positive-Integer False CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-cascade/#at-import
  (lambda [import parent-href preferences pool]
    (define-values (uri ?condition) (css-car (css-@rule-prelude import)))
    (define name : CSS:@Keyword (css-@rule-name import))
    (define ?block : (Option CSS:Block) (css-@rule-block import))
    (define ?target.css : (U Path CSS-Syntax-Error)
      (cond [(eof-object? uri) (make+exn:css:empty (css-@rule-name import))]
            [(css:string=<-? uri non-empty-string?) => (λ [url] (css-url-string->path parent-href url))]
            [(css:url=<-? uri string?) => (λ [url] (css-url-string->path parent-href url))]
            [(or (css:string? uri) (css:url? uri)) (make+exn:css:empty uri)]
            [else (make+exn:css:type uri)]))
    (cond [(exn? ?target.css) ?target.css]
          [(css:block? ?block) (make+exn:css:overconsumption ?block)]
          [(false? (regexp-match? #px"\\.css$" ?target.css)) (make+exn:css:resource uri)]
          [(false? (file-exists? ?target.css)) (make+exn:css:resource uri)]
          [else (let-values ([(?support ?media-list) (css-car ?condition)])
                  (define-values (support? media-list)
                    (if (css:function-norm=:=? ?support 'supports)
                        (let* ([components (css:function-arguments ?support)]
                               [supports (list (css-remake-token ?support css:block #\( components #false))]
                               [query (css-parse-feature-query supports name)])
                          (values (css-query-support? query (default-css-feature-support?)) ?media-list))
                        (values #true ?condition)))
                  (and support? (css-media-queries-support? (css-parse-media-queries media-list name) preferences)
                       (parameterize ([default-css-media-preferences preferences])
                         (read-css-stylesheet ?target.css pool))
                       (css-stylesheet-path->identity ?target.css)))])))
    
(define css-@namespace->namespace : (-> CSS-@Rule (U (Pairof Symbol String) CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-namespaces/#syntax
  (lambda [ns]
    (define-values (1st rest) (css-car (css-@rule-prelude ns)))
    (define-values (2nd terminal) (css-car rest))
    (define ?block : (Option CSS:Block) (css-@rule-block ns))
    (define namespace : (U String CSS-Syntax-Error)
      (let ([uri (if (eof-object? 2nd) 1st 2nd)])
        (cond [(css:string? uri) (css:string-datum uri)]
              [(css:url? uri) (~a (css:url-datum uri))]
              [(eof-object? 1st) (make+exn:css:empty (css-@rule-name ns))]
              [else (make+exn:css:type uri)])))
    (cond [(exn? namespace) namespace]
          [(css:block? ?block) (make+exn:css:overconsumption ?block)]
          [(css-pair? terminal) (make+exn:css:overconsumption terminal)]
          [(css:ident? 1st) (cons (css:ident-datum 1st) namespace)]
          [(eof-object? 2nd) (cons '|| namespace)]
          [else (make+exn:css:type 1st)])))

(define css-@media->media-rule : (-> CSS-@Rule CSS-Media-Preferences CSS-Namespace CSS-StyleSheet-Pool
                                     (U (Listof CSS-Grammar-Rule) CSS-Media-Rule CSS-Syntax-Error Void))
  ;;; https://drafts.csswg.org/css-conditional/#contents-of
  ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
  (lambda [media preferences namespaces pool]
    (define name : CSS:@Keyword (css-@rule-name media))
    (define ?block : (Option CSS:Block) (css-@rule-block media))
    (cond [(false? ?block) (make+exn:css:missing-block name)]
          [(css-null? (css:block-components ?block)) (void)]
          [else (when (css-media-queries-support? (css-parse-media-queries (css-@rule-prelude media) name) preferences)
                  (define stxes : (Listof CSS-Syntax-Rule) (css-parse-rules (css:block-components ?block)))
                  (when (pair? stxes)
                    (define-values (viewport _ grammars)
                      (css-syntax-rules->grammar-rules 'src stxes namespaces #false #false preferences pool))
                    (when (pair? grammars)
                      (cond [(eq? viewport preferences) grammars]
                            [else (cons grammars viewport)]))))])))

(define css-@support->syntax-rules : (-> CSS-@Rule (U (Listof CSS-Syntax-Rule) CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-conditional/#contents-of
  ;;; https://drafts.csswg.org/css-conditional/#at-supports
  (lambda [support]
    (define name : CSS:@Keyword (css-@rule-name support))
    (define ?block : (Option CSS:Block) (css-@rule-block support))
    (cond [(false? ?block) (make+exn:css:missing-block name)]
          [(css-null? (css:block-components ?block)) null]
          [(not (css-query-support? (css-parse-feature-query (css-@rule-prelude support) name) (default-css-feature-support?))) null]
          [else (css-parse-rules (css:block-components ?block))])))
  
(define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule (Option CSS-Namespace) (U CSS-Style-Rule CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-syntax/#style-rules
  ;;; https://drafts.csswg.org/selectors/#invalid
  (lambda [qr namespaces]
    (define prelude : (Listof+ CSS-Token) (css-qualified-rule-prelude qr))
    (define components : (Listof CSS-Token) (css:block-components (css-qualified-rule-block qr)))
    (define ?selectors : (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error) (css-components->selectors prelude namespaces))
    (cond [(exn? ?selectors) ?selectors]
          [else (make-css-style-rule ?selectors (css-components->declarations components))])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-cascade-viewport : (->* (CSS-Media-Preferences (Listof CSS-Declarations))
                                    (CSS-Declaration-Parsers (CSS-Cascaded-Value-Filter (HashTable Symbol CSS-Media-Datum)))
                                    CSS-Media-Preferences)
  ;;; https://drafts.csswg.org/css-device-adapt/#atviewport-rule
  (lambda [viewport-preferences viewport-descriptors
                                [viewport-parser (default-css-viewport-parsers)]
                                [viewport-filter (default-css-viewport-filter)]]
    (call-with-css-media #:preferences viewport-preferences
      (viewport-filter (css-cascade-declarations viewport-parser viewport-descriptors)
                       viewport-preferences
                       #false))))

(define css-cascade : (All (Preference) (-> (Listof CSS-StyleSheet) CSS-Subject
                                            CSS-Declaration-Parsers (CSS-Cascaded-Value-Filter Preference)
                                            Preference (Option CSS-Values) [#:quirk? Boolean]
                                            (Values Preference CSS-Values)))
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  ;;; https://drafts.csswg.org/css-variables/#cycles
  (lambda [stylesheets subject desc-filter value-filter initial-values inherited-values #:quirk? [quirk? #false]]
    (define declared-values : CSS-Values (make-css-values))
    (let cascade-stylesheets ([batch : (Listof CSS-StyleSheet) stylesheets])
      (for ([stylesheet (in-list batch)])
        (define child-identities : (Listof Positive-Integer) (css-stylesheet-imports stylesheet))
        (cascade-stylesheets (for/list : (Listof CSS-StyleSheet) ([import (in-list child-identities)])
                               (hash-ref (css-stylesheet-pool stylesheet) import)))
        (css-cascade-rules (css-stylesheet-rules stylesheet) subject desc-filter quirk?
                           (css-stylesheet-preferences stylesheet) declared-values)))
    (css-resolve-variables declared-values inherited-values)
    (values (value-filter declared-values initial-values inherited-values) declared-values)))

(define css-cascade-rules : (->* ((Listof CSS-Grammar-Rule) CSS-Subject CSS-Declaration-Parsers)
                                 (Boolean CSS-Media-Preferences CSS-Values) CSS-Values)
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
  ;;; https://drafts.csswg.org/selectors/#data-model
  (lambda [rules subject desc-filter [quirk? #false] [top-preferences (default-css-media-preferences)] [descbase (make-css-values)]]
    ; NOTE: defined the `Style-Metadata` as `List` will slow down the parsing,
    ;       even though this code is not reached at that stage.
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
                            (define matched-specificity : Nonnegative-Fixnum (or (css-selector-match selector subject quirk?) 0))
                            (fxmax matched-specificity max-specificity)))
                        (cond [(zero? specificity) (values styles single-preference?)]
                              [else (let ([sm : Style-Metadata (vector specificity (css-style-rule-properties style) preferences)])
                                      (values (cons sm styles) single-preference?))]))]))))
    (unless (null? selected-rules-style)
      (define ordered-sources : (Listof Style-Metadata)
        (sort (reverse selected-rules-style)
              (λ [[sm1 : Style-Metadata] [sm2 : Style-Metadata]]
                (fx< (vector-ref sm1 0) (vector-ref sm2 0)))))
      (call-with-css-media #:preferences top-preferences
        (if (and single-preference?)
            (let ([source-ref (λ [[src : Style-Metadata]] : CSS-Declarations (vector-ref src 1))])
              (css-cascade-declarations desc-filter (map source-ref ordered-sources) descbase))
            (for ([src (in-list ordered-sources)])
              (define alter-preferences : CSS-Media-Preferences (vector-ref src 2))
              (if (eq? alter-preferences top-preferences)
                  (css-cascade-declarations desc-filter (vector-ref src 1) descbase)
                  (call-with-css-media #:preferences alter-preferences
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-components->declarations : (-> (Listof CSS-Token) CSS-Declarations)
  (lambda [components]
    (let make-style-rule : CSS-Declarations ([seitreporp : CSS-Declarations null] [tokens : (Listof CSS-Token) components])
      (define-values (id any-values) (css-car tokens))
      (define-values (:values rest)
        (let collect : (Values (Listof CSS-Token) (Listof CSS-Token)) ([seulav : (Listof CSS-Token) null]
                                                                       [rest : (Listof CSS-Token) any-values])
          (define-values (head tail) (css-car/cdr rest))
          (cond [(or (eof-object? head) (css:semicolon? head)) (values (reverse seulav) tail)]
                [(and (css:block=:=? head #\{) (css:@keyword? id)) (values (reverse (cons head seulav)) tail)]
                [else (collect (cons head seulav) tail)])))
      (cond [(eof-object? id) (reverse seitreporp)]
            [(css:ident? id) (make-style-rule (css-cons (css-components->declaration id :values) seitreporp) rest)]
            [else (make-style-rule (css-cons (make+exn:css:type:identifier (cons id :values)) seitreporp) rest)]))))

(define css-stylesheet-path->identity : (-> Path-String Positive-Integer)
  (lambda [uri.css]
    (file-or-directory-identity uri.css)))

(define css-stylesheet-outdated? : (-> CSS-StyleSheet Boolean)
  (lambda [stylesheet]
    (define location : (U String Symbol) (css-stylesheet-location stylesheet))
    (and (string? location)
         (file-exists? location)
         (< (css-stylesheet-timestamp stylesheet)
            (file-or-directory-modify-seconds location)))))

(define css-update-imported-stylesheets : (-> CSS-StyleSheet Void)
  (lambda [stylesheet]
    (define pool (css-stylesheet-pool stylesheet))
    (for ([id (in-list (css-stylesheet-imports stylesheet))])
      (define child : (Option CSS-StyleSheet) (hash-ref pool id (const #false)))
      (when (css-stylesheet? child)
        (define child.css : (U Symbol String) (css-stylesheet-location child))
        (when (string? child.css)
          (read-css-stylesheet (string->path child.css) pool))))))
  
(define css-url-string->path : (-> Any String Path)
  (lambda [parent-location uri]
    (define uri.css : Path-String
      (cond [(absolute-path? (string->path uri)) uri]
            [else (let ([pwd (or (and (string? parent-location) (path-only parent-location)) (current-directory))])
                    (build-path pwd uri))]))
    (simple-form-path uri.css)))

(define css-resolve-variables : (-> CSS-Values (Option CSS-Values) CSS-Values)
  ;;; https://drafts.csswg.org/css-variables/#cycles
  (lambda [declared-values inherited-values]
    (define declared-vars : CSS-Variable-Values (css-values-variables declared-values))
    (for ([(--var --value) (in-hash declared-vars)])
      (when (and (css-declaration? --value) (css-declaration-lazy? --value))
        (define property : CSS:Ident (css-declaration-name --value))
        (define --values : (Listof+ CSS-Token) (css-declaration-values --value))
        (define flat-values : (Listof CSS-Token) (css-variable-substitute property --values declared-vars (list --var)))
        (hash-set! declared-vars --var
                   (cond [(pair? flat-values) (struct-copy css-declaration --value [values flat-values] [lazy? #false])]
                         [else null]))))
    (unless (false? inherited-values)
      (for ([(--var --value) (in-hash (css-values-variables inherited-values))])
        (hash-ref! declared-vars --var (thunk --value))))
    declared-values))

(define css-variable-substitute : (-> CSS:Ident (Listof CSS-Token) CSS-Variable-Values (Listof Symbol) (Listof CSS-Token))
  ;;; https://drafts.csswg.org/css-variables/#invalid-variables
  (lambda [property var-values varbase refpath]
    (let var-fold ([seulav : (Listof CSS-Token) null]
                   [--values : (Listof CSS-Token) var-values])
      (define-values (head tail) (css-car/cdr --values))
      (cond [(eof-object? head) (if (css-pair? seulav) (reverse (filter-not css:whitespace? seulav)) seulav)]
            [(not (css-lazy-token? head)) (var-fold (cons head seulav) tail)]
            [(and (css:function? head) (css:function-lazy? head))
             (define args : (Listof CSS-Token) (css-variable-substitute property (css:function-arguments head) varbase refpath))
             (if (null? args) null (var-fold (cons (css:function-copy head args #false) seulav) tail))]
            [(and (css:λracket? head) (css:λracket-lazy? head))
             (define args : (Listof CSS-Token) (css-variable-substitute property (css:λracket-arguments head) varbase refpath))
             (define argl : (U (Listof CSS-Token) CSS-Syntax-Error) (css-λarguments-filter args))
             (if (or (null? args) (exn:css? argl)) null (var-fold (cons (css:λracket-copy head argl #false) seulav) tail))]
            [(and (css:url? head) (css:url-lazy? head))
             (define args : (Listof CSS-Token) (css-variable-substitute property (css:url-modifiers head) varbase refpath))
             (define mods : (Listof CSS-URL-Modifier) (css-url-modifiers-filter head args))
             (if (null? args) null (var-fold (cons (css:url-copy head mods #false) seulav) tail))]
            [(and (css:block? head) (css:block-lazy? head))
             (define coms : (Listof CSS-Token) (css-variable-substitute property (css:block-components head) varbase refpath))
             (if (null? coms) null (var-fold (cons (css:block-copy head coms #false) seulav) tail))]
            [(not (css:var? head)) (var-fold (cons head seulav) tail)]
            [(memq (css:var-datum head) refpath) (make+exn:css:cyclic head property 'debug) null]
            [else (let ([--var (css:var-datum head)])
                    (define --value : (U CSS-Declaration Null) (hash-ref varbase --var (thunk null)))
                    (define-values (--vs lazy?)
                      (cond [(null? --value) (values (css:var-fallback head) (css:var-lazy? head))]
                            [else (values (css-declaration-values --value) (css-declaration-lazy? --value))]))
                    (cond [(null? --vs) (make+exn:css:missing-value head property 'debug) null]
                          [(not lazy?) (var-fold (append (reverse --vs) seulav) tail)]
                          [else (let ([vs (css-variable-substitute property --vs varbase (cons --var refpath))])
                                  (if (null? vs) vs (var-fold (append (reverse vs) seulav) tail)))]))]))))
