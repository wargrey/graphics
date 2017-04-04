#lang typed/racket

;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet

(provide (all-defined-out))
  
(require "digicore.rkt")
(require "parser.rkt")
(require "conditional.rkt")
(require "device-adapt.rkt")
(require "selector.rkt")
(require "cascade.rkt")
(require "misc.rkt")

;; https://drafts.csswg.org/css-syntax/#css-stylesheets  
(define-css-parser-entry read-css-stylesheet #:-> CSS-StyleSheet
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
          (define namespaces : (HashTable Symbol String) (make-hasheq))
          (define-values (viewport imports grammars)
            (css-syntax-rules->grammar-rules location rules namespaces #true #true init-viewport pool))
          (define timestamp : Integer (if (string? location) (file-or-directory-modify-seconds location) (current-seconds)))
          (define stylesheet : CSS-StyleSheet (make-css-stylesheet pool location timestamp viewport imports namespaces grammars))
          (when (positive? identity) (hash-set! pool identity stylesheet))
          stylesheet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-syntax-rules->grammar-rules : (->* ((U String Symbol) (Listof CSS-Syntax-Rule) (HashTable Symbol String) Boolean Boolean)
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

(define css-@media->media-rule : (-> CSS-@Rule CSS-Media-Preferences (HashTable Symbol String) CSS-StyleSheet-Pool
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
  
(define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule (Option (HashTable Symbol String)) (U CSS-Style-Rule CSS-Syntax-Error))
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
    (call-with-css-size-from-media #:preferences viewport-preferences
      (parameterize ([default-css-media-preferences viewport-preferences])
        (viewport-filter (css-cascade-declarations viewport-parser viewport-descriptors) #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
