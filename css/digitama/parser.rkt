#lang typed/racket

;;; https://drafts.csswg.org/css-syntax/#parsing

(provide (all-defined-out))

(require "digicore.rkt")
(require "tokenizer.rkt")
(require "conditional.rkt")
(require "selector.rkt")
(require "stdin.rkt")
(require "misc.rkt")

(define-syntax (define-css-parser-entry stx)
  ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
  (syntax-case stx [: :-> lambda]
    [(_ id :-> ->T (lambda [cssin [args : T defval ...] ...] body ...))
     #'(define (id [/dev/stdin : CSS-StdIn (current-input-port)] [args : T defval ...] ...) : ->T
         (define /dev/cssin : Input-Port (css-open-input-port /dev/stdin))
         (dynamic-wind (thunk (port-count-lines! /dev/cssin))
                       (thunk ((λ [[cssin : Input-Port] [args : T defval ...] ...] : ->T body ...) /dev/cssin args ...))
                       (thunk (close-input-port /dev/cssin))))]))

;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
(define-css-parser-entry css-parse-stylesheet :-> (Listof CSS-Syntax-Rule)
  ;;; https://drafts.csswg.org/css-syntax/#parse-stylesheet
  ;;; https://drafts.csswg.org/css-syntax/#declaration-rule-list
  (lambda [/dev/cssin]
    (css-consume-stylesheet /dev/cssin)))

(define-css-parser-entry css-parse-rules :-> (Listof CSS-Syntax-Rule)
  ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-rules
  ;;; https://drafts.csswg.org/css-syntax/#declaration-rule-list
  (lambda [/dev/cssin]
    (css-consume-rules /dev/cssin #false)))

(define-css-parser-entry css-parse-rule :-> (U CSS-Syntax-Rule CSS-Syntax-Error)
  ;;; https://drafts.csswg.org/css-syntax/#parse-rule
  (lambda [/dev/cssin]
    (define stx (css-read-syntax/skip-whitespace /dev/cssin))
    (define retval : (U CSS-Qualified-Rule CSS-@Rule CSS-Syntax-Error)
      (cond [(eof-object? stx) (make+exn:css:empty stx)]
            [(css:@keyword? stx) (css-consume-@rule /dev/cssin stx)]
            [else (css-consume-qualified-rule /dev/cssin stx)]))
    (define end (css-read-syntax/skip-whitespace /dev/cssin))
    (cond [(or (eof-object? end) (exn? retval)) retval]
          [else (make+exn:css:overconsumption end)])))

(define-css-parser-entry css-parse-declaration :-> (U CSS-Declaration CSS-Syntax-Error)
  ;;; https://drafts.csswg.org/css-syntax/#declaration
  ;;; https://drafts.csswg.org/css-syntax/#parse-declaration
  ;;; https://drafts.csswg.org/css-conditional/#at-ruledef-supports
  (lambda [/dev/cssin]
    (define token (css-read-syntax/skip-whitespace /dev/cssin))
    (cond [(not (css:ident? token)) (make+exn:css:type:identifier token)]
          [else (let-values ([(components _) (css-consume-components /dev/cssin)])
                  (css-components->declaration token components))])))

(define-css-parser-entry css-parse-declarations :-> (Listof (U CSS-Declaration CSS-@Rule))
  ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-declarations
  ;;; https://drafts.csswg.org/css-syntax/#consume-a-list-of-declarations
  (lambda [/dev/cssin]
    (let consume-declaration+@rule ([mixed-list : (Listof (U CSS-Declaration CSS-@Rule)) null])
      (define token (css-read-syntax /dev/cssin))
      (cond [(eof-object? token) (reverse mixed-list)]
            [(or (css:whitespace? token) (css:semicolon? token)) (consume-declaration+@rule mixed-list)]
            [(css:@keyword? token) (consume-declaration+@rule (cons (css-consume-@rule /dev/cssin token) mixed-list))]
            [else (let-values ([(components _) (css-consume-components /dev/cssin #\;)])
                    (define ?declaration : (U CSS-Declaration CSS-Syntax-Error)
                      (cond [(css:ident? token) (css-components->declaration token components)]
                            [else (make+exn:css:type:identifier token)]))
                    (consume-declaration+@rule (css-cons ?declaration mixed-list)))]))))
  
(define-css-parser-entry css-parse-component-value :-> (U CSS-Token CSS-Syntax-Error)
  ;;; https://drafts.csswg.org/css-syntax/#parse-component-value
  (lambda [/dev/cssin]
    (define token (css-read-syntax/skip-whitespace /dev/cssin))
    (cond [(eof-object? token) (make+exn:css:empty token)]
          [else (let ([retval (css-consume-component-value /dev/cssin token)])
                  (define end (css-read-syntax/skip-whitespace /dev/cssin))
                  (cond [(eof-object? end) retval]
                        [else (make+exn:css:overconsumption end)]))])))

(define-css-parser-entry css-parse-component-values :-> (Listof CSS-Token)
  ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
  (lambda [/dev/cssin]
    (define-values (components _) (css-consume-components /dev/cssin))
    components))

(define-css-parser-entry css-parse-component-valueses :-> (Listof (Listof CSS-Token))
  ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
  (lambda [/dev/cssin]
    (css-consume-componentses /dev/cssin #:omit-comma? #false)))

(define-css-parser-entry css-parse-media-queries :-> (Listof CSS-Media-Query)
  ;;; https://drafts.csswg.org/mediaqueries/#media-types
  ;;; https://drafts.csswg.org/mediaqueries/#mq-list
  ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
  ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-query-list
  ;;; https://drafts.csswg.org/mediaqueries/#error-handling
  (lambda [/dev/cssin [rulename : CSS-Syntax-Any eof]]
    (for/list : (Listof CSS-Media-Query) ([entry (in-list (css-consume-componentses /dev/cssin #:omit-comma? #true))])
      (with-handlers ([exn:css? (λ [[errcss : exn:css]] errcss)])
        (define-values (token tokens) (css-car entry))
        (define-values (next rest) (css-car tokens))
        (cond [(css:ident-norm=:=? token 'not)
               (cond [(css:ident? next) (css-components->media-type+query next #false rest)]
                     [else (css-components->negation token tokens #true)])]
              [(css:ident? token)
               (define-values (?type ?<and>)
                 (cond [(css:ident-norm=:=? token 'only) (values next rest)]
                       [else (values token tokens)]))
               (cond [(eof-object? ?type) (make+exn:css:malformed ?type)]
                     [(css:ident? ?type) (css-components->media-type+query ?type #true ?<and>)]
                     [else (make+exn:css:type:identifier ?type)])]
              [else (css-components->feature-query entry #true rulename)])))))

(define-css-parser-entry css-parse-feature-query :-> CSS-Feature-Query
  ;;; https://drafts.csswg.org/mediaqueries/#media-types
  ;;; https://drafts.csswg.org/css-conditional/#at-supports
  (lambda [/dev/cssin [rulename : CSS-Syntax-Any eof]]
    (define-values (conditions _) (css-consume-components /dev/cssin))
    (with-handlers ([exn:css? (λ [[errcss : exn:css]] errcss)])
      (css-components->feature-query conditions #false rulename))))
  
(define-css-parser-entry css-parse-selectors :-> (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error)
  ;;; https://drafts.csswg.org/selectors/#structure
  ;;; https://drafts.csswg.org/selectors/#parse-selector
  ;;; https://drafts.csswg.org/selectors/#selector-list
  ;;; https://drafts.csswg.org/selectors/#grouping
  (lambda [/dev/cssin]
    (define-values (components _) (css-consume-components /dev/cssin))
    (css-components->selectors components #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-consume-stylesheet : (-> Input-Port (Listof CSS-Syntax-Rule))
  ;;; https://drafts.csswg.org/css-syntax/#parse-stylesheet
  ;;; https://drafts.csswg.org/css-syntax/#declaration-rule-list
  (lambda [css]
    (define rules : (Listof CSS-Syntax-Rule) (css-consume-rules css #true))
    (define rule : (Option CSS-Syntax-Rule) (and (pair? rules) (car rules)))
    (if (and (css-@rule? rule) (css:@keyword-norm=:=? (css-@rule-name rule) '#:@charset))
        (cdr rules)
        rules)))
  
(define css-consume-rules : (-> Input-Port Boolean (Listof CSS-Syntax-Rule))
  ;;; https://drafts.csswg.org/css-syntax/#consume-list-of-rules
  (lambda [css toplevel?]
    (let consume-rules ([rules : (Listof CSS-Syntax-Rule) null])
      (define token (css-read-syntax css))
      (cond [(eof-object? token) (reverse rules)]
            [(css:whitespace? token) (consume-rules rules)]
            [(css:@keyword? token) (consume-rules (css-cons (css-consume-@rule css token) rules))]
            [(css:cd? token) (consume-rules (if toplevel? rules (css-cons (css-consume-qualified-rule css token) rules)))]
            [else (consume-rules (css-cons (css-consume-qualified-rule css token) rules))]))))

(define css-consume-@rule : (-> Input-Port CSS:@Keyword CSS-@Rule)
  ;;; https://drafts.csswg.org/css-syntax/#at-rule
  ;;; https://drafts.csswg.org/css-syntax/#consume-an-at-rule
  (lambda [css reconsumed-at-token]
    (define-values (prelude ?block) (css-consume-rule-item css #:@rule? #true))
    (make-css-@rule reconsumed-at-token prelude ?block)))

(define css-consume-qualified-rule : (-> Input-Port CSS-Token (U CSS-Qualified-Rule CSS-@Rule CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
  (lambda [css reconsumed]
    (define head (css-consume-component-value css reconsumed))
    (define-values (prelude ?block) (css-consume-rule-item css #:@rule? #false))
    (cond [(css:block? ?block) (make-css-qualified-rule (cons head prelude) ?block)]
          [else (make+exn:css:missing-block (cons head prelude))])))

(define css-consume-component-value : (-> Input-Port CSS-Token CSS-Token)
  ;;; https://drafts.csswg.org/css-syntax/#component-value
  ;;; https://drafts.csswg.org/css-syntax/#consume-a-component-value
  ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
  ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
  (lambda [css reconsumed]
    (cond [(css:delim? reconsumed)
           (case (css:delim-datum reconsumed)
             [(#\{) (css-consume-simple-block css reconsumed #\})]
             [(#\[) (css-consume-simple-block css reconsumed #\])]
             [(#\() (css-consume-simple-block css reconsumed #\))]
             [else reconsumed])]
          [(css:function? reconsumed) (css-consume-function css reconsumed)]
          [else reconsumed])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-consume-rule-item : (-> Input-Port #:@rule? Boolean (Values (Listof CSS-Token) (Option CSS:Block)))
  ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
  ;;; https://drafts.csswg.org/css-syntax/#consume-a-qualified-rule
  (lambda [css #:@rule? at-rule?]
    (let consume-item ([prelude : (Listof CSS-Token) null]
                       [simple-block : (Option CSS:Block) #false])
      (define token (css-read-syntax css))
      (cond [(or (eof-object? token) (and at-rule? (css:semicolon? token)))
             (when (eof-object? token) (make+exn:css:missing-delimiter prelude))
             (values (reverse prelude) simple-block)]
            [(css:delim=:=? token #\{) (values (reverse prelude) (css-consume-simple-block css token #\}))]
            [(css:block=:=? token #\{) (values (reverse prelude) token)]
            [else (consume-item (cons (css-consume-component-value css token) prelude) simple-block)]))))

(define css-consume-simple-block : (-> Input-Port CSS:Delim Char CSS:Block)
  ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
  (lambda [css open close-char]
    (define-values (components close end-token) (css-consume-block-body css open close-char))
    (css-remake-token [open end-token] css:block (css:delim-datum open) components #false)))

(define css-consume-function : (-> Input-Port CSS:Function (U CSS:Function CSS:URL))
  ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
  ;;; https://drafts.csswg.org/css-values/#functional-notations
  ;;; https://drafts.csswg.org/css-values/#urls
  (lambda [css func]
    (define fname : Symbol (css:function-datum func))
    (cond [(not (symbol-unreadable? fname)) func]
          [else (let-values ([(components close end-token) (css-consume-block-body css func #\))]
                             [(fnorm) (css:function-norm func)])
                  (if (eq? fnorm 'url)
                      (let-values ([(href modifiers) (css-car components)])
                        (define datum : (U String 'about:invalid)
                          (cond [(not (css:string? href)) 'about:invalid]
                                [else (let ([datum (css:string-datum href)])
                                        (if (string=? datum "") 'about:invalid datum))]))
                        (css-remake-token func css:url datum (css-url-modifiers-filter func modifiers) #false))
                      (let ([freadable (string->symbol (symbol->string fname))])
                        (css-remake-token [func end-token] css:function freadable fnorm
                                          (cond [(eq? fnorm 'var) components] ; whitespaces are meaningful in var() 
                                                [else (filter-not css:whitespace? components)])
                                          #false))))])))

(define css-consume-block-body : (-> Input-Port CSS-Token Char (Values (Listof CSS-Token) CSS-Syntax-Terminal CSS-Token))
  ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
  ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
  (lambda [css start-token close-char]
    (let consume-body ([components : (Listof CSS-Token) null])
      (define token (css-read-syntax css))
      (cond [(css:close=:=? token close-char) (values (reverse components) token token)]
            [(not (eof-object? token)) (consume-body (cons (css-consume-component-value css token) components))]
            [else (let ([end-token (if (null? components) start-token (car components))])
                    (make+exn:css:missing-delimiter token)
                    (values (reverse components) token end-token))]))))
  
(define css-consume-components : (->* (Input-Port) ((Option Char) Boolean) (Values (Listof CSS-Token) CSS-Syntax-Terminal))
  ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
  (lambda [css [terminating-char #false] [omit-terminate? #false]]
    (let consume-component ([stnenopmoc : (Listof CSS-Token) null])
      (define token (css-read-syntax css))
      (cond [(eof-object? token) (values (reverse stnenopmoc) token)]
            [(and terminating-char (css:delim=:=? token terminating-char))
             (define next (css-peek-syntax/skip-whitespace css))
             (cond [(and omit-terminate? (css-null? stnenopmoc))
                    (cond [(and (eof-object? next) (css-read-syntax/skip-whitespace css))
                           (make+exn:css:overconsumption token)
                           (values (reverse stnenopmoc) eof)]
                          [else (make+exn:css:empty token)
                                (css-consume-components css terminating-char omit-terminate?)])]
                   [(eof-object? next)
                    (css-read-syntax/skip-whitespace css)
                    (values (reverse stnenopmoc) next)]
                   [else (values (reverse stnenopmoc) token)])]
            [else (consume-component (cons (css-consume-component-value css token) stnenopmoc))]))))

(define css-consume-componentses : (-> Input-Port [#:omit-comma? Boolean] (Listof (Listof CSS-Token)))
  ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
  (lambda [css #:omit-comma? [omit-comma? #true]]
    (let consume-components ([componentses : (Listof (Listof CSS-Token)) null])
      (define-values (components terminating-token) (css-consume-components css #\, omit-comma?))
      (cond [(not (eof-object? terminating-token)) (consume-components (cons components componentses))]
            [(not omit-comma?) (reverse (cons components componentses))]
            [else (filter css-pair? (reverse (cons components componentses)))]))))

(define css-components->declaration : (-> CSS:Ident (Listof CSS-Token) (U CSS-Declaration CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
  ;;; https://drafts.csswg.org/css-cascade/#importance
  ;;; https://drafts.csswg.org/css-values/#component-whitespace
  ;;; https://drafts.csswg.org/css-syntax/#typedef-declaration-value
  ;;; https://drafts.csswg.org/css-variables/#defining-variables
  (lambda [id-token components]
    (define-values (?: value-list) (css-car components))
    (cond [(not (css:colon? ?:)) (make+exn:css:missing-colon id-token)]
          [else (let ([var? (and (css:ident=<-? id-token symbol-unreadable?) #true)])
                  (define-values (?values important? lazy?) (css-any->declaration-value id-token value-list var?))
                  (if (exn? ?values) ?values (make-css-declaration id-token ?values important? lazy?)))])))

(define css-any->declaration-value : (-> CSS-Token (Listof CSS-Token) Boolean
                                         (Values (U (Listof+ CSS-Token) CSS-Syntax-Error) Boolean Boolean))
  ;;; https://drafts.csswg.org/css-syntax/#typedef-declaration-value
  ;;; https://drafts.csswg.org/css-variables/#defining-variables
  (lambda [hint-token components var?]
    (let fold ([lla : (Listof CSS-Token) null]
               [<!> : (Option CSS:Delim) #false]
               [lazy? : Boolean #false]
               [rest : (Listof CSS-Token) components])
      (define-values (head tail) (if <!> (css-car/cdr rest) (css-car rest)))
      (if (eof-object? head)
          (let ([all (reverse lla)]) ; (reverse) does not know non-null list.
            ; whitespace-only <declaration-value>s are also valid, but they are only meaningful for variables.
            (cond [(pair? all) (values all (and (false? var?) <!> #true) lazy?)]
                  [(and var? (pair? components)) (values components #false #false)]
                  [else (values (make+exn:css:missing-value hint-token) #false lazy?)]))
          (cond [(and <!> (css:ident-norm=:=? head 'important)) (fold (if var? (cons head lla) lla) <!> lazy? tail)]
                [(and <!>) (values (make+exn:css:unrecognized <!>) #false lazy?)]
                [(css:delim=:=? head #\!) (fold lla head lazy? tail)]
                [(css-bad-token? head) (values (make+exn:css:malformed head) #false lazy?)]
                [else (let-values ([(shadow shadow-lazy? real-tail) (css-declaration-value-filter head tail)])
                        (cond [(exn? shadow) (values shadow #false lazy?)]
                              [else (fold (cons shadow lla) <!> (or lazy? shadow-lazy?) real-tail)]))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-components->media-type+query : (-> CSS:Ident Boolean (Listof CSS-Token) CSS-Media-Query)
  ;;; https://drafts.csswg.org/mediaqueries/#media-types
  ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-query
  (lambda [media only? conditions]
    (define downcased-type : Symbol (css:ident-norm media))
    (define-values (?and ?conditions) (css-car conditions))
    (when (css-deprecate-media-type) (make+exn:css:deprecated media))
    (cond [(memq downcased-type '(only not and or)) (make+exn:css:misplaced media)]
          [(eof-object? ?and) (make-css-media-type downcased-type only?)]
          [(not (css:ident-norm=:=? ?and 'and)) (make+exn:css:unrecognized ?and)]
          [(css-null? ?conditions) (make+exn:css:missing-feature ?and)]
          [else (cons (make-css-media-type downcased-type only?)
                      (css-components->junction ?conditions 'and #false #true))])))
  
(define css-components->feature-query : (-> (Listof CSS-Token) Boolean CSS-Syntax-Any CSS-Feature-Query)
  ;;; https://drafts.csswg.org/mediaqueries/#mq-only
  ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
  (lambda [conditions media? alt]
    (define-values (token rest) (css-car conditions))
    (define-values (op chain) (css-car rest))
    (cond [(eof-object? token) (throw-exn:css:missing-feature alt)]
          [(css:ident-norm=:=? token 'not) (css-components->negation token rest media?)]
          [(eof-object? op) (css-component->feature-query media? token)]
          [(css:ident-norm=<-? op '(and or)) (css-components->junction chain (css:ident-norm op) token media?)]
          [else (throw-exn:css:unrecognized op)])))

(define css-component->feature-query : (-> Boolean CSS-Token CSS-Feature-Query)
  ;;; https://drafts.csswg.org/css-syntax/#preserved-tokens
  ;;; https://drafts.csswg.org/css-conditional/#at-supports
  ;;; https://drafts.csswg.org/mediaqueries/#mq-features
  ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
  ;;; https://drafts.csswg.org/mediaqueries/#mq-boolean-context
  ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
  (lambda [media? condition]
    (cond [(css:block=:=? condition #\()
           (define subany : (Listof CSS-Token) (css:block-components condition))
           (define-values (name any-values) (css-car subany))
           (define-values (op value-list) (css-car any-values))
           (cond [(css:block=:=? name #\() (css-components->feature-query subany media? condition)]
                 [(css:ident-norm=:=? name 'not) (css-components->negation name any-values media?)]
                 [(and (css:ident? name) (css:colon? op))
                  (define descriptor (css-components->declaration name any-values))
                  (cond [(exn? descriptor) (if media? (throw-exn:css:enclosed condition) (raise descriptor))]
                        [(and media?) (css-declaration->media-query descriptor condition)]
                        [else descriptor])]
                 [(and media?)
                  (cond [(and (css:ident? name) (eof-object? op)) (css-make-media-feature name #false #\? #false)]
                        [else (css-components->media-range-query subany condition)])]
                 [(eof-object? name) (throw-exn:css:empty condition)]
                 [(css:ident? name) (throw-exn:css:missing-colon condition)]
                 [(css:function? condition) (throw-exn:css:enclosed condition)]
                 [else (throw-exn:css:type:identifier condition)])]
          [else (throw-exn:css:missing-feature condition)])))

(define css-components->negation : (-> CSS:Ident (Listof CSS-Token) Boolean CSS-Not)
  ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-not
  (lambda [<not> tokens media?]
    (define-values (token rest) (css-car tokens))
    (cond [(eof-object? token) (throw-exn:css:missing-feature <not>)]
          [(css:ident-norm=:=? token 'not) (throw-exn:css:misplaced token)]
          [(css-null? rest) (make-css-not (css-component->feature-query media? token))]
          [else (throw-exn:css:overconsumption rest)])))

(define css-components->junction : (-> (Listof CSS-Token) Symbol (Option CSS-Token) Boolean (U CSS-And CSS-Or))
  ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-and
  ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-or
  (lambda [conditions op ?head media?]
    (define make-junction (if (eq? op 'and) make-css-and make-css-or))
    (let components->junction ([junctions : (Listof CSS-Token) (if (false? ?head) null (list ?head))]
                               [--conditions : (Listof CSS-Token) conditions])
      (define-values (condition rest) (css-car --conditions))
      (define-values (token others) (css-car rest))
      (cond [(eof-object? condition) (make-junction (map (curry css-component->feature-query media?) (reverse junctions)))]
            [(css:ident-norm=:=? condition 'not) (throw-exn:css:misplaced condition)]
            [(or (eof-object? token) (css:ident-norm=:=? token op)) (components->junction (cons condition junctions) others)]
            [(css:ident-norm=<-? token '(and or)) (throw-exn:css:misplaced token)]
            [else (throw-exn:css:overconsumption token)]))))

(define css-query-support? : (-> CSS-Media-Query (U CSS-Media-Preferences CSS-Feature-Support?) Boolean)
  ;;; https://drafts.csswg.org/css-conditional/#at-supports
  ;;; https://drafts.csswg.org/mediaqueries/#evaluating
  ;;; https://drafts.csswg.org/mediaqueries/#media-types
  ;;; https://drafts.csswg.org/mediaqueries/#boolean-context
  (lambda [query support?]
    (cond [(css-not? query) (not (css-query-support? (css-not-condition query) support?))]
          [(css-and? query) (andmap (λ [[q : CSS-Feature-Query]] (css-query-support? q support?)) (css-and-conditions query))]
          [(css-or? query) (ormap (λ [[q : CSS-Feature-Query]] (css-query-support? q support?)) (css-or-conditions query))]
          [(hash? support?)
           (cond [(css-media-feature? query)
                  (define downcased-name : Symbol (css-media-feature-name query))
                  (define datum : CSS-Media-Datum (css-media-feature-value query))
                  (define metadata : (U CSS-Media-Datum EOF) (hash-ref support? downcased-name (λ _ eof)))
                  (cond [(symbol? datum) (and (symbol? metadata) (eq? datum metadata))]
                        [(real? metadata) (case (css-media-feature-operator query)
                                            [(#\>) (> metadata datum)] [(#\≥) (>= metadata datum)]
                                            [(#\<) (< metadata datum)] [(#\≤) (<= metadata datum)]
                                            [else (= metadata datum)])]
                        [else #false])]
                 [(symbol? query)
                  (define metadata : CSS-Media-Datum (hash-ref support? query (λ _ 'none)))
                  (not (if (symbol? metadata) (eq? metadata 'none) (zero? metadata)))]
                 [(css-media-type? query)
                  (define result (memq (css-media-type-name query) (list (default-css-media-type) 'all)))
                  (if (css-media-type-only? query) (and result #true) (not result))]
                 [else (and (pair? query)
                            (css-query-support? (car query) support?)
                            (css-query-support? (cdr query) support?))])]
          [else (and (procedure? support?)
                     (css-declaration? query)
                     (support? (css:ident-norm (css-declaration-name query))
                               (css-declaration-values query)))])))

(define css-components->media-range-query : (-> (Listof CSS-Token) CSS:Block CSS-Feature-Query)
  ;;; https://drafts.csswg.org/mediaqueries/#mq-features
  ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
  (lambda [components broken-condition]
    (define-values (value0 rest0) (css-car-media-value components))
    (define-values (d0 op0 po0 rest1) (css-car-comparison-operator rest0))
    (define-values (value1 rest2) (css-car-media-value rest1))
    (define-values (d1 op1 po1 rest3) (css-car-comparison-operator rest2))
    (define-values (value2 terminal) (css-car-media-value rest3))
    (cond [(eof-object? value0) (throw-exn:css:empty broken-condition)]
          [(eof-object? d0) (throw-exn:css:missing-delimiter components)]
          [(eof-object? value1) (throw-exn:css:missing-value rest0)]
          [(and (css:ident? value0) (css:delim? d1)) (throw-exn:css:enclosed broken-condition)]
          [(and (eq? op0 #\=) (css:delim? d1)) (throw-exn:css:overconsumption broken-condition)]
          [(css:ident? value0) (css-make-media-feature value0 value1 op0 d0)]
          [(and (eof-object? d1) (css:ident? value1)) (css-make-media-feature value1 value0 po0 d0)]
          [(not (css:ident? value1)) (throw-exn:css:type:identifier value1)]
          [(or (eof-object? value2) (css:ident? value2)) (throw-exn:css:missing-value rest2)]
          [(css-pair? terminal) (throw-exn:css:overconsumption terminal)]
          [(not (eq? (css:delim-datum d0) (css:delim-datum d1))) (throw-exn:css:malformed (list d0 value1 d1))]
          [else (make-css-and (list (css-make-media-feature value1 value0 po0 d0)
                                    (css-make-media-feature value1 value2 op1 d1)))])))
  
(define css-declaration->media-query : (-> CSS-Declaration CSS:Block CSS-Feature-Query)
  ;;; https://drafts.csswg.org/mediaqueries/#mq-features
  (lambda [property broken-condition]
    (define-values (media-value rest) (css-car-media-value (css-declaration-values property)))
    (cond [(eof-object? media-value) (throw-exn:css:enclosed broken-condition)]
          [(css-pair? rest) (throw-exn:css:enclosed broken-condition)]
          [else (css-make-media-feature (css-declaration-name property) media-value #\: #false)])))

(define css-car-comparison-operator : (-> (Listof CSS-Token) (Values (U CSS:Delim EOF) Char Char (Listof CSS-Token)))
  ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
  (lambda [components]
    (define-values (d rest) (css-car components))
    (define-values (?= terminal) (css-car/cdr rest))
    (cond [(eof-object? d) (values eof #\≠ #\≠ rest)]
          [(not (css:delim? d)) (throw-exn:css:type d)]
          [else (case (css:delim-datum d)
                  [(#\=) (values d #\= #\= rest)]
                  [(#\>) (if (css:delim=:=? ?= #\=) (values d #\≥ #\≤ terminal) (values d #\> #\< rest))]
                  [(#\<) (if (css:delim=:=? ?= #\=) (values d #\≤ #\≥ terminal) (values d #\< #\> rest))]
                  [else (throw-exn:css:range d)])])))
  
(define css-car-media-value : (-> (Listof CSS-Token) (Values (U CSS-Media-Value EOF) (Listof CSS-Token)))
  ;;; https://drafts.csswg.org/mediaqueries/#typedef-mf-value
  ;;; https://drafts.csswg.org/mediaqueries/#typedef-ratio
  (lambda [components]
    (define-values (value rest) (css-car components))
    (define-values (?/ ?rest) (css-car rest))
    (define-values (?int terminal) (css-car ?rest))
    (cond [(eof-object? value) (values eof rest)]
          [(css:slash? ?/)
           (define width : (Option Positive-Integer) (css:integer=<-? value exact-positive-integer?))
           (define height : (Option Positive-Integer) (css:integer=<-? ?int exact-positive-integer?))
           (values (cond [(and width height (css-token? ?int)) (css-remake-token [value ?int] css:ratio (/ width height))]
                         [(css-number? value) (throw-exn:css:range value)]
                         [(css-number? ?int) (throw-exn:css:range ?int)]
                         [else (throw-exn:css:type (filter css-token? (list value ?/ ?int)))])
                   terminal)]
          [(or (css:ident? value) (css-numeric? value)) (values value rest)]
          [else (values (throw-exn:css:type value) rest)])))

(define css-make-media-feature : (-> CSS:Ident (Option CSS-Media-Value) Char (Option CSS:Delim) (U Symbol CSS-Media-Feature))
  ;;; https://drafts.csswg.org/mediaqueries/#mq-features
  ;;; https://drafts.csswg.org/mediaqueries/#mq-min-max
  (lambda [desc-name ?value ophint ?op]
    (define errobj : (Listof CSS-Token) (filter css-token? (list desc-name ?op ?value)))
    (define name : String (symbol->string (css:ident-norm desc-name)))
    (define-values (downcased-name op min/max?)
      (cond [(string-prefix? name "min-") (values (string->symbol (substring name 4)) #\≥ #true)]
            [(string-prefix? name "max-") (values (string->symbol (substring name 4)) #\≤ #true)]
            [else (values (string->symbol name) ophint #false)]))
    (when (and min/max?)
      (cond [(or (not ?value) (css:delim? ?op)) (throw-exn:css:misplaced errobj)]
            [(not (css-numeric? ?value)) (throw-exn:css:type errobj)]))
    (define feature-filter : (U Void (CSS:Filter CSS-Media-Datum))
      ((default-css-media-feature-filter) downcased-name min/max? (thunk (void (make+exn:css:deprecated desc-name)))))
    (cond [(void? feature-filter) (throw-exn:css:unrecognized errobj)]
          [(false? ?value) downcased-name]
          [else (let ([datum (feature-filter ?value)])
                  (cond [(false? datum) (throw-exn:css:type ?value desc-name)]
                        [(exn:css? datum) (css-log-syntax-error datum desc-name) (raise datum)]
                        [else (make-css-media-feature downcased-name datum op)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-components->selectors : (-> (Listof CSS-Token) CSS-Namespace-Hint (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/selectors/#structure
  ;;; https://drafts.csswg.org/selectors/#parse-selector
  ;;; https://drafts.csswg.org/selectors/#selector-list
  ;;; https://drafts.csswg.org/selectors/#grouping
  (lambda [components namespaces]
    (with-handlers ([exn:css? (λ [[errcss : exn:css]] errcss)])
      (define-values (head-complex-selector ?eof ?rest) (css-car-complex-selector components namespaces))
      (let extract-complex-selector ([srotceles : (Listof CSS-Complex-Selector) null]
                                     [terminal : (U EOF CSS:Delim) ?eof]
                                     [rest : (Listof CSS-Token) ?rest])
        (if (css-null? rest)
            (cond [(eof-object? terminal) (cons head-complex-selector (reverse srotceles))]
                  [else (throw-exn:css:overconsumption terminal)])
            (let-values ([(complex-selector ?terminal ?rest) (css-car-complex-selector rest namespaces)])
              (extract-complex-selector (cons complex-selector srotceles) ?terminal ?rest)))))))
  
(define css-car-complex-selector : (-> (Listof CSS-Token) CSS-Namespace-Hint
                                       (Values CSS-Complex-Selector (U EOF CSS:Delim) (Listof CSS-Token)))
  ;;; https://drafts.csswg.org/selectors/#structure
  ;;; https://drafts.csswg.org/selectors/#combinators
  ;;; https://drafts.csswg.org/selectors/#grammar
  (lambda [components namespaces]
    (define-values (head-compound-selector rest) (css-car-compound-selector components #false namespaces))
    (let extract-selector ([srotceles : (Listof CSS-Compound-Selector) null]
                           [tokens : (Listof CSS-Token) rest])
      (define-values (?terminal rest) (css-car tokens))
      (define-values (token ?selectors) (css-car/cdr tokens))
      (cond [(or (eof-object? ?terminal) (css:comma? ?terminal))
             (values (css-make-complex-selector (cons head-compound-selector (reverse srotceles))) ?terminal rest)]
            [(not (css-selector-combinator? token)) (throw-exn:css:unrecognized ?terminal)]
            [else (let*-values ([(combinator ?selectors) (css-car-combinator token ?selectors)]
                                [(?selector ?rest) (css-car ?selectors)])
                    (cond [(or (eof-object? ?selector) (css:comma? ?selector)) (throw-exn:css:overconsumption ?selectors)]
                          [else (let-values ([(selector rest) (css-car-compound-selector ?selectors combinator namespaces)])
                                  (extract-selector (cons selector srotceles) rest))]))]))))

(define css-car-compound-selector : (-> (Listof CSS-Token) (Option CSS-Selector-Combinator) CSS-Namespace-Hint
                                        (Values CSS-Compound-Selector (Listof CSS-Token)))
  ;;; https://drafts.csswg.org/selectors/#structure
  ;;; https://drafts.csswg.org/selectors/#grammar
  ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
  ;;; https://github.com/w3c/csswg-drafts/issues/202
  (lambda [components combinator namespaces]
    (define-values (head heads) (css-car components))
    (define-values (typename quirkname namespace simple-selector-components)
      (cond [(css:ident? head) (css-car-elemental-selector head heads namespaces)]
            [(css:delim=<-? head '(#\| #\*)) (css-car-elemental-selector head heads namespaces)]
            [(or (eof-object? head) (css:comma? head)) (throw-exn:css:empty head)]
            [else (values #true #true (or (css-declared-namespace namespaces '||) #true) (cons head heads))]))
    (define-values (pseudo-classes selector-components) (css-car-pseudo-class-selectors simple-selector-components))
    (let extract-simple-selector ([sessalc : (Listof Symbol) null]
                                  [sdi : (Listof Keyword) null]
                                  [setubirtta : (Listof CSS-Attribute-Selector) null]
                                  [pseudo-element : (Option CSS-Pseudo-Element-Selector) #false]
                                  [selector-tokens : (Listof CSS-Token) selector-components])
      (define-values (token tokens) (css-car/cdr selector-tokens))
      (cond [(or (eof-object? token) (css:comma? token) (css-selector-combinator? token))
             (values (make-css-compound-selector combinator typename quirkname namespace pseudo-classes
                                                 (reverse sessalc) (reverse sdi) (reverse setubirtta) pseudo-element)
                     selector-tokens)]
            [(and pseudo-element) (throw-exn:css:overconsumption token)]
            [(css:delim=:=? token #\.)
             (define-values (next rest) (css-car/cdr tokens))
             (cond [(not (css:ident? next)) (throw-exn:css:type:identifier next)]
                   [else (extract-simple-selector (cons (css:ident-datum next) sessalc) sdi setubirtta pseudo-element rest)])]
            [(css:colon? token)
             (define-values (?pseudo-classes ?rest) (css-car-pseudo-class-selectors tokens))
             (define-values (next rest) (css-car/cdr ?rest))
             (cond [(null? ?pseudo-classes) (throw-exn:css:misplaced (list token (car tokens)))]
                   [else (let ([pclass (car ?pseudo-classes)])
                           (define pelement : CSS-Pseudo-Element-Selector
                             (make-css-pseudo-element-selector (css-pseudo-class-selector-name pclass)
                                                               (css-pseudo-class-selector-arguments pclass)
                                                               (cdr ?pseudo-classes)))
                           (extract-simple-selector sessalc sdi setubirtta pelement ?rest))])]
            [(css:block=:=? token #\[)
             (define attribute-selector : CSS-Attribute-Selector (css-simple-block->attribute-selector token namespaces))
             (extract-simple-selector sessalc sdi (cons attribute-selector setubirtta) pseudo-element tokens)]
            [(css:hash? token) (extract-simple-selector sessalc (cons (css:hash-datum token) sdi) setubirtta pseudo-element tokens)]
            [else (throw-exn:css:unrecognized token)]))))

(define css-car-combinator : (-> (U CSS:WhiteSpace CSS:Delim) (Listof CSS-Token) (Values CSS-Selector-Combinator (Listof CSS-Token)))
  ;;; https://drafts.csswg.org/selectors/#structure
  ;;; https://drafts.csswg.org/selectors/#grammar
  (lambda [token tokens]
    (case (cond [(css:whitespace? token) #\space] [(css:delim? token) (css:delim-datum token)] [else #\null])
      [(#\space)
       (define-values (next tail) (css-car tokens))
       (cond [(css-selector-combinator? next) (css-car-combinator next tail)]
             [else (values '>> tokens)])]
      [(#\>)
       (define-values (next tail) (css-car/cdr tokens))
       (define-values (next2 tail2) (css-car tail))
       (cond [(css:delim=:=? next #\>) (values '>> tail2)]
             [else (values '> tail)])]
      [(#\+)   (values '+ tokens)]
      [(#\~)   (values '~ tokens)]
      [(#\tab) (values '|| tokens)]
      [else (throw-exn:css:unrecognized token)])))
  
(define css-car-elemental-selector : (-> (U CSS:Ident CSS:Delim) (Listof CSS-Token) CSS-Namespace-Hint
                                         (Values (U Symbol True) (U Symbol True) (U Symbol Boolean) (Listof CSS-Token)))
  ;;; https://drafts.csswg.org/selectors/#structure
  ;;; https://drafts.csswg.org/selectors/#elemental-selectors
  ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
  (lambda [token tokens namespaces]
    (define-values (next rest next2 rest2) (css-car/cadr tokens))
    (cond [(css:vbar? token)
           (cond [(css:ident? next) (values (css:ident-datum next) (css:ident-norm next) #false rest)]
                 [(css:delim=:=? next #\*) (values #true #true #false rest)]
                 [else (throw-exn:css:type:identifier next)])]
          [(css:vbar? next)
           (define ns : (U Symbol Boolean) (css-declared-namespace namespaces token))
           (cond [(false? ns) (throw-exn:css:namespace token)]
                 [(css:ident? next2) (values (css:ident-datum next2) (css:ident-norm next2) ns rest2)]
                 [(css:delim=:=? next2 #\*) (values #true #true ns rest2)]
                 [else (throw-exn:css:type:identifier (list token next))])]
          [else (let ([ns (or (css-declared-namespace namespaces '||) #true)])
                  (cond [(css:delim? token) (values #true #true ns tokens)]
                        [else (values (css:ident-datum token) (css:ident-norm token) ns tokens)]))])))

(define css-car-pseudo-class-selectors : (-> (Listof CSS-Token) (Values (Listof CSS-Pseudo-Class-Selector) (Listof CSS-Token)))
  ;;; https://drafts.csswg.org/selectors/#structure
  ;;; https://drafts.csswg.org/selectors/#elemental-selectors
  ;;; https://drafts.csswg.org/selectors/#pseudo-classes
  (lambda [components]
    (let extract-pseudo-class-selector ([srotceles : (Listof CSS-Pseudo-Class-Selector) null]
                                        [tokens : (Listof CSS-Token) components])
      (define-values (maybe: rest ?id rest2) (css-car/cadr tokens))
      (cond [(or (not (css:colon? maybe:)) (css:colon? ?id)) (values (reverse srotceles) tokens)]
            [(css:ident? ?id)
             (let ([selector (make-css-pseudo-class-selector (css:ident-datum ?id) #false)])
               (extract-pseudo-class-selector (cons selector srotceles) rest2))]
            [(css:function? ?id)
             (let ([selector (make-css-pseudo-class-selector (css:function-norm ?id) (css:function-arguments ?id))])
               (extract-pseudo-class-selector (cons selector srotceles) rest2))]
            [else (throw-exn:css:type:identifier maybe:)]))))
  
(define css-simple-block->attribute-selector : (-> CSS:Block CSS-Namespace-Hint CSS-Attribute-Selector)
  ;;; https://drafts.csswg.org/selectors/#attribute-selectors
  ;;; https://drafts.csswg.org/selectors/#attrnmsp
  ;;; https://drafts.csswg.org/selectors/#attribute-case
  ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
  (lambda [block namespaces]
    (define-values (1st rest1) (css-car (css:block-components block)))
    (define-values (2nd rest2 3rd rest3) (css-car/cadr rest1))
    (define-values (attrname quirkname namespace op-part)
      (cond [(eof-object? 1st) (throw-exn:css:empty block)]
            [(or (css:match? 1st) (css:delim=:=? 1st #\=))
             (throw-exn:css:type:identifier block)]
            [(or (eof-object? 2nd) (css:match? 2nd) (css:delim=:=? 2nd #\=) (css:whitespace? 2nd))
             ; WARNING: the namespace behavior for attributes is different from that for elements 
             (cond [(css:ident? 1st) (values (css:ident-datum 1st) (css:ident-norm 1st) #false rest1)]
                   [else (throw-exn:css:type:identifier 1st)])]
            [(or (eof-object? 3rd) (css:match? 3rd) (css:delim=:=? 3rd #\=) (css:whitespace? 3rd))
             (cond [(and (css:vbar? 1st) (css:ident? 2nd)) (values (css:ident-datum 2nd) (css:ident-norm 2nd) #false rest2)]
                   [(css:vbar? 2nd) (throw-exn:css:type:identifier 2nd)]
                   [else (throw-exn:css:unrecognized 1st)])]
            [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:vbar? 2nd) (css:ident? 3rd))
             (define ns (css-declared-namespace namespaces 1st))
             (cond [(false? ns) (throw-exn:css:namespace 1st)]
                   [else (values (css:ident-datum 3rd) (css:ident-norm 3rd) ns rest3)])]
            [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:vbar? 2nd))
             (throw-exn:css:type:identifier 3rd)]
            [(or (css:ident? 1st) (css:delim=:=? 1st #\*))
             (throw-exn:css:unrecognized 2nd)]
            [else (throw-exn:css:unrecognized 1st)]))
    (define-values (op value-part value ci-part) (css-car/cadr op-part))
    (define-values (i terminal) (css-car ci-part))
    (unless (eof-object? op)
      (cond [(eof-object? value) (throw-exn:css:missing-value op)]
            [(nor (eof-object? i) (css:ident-norm=:=? i 'i)) (throw-exn:css:overconsumption i)]
            [(css-pair? terminal) (throw-exn:css:overconsumption terminal)]))
    (define val : (U String Symbol)
      (cond [(css:string? value) (css:string-datum value)]
            [(css:ident? value) (css:ident-datum value)]
            [(or (css:whitespace? value) (eof-object? value)) ""]
            [else (throw-exn:css:type value)]))
    (cond [(or (css:whitespace? op) (eof-object? op)) (make-css-attribute-selector attrname quirkname namespace)]
          [(css:delim=:=? op #\=) (make-css-attribute~selector attrname quirkname namespace #\= val (css:ident? i))]
          [(css:match? op) (make-css-attribute~selector attrname quirkname namespace (css:match-datum op) val (css:ident? i))]
          [else (throw-exn:css:unrecognized op)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-selector-combinator? : (-> CSS-Syntax-Any Boolean : #:+ (U CSS:WhiteSpace CSS:Delim))
  (lambda [token]
    (or (css:whitespace? token)
        (and (css:delim=<-? token '(#\~ #\+ #\> #\tab))
             #true))))

(define css-function->var : (-> CSS:Function (U CSS:Var CSS-Syntax-Error))
  (lambda [var]
    (define-values (--var ?fallback-list) (css-car (css:function-arguments var)))
    (define-values (?comma fallback) (css-car ?fallback-list))
    (cond [(not (css:ident=<-? --var symbol-unreadable?)) (make+exn:css:type:variable --var)]
          [(eof-object? ?comma) (css-remake-token var css:var (css:ident-datum --var) null #false)]
          [(not (css:comma? ?comma)) (make+exn:css:missing-comma ?comma)]
          [else (let-values ([(?fallback _ lazy?) (css-any->declaration-value ?comma fallback #true)])
                  (cond [(exn? ?fallback) ?fallback]
                        [else (css-remake-token var css:var (css:ident-datum --var) ?fallback lazy?)]))])))

(define css-declaration-value-filter : (-> CSS-Token (Listof CSS-Token)
                                           (Values (U CSS-Token CSS-Syntax-Error) Boolean (Listof CSS-Token)))
  (lambda [token candidates]
    (cond [(css:url? token)
           (define-values (submodifiers lazy?) (css-lazy-subtokens-map (css:url-modifiers token)))
           (define url : (U CSS:URL CSS-Syntax-Error)
             (cond [(exn:css? submodifiers) submodifiers]
                   [(false? lazy?) token]
                   [else (css:url-copy token (css-url-modifiers-filter token submodifiers) lazy?)]))
           (values url (not (eq? token url)) candidates)]
          [(css:function? token)
           (define func : (U CSS:Function CSS:Var CSS-Syntax-Error)
             (cond [(eq? (css:function-norm token) 'var) (css-function->var token)]
                   [else (let-values ([(subarguments lazy?) (css-lazy-subtokens-map (css:function-arguments token))])
                           (cond [(exn:css? subarguments) subarguments]
                                 [(false? lazy?) token]
                                 [else (css:function-copy token subarguments lazy?)]))]))
           (values func (not (eq? token func)) candidates)]
          [(css:@keyword? token)
           (define-values (next rest) (css-car/cdr candidates))
           (define binding : Symbol (string->symbol (substring (keyword->string (css:@keyword-datum token)) 1)))
           (if (or (eof-object? next) (not (css:block? next)))
               (values (css-remake-token token css:racket binding) #false candidates)
               (let-values ([(argl lazy?) (css-lazy-subtokens-map (filter-not css:whitespace? (css:block-components next)))])
                 (cond [(exn:css? argl) (values argl #false candidates)]
                       [(and lazy?) (values (css-remake-token [token next] css:λracket binding argl lazy?) lazy? rest)]
                       [else (let ([reargl (css-λarguments-filter argl)])
                               (cond [(exn:css? reargl) (values reargl #false candidates)]
                                     [else (values (css-remake-token [token next] css:λracket binding reargl lazy?) lazy? rest)]))])))]
          [(css:block? token)
           (define-values (subcomponents lazy?) (css-lazy-subtokens-map (css:block-components token)))
           (define block : (U CSS:Block CSS-Syntax-Error)
             (cond [(exn:css? subcomponents) subcomponents]
                   [(false? lazy?) token]
                   [else (css:block-copy token subcomponents lazy?)]))
           (values block (not (eq? token block)) candidates)]
          [else (values token #false candidates)])))

(define css-lazy-subtokens-map : (-> (Listof CSS-Token) (Values (U (Listof CSS-Token) CSS-Syntax-Error) Boolean))
  (lambda [subtokens]
    (let lazy-fold ([tokens : (Listof CSS-Token) null]
                    [lazy? : Boolean #false]
                    [subrest : (Listof CSS-Token) subtokens])
      (cond [(null? subrest) (values (reverse tokens) lazy?)]
            [else (let ([head (car subrest)])
                    (define-values (shadow shadow-lazy? tail) (css-declaration-value-filter head (cdr subrest)))
                    (cond [(exn:css? shadow) (values shadow #false)]
                          [else (lazy-fold (cons shadow tokens) (or lazy? shadow-lazy?) tail)]))]))))

(define css-url-modifiers-filter : (-> CSS-Token (Listof CSS-Token) (Listof CSS-URL-Modifier))
  (lambda [url modifiers]
    (let modifiers-filter ([sreifidom : (Listof CSS-URL-Modifier) null]
                           [tail : (Listof CSS-Token) modifiers])
      (define-values (head rest) (css-car tail))
      (cond [(eof-object? head) (reverse sreifidom)]
            [(or (css:ident? head) (css-lazy-token? head)) (modifiers-filter (cons head sreifidom) rest)]
            [else (make+exn:css:type (list url head)) (modifiers-filter sreifidom rest)]))))

(define css-λarguments-filter : (-> (Listof CSS-Token) (U (Listof CSS-Token) CSS-Syntax-Error))
  (lambda [argl]
    (let rearrange ([swk : (Listof CSS-Token) null]
                    [lgra : (Listof CSS-Token) null]
                    [tail : (Listof CSS-Token) argl])
      (define-values (head rest) (css-car/cdr tail))
      (cond [(eof-object? head) (append (reverse swk) (reverse lgra))]
            [(css:hash? head)
             (define-values (kw-value others) (css-car/cdr rest))
             (cond [(eof-object? kw-value) (make+exn:css:missing-value head)]
                   [else (rearrange (cons kw-value (cons head swk)) lgra others)])]
            [else (rearrange swk (cons head lgra) rest)]))))
  
(define css-media-queries-support? : (-> (Listof CSS-Media-Query) CSS-Media-Preferences Boolean)
  (lambda [queries preferences]
    (or (null? queries)
        (for/or ([query (in-list queries)])
          (css-query-support? query preferences)))))
