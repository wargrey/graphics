#lang typed/racket

;;; Parser Combinators and Syntax Sugars of dealing with declarations for client applications
;;; WARNING: Notations are not following the CSS Specifications https://drafts.csswg.org/css-values/#component-combinators

(provide (all-defined-out))
(provide (rename-out [exact-nonnegative-integer? natural?]))

(require "digitama/misc.rkt")
(require "digitama/digicore.rkt")

(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-css-disjoint-filter stx)
  (syntax-case stx [:]
    [(_ compound-filter #:-> RangeType #:with [[dom : DomType defval ...] ...] atom-filters ...)
     #'(define (compound-filter [dom : DomType defval ...] ...) : (CSS:Filter RangeType) (CSS:<+> atom-filters ...))]
    [(_ compound-filter #:-> RangeType atom-filters ...)
     #'(define (compound-filter) : (CSS:Filter RangeType) (CSS:<+> atom-filters ...))]))
  
(define-syntax (define-css-atomic-filter stx)
  (syntax-case stx [:]
    [(_ atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ... #:where [defaux ...])
     #'(define (atom-filter [dom : DomType defval ...] ...) : (CSS:Filter RangeType) defaux ...
         (λ [[token : CSS-Syntax-Any]] : (CSS-Option RangeType)
           (cond [(css:token? token) atom-body ...]
                 [else #false])))]
    [(defilter atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ...)
     #'(defilter atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ... #:where [])]))

(define-syntax (define-css-function-filter stx)
  (define (parse-pattern <constructor> <matches>)
    (define-values (snerttap stnemugra)
      (for/fold ([snrettap null] [stnemugra null])
                ([<pattern> (in-list <matches>)])
        (syntax-case <pattern> [: _]
          [(field ? type?) (values (cons #'(? type? field) snrettap) (cons #'field stnemugra))]
          [(field ? type? ...) (values (cons #'(? (λ [v] (or (type? v) ...)) field) snrettap) (cons #'field stnemugra))]
          [(field : Type) (values (cons #'(? (make-predicate Type) field) snrettap) (cons #'field stnemugra))]
          [(field : Type ...) (values (cons #'(? (make-predicate (U Type ...)) field) snrettap) (cons #'field stnemugra))]
          [,field (values (cons #'field snrettap) (cons #'field stnemugra))]
          [_ (let ([? (datum->syntax #'_ (gensym))]) (values (cons ? snrettap) (cons ? stnemugra)))]
          [field (values snrettap (cons #'field stnemugra))])))
    (list (list* #'list #'_ (reverse snerttap)) (cons <constructor> (reverse stnemugra))))
  (syntax-parse stx
    [(self func-filter #:-> RangeType
           [(fname aliases ...) #:=> [transforms ...] fparser ...] ...
           (~optional (~seq #:where [defaux ...])))
     (with-syntax ([defines (if (attribute defaux) #'(begin defaux ...) #'(void))]
                   [((([pattern ...] [transform ...]) ...) ...)
                    (for/list ([<function> (in-list (syntax->list #'(fname ...)))]
                               [<transforms> (in-list (syntax->list #'([transforms ...] ...)))])
                      (define transforms (syntax-e <transforms>))
                      (when (null? transforms) (raise-syntax-error (syntax-e #'self) "empty value transform" <function>))
                      (for/list ([<transform> (in-list transforms)])
                        (define transform (syntax-e <transform>))
                        (cond [(pair? transform) (parse-pattern (car transform) (cdr transform))]
                              [(null? transform) (raise-syntax-error (syntax-e #'self) "missing value constructor" <transform>)]
                              [else (let ([? (datum->syntax <transform> (gensym))])
                                      (list (list #'? #'list? ?) (list #'values ?)))])))])
       #'(define (func-filter) : (CSS:Filter RangeType) defines
           (define do-parse : (-> Symbol (CSS-Parser (Listof CSS-Datum)) (Listof CSS-Token) (U (Listof CSS-Datum) CSS-Syntax-Error))
             (lambda [func-name func-parse func-argl]
               (define-values (fargs --tokens) (func-parse (list func-name) func-argl))
               (cond [(exn:css? fargs) fargs]
                     [(false? fargs) (make-exn:css:type --tokens)]
                     [(pair? --tokens) (make-exn:css:overconsumption --tokens)]
                     [else (reverse fargs)])))
           (λ [[token : CSS-Syntax-Any]] : (CSS-Option RangeType)
             (and (css:function? token)
                  (let ([argl : (Listof CSS-Token) (css:function-arguments token)])
                    (case (css:function-norm token)
                      [(fname aliases ...)
                       (match (do-parse 'fname (CSS<+> fparser ...) argl)
                         [(pattern ...) (transform ...)] ...
                         [(? exn? e) e]
                         [_ (make-exn:css:arity token)])]
                      ...
                      [else (make-exn:css:range token)]))))))]))

(define-syntax (define-css-system-parameters-filter stx)
  (syntax-case stx []
    [(_ <id> #:-> RangeType [css racket] ... [otherwise last-one])
     (with-syntax ([(current ... current-last)
                    (let ([suffix (string-downcase (symbol->string (syntax-e #'RangeType)))])
                      (for/list ([<p> (in-list (syntax->list #'(css ... otherwise)))])
                        (format-id <p> "default-css-~a-~a" (syntax-e <p>) suffix)))])
       #'(begin (define current : (Parameterof RangeType) (make-parameter racket)) ...
                (define current-last : (Parameterof RangeType) (make-parameter last-one))
                (define-css-disjoint-filter <id> #:-> RangeType
                  (CSS:<~> (<css:ident-norm> (list 'css ... 'otherwise))
                           (λ [[id : Symbol]] : RangeType
                             (case id [(css) (current)] ... [else (current-last)]))))))]))

(define-syntax (CSS<?> stx)
  (syntax-case stx [else]
    [(_) #'values]
    [(_ [else <else> ...]) #'(CSS<&> <else> ...)]
    [(_ [<if> <then> ...]) #'(css:if <if> (CSS<&> <then> ...) #false)]
    [(_ [<if> <then> ...] [else <else> ...]) #'(css:if <if> (CSS<&> <then> ...) (CSS<&> <else> ...))]
    [(_ [<if> <then> ...] ... [else <else> ...]) #'(css:if (list (cons <if> (CSS<&> <then> ...)) ...) (CSS<&> <else> ...))]
    [(_ [<if> <then> ...] ...) #'(css:if (list (cons <if> (CSS<&> <then> ...)) ...) #false)]))
  
(define-syntax (CSS:<+> stx)
  (syntax-case stx []
    [(_ css-filter) #'css-filter]
    [(_ css-filter css-filters ...) #'(css:disjoin css-filter (CSS:<+> css-filters ...))]))
  
(define-syntax (CSS:<~> stx)
  (syntax-case stx []
    [(_ css-filter f) #'(css:compose css-filter f)]
    [(_ css-filter f g) #'(css:compose (css:compose css-filter g) f)]
    [(_ css-filter f g h ...) #'(css:compose css-filter (compose f g h ...))]))

(define CSS:<=> : (All (a b) (-> (CSS:Filter a) b (CSS:Filter b)))
  (lambda [css-filter const]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option a) (css-filter token))
      (if (exn:css? datum) datum (and datum const)))))

(define CSS:<?> : (All (a b c) (case-> [(CSS:Filter a) (-> (U CSS-Syntax-Any (Listof CSS-Token)) CSS-Syntax-Error) -> (CSS:Filter a)]
                                       [(CSS:Filter a) b (-> CSS-Syntax-Error c) -> (CSS:Filter (U a b c))]))
  (case-lambda
    [(css:filter make-exn)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css:filter token))
       (cond [(or (false? datum) (exn:css? datum)) (make-exn token)]
             [else datum]))]
    [(css:filter false-value fexn-value)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css:filter token))
       (cond [(false? datum) false-value]
             [(exn:css? datum) (or (fexn-value datum) datum)]
             [else datum]))]))
  
(define CSS<^> : (All (a) (case-> [(U (CSS:Filter CSS-Datum) (Listof (CSS:Filter CSS-Datum))) -> (CSS-Parser (Listof CSS-Datum))]
                                  [(CSS:Filter CSS-Datum) Symbol -> CSS-Shorthand-Parser]
                                  [(CSS:Filter CSS-Datum) Symbol CSS-Longhand-Update -> CSS-Shorthand-Parser]
                                  [(CSS-Parser (Listof CSS-Datum)) (List Symbol) -> CSS-Shorthand-Parser]
                                  [(CSS-Parser (Listof CSS-Datum)) (List Symbol) CSS-Longhand-Update -> CSS-Shorthand-Parser]
                                  [(CSS:Filter a) (->* (a) (CSS-Longhand-Values) CSS-Longhand-Values) -> CSS-Shorthand-Parser]))
  (case-lambda
    [(atom-filter)
     (if (list? atom-filter)
         (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
           (let datum-fold ([data++ : (Listof CSS-Datum) data]
                            [tokens-- : (Listof CSS-Token) tokens]
                            [filters : (Listof (CSS:Filter CSS-Datum)) atom-filter])
             (cond [(null? filters) (values data++ tokens--)]
                   [else (let ([css-filter (car filters)])
                           (define-values (token --tokens) (css-car/cdr tokens--))
                           (define datum : (CSS-Option CSS-Datum) (css-filter token))
                           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens--)]
                                 [else (datum-fold (cons datum data++) --tokens (cdr filters))]))])))
         (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
           (define-values (head tail) (css-car/cdr tokens))
           (define datum : (CSS-Option CSS-Datum) (atom-filter head))
           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                 [else (values (cons datum data) tail)])))]
    [(atom-filter tag)
     (cond [(symbol? tag)
            (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
              (define-values (token --tokens) (css-car/cdr tokens))
              (define datum : (CSS-Option CSS-Datum) (atom-filter token))
              (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                    [else (values (hash-set data tag datum) --tokens)]))]
           [(list? tag)
            (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
              (define-values (subdata --tokens) (atom-filter null tokens))
              (cond [(or (false? subdata) (exn:css? subdata)) (values subdata --tokens)]
                    [else (values (hash-set data (car tag) (reverse subdata)) --tokens)]))]
           [(λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
              (define-values (token --tokens) (css-car/cdr tokens))
              (define datum : (CSS-Option a) (atom-filter token))
              (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                    [else (values (tag datum data) --tokens)]))])]
    [(atom-filter tag updater)
     (if (symbol? tag)
         (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
           (define-values (token --tokens) (css-car/cdr tokens))
           (define datum : (CSS-Option CSS-Datum) (atom-filter token))
           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                 [else (values (hash-update data tag (λ [[v : CSS-Datum]] (updater tag v datum)) (thunk #false)) --tokens)]))
         (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
           (define-values (subdata --tokens) (atom-filter null tokens))
           (cond [(or (false? subdata) (exn:css? subdata)) (values subdata --tokens)]
                 [else (values (hash-update data (car tag)
                                            (λ [[v : CSS-Datum]] (updater (car tag) v (reverse subdata)))
                                            (thunk #false))
                               --tokens)])))]))

(define CSS<$> : (case-> [(CSS-Parser (Listof CSS-Datum)) -> (CSS-Parser (Listof CSS-Datum))]
                         [(CSS-Parser (Listof CSS-Datum)) CSS-Datum -> (CSS-Parser (Listof CSS-Datum))]
                         [(CSS:Filter CSS-Datum) Symbol (-> CSS-Longhand-Values CSS-Datum) -> CSS-Shorthand-Parser]
                         [(CSS-Parser (Listof CSS-Datum)) (List Symbol) (-> CSS-Longhand-Values CSS-Datum) -> CSS-Shorthand-Parser])
  (case-lambda
    [(css-parser)
     (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
       (cond [(pair? tokens) (css-parser data tokens)]
             [else (values data null)]))]
    [(css-parser eof-value)
     (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
       (cond [(pair? tokens) (css-parser data tokens)]
             [else (values (cons eof-value data) null)]))]
    [(css:filter tag fdatum)
     (if (symbol? tag)
         (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
           (cond [(pair? tokens) (let ([css-parser (CSS<^> css:filter tag)]) (css-parser data tokens))]
                 [else (values (hash-set data tag (fdatum data)) null)]))
         (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
           (cond [(pair? tokens) (let ([css-parser (CSS<^> css:filter tag)]) (css-parser data tokens))]
                 [else (values (hash-set data (car tag) (fdatum data)) null)])))]))

(define CSS<~> : (All (a) (-> (CSS-Parser a) (-> a a) (CSS-Parser a)))
  (lambda [css-parser data=>data]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (++data --tokens) (css-parser data tokens))
      (cond [(or (exn:css? ++data) (false? ++data)) (values ++data --tokens)]
            [else (values (data=>data ++data) --tokens)]))))

(define CSS<_> : (All (a) (-> (CSS-Parser a) (CSS-Parser a)))
  (lambda [css-parser]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (++data --tokens) (css-parser data tokens))
      (cond [(or (exn:css? ++data) (false? ++data)) (values ++data --tokens)]
            [else (values data --tokens)]))))

(define CSS<+> : (All (a) (-> (CSS-Parser a) (CSS-Parser a) * (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#comb-one
  (lambda [head-branch . tail-branches]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (let switch ([head-parser : (CSS-Parser a) head-branch]
                   [tail-parsers : (Listof (CSS-Parser a)) tail-branches])
        (define-values (++data --tokens) (head-parser data tokens))
        (cond [(nor (false? ++data) (exn:css? ++data)) (values ++data --tokens)]
              [(pair? tail-parsers) (switch (car tail-parsers) (cdr tail-parsers))]
              [else (values ++data --tokens)])))))

(define CSS<&> : (All (a) (-> (CSS-Parser a) * (CSS-Parser a)))
  ;;; TODO: https://drafts.csswg.org/css-values/#comb-all
  (case-lambda
    [() values]
    [(css-parser) css-parser]
    [css-parsers (λ [[data : a] [tokens : (Listof CSS-Token)]]
                   (let datum-fold ([data++ : a data]
                                    [tokens-- : (Listof CSS-Token) tokens]
                                    [parsers : (Listof (CSS-Parser a)) css-parsers])
                     (cond [(null? parsers) (values data++ tokens--)]
                           [else (let ([css-parser (car parsers)])
                                   (define-values (++data --tokens) (css-parser data++ tokens--))
                                   (cond [(or (false? ++data) (exn:css? ++data)) (values ++data --tokens)]
                                         [else (datum-fold ++data --tokens (cdr parsers))]))])))]))
  
(define CSS<*> : (All (a) (->* ((CSS-Parser a)) ((U (CSS-Multiplier Index) '+ '? '*)) (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-zero-plus
  ;;; https://drafts.csswg.org/css-values/#comb-any
  (lambda [css-parser [multiplier '*]]
    (define-values (least most) (css:multiplier-range multiplier 0))
    (cond [(zero? most) values]
          [else (λ [[data : a] [tokens : (Listof CSS-Token)]]
                  (let seq ([data++ : a data]
                            [tokens-- : (Listof CSS-Token) tokens]
                            [n : Natural 1])
                    (define-values (++data --tokens) (css-parser data++ tokens--))
                    (cond [(or (false? ++data) (exn:css? ++data)) (if (< least n) (values data++ tokens--) (values ++data --tokens))]
                          [(= n most) (values ++data --tokens)] ; (= n +inf.0) also does not make much sense
                          [else (seq ++data --tokens (add1 n))])))])))

(define CSS<#> : (All (a) (->* ((CSS-Parser a)) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-comma
  (lambda [css-parser [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (let seq ([data++ : a data]
                [tokens-- : (Listof CSS-Token) tokens]
                [n : Natural 1])
        (define-values (++data tail) (css-parser data++ tokens--))
        (cond [(or (false? ++data) (exn:css? ++data)) (if (< least n) (values data++ tokens--) (values ++data tail))]
              [(= n most) (values ++data tail)]
              [else (let-values ([(?comma --tokens) (css-car/cdr tail)])
                      (cond [(eof-object? ?comma) (seq ++data --tokens (add1 n))] ; to check least boundry
                            [(not (css:comma? ?comma)) (values (make-exn:css:missing-comma ?comma) --tokens)]
                            [(null? --tokens) (values (make-exn:css:overconsumption ?comma) --tokens)]
                            [else (seq ++data --tokens (add1 n))]))])))))

(define CSS<!> : (->* ((CSS-Parser (Listof CSS-Datum))) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser (Listof CSS-Datum)))
  ;;; (WARNING: this is *not*) https://drafts.csswg.org/css-values/#mult-req
  (lambda [css-parser [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
      (let seq ([sub++ : (Listof CSS-Datum) null]
                [tokens-- : (Listof CSS-Token) tokens]
                [n : Natural 1])
        (define-values (subdata --tokens) (css-parser sub++ tokens--))
        (cond [(or (false? subdata) (exn:css? subdata))
               (cond [(< least n) (values (cons (reverse sub++) data) tokens--)]
                     [else (values subdata --tokens)])]
              [(= n most) (values (cons (reverse subdata) data) --tokens)]
              [else (seq subdata --tokens (add1 n))])))))

(define css:disjoin : (All (a b) (-> (CSS:Filter a) (CSS:Filter b) (CSS:Filter (U a b))))
  (lambda [css-filter1 css-filter2]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option a) (css-filter1 token))
      (cond [(false? datum) (css-filter2 token)]
            [else datum]))))
  
(define css:compose : (All (a b) (-> (CSS:Filter a) (-> a b) (CSS:Filter b)))
  (lambda [css-filter css->racket]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option a) (css-filter token))
      (if (exn:css? datum) datum (and datum (css->racket datum))))))

(define css:if : (All (a) (case-> [(Listof+ (Pairof (CSS:Filter CSS-Datum) (CSS-Parser a))) (Option (CSS-Parser a)) -> (CSS-Parser a)]
                                  [(CSS:Filter CSS-Datum) (CSS-Parser a) (Option (CSS-Parser a)) -> (CSS-Parser a)]))
  (case-lambda
    [(cond-parsers else-parser)
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let else-if ([branch (car cond-parsers)]
                     [branches-- (cdr cond-parsers)])
         (define-values (if:filter then-parser) (css-car/cdr branch))
         (define-values (token --tokens) (css-car/cdr tokens))
         (define if:datum : (CSS-Option CSS-Datum) (if:filter token))
         (cond [(nor (false? if:datum) (exn:css? if:datum)) (then-parser data --tokens)]
               [(pair? branches--) (else-if (car branches--) (cdr branches--))]
               [(false? else-parser) (values if:datum tokens)]
               [else (else-parser data tokens)])))]
    [(if:filter then-parser else-parser)
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (define-values (token --tokens) (css-car/cdr tokens))
       (define if:datum : (CSS-Option CSS-Datum) (if:filter token))
       (cond [(nor (false? if:datum) (exn:css? if:datum)) (then-parser data --tokens)]
             [(false? else-parser) (values if:datum tokens)]
             [else (else-parser data tokens)]))]))

(define css:multiplier-range : (-> (U (CSS-Multiplier Index) '+ '? '*) Index (Values Natural (U Natural +inf.0)))
  (lambda [multiplier least]
    (case multiplier
      [(?) (values 0 1)]
      [(*) (values 0 +inf.0)]
      [(+) (values 1 +inf.0)]
      [else (cond [(index? multiplier) (values (max multiplier least) (max multiplier least))]
                  [else (let ([n (let ([n (car multiplier)]) (if (index? n) (max n least) least))]) 
                          (define m : (U Index Symbol Null) (cdr multiplier))
                          (cond [(index? m) (values n (max n m))]
                                [(symbol? m) (values n +inf.0)]
                                [else (values n n)]))])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-predicate positive-fixnum? Positive-Fixnum)
(define-predicate nonnegative-fixnum? Nonnegative-Fixnum)

(define-predicate positive-flonum? Positive-Flonum)
(define-predicate nonnegative-flonum? Nonnegative-Flonum)

(define-predicate positive-single-flonum? Positive-Single-Flonum)
(define-predicate nonnegative-single-flonum? Nonnegative-Single-Flonum)

(define-predicate positive-byte? Positive-Byte)
(define-predicate positive-index? Positive-Index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <css-boolean> #:-> (U Zero One)
  (CSS:<=> (<css:integer> = 0) 0)
  (CSS:<=> (<css:integer> = 1) 1))

(define-css-disjoint-filter <css-keyword> #:-> Symbol
  #:with [[options : (U (-> Symbol Boolean) (Listof Symbol) Symbol)]]
  (<css:ident-norm> options))
  
(define-css-disjoint-filter <css-natural> #:-> Natural
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (<css:integer> exact-positive-integer?)]
        [else    (<css:integer> exact-nonnegative-integer?)]))

(define-css-disjoint-filter <css+real> #:-> (U Natural Nonnegative-Flonum)
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (CSS:<+> (<css:flonum> positive-flonum?) (<css:integer> exact-positive-integer?))]
        [else    (CSS:<+> (<css:flonum> nonnegative-flonum?) (<css:integer> exact-nonnegative-integer?))]))

(define-css-disjoint-filter <css+%real> #:-> (U Natural Nonnegative-Inexact-Real)
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (CSS:<+> (<css:percentage> positive-single-flonum?)
                          (<css:flonum> positive-flonum?)
                          (<css:integer> exact-positive-integer?))]
        [else    (CSS:<+> (<css:percentage> nonnegative-single-flonum?)
                          (<css:flonum> nonnegative-flonum?)
                          (<css:integer> exact-nonnegative-integer?))]))

(define-css-disjoint-filter <css-flunit> #:-> Nonnegative-Flonum
  (CSS:<~> (<css:flonum> 0.0 fl<= 1.0) flabs)
  (CSS:<=> (<css:integer> = 0) 0.0)
  (CSS:<=> (<css:integer> = 1) 1.0))

(define-css-disjoint-filter <css-%flunit> #:-> Nonnegative-Flonum
  (CSS:<~> (<css:percentage> 0f0 <= 1f0) flabs real->double-flonum)
  (CSS:<~> (<css:flonum> 0.0 fl<= 1.0) flabs)
  (CSS:<=> (<css:integer> = 0) 0.0)
  (CSS:<=> (<css:integer> = 1) 1.0))

(define <:css-keywords:> : (->* ((Listof Symbol)) (Symbol) (CSS-Parser (Listof CSS-Datum)))
  (lambda [options [none 'none]]
    (CSS<+> (CSS<^> (CSS:<=> (<css-keyword> none) null))
            (CSS<*> (CSS<^> (<css-keyword> options)) '+))))

(define (<css-comma>) : (CSS:Filter Char) (CSS:<?> (<css:delim> #\,) make-exn:css:missing-comma))
(define (<css-slash>) : (CSS:Filter Char) (CSS:<?> (<css:delim> #\/) make-exn:css:missing-slash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <css-size> #:-> (U Nonnegative-Inexact-Real CSS:Length:Font)
  (<css+length>)
  (CSS:<~> (<css+%real>) exact->inexact))

(define-css-disjoint-filter <css-unitless-size> #:-> (U Nonnegative-Flonum Single-Flonum CSS:Length:Font)
  ;;; NOTE
  ; `unitless` means the computed value and used value are different,
  ; hence the `negative single flonum` to tell the `css->*` the unitless value must be inheritable.
  (CSS:<~> (<css+real>) (λ [[v : Nonnegative-Real]] (- (real->single-flonum v))))
  (<css:percentage> nonnegative-single-flonum?)
  (<css+length>))

(define make-css-pair-parser : (case-> [(CSS:Filter CSS-Datum) Symbol Symbol -> (Pairof CSS-Shorthand-Parser (Listof Symbol))]
                                       [(CSS:Filter CSS-Datum) Symbol Symbol Symbol Symbol
                                                               -> (Pairof CSS-Shorthand-Parser (Listof Symbol))])
  (case-lambda
    [(filter name1 name2)
     (cons (CSS<&> (CSS<^> filter name1)
                   (CSS<$> filter name2 (λ [longhand] (hash-ref longhand name1))))
           (list name1 name2))]
    [(filter top right bottom left)
     (cons (CSS<&> (CSS<^> filter top)
                   (CSS<$> filter right (λ [longhand] (hash-ref longhand top)))
                   (CSS<$> filter bottom (λ [longhand] (hash-ref longhand top)))
                   (CSS<$> filter left (λ [longhand] (hash-ref longhand right))))
           (list top right bottom left))]))

(define make-css->size : (All (a) (-> a #:100% Nonnegative-Flonum (CSS->Racket (U a Nonnegative-Flonum))))
  (lambda [defval #:100% fl%]
    (λ [property datum]
      (cond [(nonnegative-flonum? datum) datum]
            [(nonnegative-single-flonum? datum) (fl* (real->double-flonum datum) fl%)]
            [(css:length? datum) (css:length->scalar datum #false)]
            [else defval]))))

(define make-css->pixels : (All (a b) (-> (-> Any Boolean : #:+ a) b #:100% Nonnegative-Real [#:size->pixels (-> Real Integer)]
                                          (CSS->Racket (U a b))))
  (lambda [pixels? defval #:100% fl% #:size->pixels [-> exact-round]]
    (λ [property datum]
      (define size : Integer
        (cond [(flonum? datum) (-> datum)]
              [(single-flonum? datum) (-> (* datum fl%))]
              [(css:length? datum) (-> (css:length->scalar datum #false))]
              [else -1]))
      (if (pixels? size) size defval))))
