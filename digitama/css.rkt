#lang typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.w3.org/Style/CSS/specs.en.html                                                   ;;;
;;;                                                                                             ;;;
;;; https://drafts.csswg.org/css-syntax                                                         ;;;
;;; https://drafts.csswg.org/css-values                                                         ;;;
;;; https://drafts.csswg.org/css-cascade                                                        ;;;
;;; https://drafts.csswg.org/selectors                                                          ;;;
;;; https://drafts.csswg.org/css-namespaces                                                     ;;;
;;; https://drafts.csswg.org/css-variables                                                      ;;;
;;; https://drafts.csswg.org/css-conditional                                                    ;;;
;;; https://drafts.csswg.org/mediaqueries                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(provide (except-out (all-from-out (submod "." digitama)) symbol-downcase symbol-ci=? keyword-ci=?))
(provide (except-out (all-from-out (submod "." grammar)) css-stylesheet-placeholder))

; https://drafts.csswg.org/css-syntax/#parser-entry-points
(provide css-parse-stylesheet
         css-parse-rule
         css-parse-rules
         css-parse-declaration
         css-parse-declarations
         css-parse-component-value
         css-parse-component-values
         css-parse-component-valueses
         css-parse-media-queries
         css-parse-feature-query
         css-parse-selectors)

(provide define-css-parser-entry
         css-consume-stylesheet
         css-query-support?
         css-components->declaration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama typed/racket
  (provide (all-defined-out))

  (require (for-syntax racket/syntax))
  (require (for-syntax syntax/parse))
  
  (define-syntax (struct: stx)
    (syntax-case stx [:]
      [(_ id : ID rest ...)
       (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))])
         #'(begin (define-type ID id)
                  (struct id rest ... #:prefab #:extra-constructor-name make-id)))]))
  
  (define-syntax (define-token stx)
    (syntax-parse stx
      [(_ id #:+ ID #:-> parent #:as Racket-Type (~optional (~seq #:=? maybe=?)) extra-fields ...)
       (with-syntax ([id? (format-id #'id "~a?" (syntax-e #'id))]
                     [id=? (format-id #'id "~a=?" (syntax-e #'id))]
                     [id=> (format-id #'id "~a=>" (syntax-e #'id))]
                     [id=:=? (format-id #'id "~a=:=?" (syntax-e #'id))]
                     [id-datum (format-id #'id "~a-datum" (syntax-e #'id))]
                     [type=? (or (attribute maybe=?) #'(λ [v1 v2] (or (eq? v1 v2) (equal? v1 v2))))])
         #'(begin (struct: id : ID parent ([datum : Racket-Type] extra-fields ...))

                  (define id=? : (-> ID ID Boolean)
                    (lambda [left right]
                      (define left-value : Racket-Type (id-datum left))
                      (define right-value : Racket-Type (id-datum right))
                      (type=? left-value right-value)))
                  
                  (define id=:=? : (-> Any (U Racket-Type (-> Racket-Type Boolean)) Boolean : #:+ ID)
                    (lambda [token racket-value]
                      (and (id? token)
                           (let ([css-value : Racket-Type (id-datum token)])
                             (cond [(procedure? racket-value) (racket-value css-value)]
                                   [else (type=? css-value racket-value)])))))

                  (define id=> : (All (a) (-> ID (-> Racket-Type a) a))
                    (lambda [token =>]
                      (define css-value : Racket-Type (id-datum token))
                      (=> css-value)))))]))

  (define-syntax (define-tokens stx)
    (syntax-case stx []
      [(_ token
          #:+ Token (extra ...)
          #:with [[subid #:+ SubID subrest ...] ...]
          [id #:+ ID #:-> parent #:as Type rest ...] ...)
       (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                     [token->syntax (format-id #'token "~a->syntax" (syntax-e #'token))]
                     [token->string (format-id #'token "~a->string" (syntax-e #'token))]
                     [token-source (format-id #'token "~a-source" (syntax-e #'token))]
                     [token-line (format-id #'token "~a-line" (syntax-e #'token))]
                     [token-column (format-id #'token "~a-column" (syntax-e #'token))]
                     [token-position (format-id #'token "~a-position" (syntax-e #'token))]
                     [token-span (format-id #'token "~a-span" (syntax-e #'token))]
                     [token-position<? (format-id #'token "~a-position<?" (syntax-e #'token))]
                     [([id? id-datum] ...) (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                                             (list (format-id <id> "~a?" (syntax-e <id>))
                                                   (format-id <id> "~a-datum" (syntax-e <id>))))])
         #'(begin (struct: token : Token ([source : Any] [line : Natural] [column : Natural] [position : Natural] [span : Natural]
                                                         extra ...))
                  
                  (struct: subid : SubID subrest ...) ...
                  (define-token id #:+ ID #:-> parent #:as Type rest ...) ...

                  (define token->datum : (-> Token Datum)
                    (lambda [instance]
                      ;;; Meanwhile Typed Racket is buggy for this way
                      ;(cond [(id? instance) (id-datum instance)]
                      ;      ...
                      ;      [else (assert (object-name instance) symbol?)])
                      (define v : (Vectorof Any) (struct->vector instance))
                      (define-values (struct:token _) (struct-info instance))
                      (define datum-index : Integer
                        (let count : Integer ([folded-index : Integer -1] [struct:this : (Option Struct-TypeTop) struct:token])
                          (cond [(false? struct:this) (+ folded-index 1)]
                                [else (let-values ([(_n cnt _a _r _! _i struct:parent _?) (struct-type-info struct:this)])
                                        (count (if (< folded-index 0) 0 (+ folded-index cnt)) struct:parent))])))
                      (define datum : Any (vector-ref v (min datum-index (sub1 (vector-length v)))))
                      (cond [(css-base-datum? datum) datum]
                            [(and (pair? datum) (css-base-datum? (car datum)) (css-base-datum? (cdr datum))) datum]
                            [else (string->symbol (~a datum))])))

                  (define token->syntax : (-> Token Syntax)
                    (lambda [instance]
                      (datum->syntax #false (token->datum instance)
                                     (list (token-source instance)
                                           (token-line instance) (token-column instance)
                                           (token-position instance) (token-span instance)))))

                  (define token->string : (-> Token String)
                    (lambda [instance]
                      (format "~a:~a:~a: ~a: ~s" (token-source instance)
                              (token-line instance) (add1 (token-column instance))
                              (object-name instance) (token->datum instance))))
                  
                  (define token-position<? : (-> Token Token Boolean)
                    (lambda [token1 token2]
                      (< (token-position token1)
                         (token-position token1))))))]))

  ;;; https://drafts.csswg.org/css-syntax/#tokenization
  ;; https://drafts.csswg.org/css-syntax/#component-value
  ;; https://drafts.csswg.org/css-syntax/#current-input-token
  (define-tokens css-token #:+ CSS-Token ()
    #:with [[css:numeric #:+ CSS:Numeric css-token ([representation : String])]
            [css:number #:+ CSS:Number css:numeric ()]]
    [css:bad        #:+ CSS:Bad #:-> css-token #:as (Pairof Symbol Datum)]
    [css:close      #:+ CSS:Close #:-> css-token #:as Char]
    [css:cd         #:+ CSS:CD #:-> css-token #:as Symbol #:=? eq?]
    [css:||         #:+ CSS:|| #:-> css-token #:as Symbol #:=? (const #true)]
    [css:match      #:+ CSS:Match #:-> css-token #:as Char]
    [css:ident      #:+ CSS:Ident #:-> css-token #:as Symbol #:=? symbol-ci=?]
    [css:url        #:+ CSS:URL #:-> css-token #:as (U String Symbol) [modifiers : (Listof CSS-URL-Modifier)]]
    [css:function   #:+ CSS:Function #:-> css-token #:as Symbol #:=? symbol-ci=? [arguments : (Listof CSS-Token)]]
    [css:block      #:+ CSS:Block #:-> css-token #:as Char [components : (Listof CSS-Token)]]
    [css:hash       #:+ CSS:Hash #:-> css-token #:as Keyword #:=? keyword-ci=? [flag : Symbol]]
    [css:@keyword   #:+ CSS:@Keyword #:-> css-token #:as Keyword #:=? keyword-ci=?]
    [css:string     #:+ CSS:String #:-> css-token #:as String #:=? string=?]
    [css:delim      #:+ CSS:Delim #:-> css-token #:as Char]
    [css:urange     #:+ CSS:URange #:-> css-token #:as (Pairof Index Index)]
    [css:ratio      #:+ CSS:Ratio #:-> css:number #:as Positive-Exact-Rational]
    [css:integer    #:+ CSS:Integer #:-> css:number #:as Integer]
    [css:flonum     #:+ CSS:Flonum #:-> css:number #:as Float]
    [css:percentage #:+ CSS:Percentage #:-> css:numeric #:as Exact-Rational #:=? (λ [f1 f2] (= (* f1 1/100) f2))]
    [css:dimension  #:+ CSS:Dimension #:-> css:numeric #:as (Pairof Real Symbol)
                    #:=? (λ [d1 d2] (and (= (car d1) (car d2)) (symbol-ci=? (cdr d1) (cdr d2))))]
    [css:whitespace #:+ CSS:WhiteSpace #:-> css-token #:as (U String Char)
                    #:=? (λ [ws1 ws2] (cond [(and (char? ws1) (char? ws2)) (char=? ws1 ws2 #\space)]  ; whitespace
                                            [(and (string? ws1) (string? ws2)) (string=? ws1 ws2)]    ; comment
                                            [else #false]))])

  (define-syntax (css-remake-token stx)
    (syntax-case stx []
      [(_ [start-token end-token] make-css:token datum extra ...)
       #'(make-css:token (css-token-source start-token)
                         (css-token-line start-token) (css-token-column start-token)
                         (css-token-position start-token)
                         (max (- (+ (css-token-position end-token) (css-token-span end-token))
                                 (css-token-position start-token)) 0)
                         datum extra ...)]
      [(_ here-token make-css:token datum ...)
       #'(css-remake-token [here-token here-token] make-css:token datum ...)]))

  (struct: css:bad:eof : CSS:Bad:EOF css:bad ())
  (struct: css:bad:eol : CSS:Bad:EOL css:bad ())
  (struct: css:bad:char : CSS:Bad:Char css:bad ())
  (struct: css:bad:blank : CSS:Bad:Blank css:bad ())
  (struct: css:bad:range : CSS:Bad:Range css:bad ())
  (struct: css:bad:range:index : CSS:Bad:Range:Index css:bad ())
  (struct: css:bad:stdin : CSS:Bad:StdIn css:bad ())

  ;;; https://drafts.csswg.org/css-syntax/#parsing
  (define-type (Listof+ CSS) (Pairof CSS (Listof CSS)))
  (define-type CSS-StdIn (U Input-Port Path-String Bytes (Listof CSS-Token)))
  (define-type CSS-URL-Modifier (U CSS:Ident CSS:Function CSS:URL))
  (define-type CSS-Syntax-Any (U CSS-Token EOF))
  (define-type CSS-Syntax-Terminal (U CSS:Delim CSS:Close EOF))
  (define-type CSS-Syntax-Rule (U CSS-Qualified-Rule CSS-@Rule))
  (define-type CSS-Descriptors (Listof CSS-Declaration))

  (struct: css-@rule : CSS-@Rule ([name : CSS:@Keyword] [prelude : (Listof CSS-Token)] [block : (Option CSS:Block)]))
  (struct: css-qualified-rule : CSS-Qualified-Rule ([prelude : (Listof+ CSS-Token)] [block : CSS:Block]))
  (struct: css-declaration : CSS-Declaration ([name : CSS:Ident] [values : (Listof+ CSS-Token)]
                                                                 [important? : Boolean]
                                                                 [custom? : Boolean]))

  (define-type CSS-Syntax-Error exn:css)
  (define-type Make-CSS-Syntax-Error (-> String Continuation-Mark-Set (Listof Syntax) CSS-Syntax-Error))

  (struct exn:css exn:fail:syntax ())
  (struct exn:css:resource exn:css ())
  (struct exn:css:deprecated exn:css ())
  (struct exn:css:unrecognized exn:css ())
  (struct exn:css:namespace exn:css ())
  (struct exn:css:misplaced exn:css:unrecognized ())
  (struct exn:css:type exn:css:unrecognized ())
  (struct exn:css:range exn:css:unrecognized ())
  (struct exn:css:unit exn:css:range ())
  (struct exn:css:overconsumption exn:css:unrecognized ())
  (struct exn:css:enclosed exn:css:overconsumption ())
  (struct exn:css:malformed exn:css ())
  (struct exn:css:empty exn:css:malformed ())
  (struct exn:css:missing-identifier exn:css:malformed ())
  (struct exn:css:missing-colon exn:css:malformed ())
  (struct exn:css:missing-block exn:css:malformed ())
  (struct exn:css:missing-value exn:css:malformed ())
  (struct exn:css:missing-feature exn:css:malformed ())
  (struct exn:css:missing-delimiter exn:css:malformed ())

  (define css-make-syntax-error : (-> Make-CSS-Syntax-Error (U CSS-Syntax-Any (Listof CSS-Token)) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#style-rules
    ;;; https://drafts.csswg.org/selectors/#invalid
    (lambda [exn:css v]
      (define tokens : (Listof CSS-Token)
        (cond [(eof-object? v) null]
              [(css:function? v) (cons v (css:function-arguments v))]
              [(css:block? v) (cons v (css:block-components v))]
              [(list? v) (filter-not css:whitespace? v)]
              [else (list v)]))
      (define errobj (exn:css (~a (object-name exn:css)) (continuation-marks #false) (map css-token->syntax tokens)))
      (log-message (current-logger) 'warning 'exn:css:syntax
                   (let-values ([(token others) (css-car tokens #false)])
                     (cond [(eof-object? token) (format "~a: ~a" (object-name exn:css) (if (eof-object? v) eof null))]
                           [(null? others) (format "~a: ~a" (object-name exn:css) (css-token->string token))]
                           [else (format "~a: ~a; others: ~s" (object-name exn:css) (css-token->string token)
                                         (map css-token->datum others))]))
                   errobj)
      errobj))

  (define-syntax (css-throw-syntax-error stx)
    (syntax-case stx []
      [(_ sexp ...)
       #'(raise (css-make-syntax-error sexp ...))]))

  ;; https://drafts.csswg.org/selectors/#grammar
  ;; https://drafts.csswg.org/selectors/#structure
  ;; https://drafts.csswg.org/selectors/#data-model
  (define-syntax (define-selectors stx)
    (syntax-case stx []
      [(_ [s-id #:+ S-ID rest ...] ...)
       #'(begin (struct: s-id : S-ID rest ...) ...)]))

  (define-syntax (define-css-subject stx)
    (syntax-case stx [: =]
      [(_ css-subject : CSS-Subject ([field : DataType maybe-defval ...] ...))
       (with-syntax ([make-subject (format-id #'css-subject "make-~a" (syntax-e #'css-subject))]
                     [(args ...) (for/fold ([args null])
                                           ([field (in-list (syntax->list #'(field ...)))]
                                            [arg (in-list (syntax->list #'([field : DataType maybe-defval ...] ...)))])
                                   (cons (datum->syntax field (string->keyword (symbol->string (syntax-e field))))
                                         (cons (datum->syntax arg (filter (λ [token] (not (eq? (syntax-e token) '=)))
                                                                          (syntax->list arg))) args)))])
         #'(begin (define-type CSS-Subject css-subject)
                  (struct css-subject ([field : DataType] ...) #:prefab)
                  (define (make-subject args ...) : CSS-Subject (css-subject field ...))))]))

  (define-type CSS-NameSpace (HashTable Symbol String))
  (define-type CSS-NameSpace-Hint (U CSS-NameSpace (Listof Symbol) False))
  (define-type CSS-Combinator (U '>> '> '+ '~ '||))
  (define-type CSS-Attribute-Datum (U String Symbol (Listof (U String Symbol))))
  (define-type CSS-Attribute-Value (U CSS-Attribute-Datum (Vector Symbol CSS-Attribute-Datum)))
  
  (define-selectors
    [css-attribute-selector #:+ CSS-Attribute-Selector ([name : Symbol] [namespace : (U Symbol Boolean)])]
    [css-attribute~selector #:+ CSS-Attribute~Selector css-attribute-selector ([operator : Char]
                                                                               [value : (U Symbol String)]
                                                                               [i? : Boolean])]
    
    [css-pseudo-class-selector #:+ CSS-Pseudo-Class-Selector ([name : Symbol] [arguments : (Option (Listof CSS-Token))])]
    [css-pseudo-element-selector #:+ CSS-Pseudo-Element-Selector ([name : Symbol]
                                                                  [arguments : (Option (Listof CSS-Token))]
                                                                  [pseudo-classes : (Listof CSS-Pseudo-Class-Selector)])]

    [css-compound-selector #:+ CSS-Compound-Selector ([combinator : (Option CSS-Combinator)]
                                                      [type : (U Symbol True)]
                                                      [namespace : (U Symbol Boolean)]
                                                      [pseudo-classes : (Listof CSS-Pseudo-Class-Selector)]
                                                      [classes : (Listof Symbol)]
                                                      [ids : (Listof Keyword)]
                                                      [attributes : (Listof CSS-Attribute-Selector)]
                                                      [pseudo-element : (Option CSS-Pseudo-Element-Selector)])]
    
    [css-complex-selector #:+ CSS-Complex-Selector ([specificity : Natural]
                                                    [list : (Listof+ CSS-Compound-Selector)]
                                                    [A : Natural]
                                                    [B : Natural]
                                                    [C : Natural])])

  (define-css-subject css-subject : CSS-Subject
    ([type : Symbol]
     [id : (U Keyword (Listof+ Keyword))]
     [namespace : (U Symbol Boolean) = #true]
     [classes : (Listof Symbol) = null]
     [attributes : (HashTable Symbol CSS-Attribute-Value) = (make-hasheq)]))

  (define css-selector-match : (-> CSS-Complex-Selector CSS-Subject (Option CSS-Complex-Selector))
    ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
    (lambda [selector element]
      (define s : CSS-Compound-Selector (last (css-complex-selector-list selector)))
      (define match? : Boolean
        (and (let ([s:type (css-compound-selector-type s)])
               (and (css-attribute-namespace-match? (css-compound-selector-namespace s) (css-subject-namespace element))
                    (or (eq? s:type #true) (symbol-ci=? s:type (css-subject-type element)))))
             (let ([s:ids : (Listof Keyword) (css-compound-selector-ids s)]
                   [id : (U Keyword (Listof+ Keyword)) (css-subject-id element)])
               (cond [(null? s:ids) #true]
                     [(keyword? id) (and (null? (cdr s:ids)) (eq? (car s:ids) id))]
                     [else (set=? (list->set s:ids) (list->set id))]))
             (let ([s:classes : (Listof Symbol) (css-compound-selector-classes s)]
                   [classes : (Listof Symbol) (css-subject-classes element)])
               (for/and : Boolean ([s:c (in-list s:classes)]) (and (memq s:c classes) #true)))
             (let ([s:attrs : (Listof CSS-Attribute-Selector) (css-compound-selector-attributes s)]
                   [attrs : (HashTable Symbol CSS-Attribute-Value) (css-subject-attributes element)])
               (for/and : Boolean ([attr : CSS-Attribute-Selector (in-list s:attrs)])
                 (and (hash-has-key? attrs (css-attribute-selector-name attr))
                      (let*-values ([(ns.val) (hash-ref attrs (css-attribute-selector-name attr))]
                                    [(ns datum) (cond [(not (vector? ns.val)) (values #false ns.val)]
                                                      [else (values (vector-ref ns.val 0) (vector-ref ns.val 1))])])
                        (and (css-attribute-namespace-match? (css-attribute-selector-namespace attr) ns)
                             (or (not (css-attribute~selector? attr)) ; [attr]
                                 (let* ([px:val : String (regexp-quote (~a (css-attribute~selector-value attr)))]
                                        [mode : String (if (css-attribute~selector-i? attr) "i" "-i")]
                                        [val : String (if (list? datum) (string-join ((inst map String Any) ~a datum)) (~a datum))])
                                   (and (non-empty-string? px:val)
                                        (case (css-attribute~selector-operator attr)
                                          [(#\=) (regexp-match? (pregexp (format "(?~a:^~a$)" mode px:val)) val)]
                                          [(#\~) (regexp-match? (pregexp (format "(?~a:\\b~a\\b)" mode px:val)) val)]
                                          [(#\|) (regexp-match? (pregexp (format "(?~a:^~a(-|$))" mode px:val)) val)]
                                          [(#\^) (regexp-match? (pregexp (format "(?~a:^~a)" mode px:val)) val)]
                                          [(#\$) (regexp-match? (pregexp (format "(?~a:~a$)" mode px:val)) val)]
                                          [(#\*) (regexp-match? (pregexp (format "(?~a:~a)" mode px:val)) val)]
                                          [else #false])))))))))))
      (and match? selector)))
  
  (define css-selector-specificity : (-> (Listof CSS-Compound-Selector) (values Natural Natural Natural Natural))
    ;;; https://drafts.csswg.org/selectors/#specificity-rules
    (lambda [complex-selector]
      (define-values (A B C)
        (for/fold ([A : Natural 0] [B : Natural 0] [C : Natural 0])
                  ([static-unit (in-list complex-selector)])
          (values (+ A (length (css-compound-selector-ids static-unit)))
                  (+ B (length (css-compound-selector-classes static-unit))
                     (length (css-compound-selector-pseudo-classes static-unit))
                     (length (css-compound-selector-attributes static-unit)))
                  (+ C (if (css-compound-selector-pseudo-element static-unit) 1 0)
                     (if (symbol? (css-compound-selector-type static-unit)) 1 0)))))
      (values (bitwise-ior (arithmetic-shift A 16) (arithmetic-shift B 8) C) A B C)))
  
  (define css-make-complex-selector : (-> (Listof+ CSS-Compound-Selector) CSS-Complex-Selector)
    (lambda [complex-selector]
      (define-values (specificity A B C) (css-selector-specificity complex-selector))
      (make-css-complex-selector specificity complex-selector A B C)))

  (define css-declared-namespace : (-> CSS-NameSpace-Hint (U CSS:Ident CSS:Delim Symbol) (U Symbol Boolean))
    (lambda [namespaces namespace]
      (or (css:delim? namespace)        ; *
          (let ([ns (if (css:ident? namespace) (css:ident-datum namespace) namespace)])
            (if (or (false? namespaces) ; application does not care namespaces
                    (and (hash? namespaces) (hash-has-key? namespaces ns))
                    (and (list? namespaces) (memq ns namespaces) #true))
                ns #false)))))

  (define css-attribute-namespace-match? : (-> (U Symbol Boolean) (U Symbol Boolean) Boolean)
    (lambda [src ns]
      (cond [(eq? src #true) #true]
            [(false? src) (false? ns)]
            [else (eq? src ns)])))
  
  ;; https://drafts.csswg.org/css-conditional/#at-supports
  ;; https://drafts.csswg.org/mediaqueries/#media-types
  ;; https://drafts.csswg.org/mediaqueries/#mq-syntax
  ;; https://drafts.csswg.org/mediaqueries/#mq-features
  (define-type CSS-Media-Query (U CSS-Media-Type CSS-Feature-Query (Pairof CSS-Media-Type CSS-Feature-Query)))
  (define-type CSS-Feature-Query (U CSS-Not CSS-And CSS-Or CSS-Media-Feature CSS-Declaration Symbol CSS-Syntax-Error))
  (define-type CSS-Media-Value (U CSS:Number CSS:Dimension CSS:Ident))
  (define-type CSS-Media-Datum (U Real Symbol))

  (struct: css-media-type : CSS-Media-Type ([name : Symbol] [only? : Boolean]))
  (struct: css-media-feature : CSS-Media-Feature ([name : Symbol] [value : CSS-Media-Datum] [operator : Char]))
  (struct: css-not : CSS-Not ([condition : CSS-Feature-Query]))
  (struct: css-and : CSS-And ([conditions : (Listof CSS-Feature-Query)]))
  (struct: css-or : CSS-Or ([conditions : (Listof CSS-Feature-Query)]))

  ;; https://drafts.csswg.org/mediaqueries/#media-descriptor-table
  ;; https://drafts.csswg.org/mediaqueries/#mf-deprecated
  (define-type CSS-Feature-Support? (-> Symbol (Listof+ CSS-Token) Boolean))
  (define-type CSS-Media-Preferences (HashTable Symbol CSS-Media-Datum))
  (define-type CSS-Media-Feature-Filter (-> Symbol (Option CSS-Media-Value) Boolean
                                            (Values (U CSS-Media-Datum Make-CSS-Syntax-Error Void)
                                                    Boolean)))

  (define css-media-feature-filter : CSS-Media-Feature-Filter
    (lambda [downcased-name maybe-value min/max?]
      (values
       (case downcased-name
         [(width height device-width device-height resolution)
          (define resolution? : Boolean (eq? downcased-name 'resolution))
          (cond [(and resolution? (css:ident=:=? maybe-value 'infinite)) +inf.0]
                [(and (not resolution?) (or (css:integer=:=? maybe-value zero?) (css:flonum=:=? maybe-value zero?))) 0]
                [(css:dimension? maybe-value)
                 (define n : Real (css-dimension->scalar maybe-value (if resolution? 'resolution 'length)))
                 (cond [(nan? n) exn:css:unit]
                       [(negative? n) exn:css:range]
                       [else n])]
                [(css-token? maybe-value) exn:css:type])]
         [(orientation scan update overflow-block overflow-inline color-gamut pointer hover any-pointer any-hover scripting)
          (cond [(css:ident? maybe-value)
                 (define downcased-option : Symbol (css:ident=> maybe-value symbol-downcase))
                 (cond [(memq downcased-option
                              (case downcased-name
                                [(orientation) '(portrait landscape)]
                                [(scan) '(interlace progressive)]
                                [(update) '(none slow fast)]
                                [(overflow-block) '(none scroll optional-paged paged)]
                                [(overflow-inline) '(none scroll)]
                                [(color-gamut) '(srgb p3 rec2020)]
                                [(pointer any-pointer) '(none coarse fine)]
                                [(havor any-havor) '(none havor)]
                                [(scripting) '(none initial-only enabled)]
                                [else null])) downcased-option]
                       [else exn:css:range])]
                [(css-token? maybe-value) exn:css:type])]
         [(aspect-ratio device-aspect-ratio)
          (cond [(css:ratio? maybe-value) (css:ratio-datum maybe-value)]
                [(css-token? maybe-value) exn:css:type])]
         [(color color-index monochrome)
          (cond [(css:integer=:=? maybe-value negative?) exn:css:range]
                [(css:integer? maybe-value) (css:integer-datum maybe-value)]
                [(css-token? maybe-value) exn:css:type])]
         [(grid)
          (cond [(and min/max?) exn:css:unrecognized]
                [(css:integer? maybe-value) (when (css:integer=:=? maybe-value zero?) 0)]
                [(css-token? maybe-value) exn:css:type])]
         [else exn:css:unrecognized])
       (and (memq downcased-name '(device-width device-height device-aspect-ratio))
            #true))))

  (define css-deprecate-media-type : (Parameterof Boolean) (make-parameter #false))
  (define current-css-media-type : (Parameterof Symbol) (make-parameter 'all))
  
  (define-values (current-css-media-preferences current-css-media-feature-filter current-css-feature-support?)
    (values (make-parameter ((inst make-hasheq Symbol CSS-Media-Datum)))
            (make-parameter css-media-feature-filter)
            (make-parameter (const #false))))

  ;; https://drafts.csswg.org/css-cascade/#shorthand
  ;; https://drafts.csswg.org/css-cascade/#filtering
  ;; https://drafts.csswg.org/css-cascade/#cascading
  (define-type CSS-Declared-Value (Pairof (Listof+ CSS-Token) Boolean))
  (define-type CSS-Declared-Values (HashTable Symbol CSS-Declared-Value))
  (define-type CSS-Declared-Result (U (Listof+ CSS-Token) Void Make-CSS-Syntax-Error (Pairof (Listof CSS-Token) Make-CSS-Syntax-Error)))
  (define-type CSS-Declared-Value-Filter (-> Symbol (Listof+ CSS-Token)
                                             (Values (U (HashTable Symbol (Listof+ CSS-Token)) CSS-Declared-Result)
                                                     Boolean)))
  (define-type (CSS-Value-Filter CSS-Datum) (-> CSS-Declared-Values (HashTable Symbol CSS-Datum) (Option (HashTable Symbol CSS-Datum))
                                                (Option Symbol) (HashTable Symbol CSS-Datum)))

  (define-syntax (call-with-css-preferences stx)
    (syntax-parse stx
      [(_ (~optional (~seq #:media maybe-preferences)) sexp ...)
       (with-syntax ([preferences (or (attribute maybe-preferences) #'(current-css-media-preferences))])
         #'(let ([w (hash-ref preferences 'width (thunk (current-css-containing-block-width)))]
                 [h (hash-ref preferences 'height (thunk (current-css-containing-block-height)))])
             (define-values (width height)
               (values (if (and (real? w) (positive? w)) (exact-round w) (current-css-containing-block-width))
                       (if (and (real? h) (positive? h)) (exact-round h) (current-css-containing-block-height))))
             (parameterize ([current-css-media-preferences preferences]
                            [current-css-containing-block-width width]
                            [current-css-containing-block-height height])
               sexp ...)))]))

  (define css-descriptor-ref : (All (a) (case-> [-> CSS-Declared-Values Symbol (-> Symbol (Option (Listof+ CSS-Token)) a) a]
                                                [-> CSS-Declared-Values Symbol (Option (Listof+ Datum))]))
    (lambda [descriptors desc-name [css->racket-values #false]]
      (let ([cascaded-values (hash-ref descriptors desc-name (const #false))])
        (cond [(false? cascaded-values) (and css->racket-values (css->racket-values desc-name #false))]
              [(false? css->racket-values) (map css-token->datum (car cascaded-values))]
              [else (css->racket-values desc-name (car cascaded-values))]))))
  
  (define css-all-descriptor-filter : CSS-Declared-Value-Filter
    ;; https://drafts.csswg.org/css-cascade/#all-shorthand
    (lambda [all-name all-values]
      (define argsize : Index (length all-values))
      (define all-value : CSS-Token (car all-values))
      (values (cond [(> argsize 1) (cons (cdr all-values) exn:css:overconsumption)]
                    [(not (css:ident? all-value)) exn:css:type]
                    [(memq (css:ident=> all-value symbol-downcase) '(initial inherit unset revert)) all-values]
                    [else exn:css:range])
              #false)))

  ;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
  (define css-viewport-descriptor-filter : CSS-Declared-Value-Filter
    (lambda [suitcased-name desc-values]
      (define argsize : Index (length desc-values))
      (define desc-value : CSS-Token (car desc-values))
      (define (viewport-length [v : CSS-Token]) : (U CSS-Token Make-CSS-Syntax-Error)
        (cond [(or (css:ident=:=? v 'auto) (css:percentage=:=? v (negate negative?))) v]
              [(or (css:ident? v) (css:percentage? v)) exn:css:range]
              [(not (css:dimension? v)) exn:css:type]
              [else (let ([maybe-dim (css-canonicalize-dimension v)])
                      ; this is okay even for font relative length since the @viewport is the first rule to be cascaded.
                      ; the font relative parameters should be initialized when the program starts.
                      (cond [(false? maybe-dim) exn:css:unit]
                            [else (let ([scalar (car (css:dimension-datum maybe-dim))])
                                    (cond [(negative? scalar) exn:css:range]
                                          [else maybe-dim]))]))]))
      (values
       (case suitcased-name
         [(width height)
          (define min-name : Symbol (if (eq? suitcased-name 'width) 'min-width 'min-height))
          (define max-name : Symbol (if (eq? suitcased-name 'width) 'max-width 'max-height))
          (define min-value : (U CSS-Token Make-CSS-Syntax-Error) (viewport-length desc-value))
          (define max-value : (U CSS-Token Make-CSS-Syntax-Error) (viewport-length (list-ref desc-values (if (> argsize 1) 1 0))))
          (cond [(> argsize 2) (cons (cddr desc-values) exn:css:overconsumption)]
                [(procedure? min-value) (cons (list desc-value) min-value)]
                [(procedure? max-value) (cons (cdr desc-values) max-value)]
                [else (hasheq min-name (list min-value) max-name (list max-value))])]
         [(min-width max-width min-height max-height)
          (define length-value : (U CSS-Token Make-CSS-Syntax-Error) (viewport-length desc-value))
          (cond [(> argsize 1) (cons (cdr desc-values) exn:css:overconsumption)]
                [(procedure? length-value) (cons (list desc-value) length-value)]
                [(eq? length-value desc-value) desc-values]
                [else (list length-value)])]
         [(zoom min-zoom max-zoom)
          (cond [(> argsize 1) (cons (cdr desc-values) exn:css:overconsumption)]
                [(or (css:ident=:=? desc-value 'auto)
                     (css:percentage=:=? desc-value (negate negative?))
                     (css:integer=:=? desc-value (negate negative?))
                     (css:flonum=:=? desc-value (negate negative?)))
                 desc-values]
                [(or (css:ident? desc-value) (css:percentage? desc-value)
                     (css:integer? desc-value) (css:flonum? desc-value))
                 exn:css:range]
                [else exn:css:type])]
         [(orientation user-zoom)
          (cond [(> argsize 1) (cons (cdr desc-values) exn:css:overconsumption)]
                [(not (css:ident? desc-value)) exn:css:type]
                [(memq (css:ident=> desc-value symbol-downcase)
                       (case suitcased-name
                         [(orientation) '(auto portrait landscape)]
                         [(user-zoom) '(zoom fixed)]
                         [else null])) desc-values]
                [else exn:css:range])]
         [else exn:css:unrecognized])
       #false)))

  (define css-viewport-filter : (CSS-Value-Filter CSS-Media-Datum)
    ;;; https://drafts.csswg.org/css-device-adapt/#constraining
    ;;; https://drafts.csswg.org/css-device-adapt/#handling-auto-zoom
    ;;; https://drafts.csswg.org/css-device-adapt/#media-queries
    (lambda [cascaded-values initial-viewport inherit-viewport all]
      ; Notes: We do not check the `initial-viewport` to specific the `specified values` since
      ;          @viewport is a controversial @rule which is easy to be used incorrectly,
      ;          @viewport is rarely used in desktop applications, and
      ;          this behavior is indeed specified if I understand the specification correctly.
      (define initial-width : Real (let ([w (hash-ref initial-viewport 'width (const 1))]) (if (symbol? w) 1 w)))
      (define initial-height : Real (let ([h (hash-ref initial-viewport 'height (const 1))]) (if (symbol? h) 1 h)))
      (define (smart [maix : (-> Real * Real)] [v1 : (U Real Symbol)] [v2 : (U Real Symbol)]) : (U Real Symbol)
        (cond [(and (symbol? v1) (symbol? v2)) 'auto]
              [(symbol? v1) v2]
              [(symbol? v2) v1]
              [else (maix v1 v2)]))
      (define (zoom->real [desc-name : Symbol] [maybe-values : (Option (Listof+ CSS-Token))]) : Real
        (define zoom : (Option CSS-Token) (and (pair? maybe-values) (car maybe-values)))
        (cond [(css:percentage? zoom) (* (css:percentage-datum zoom) 1/100)]
              [(css:integer? zoom) (css:integer-datum zoom)]
              [(css:flonum? zoom) (css:flonum-datum zoom)]
              [(eq? desc-name 'max-zoom) +inf.0]
              [(eq? desc-name 'min-zoom) 0]
              [else (current-css-viewport-auto-zoom)]))
      (define (size->scalar [desc-name : Symbol] [maybe-values : (Option (Listof+ CSS-Token))]) : (U Real Symbol)
        (define size : (Option CSS-Token) (and (pair? maybe-values) (car maybe-values)))
        (cond [(css:dimension? size) (car (css:dimension-datum size))] ; already canonicalized
              [(not (css:percentage? size)) 'auto]
              [(memq desc-name '(min-width max-width)) (* (css:percentage-datum size) 1/100 initial-width)]
              [else (* (css:percentage-datum size) 1/100 initial-height)]))
      (define (enum-value [desc-name : Symbol] [maybe-values : (Option (Listof+ CSS-Token))]) : (U Real Symbol)
        (define desc-value : (Option CSS-Token) (and (pair? maybe-values) (car maybe-values)))
        (cond [(css:ident? desc-value) (css:ident-datum desc-value)]
              [(eq? desc-name 'user-zoom) 'zoom]
              [else 'auto]))
      (define min-zoom : Real (css-descriptor-ref cascaded-values 'min-zoom zoom->real))
      (define max-zoom : Real (max min-zoom (css-descriptor-ref cascaded-values 'max-zoom zoom->real)))
      (define min-width : (U Real Symbol) (css-descriptor-ref cascaded-values 'min-width size->scalar))
      (define max-width : (U Real Symbol) (css-descriptor-ref cascaded-values 'max-width size->scalar))
      (define min-height : (U Real Symbol) (css-descriptor-ref cascaded-values 'min-height size->scalar))
      (define max-height : (U Real Symbol) (css-descriptor-ref cascaded-values 'max-height size->scalar))
      (define-values (width height)
        (let* ([width (smart max min-width (smart min max-width initial-width))]
               [height (smart max min-height (smart min max-height initial-height))]
               [width (cond [(and (symbol? width) (symbol? height)) initial-width]
                            [(symbol? width) (if (zero? initial-height) initial-width (* height (/ initial-width initial-height)))]
                            [else width])])
          (values width (cond [(real? height) height]
                              [(zero? initial-width) initial-height]
                              [else (* width (/ initial-height initial-width))]))))
      (define actual-viewport (hash-copy initial-viewport))
      (for ([name (in-list      '(min-zoom max-zoom width height))]
            [value (in-list (list min-zoom max-zoom width height))])
        (hash-set! actual-viewport name value))
      (hash-set! actual-viewport 'orientation (css-descriptor-ref cascaded-values 'orientation enum-value))
      (hash-set! actual-viewport 'user-zoom (css-descriptor-ref cascaded-values 'user-zoom enum-value))
      (hash-set! actual-viewport 'zoom (max min-zoom (min max-zoom (css-descriptor-ref cascaded-values 'zoom zoom->real))))
      actual-viewport))

  (define-values (current-css-viewport-descriptor-filter current-css-viewport-filter current-css-viewport-auto-zoom)
    (values (make-parameter css-viewport-descriptor-filter)
            (make-parameter css-viewport-filter)
            (make-parameter 1.0)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; https://drafts.csswg.org/css-values/#lengths
  ;;; https://drafts.csswg.org/css-values/#other-units
  (define-type Quantity->Scalar (case-> [Nonnegative-Exact-Rational Symbol -> (U Nonnegative-Exact-Rational Flonum-Nan)]
                                        [Exact-Rational Symbol -> (U Exact-Rational Flonum-Nan)]
                                        [Nonnegative-Real Symbol -> Nonnegative-Real]
                                        [Real Symbol -> Real]))

  (define-values (current-css-containing-block-width current-css-containing-block-height)
    (values (ann (make-parameter 1) (Parameterof Nonnegative-Exact-Rational))
            (ann (make-parameter 1) (Parameterof Nonnegative-Exact-Rational))))

  (define-values (current-css-element-font-size current-css-root-element-font-size)
    (values (ann (make-parameter 1) (Parameterof Nonnegative-Exact-Rational))
            (ann (make-parameter 1) (Parameterof Nonnegative-Exact-Rational))))

  (define-values (current-css-x-height current-css-char-width current-css-word-width)
    (values (ann (make-parameter 1/2) (Parameterof Nonnegative-Exact-Rational))
            (ann (make-parameter 1/2) (Parameterof Nonnegative-Exact-Rational))
            (ann (make-parameter 1) (Parameterof Nonnegative-Exact-Rational))))

  (define css-font-relative-length? : (-> (U CSS:Dimension (Pairof Real Symbol)) Boolean)
    (lambda [dim]
      (cond [(css:dimension? dim) (css-font-relative-length? (css:dimension-datum dim))]
            [else (and (memq (cdr dim) '(em ex ch ic rem)) #true)])))

  (define css-viewport-relative-length? : (-> (U CSS:Dimension (Pairof Real Symbol)) Boolean)
    (lambda [dim]
      (cond [(css:dimension? dim) (css-font-relative-length? (css:dimension-datum dim))]
            [else (and (memq (cdr dim) '(vw vh vi vb vmin vmax)) #true)])))
  
  (define css-dimension->scalar : (->* ((U CSS:Dimension (Pairof Real Symbol))) ((Option Symbol) (Option (Boxof Symbol))) Real)
    (lambda [dim [type #false] [&canonical-unit #false]]
      (cond [(css:dimension? dim) (css-dimension->scalar (css:dimension-datum dim) type)]
            [else (let-values ([(n unit) (values (car dim) (cdr dim))])
                    (define-values (scalar canonical-unit)
                      (case (or type unit)
                        [(length px cm mm q in pc pt em ex ch ic rem vw vh vi vb vmin vmax) (values (css-length->scalar n unit) 'px)]
                        [(angle deg grad rad turn) (values (css-angle->scalar n unit) 'deg)]
                        [(time s ms min h) (values (css-time->scalar n unit) 's)]
                        [(frequency hz khz) (values (css-frequency->scalar n unit) 'hz)]
                        [(resolution dpi dpcm dppx) (values (css-resolution->scalar n unit) 'dpi)]
                        [else (values +nan.0 unit)]))
                    (when (box? &canonical-unit) (set-box! &canonical-unit (if (nan? scalar) 'NaN canonical-unit)))
                    scalar)])))

  (define css-canonicalize-dimension : (->* (CSS:Dimension) ((Option Symbol)) (Option CSS:Dimension))
    (lambda [dim [type #false]]
      (define unit : Symbol (cdr (css:dimension-datum dim)))
      (define &canonical-unit : (Boxof Symbol) (box unit))
      (define scalar : Real (css-dimension->scalar dim type &canonical-unit))
      (define canonical-unit : Symbol (unbox &canonical-unit))
      (cond [(nan? scalar) #false]
            [(eq? canonical-unit unit) dim]
            [else (css-remake-token dim css:dimension (css:numeric-representation dim) (cons scalar canonical-unit))])))
  
  (define css-length->scalar : Quantity->Scalar
    ;;; https://drafts.csswg.org/css-values/#absolute-lengths
    ;;; https://drafts.csswg.org/css-values/#relative-lengths
    (lambda [n unit]
      (case unit
        [(px) n]
        [(cm) (* n 9600/254)]
        [(mm) (* n 9600/254 1/10)]
        [(q) (* n 9600/254 1/40)]
        [(in) (* n 96)]
        [(pc) (* n 96 1/6)]
        [(pt) (* n 96 1/72)]
        [(em) (* n (current-css-element-font-size))]
        [(ex) (* n (current-css-x-height))]
        [(ch) (* n (current-css-char-width))]
        [(ic) (* n (current-css-word-width))]
        [(rem) (* n (current-css-root-element-font-size))]
        [(vw vi) (* n 1/100 (current-css-containing-block-width))]
        [(vh vb) (* n 1/100 (current-css-containing-block-height))]
        [(vmin) (* n 1/100 (min (current-css-containing-block-width) (current-css-containing-block-height)))]
        [(vmax) (* n 1/100 (max (current-css-containing-block-width) (current-css-containing-block-height)))]
        [else +nan.0])))
  
  (define css-angle->scalar : (case-> [Nonnegative-Real Symbol -> Nonnegative-Real] [Real Symbol -> Real])
    ;;; https://drafts.csswg.org/css-values/#angles
    (lambda [n unit]
      (case unit
        [(deg) n]
        [(grad) (* n 9/10)]
        [(rad) (* n (/ 180 pi))]
        [(turn) (* n 360)]
        [else +nan.0])))

  (define css-time->scalar : Quantity->Scalar
    ;;; https://drafts.csswg.org/css-values/#time
    (lambda [n unit]
      (case unit
        [(s) n]
        [(ms) (* n 1/1000)]
        [(min) (* n 60)]
        [(h) (* n 60 60)]
        [else +nan.0])))

  (define css-frequency->scalar : Quantity->Scalar
    ;;; https://drafts.csswg.org/css-values/#frequency
    (lambda [n unit]
      (case unit
        [(hz) n]
        [(khz) (* n 1/1000)]
        [else +nan.0])))

  (define css-resolution->scalar : Quantity->Scalar
    ;;; https://drafts.csswg.org/css-values/#resolution
    (lambda [n unit]
      (case unit
        [(dppx) n]
        [(dpcm) (css-length->scalar n 'cm)]
        [(dpi) (css-length->scalar n 'in)]
        [else +nan.0])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-base-datum? : (-> Any Boolean : #:+ Datum)
    (lambda [datum]
      (or (symbol? datum) (number? datum)
          (string? datum) (char? datum) (keyword? datum))))
  
  (define css-car : (All (CSS-DT) (->* ((Listof CSS-DT)) (Boolean) (Values (U CSS-DT EOF) (Listof CSS-DT))))
    (lambda [dirty [skip-whitespace? #true]]
      (let skip-whitespace ([rest dirty])
        (cond [(null? rest) (values eof null)]
              [else (let-values ([(head tail) (values (car rest) (cdr rest))])
                      (cond [(and (css:whitespace? head) skip-whitespace?) (skip-whitespace tail)]
                            [else (values head tail)]))]))))
  
  (define css-null? : (-> (Listof Any) Boolean)
    (lambda [dirty]
      (let skip-whitespace : Boolean ([rest dirty])
        (or (null? rest)
            (and (css:whitespace? (car rest))
                 (skip-whitespace (cdr rest)))))))

  (define css-tokens? : (-> (Listof CSS-Token) Boolean)
    (lambda [dirty]
      (let skip-whitespace : Boolean ([rest dirty])
        (cond [(null? rest) #false]
              [else (implies (css:whitespace? (car rest))
                             (skip-whitespace (cdr rest)))]))))
  
  (define css-cons : (All (CSS-DT) (-> (U CSS-Syntax-Error CSS-DT) (Listof CSS-DT) (Listof CSS-DT)))
    (lambda [item items]
      (cond [(exn? item) items]
            [else (cons item items)])))

  (define symbol-downcase : (-> Symbol Symbol)
    (lambda [sym]
      (string->symbol (string-downcase (symbol->string sym)))))

  (define --symbol? : (-> Symbol Boolean)
    (lambda [sym]
      (string-prefix? (symbol->string sym) "--")))
  
  (define symbol-ci=? : (-> Symbol Symbol Boolean)
    (lambda [sym1 sym2]
      (or (eq? sym1 sym2)
          (string-ci=? (symbol->string sym1)
                       (symbol->string sym2)))))

  (define keyword-ci=? : (-> Keyword Keyword Boolean)
    (lambda [kw1 kw2]
      (or (eq? kw1 kw2)
          (string-ci=? (keyword->string kw1)
                       (keyword->string kw2))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module tokenizer typed/racket ;;; https://drafts.csswg.org/css-syntax/#tokenization
  (provide (all-defined-out))

  (require (submod ".." digitama))

  (struct css-srcloc ([in : Input-Port] [source : (U String Symbol)]
                                        [line : (Option Positive-Integer)] [col : (Option Natural)]
                                        [pos : (Option Positive-Integer)])
    #:type-name CSS-Srcloc)

  (define-syntax (css-make-token stx)
    (syntax-case stx []
      [(_ src make-css:token datum extra ...)
       #'(let-values ([(start-position) (css-srcloc-pos src)]
                      [(line column position) (port-next-location (css-srcloc-in src))])
           (make-css:token (css-srcloc-source src)
                           (or (css-srcloc-line src) line 0)
                           (or (css-srcloc-col src) column 0)
                           (or start-position 0)
                           (cond [(nand (integer? position) (integer? start-position)) 0]
                                 [else (max (- position start-position) 0)])
                           datum extra ...))]))

  (define-syntax (css-make-bad-token stx)
    (syntax-case stx []
      [(_ src css:bad:sub token datum)
       #'(let ([bad (css-make-token src css:bad:sub (cons (assert (object-name token) symbol?) datum))])
           (log-message (current-logger) 'warning 'exn:css:read (css-token->string bad) bad)
           bad)]))
  
  (define css-consume-token : (-> Input-Port (U String Symbol) (U EOF CSS-Token))
    ;;; https://drafts.csswg.org/css-syntax/#error-handling
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-token
    (lambda [/dev/cssin source]
      (define-values (line column position) (port-next-location /dev/cssin))
      (define srcloc (css-srcloc /dev/cssin source line column position))
      (define ch (read-char /dev/cssin))
      (cond [(eof-object? ch) eof]
            [(char-whitespace? ch) (css-consume-whitespace-token srcloc)]
            [(char-numeric? ch) (css-consume-numeric-token srcloc ch)]
            [(css-char-name-prefix? ch) (css-consume-ident-token srcloc ch)]
            [else (case ch
                    [(#\) #\] #\}) (css-make-token srcloc css:close ch)]
                    [(#\' #\") (css-consume-string-token srcloc ch)]
                    [(#\+ #\.) (css-consume-numeric-token srcloc ch)]
                    [(#\^ #\$ #\| #\~ #\*) (css-consume-match-token srcloc ch)]
                    [(#\#) (css-consume-hash-token srcloc)]
                    [(#\@) (css-consume-@keyword-token srcloc)]
                    [(#\/) (css-consume-comment-token srcloc)]
                    [(#\< #\-) (css-consume-cd-token srcloc ch)]
                    [(#\null) (css-make-token srcloc css:delim #\uFFFD)]
                    [else (css-make-token srcloc css:delim ch)])])))

  (define css-consume-cd-token : (-> CSS-Srcloc Char CSS-Token)
    ;;; https://drafts.csswg.org/css-syntax/#CDO-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#CDC-token-diagram
    (lambda [srcloc open/close]
      (define css : Input-Port (css-srcloc-in srcloc))
      (if (char=? open/close #\<)
          (let ([cdo : (U EOF String) (peek-string 3 0 css)])
            (cond [(and (string? cdo) (string=? cdo "!--")) (read-string 3 css) (css-make-token srcloc css:cd '<!--)]
                  [else (css-make-token srcloc css:delim #\<)]))
          (let ([cdc : (U EOF String) (peek-string 2 0 css)])
            (cond [(eof-object? cdc) (css-make-token srcloc css:delim #\-)]
                  [(string=? cdc "->") (read-string 2 css) (css-make-token srcloc css:cd '-->)]
                  [(css-identifier-prefix? #\- (string-ref cdc 0) (string-ref cdc 1)) (css-consume-ident-token srcloc #\-)]
                  [else (css-consume-numeric-token srcloc #\-)])))))

  (define css-consume-comment-token : (-> CSS-Srcloc (U CSS:WhiteSpace CSS:Delim CSS:Bad))
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (cond [(or (eof-object? ch1) (not (char=? ch1 #\*))) (css-make-token srcloc css:delim #\/)]
            [(regexp-match #px".*?\\*/" css) => (λ [**/] (css-make-token srcloc css:whitespace (format "/~a" (car **/))))]
            [else (css-make-bad-token srcloc css:bad:eof struct:css:whitespace "/*")])))

  (define css-consume-whitespace-token : (-> CSS-Srcloc CSS:WhiteSpace)
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (css-make-token srcloc css:whitespace #\space)))
  
  (define css-consume-ident-token : (-> CSS-Srcloc Char (U CSS:Ident CSS:Function CSS:URL CSS:URange CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-ident-like-token
    ;;; https://drafts.csswg.org/css-syntax/#urange-syntax
    ;;; https://drafts.csswg.org/css-values/#urls
    (lambda [srcloc id0]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (cond [(and (char-ci=? id0 #\u) (char? ch1) (char=? ch1 #\+)
                  (or (char-hexdigit? ch2) (and (char? ch2) (char=? ch2 #\?))))
             (read-char css) (css-consume-unicode-range-token srcloc)]
            [else (let ([name : String (css-consume-name css id0)])
                    (define ch : (U EOF Char) (peek-char css))
                    (cond [(or (eof-object? ch) (not (char=? ch #\()))
                           (css-make-token srcloc css:ident (string->symbol name))]
                          [(or (not (string-ci=? name "url")) (regexp-match-peek #px"^.\\s*[\"']" css))
                           (read-char css) (css-make-token srcloc css:function (string->unreadable-symbol name) null)]
                          [else (read-char css) (css-consume-url-token srcloc)]))])))
      
  (define css-consume-string-token : (-> CSS-Srcloc Char (U CSS:String CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-string-token
    (lambda [srcloc quotation]
      (define css : Input-Port (css-srcloc-in srcloc))
      (let consume-string-token : (U CSS:String CSS:Bad) ([chars : (Listof Char) null])
        (define ch : (U EOF Char) (read-char css))
        (cond [(or (eof-object? ch) (char=? ch quotation))
               (when (eof-object? ch) (css-make-bad-token srcloc css:bad:eof struct:css:string (list->string (reverse chars))))
               (css-make-token srcloc css:string (list->string (reverse chars)))]
              [(char=? ch #\newline)
               (css-make-bad-token srcloc css:bad:eol struct:css:string (list->string (reverse chars)))]
              [(not (char=? ch #\\))
               (consume-string-token (cons ch chars))]
              [else (let ([next (peek-char css)])
                      (cond [(eof-object? next) (consume-string-token chars)]
                            [(char=? next #\newline) (read-char css) (consume-string-token (cons ch chars))]
                            [else (consume-string-token (cons (css-consume-escaped-char css) chars))]))]))))

  (define css-consume-numeric-token : (-> CSS-Srcloc Char (U CSS:Numeric CSS:Delim CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    ;;; https://drafts.csswg.org/css-values/#numeric-types
    (lambda [srcloc sign/digit]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (cond [(not (css-number-prefix? sign/digit ch1 ch2)) (css-make-token srcloc css:delim sign/digit)]
            [else (let-values ([(n representation) (css-consume-number css sign/digit)])
                    (let ([ch1 : (U EOF Char) (peek-char css 0)]
                          [ch2 : (U EOF Char) (peek-char css 1)]
                          [ch3 : (U EOF Char) (peek-char css 2)])
                      (cond [(css-identifier-prefix? ch1 ch2 ch3)
                             (define unit : Symbol (string->symbol (string-downcase (css-consume-name css #false))))
                             (css-make-token srcloc css:dimension representation (cons n unit))]
                            [(and (char? ch1) (char=? ch1 #\%)) (read-char css)
                             (css-make-token srcloc css:percentage representation (real->fraction n))]
                            [(exact-integer? n) (css-make-token srcloc css:integer representation n)]
                            [else (css-make-token srcloc css:flonum representation (real->double-flonum n))])))])))

  (define css-consume-url-token : (-> CSS-Srcloc (U CSS:URL CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-url-token
    ;;; https://drafts.csswg.org/css-values/#urls
    ;;; https://drafts.csswg.org/css-values/#url-empty
    ;;; https://drafts.csswg.org/css-values/#about-invalid
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (define start : (U EOF Char) (read-char css))
      (cond [(or (eof-object? start) (char=? start #\)))
             (when (eof-object? start) (css-make-bad-token srcloc css:bad:eof struct:css:url ""))            
             (css-make-token srcloc css:url 'about:invalid null)]
            [else (let consume-url-token : (U CSS:URL CSS:Bad) ([chars (list start)])
                    (define ch : (U EOF Char) (read-char css))
                    (cond [(or (eof-object? ch) (char=? ch #\)))
                           (when (eof-object? ch) (css-make-bad-token srcloc css:bad:eof struct:css:url (list->string (reverse chars))))
                           (css-make-token srcloc css:url (list->string (reverse chars)) null)]
                          [(char-whitespace? ch)
                           (css-consume-whitespace css)
                           (define end : (U EOF Char) (read-char css))
                           (define uri : String (list->string (reverse chars)))
                           (cond [(or (eof-object? end) (char=? end #\))) (css-make-token srcloc css:url uri null)]
                                 [else (css-consume-bad-url-remnants css (css-make-bad-token srcloc css:bad:blank struct:css:url uri))])]
                          [(css-valid-escape? ch (peek-char css))
                           (consume-url-token (cons (css-consume-escaped-char css) chars))]
                          [(or (memq ch '(#\\ #\" #\' #\()) (css-char-non-printable? ch))
                           (css-consume-bad-url-remnants css (css-make-bad-token srcloc css:bad:char struct:css:url ch))]
                          [else (consume-url-token (cons ch chars))]))])))

  (define css-consume-unicode-range-token : (-> CSS-Srcloc (U CSS:URange CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#urange-syntax
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define-values (n rest) (css-consume-hexadecimal (css-srcloc-in srcloc) 6))
      (define-values (start0 end0)
        (let consume-? : (Values Integer Integer) ([s : Integer n] [e : Integer n] [? : Integer rest])
          (cond [(zero? ?) (values s e)]
                [else (let ([ch : (U EOF Char) (peek-char css)])
                        (cond [(or (eof-object? ch) (not (char=? ch #\?))) (values s e)]
                              [else (read-char css) (consume-? (* s 16) (+ (* e 16) (char->hexadecimal #\f)) (sub1 ?))]))])))
      (define-values (start end)
        (cond [(not (= start0 end0)) (values start0 end0)]
              [else (let ([ch1 (peek-char css 0)]
                          [ch2 (peek-char css 1)])
                      (cond [(and (char? ch1) (char=? ch1 #\-) (char-hexdigit? ch2)) (read-char css)
                             (define-values (end _) (css-consume-hexadecimal (css-srcloc-in srcloc) 6))
                             (values start0 end)]
                            [else (values start0 start0)]))]))
      (cond [(and (index? start) (index? end) (<= start end #x10FFFF)) (css-make-token srcloc css:urange (cons start end))]
            [(> end #x10FFFF) (css-make-bad-token srcloc css:bad:range:index struct:css:urange end)]
            [else (css-make-bad-token srcloc css:bad:range struct:css:urange (cons start end))])))

  (define css-consume-hash-token : (-> CSS-Srcloc (U CSS:Hash CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#hash-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (or (css-char-name? ch1) (css-valid-escape? ch1 ch2))
          (let ([name : String (css-consume-name (css-srcloc-in srcloc) #false)])
            (css-make-token srcloc css:hash (string->keyword name)
                            (cond [(css-identifier-prefix? ch1 ch2 ch3) 'id]
                                  [else 'unrestricted])))
          (css-make-token srcloc css:delim #\#))))

  (define css-consume-@keyword-token : (-> CSS-Srcloc (U CSS:@Keyword CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#at-keyword-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (css-identifier-prefix? ch1 ch2 ch3)
          (let ([name : String (css-consume-name (css-srcloc-in srcloc) #\@)])
            (css-make-token srcloc css:@keyword (string->keyword name)))
          (css-make-token srcloc css:delim #\@))))

  (define css-consume-match-token : (-> CSS-Srcloc Char (U CSS:Match CSS:|| CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#include-match-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#column-token-diagram
    (lambda [srcloc prefix]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch : (U EOF Char) (peek-char css))
      (cond [(and (char=? prefix #\|) (eq? ch #\|)) (read-char css) (css-make-token srcloc css:|| '||)]
            [(eq? ch #\=) (read-char css) (css-make-token srcloc css:match prefix)]
            [else (css-make-token srcloc css:delim prefix)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-whitespace : (-> Input-Port Void)
    (lambda [css]
      (regexp-match #px"\\s*" css)
      (void)))
  
  (define css-consume-name : (-> Input-Port (Option Char) String)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-name
    (lambda [css maybe-head]
      (let consume-name : String ([srahc : (Listof Char) (if maybe-head (list maybe-head) null)])
        (define ch : (U EOF Char) (peek-char css))
        (cond [(css-char-name? ch) (read-char css) (consume-name (cons ch srahc))]
              [(css-valid-escape? ch (peek-char css 1)) (read-char css) (consume-name (cons (css-consume-escaped-char css) srahc))]
              [else (list->string (reverse srahc))]))))

  (define css-consume-number : (-> Input-Port Char (Values Real String))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [css sign/digit]
      (let consume-number : (Values Real String) ([chars (list sign/digit)])
        (define ch : (U EOF Char) (peek-char css))
        (cond [(and (char? ch)
                    (or (char-numeric? ch)
                        (char=? ch #\+) (char=? ch #\-)
                        (css-decimal-point? ch (peek-char css 1))
                        (css-scientific-notation? ch (peek-char css 1) (peek-char css 2))))
               (read-char css)
               (consume-number (cons ch chars))]
              [else (let* ([representation : String (list->string (reverse chars))]
                           [maybe-number : (Option Complex) (string->number representation)])
                      (cond [(real? maybe-number) (values maybe-number representation)]
                            [else (values +nan.0 representation)]))]))))

  (define css-consume-hexadecimal : (->* (Input-Port Byte) (Integer #:\s?$? Boolean) (Values Integer Byte))
    (lambda [css rest-count [result 0] #:\s?$? [eat-last-whitespace? #false]]
      (define hex : (U EOF Char) (peek-char css))
      (cond [(or (eof-object? hex) (not (char-hexdigit? hex)) (zero? rest-count))
             (when (and eat-last-whitespace? (char? hex) (char-whitespace? hex)) (read-char css))
             (values result rest-count)]
            [else (read-char css)
             (css-consume-hexadecimal #:\s?$? eat-last-whitespace?
                                      css (sub1 rest-count)
                                      (+ (* result 16)
                                         (char->hexadecimal hex)))])))

  (define css-consume-escaped-char : (-> Input-Port Char)
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-escaped-code-point
    (lambda [css]
      (define esc : (U EOF Char) (read-char css))
      (cond [(eof-object? esc) #\uFFFD]
            [(not (char-hexdigit? esc)) esc]
            [else (let-values ([(hex _) (css-consume-hexadecimal css (sub1 6) (char->hexadecimal esc) #:\s?$? #true)])
                    (cond [(or (<= hex 0) (> hex #x10FFFF)) #\uFFFD] ; #\nul and max unicode
                          [(<= #xD800 hex #xDFFF) #\uFFFD] ; surrogate
                          [else (integer->char hex)]))])))

  (define css-consume-bad-url-remnants : (-> Input-Port CSS:Bad CSS:Bad)
    ;;; https://drafts.csswg.org/css-syntax/#consume-the-remnants-of-a-bad-url
    (lambda [css bad-url-token]
      (define ch : (U EOF Char) (read-char css))
      (cond [(or (eof-object? ch) (char=? ch #\))) bad-url-token]
            [(char=? ch #\\) (read-char css) (css-consume-bad-url-remnants css bad-url-token)]
            [else (css-consume-bad-url-remnants css bad-url-token)])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define char-hexdigit? : (-> (U EOF Char) Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char<=? #\0 ch #\9)
               (char-ci<=? #\a ch #\f)))))

  (define char->hexadecimal : (-> Char Integer)
    (lambda [hexch]
      (cond [(char<=? #\A hexch #\F) (- (char->integer hexch) 55)]
            [(char<=? #\a hexch #\f) (- (char->integer hexch) 87)]
            [else (- (char->integer hexch) 48)])))

  (define real->fraction : (-> Real Exact-Rational)
    (lambda [r]
      (cond [(and (rational? r) (exact? r)) r]
            [(and (integer? r) (inexact? r)) (inexact->exact r)]
            [else (let* ([representation (number->string r)]
                         [fraction (string->number (string-append (string-replace representation "." "") "/1"
                                                                  (string-replace (string-replace representation #px".*?\\." "")
                                                                                  #px"." "0")))])
                    (cond [(and (rational? fraction) (exact? fraction)) fraction]
                          [else 0 #| this should not happen |#]))])))

  (define css-char-non-printable? : (-> (U EOF Char) Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char<=? #\null ch #\backspace)
               (char=? ch #\vtab)
               (char<=? #\u000E ch #\u001F)
               (char=? ch #\rubout)))))

  (define css-char-name-prefix? : (-> (U EOF Char) Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char<=? #\a ch #\z)
               (char<=? #\A ch #\Z)
               (char>=? ch #\u0080)
               (char=? #\_  ch)))))

  (define css-char-name? : (-> (U EOF Char) Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (css-char-name-prefix? ch)
               (char<=? #\0 ch #\9)
               (char=? #\- ch)))))
  
  (define css-valid-escape? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#escaping
    ;;; https://drafts.csswg.org/css-syntax/#starts-with-a-valid-escape
    (lambda [ch1 ch2]
      (and (char? ch1)
           (char=? ch1 #\\)
           (or (eof-object? ch2)
               (not (char=? ch2 #\newline))))))

  (define css-identifier-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#would-start-an-identifier
    (lambda [ch1 ch2 ch3]
      (or (css-char-name-prefix? ch1)
          (css-valid-escape? ch1 ch2)
          (and (char? ch1) (char=? ch1 #\-)
               (or (css-char-name-prefix? ch2)
                   (and (char? ch2) (char=? ch2 #\-))
                   (css-valid-escape? ch2 ch3))))))

  (define css-number-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#starts-with-a-number
    (lambda [ch1 ch2 ch3]
      (or (and (char? ch1) (char<=? #\0 ch1 #\9))
          (and (char? ch1) (char? ch2)
               (char=? ch1 #\.) (char<=? #\0 ch2 #\9))
          (and (char? ch1) (char? ch2)
               (or (char=? ch1 #\+) (char=? ch1 #\-))
               (or (char<=? #\0 ch2 #\9)
                   (and (char=? ch2 #\.)
                        (char? ch3) (char<=? #\0 ch3 #\9)))))))

  (define css-scientific-notation? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [ch1 ch2 ch3]
      (and (char? ch1) (char? ch2)
           (char-ci=? ch1 #\e)
           (or (char<=? #\0 ch2 #\9)
               (and (or (char=? ch2 #\+) (char=? ch2 #\-))
                    (char? ch3)
                    (char<=? #\0 ch3 #\9))))))

  (define css-decimal-point? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [ch1 ch2]
      (and (char? ch1) (char? ch2)
           (char=? ch1 #\.)
           (char<=? #\0 ch2 #\9))))

  (define css-fallback-charset : (-> String String)
    (lambda [from]
      (define CHARSET : String (string-upcase from))
      (cond [(member CHARSET '("UTF-16BE" "UTF-16LE")) "UTF-8"]
            [else CHARSET])))
  
  (define css-fallback-encode-input-port : (-> Input-Port Input-Port)
    ;;; https://drafts.csswg.org/css-syntax/#input-byte-stream
    ;;; https://drafts.csswg.org/css-syntax/#charset-rule
    (lambda [/dev/rawin]
      (define magic : (U EOF String) (peek-string 1024 0 /dev/rawin))
      (cond [(eof-object? magic) /dev/rawin]
            [(let ([charset? (regexp-match #px"^@charset \"(.*?)\";" magic)])
               (and charset? (let ([name (cdr charset?)]) (and (pair? name) (car name)))))
             => (λ [v] (let ([charset (css-fallback-charset v)])
                         (cond [(string-ci=? charset "UTF-8") /dev/rawin]
                               [else (with-handlers ([exn? (const /dev/rawin)])
                                       (reencode-input-port /dev/rawin charset
                                                            (string->bytes/utf-8 (format "@charset \u22~a\u22;" v))
                                                            #false (object-name /dev/rawin) #true))])))]
            [else /dev/rawin]))))

(module parser typed/racket ;;; https://drafts.csswg.org/css-syntax/#parsing
  (provide (all-defined-out))

  (require (submod ".." digitama))
  (require (submod ".." tokenizer))
  
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
        (cond [(eof-object? stx) (css-make-syntax-error exn:css:empty stx)]
              [(css:@keyword? stx) (css-consume-@rule /dev/cssin stx)]
              [else (css-consume-qualified-rule /dev/cssin stx)]))
      (define end (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(or (eof-object? end) (exn? retval)) retval]
            [else (css-make-syntax-error exn:css:overconsumption end)])))

  (define-css-parser-entry css-parse-declaration :-> (U CSS-Declaration CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#declaration
    ;;; https://drafts.csswg.org/css-syntax/#parse-declaration
    ;;; https://drafts.csswg.org/css-conditional/#at-ruledef-supports
    (lambda [/dev/cssin]
      (define token (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(not (css:ident? token)) (css-make-syntax-error exn:css:missing-identifier token)]
            [else (let-values ([(components _) (css-consume-components /dev/cssin)])
                    (css-components->declaration token components))])))

  (define-css-parser-entry css-parse-declarations :-> (Listof (U CSS-Declaration CSS-@Rule))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-declarations
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-list-of-declarations
    (lambda [/dev/cssin]
      (let consume-declaration+@rule ([mixed-list : (Listof (U CSS-Declaration CSS-@Rule)) null])
        (define token (css-read-syntax /dev/cssin))
        (cond [(eof-object? token) (reverse mixed-list)]
              [(or (css:whitespace? token) (css:delim=:=? token #\;)) (consume-declaration+@rule mixed-list)]
              [(css:@keyword? token) (consume-declaration+@rule (cons (css-consume-@rule /dev/cssin token) mixed-list))]
              [else (let-values ([(components _) (css-consume-components /dev/cssin #\;)])
                      (consume-declaration+@rule (css-cons (cond [(css:ident? token) (css-components->declaration token components)]
                                                                 [else (css-make-syntax-error exn:css:missing-identifier token)])
                                                           mixed-list)))]))))
  
  (define-css-parser-entry css-parse-component-value :-> (U CSS-Token CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#parse-component-value
    (lambda [/dev/cssin]
      (define token (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(eof-object? token) (css-make-syntax-error exn:css:empty token)]
            [else (let ([retval (css-consume-component-value /dev/cssin token)])
                    (define end (css-read-syntax/skip-whitespace /dev/cssin))
                    (cond [(eof-object? end) retval]
                          [else (css-make-syntax-error exn:css:overconsumption end)]))])))

  (define-css-parser-entry css-parse-component-values :-> (Listof CSS-Token)
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [/dev/cssin]
      (define-values (components _) (css-consume-components /dev/cssin))
      components))

  (define-css-parser-entry css-parse-component-valueses :-> (Listof (Listof CSS-Token))
    ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
    ;;; https://drafts.csswg.org/css-values/#comb-comma
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
          (cond [(css:ident=:=? token 'not)
                 (cond [(css:ident? next) (css-components->media-type+query next #false rest)]
                       [else (css-components->negation token tokens #true)])]
                [(css:ident? token)
                 (define-values (maybe-type maybe-<and>)
                   (cond [(css:ident=:=? token 'only) (values next rest)]
                         [else (values token tokens)]))
                 (cond [(eof-object? maybe-type) (css-make-syntax-error exn:css:missing-identifier maybe-type)]
                       [(css:ident? maybe-type) (css-components->media-type+query maybe-type #true maybe-<and>)]
                       [else (css-make-syntax-error exn:css:unrecognized maybe-type)])]
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

  (define-css-parser-entry css-parse-relative-selectors :-> (U (Listof CSS-Complex-Selector) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-relative-selector
    ;;; https://drafts.csswg.org/selectors/#the-scope-pseudo
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [/dev/cssin]
      (css-make-syntax-error exn:css:unrecognized eof)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-stylesheet : (-> Input-Port (Listof CSS-Syntax-Rule))
    ;;; https://drafts.csswg.org/css-syntax/#parse-stylesheet
    ;;; https://drafts.csswg.org/css-syntax/#declaration-rule-list
    (lambda [css]
      (define rules : (Listof CSS-Syntax-Rule) (css-consume-rules css #true))
      (define rule : (Option CSS-Syntax-Rule) (and (pair? rules) (car rules)))
      (if (and (css-@rule? rule) (css:@keyword=:=? (css-@rule-name rule) '#:@charset))
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
      (define-values (prelude maybe-block) (css-consume-rule-item css #:@rule? #true))
      (make-css-@rule reconsumed-at-token prelude maybe-block)))

  (define css-consume-qualified-rule : (-> Input-Port CSS-Token (U CSS-Qualified-Rule CSS-@Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    (lambda [css reconsumed]
      (define head (css-consume-component-value css reconsumed))
      (define-values (prelude maybe-block) (css-consume-rule-item css #:@rule? #false))
      (cond [(css:block? maybe-block) (make-css-qualified-rule (cons head prelude) maybe-block)]
            [else (css-make-syntax-error exn:css:missing-block (cons head prelude))])))

  (define css-consume-component-value : (-> Input-Port CSS-Token CSS-Token)
    ;;; https://drafts.csswg.org/css-syntax/#component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    (lambda [css reconsumed]
      (cond [(css:delim=:=? reconsumed #\{) (css-consume-simple-block css reconsumed #\})]
            [(css:delim=:=? reconsumed #\[) (css-consume-simple-block css reconsumed #\])]
            [(css:delim=:=? reconsumed #\() (css-consume-simple-block css reconsumed #\))]
            [(css:function=:=? reconsumed 'url) (css-function->url (css-consume-function css reconsumed))]
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
        (cond [(or (eof-object? token) (and at-rule? (css:delim=:=? token #\;)))
               (when (eof-object? token) (css-make-syntax-error exn:css:missing-delimiter prelude))
               (values (reverse prelude) simple-block)]
              [(css:delim=:=? token #\{) (values (reverse prelude) (css-consume-simple-block css token #\}))]
              [(css:block=:=? token #\{) (values (reverse prelude) token)]
              [else (consume-item (cons (css-consume-component-value css token) prelude) simple-block)]))))

  (define css-consume-simple-block : (-> Input-Port CSS:Delim Char CSS:Block)
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    (lambda [css open close-char]
      (define-values (components close end-token) (css-consume-block-body css open close-char))
      (css-remake-token [open end-token] css:block (css:delim-datum open) components)))

  (define css-consume-function : (-> Input-Port CSS:Function CSS:Function)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    ;;; https://drafts.csswg.org/css-values/#functional-notations
    (lambda [css func]
      (define fname : Symbol (css:function-datum func))
      (cond [(false? (symbol-unreadable? fname)) func]
            [else (let-values ([(components close end-token) (css-consume-block-body css func #\))])
                    (css-remake-token [func end-token] css:function
                                      (string->symbol (symbol->string fname)) ; <==> (symbol-unreadable->symbol fname)
                                      (filter-not css:whitespace? components)))])))

  (define css-consume-block-body : (-> Input-Port CSS-Token Char (Values (Listof CSS-Token) CSS-Syntax-Terminal CSS-Token))
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css start-token close-char]
      (let consume-body ([components : (Listof CSS-Token) null])
        (define token (css-read-syntax css))
        (cond [(css:close=:=? token close-char) (values (reverse components) token token)]
              [(not (eof-object? token)) (consume-body (cons (css-consume-component-value css token) components))]
              [else (let ([end-token (if (null? components) start-token (car components))])
                      (css-make-syntax-error exn:css:missing-delimiter token)
                      (values (reverse components) token end-token))]))))
  
  (define css-consume-components : (->* (Input-Port) ((Option Char) Boolean) (Values (Listof CSS-Token) CSS-Syntax-Terminal))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    ;;; https://drafts.csswg.org/css-values/#comb-comma
    (lambda [css [terminal-char #false] [omit-terminal? #false]]
      (let consume-component ([stnenopmoc : (Listof CSS-Token) null])
        (define token (css-read-syntax css))
        (cond [(eof-object? token) (values (reverse stnenopmoc) token)]
              [(and terminal-char (css:delim=:=? token terminal-char))
               (define next (css-peek-syntax/skip-whitespace css))
               (cond [(and omit-terminal? (css-null? stnenopmoc))
                      (cond [(eof-object? next)
                             (css-make-syntax-error exn:css:overconsumption token)
                             (css-read-syntax/skip-whitespace css)
                             (values (reverse stnenopmoc) eof)]
                            [else (css-make-syntax-error exn:css:empty token)
                                  (css-consume-components css terminal-char omit-terminal?)])]
                     [(eof-object? next)
                      (css-read-syntax/skip-whitespace css)
                      (values (reverse stnenopmoc) next)]
                     [else (values (reverse stnenopmoc) token)])]
              [else (consume-component (cons (css-consume-component-value css token) stnenopmoc))]))))

  (define css-consume-componentses : (-> Input-Port [#:omit-comma? Boolean] (Listof (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
    ;;; https://drafts.csswg.org/css-values/#comb-comma
    (lambda [css #:omit-comma? [omit-comma? #true]]
      (let consume-components ([componentses : (Listof (Listof CSS-Token)) null])
        (define-values (components terminal) (css-consume-components css #\, omit-comma?))
        (cond [(not (eof-object? terminal)) (consume-components (cons components componentses))]
              [(not omit-comma?) (reverse (cons components componentses))]
              [else (filter css-tokens? (reverse (cons components componentses)))]))))

  (define css-components->declaration : (-> CSS:Ident (Listof CSS-Token) (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
    ;;; https://drafts.csswg.org/css-syntax/#typedef-declaration-value
    ;;; https://drafts.csswg.org/css-cascade/#importance
    ;;; https://drafts.csswg.org/css-variables/#defining-variables
    ;;; https://drafts.csswg.org/css-values/#component-whitespace
    (lambda [id-token components]
      (define-values (maybe-: value-list) (css-car components))
      (cond [(not (css:delim=:=? maybe-: #\:)) (css-make-syntax-error exn:css:missing-colon id-token)]
            [else (let verify ([lla : (Listof CSS-Token) null]
                               [info : (U False CSS-Syntax-Error CSS:Delim) #false]
                               [rest : (Listof CSS-Token) value-list])
                    (define-values (1st tail) (css-car rest))
                    (cond [(not (eof-object? 1st))
                           (cond [(and (css:delim? info) (css:ident=:=? 1st 'important)) (verify lla info tail)]
                                 [(css:delim? info) (verify lla (css-make-syntax-error exn:css:unrecognized info) null)]
                                 [(css:delim=:=? 1st #\!) (verify lla 1st tail)]
                                 [(or (css:bad? 1st) (css:close? 1st)) (verify lla (css-make-syntax-error exn:css:malformed 1st) null)]
                                 [else (verify (cons 1st lla) #false tail)])]
                          [(exn:css? info) info]
                          [else (let ([all-argl (reverse lla)]
                                      [custom? (css:ident=:=? id-token --symbol?)])
                                  (cond [(null? all-argl) (css-make-syntax-error exn:css:missing-value id-token)]
                                        [else (make-css-declaration id-token all-argl (css:delim? info) custom?)]))]))])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->media-type+query : (-> CSS:Ident Boolean (Listof CSS-Token) CSS-Media-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#media-types
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-query
    (lambda [media only? conditions]
      (define downcased-type : Symbol (css:ident=> media symbol-downcase))
      (define-values (maybe-and maybe-conditions) (css-car conditions))
      (when (css-deprecate-media-type) (css-make-syntax-error exn:css:deprecated media))
      (cond [(memq downcased-type '(only not and or)) (css-make-syntax-error exn:css:misplaced media)]
            [(eof-object? maybe-and) (make-css-media-type downcased-type only?)]
            [(not (css:ident=:=? maybe-and 'and)) (css-make-syntax-error exn:css:unrecognized maybe-and)]
            [(css-null? maybe-conditions) (css-make-syntax-error exn:css:missing-feature maybe-and)]
            [else (cons (make-css-media-type downcased-type only?)
                        (css-components->junction maybe-conditions 'and #false #true))])))
  
  (define css-components->feature-query : (-> (Listof CSS-Token) Boolean CSS-Syntax-Any CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-only
    ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
    (lambda [conditions media? alt]
      (define-values (token rest) (css-car conditions))
      (define-values (operator chain) (css-car rest))
      (cond [(eof-object? token) (css-throw-syntax-error exn:css:missing-feature alt)]
            [(css:ident=:=? token 'not) (css-components->negation token rest media?)]
            [(eof-object? operator) (css-component->feature-query media? token)]
            [(or (css:ident=:=? operator 'and) (css:ident=:=? operator 'or))
             (css-components->junction chain (css:ident-datum operator) token media?)]
            [else (css-throw-syntax-error exn:css:unrecognized operator)])))

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
                   [(css:ident=:=? name 'not) (css-components->negation name any-values media?)]
                   [(and (css:ident? name) (css:delim=:=? op #\:))
                    (define descriptor (css-components->declaration name any-values))
                    (cond [(exn? descriptor) (if media? (css-throw-syntax-error exn:css:enclosed condition) (raise descriptor))]
                          [(and media?) (css-declaration->media-query descriptor condition)]
                          [else descriptor])]
                   [(and media?)
                    (cond [(and (css:ident? name) (eof-object? op)) (css-make-media-feature name #false #\? #false)]
                          [else (css-components->media-range-query subany condition)])]
                   [(eof-object? name) (css-throw-syntax-error exn:css:empty condition)]
                   [(css:ident? name) (css-throw-syntax-error exn:css:missing-colon condition)]
                   [(css:function? condition) (css-throw-syntax-error exn:css:enclosed condition)]
                   [else (css-throw-syntax-error exn:css:missing-identifier condition)])]
            [else (css-throw-syntax-error exn:css:missing-feature condition)])))

  (define css-components->negation : (-> CSS:Ident (Listof CSS-Token) Boolean CSS-Not)
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-not
    (lambda [<not> tokens media?]
      (define-values (token rest) (css-car tokens))
      (cond [(eof-object? token) (css-throw-syntax-error exn:css:missing-feature <not>)]
            [(css:ident=:=? token 'not) (css-throw-syntax-error exn:css:misplaced token)]
            [(css-null? rest) (make-css-not (css-component->feature-query media? token))]
            [else (css-throw-syntax-error exn:css:overconsumption rest)])))

  (define css-components->junction : (-> (Listof CSS-Token) Symbol (Option CSS-Token) Boolean (U CSS-And CSS-Or))
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-and
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-or
    (lambda [conditions op maybe-head media?]
      (define make-junction (if (eq? op 'and) make-css-and make-css-or))
      (let components->junction ([junctions : (Listof CSS-Token) (if (false? maybe-head) null (list maybe-head))]
                                 [rest-conditions : (Listof CSS-Token) conditions])
        (define-values (condition rest) (css-car rest-conditions))
        (define-values (token others) (css-car rest))
        (cond [(eof-object? condition) (make-junction (map (curry css-component->feature-query media?) (reverse junctions)))]
              [(css:ident=:=? condition 'not) (css-throw-syntax-error exn:css:misplaced condition)]
              [(or (eof-object? token) (css:ident=:=? token op)) (components->junction (cons condition junctions) others)]
              [(or (css:ident=:=? token 'and) (css:ident=:=? token 'or)) (css-throw-syntax-error exn:css:misplaced token)]
              [else (css-throw-syntax-error exn:css:overconsumption token)]))))

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
                    (cond [(symbol? datum) (and (symbol? metadata) (symbol-ci=? datum metadata))]
                          [(real? metadata) (case (css-media-feature-operator query)
                                              [(#\>) (> metadata datum)] [(#\≥) (>= metadata datum)]
                                              [(#\<) (< metadata datum)] [(#\≤) (<= metadata datum)]
                                              [else (= metadata datum)])]
                          [else #false])]
                   [(symbol? query)
                    (define metadata : CSS-Media-Datum (hash-ref support? query (λ _ 'none)))
                    (not (if (symbol? metadata) (symbol-ci=? metadata 'none) (zero? metadata)))]
                   [(css-media-type? query)
                    (define result (memq (css-media-type-name query) (list (current-css-media-type) 'all)))
                    (if (css-media-type-only? query) (and result #true) (not result))]
                   [else (and (pair? query)
                              (css-query-support? (car query) support?)
                              (css-query-support? (cdr query) support?))])]
            [else (and (procedure? support?)
                       (css-declaration? query)
                       (support? (css:ident-datum (css-declaration-name query))
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
      (cond [(eof-object? value0) (css-throw-syntax-error exn:css:empty broken-condition)]
            [(eof-object? d0) (css-throw-syntax-error exn:css:missing-delimiter components)]
            [(eof-object? value1) (css-throw-syntax-error exn:css:missing-value rest0)]
            [(and (css:ident? value0) (css:delim? d1)) (css-throw-syntax-error exn:css:enclosed broken-condition)]
            [(and (eq? op0 #\=) (css:delim? d1)) (css-throw-syntax-error exn:css:overconsumption broken-condition)]
            [(css:ident? value0) (css-make-media-feature value0 value1 op0 d0)]
            [(and (eof-object? d1) (css:ident? value1)) (css-make-media-feature value1 value0 po0 d0)]
            [(not (css:ident? value1)) (css-throw-syntax-error exn:css:missing-identifier value1)]
            [(or (eof-object? value2) (css:ident? value2)) (css-throw-syntax-error exn:css:missing-value rest2)]
            [(css-tokens? terminal) (css-throw-syntax-error exn:css:overconsumption terminal)]
            [(not (css:delim=? d0 d1)) (css-throw-syntax-error exn:css:malformed (list d0 value1 d1))]
            [else (make-css-and (list (css-make-media-feature value1 value0 po0 d0)
                                      (css-make-media-feature value1 value2 op1 d1)))])))
  
  (define css-declaration->media-query : (-> CSS-Declaration CSS:Block CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    (lambda [property broken-condition]
      (define-values (media-value rest) (css-car-media-value (css-declaration-values property)))
      (cond [(eof-object? media-value) (css-throw-syntax-error exn:css:enclosed broken-condition)]
            [(css-tokens? rest) (css-throw-syntax-error exn:css:enclosed broken-condition)]
            [else (css-make-media-feature (css-declaration-name property) media-value #\: #false)])))

  (define css-car-comparison-operator : (-> (Listof CSS-Token) (Values (U CSS:Delim EOF) Char Char (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
    (lambda [components]
      (define-values (d rest) (css-car components))
      (define-values (maybe-= terminal) (css-car rest #false))
      (cond [(css:delim=:=? d #\=) (values d #\= #\= rest)]
            [(css:delim=:=? d #\>) (if (css:delim=:=? maybe-= #\=) (values d #\≥ #\≤ terminal) (values d #\> #\< rest))]
            [(css:delim=:=? d #\<) (if (css:delim=:=? maybe-= #\=) (values d #\≤ #\≥ terminal) (values d #\< #\> rest))]
            [(eof-object? d) (values eof #\≠ #\≠ rest)]
            [else (css-throw-syntax-error exn:css:unrecognized d)])))
  
  (define css-car-media-value : (-> (Listof CSS-Token) (Values (U CSS-Media-Value EOF) (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-mf-value
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-ratio
    (lambda [components]
      (define-values (value rest) (css-car components))
      (define-values (maybe-/ maybe-rest) (css-car rest))
      (define-values (maybe-int terminal) (css-car maybe-rest))
      (cond [(css:delim=:=? maybe-/ #\/)
             (values (if (and (css:integer=:=? value positive?) (css:integer=:=? maybe-int positive?))
                         (let ([width : Positive-Integer (max (css:integer-datum value) 1)]
                               [height : Positive-Integer (max (css:integer-datum maybe-int) 1)])
                           (css-remake-token [value maybe-int] css:ratio (format "~a/~a" width height) (/ width height)))
                         (css-throw-syntax-error exn:css:malformed (filter css-token? (list value maybe-/ maybe-int))))
                     terminal)]
            [(or (css:ident? value) (css:number? value) (css:dimension? value)) (values value rest)]
            [(eof-object? value) (values eof rest)]
            [else (values (css-throw-syntax-error exn:css:unrecognized value) rest)])))

  (define css-make-media-feature : (-> CSS:Ident (Option CSS-Media-Value) Char (Option CSS:Delim) (U Symbol CSS-Media-Feature))
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    ;;; https://drafts.csswg.org/mediaqueries/#mq-min-max
    (lambda [desc-name maybe-value ophint maybe-op]
      (define errobj (filter css-token? (list desc-name maybe-op maybe-value)))
      (define name (css:ident=> desc-name (compose1 string-downcase symbol->string)))
      (define-values (downcased-name op min/max?)
        (cond [(string-prefix? name "min-") (values (string->symbol (substring name 4)) #\≥ #true)]
              [(string-prefix? name "max-") (values (string->symbol (substring name 4)) #\≤ #true)]
              [else (values (string->symbol name) ophint #false)]))
      (when (and min/max?)
        (cond [(or (not maybe-value) (css:delim? maybe-op)) (css-throw-syntax-error exn:css:misplaced errobj)]
              [(not (css:numeric? maybe-value)) (css-throw-syntax-error exn:css:unrecognized errobj)]))
      (define-values (v deprecated?) ((current-css-media-feature-filter) downcased-name maybe-value min/max?))
      (unless (not deprecated?) (css-make-syntax-error exn:css:deprecated desc-name))
      (cond [(void? v) downcased-name]
            [(procedure? v) (css-throw-syntax-error v errobj)]
            [else (make-css-media-feature downcased-name v op)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->selectors : (-> (Listof CSS-Token) CSS-NameSpace-Hint (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-selector
    ;;; https://drafts.csswg.org/selectors/#selector-list
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [components namespaces]
      (with-handlers ([exn:css? (λ [[errcss : exn:css]] errcss)])
        (define-values (head-complex-selector maybe-eof maybe-rest) (css-car-complex-selector components namespaces))
        (let extract-complex-selector ([srotceles : (Listof CSS-Complex-Selector) null]
                                       [terminal : (U EOF CSS:Delim) maybe-eof]
                                       [rest : (Listof CSS-Token) maybe-rest])
          (if (css-null? rest)
              (cond [(eof-object? terminal) (cons head-complex-selector (reverse srotceles))]
                    [else (css-throw-syntax-error exn:css:overconsumption terminal)])
              (let-values ([(complex-selector maybe-terminal maybe-rest) (css-car-complex-selector rest namespaces)])
                (extract-complex-selector (cons complex-selector srotceles) maybe-terminal maybe-rest)))))))
  
  (define css-car-complex-selector : (-> (Listof CSS-Token) CSS-NameSpace-Hint
                                         (Values CSS-Complex-Selector (U EOF CSS:Delim) (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#combinators
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [components namespaces]
      (define-values (head-compound-selector rest) (css-car-compound-selector components #false namespaces))
      (let extract-relative-selector ([srotceles : (Listof CSS-Compound-Selector) null]
                                      [tokens : (Listof CSS-Token) rest])
        (define-values (maybe-terminal rest) (css-car tokens))
        (define-values (token maybe-selectors) (css-car tokens #false))
        (cond [(or (eof-object? maybe-terminal) (css:delim=:=? maybe-terminal #\,))
               (values (css-make-complex-selector (cons head-compound-selector (reverse srotceles))) maybe-terminal rest)]
              [(not (css-selector-combinator? token)) (css-throw-syntax-error exn:css:unrecognized maybe-terminal)]
              [else (let*-values ([(combinator maybe-selectors) (css-car-combinator token maybe-selectors)]
                                  [(maybe-selector maybe-rest) (css-car maybe-selectors)])
                      (if (or (eof-object? maybe-selector) (css:delim=:=? maybe-selector #\,))
                          (css-throw-syntax-error exn:css:overconsumption maybe-selectors)
                          (let-values ([(selector rest) (css-car-compound-selector maybe-selectors combinator namespaces)])
                            (extract-relative-selector (cons selector srotceles) rest))))]))))

  (define css-car-compound-selector : (-> (Listof CSS-Token) (Option CSS-Combinator) CSS-NameSpace-Hint
                                          (Values CSS-Compound-Selector (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    ;;; https://github.com/w3c/csswg-drafts/issues/202
    (lambda [components combinator namespaces]
      (define-values (head heads) (css-car components))
      (define-values (typename namespace simple-selector-components)
        (cond [(css:ident? head) (css-car-elemental-selector head heads namespaces)]
              [(or (css:delim=:=? head #\|) (css:delim=:=? head #\*)) (css-car-elemental-selector head heads namespaces)]
              [(or (eof-object? head) (css:delim=:=? head #\,)) (css-throw-syntax-error exn:css:empty head)]
              [else (values #true (or (css-declared-namespace namespaces '||) #true) (cons head heads))]))
      (define-values (pseudo-classes selector-components) (css-car-pseudo-class-selectors simple-selector-components))
      (let extract-simple-selector ([sessalc : (Listof Symbol) null]
                                    [sdi : (Listof Keyword) null]
                                    [setubirtta : (Listof CSS-Attribute-Selector) null]
                                    [pseudo-element : (Option CSS-Pseudo-Element-Selector) #false]
                                    [selector-tokens : (Listof CSS-Token) selector-components])
        (define-values (token tokens) (css-car selector-tokens #false))
        (cond [(or (eof-object? token) (css:delim=:=? token #\,) (css-selector-combinator? token))
               (values (make-css-compound-selector combinator typename namespace pseudo-classes
                                                   (reverse sessalc) (reverse sdi) (reverse setubirtta) pseudo-element)
                       selector-tokens)]
              [(and pseudo-element) (css-throw-syntax-error exn:css:overconsumption token)]
              [(css:delim=:=? token #\.)
               (define-values (next rest) (css-car tokens #false))
               (cond [(not (css:ident? next)) (css-throw-syntax-error exn:css:missing-identifier next)]
                     [else (extract-simple-selector (cons (css:ident-datum next) sessalc) sdi setubirtta pseudo-element rest)])]
              [(css:delim=:=? token #\:)
               (define-values (maybe-pseudo-classes maybe-rest) (css-car-pseudo-class-selectors tokens))
               (define-values (next rest) (css-car maybe-rest #false))
               (when (null? maybe-pseudo-classes)
                 (css-throw-syntax-error exn:css:misplaced (list token (car tokens))))
               (define pelement : CSS-Pseudo-Element-Selector
                 (let ([pclass (car maybe-pseudo-classes)])
                   (make-css-pseudo-element-selector (css-pseudo-class-selector-name pclass)
                                                     (css-pseudo-class-selector-arguments pclass)
                                                     (cdr maybe-pseudo-classes))))
               (extract-simple-selector sessalc sdi setubirtta pelement maybe-rest)]
              [(css:block=:=? token #\[)
               (define attribute-selector : CSS-Attribute-Selector (css-simple-block->attribute-selector token namespaces))
               (extract-simple-selector sessalc sdi (cons attribute-selector setubirtta) pseudo-element tokens)]
              [(css:hash? token) (extract-simple-selector sessalc (cons (css:hash-datum token) sdi) setubirtta pseudo-element tokens)]
              [else (css-throw-syntax-error exn:css:unrecognized token)]))))

  (define css-car-combinator : (-> (U CSS:WhiteSpace CSS:Delim CSS:||) (Listof CSS-Token) (Values CSS-Combinator (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [token tokens]
      (cond [(css:whitespace? token)
             (define-values (next tail) (css-car tokens))
             (cond [(css-selector-combinator? next) (css-car-combinator next tail)]
                   [else (values '>> tokens)])]
            [(css:delim=:=? token #\>)
             (define-values (next tail) (css-car tokens #false))
             (define-values (next2 tail2) (css-car tail))
             (cond [(css:delim=:=? next #\>) (values '>> tail2)]
                   [else (values '> tail)])]
            [(css:delim=:=? token #\+) (values '+ tokens)]
            [(css:delim=:=? token #\~) (values '~ tokens)]
            [(css:||? token) (values '|| tokens)]
            [else (css-throw-syntax-error exn:css:unrecognized token)])))
  
  (define css-car-elemental-selector : (-> (U CSS:Ident CSS:Delim) (Listof CSS-Token) CSS-NameSpace-Hint
                                           (Values (U Symbol True) (U Symbol Boolean) (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#elemental-selectors
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    (lambda [token tokens namespaces]
      (define-values (next rest) (css-car tokens #false))
      (define-values (next2 rest2) (css-car rest #false))
      (cond [(css:delim=:=? token #\|)
             (cond [(css:ident? next) (values (css:ident-datum next) #false rest)]
                   [(css:delim=:=? next #\*) (values #true #false rest)]
                   [else (css-throw-syntax-error exn:css:missing-identifier next)])]
            [(css:delim=:=? next #\|)
             (define ns : (U Symbol Boolean) (css-declared-namespace namespaces token))
             (cond [(false? ns) (css-throw-syntax-error exn:css:namespace token)]
                   [(css:ident? next2) (values (css:ident-datum next2) ns rest2)]
                   [(css:delim=:=? next2 #\*) (values #true ns rest2)]
                   [else (css-throw-syntax-error exn:css:missing-identifier (list token next))])]
            [else (let ([ns (or (css-declared-namespace namespaces '||) #true)])
                    (cond [(css:delim? token) (values #true ns tokens)]
                          [else (values (css:ident-datum token) ns tokens)]))])))

  (define css-car-pseudo-class-selectors : (-> (Listof CSS-Token) (Values (Listof CSS-Pseudo-Class-Selector) (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#elemental-selectors
    ;;; https://drafts.csswg.org/selectors/#pseudo-classes
    (lambda [components]
      (let extract-pseudo-class-selector ([srotceles : (Listof CSS-Pseudo-Class-Selector) null]
                                          [tokens : (Listof CSS-Token) components])
        (define-values (maybe: rest) (css-car tokens #false))
        (define-values (maybe-id rest2) (css-car rest #false))
        (cond [(or (not (css:delim=:=? maybe: #\:)) (css:delim=:=? maybe-id #\:)) (values (reverse srotceles) tokens)]
              [(css:ident? maybe-id)
               (let ([selector (make-css-pseudo-class-selector (css:ident-datum maybe-id) #false)])
                 (extract-pseudo-class-selector (cons selector srotceles) rest2))]
              [(css:function? maybe-id)
               (let ([selector (make-css-pseudo-class-selector (css:function-datum maybe-id) (css:function-arguments maybe-id))])
                 (extract-pseudo-class-selector (cons selector srotceles) rest2))]
              [else (css-throw-syntax-error exn:css:missing-identifier maybe:)]))))
  
  (define css-simple-block->attribute-selector : (-> CSS:Block CSS-NameSpace-Hint CSS-Attribute-Selector)
    ;;; https://drafts.csswg.org/selectors/#attribute-selectors
    ;;; https://drafts.csswg.org/selectors/#attrnmsp
    ;;; https://drafts.csswg.org/selectors/#attribute-case
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    (lambda [block namespaces]
      (define-values (1st rest1) (css-car (css:block-components block)))
      (define-values (2nd rest2) (css-car rest1 #false))
      (define-values (3rd rest3) (css-car rest2 #false))
      (define-values (attr namespace op-part)
        (cond [(eof-object? 1st) (css-throw-syntax-error exn:css:empty block)]
              [(or (css:match? 1st) (css:delim=:=? 1st #\=))
               (css-throw-syntax-error exn:css:missing-identifier block)]
              [(or (eof-object? 2nd) (css:match? 2nd) (css:delim=:=? 2nd #\=) (css:whitespace? 2nd))
               ; WARNING: the namespace behavior for attributes is different from that for elements 
               (cond [(css:ident? 1st) (values (css:ident-datum 1st) #false rest1)]
                     [else (css-throw-syntax-error exn:css:missing-identifier 1st)])]
              [(or (eof-object? 3rd) (css:match? 3rd) (css:delim=:=? 3rd #\=) (css:whitespace? 3rd))
               (cond [(and (css:delim=:=? 1st #\|) (css:ident? 2nd)) (values (css:ident-datum 2nd) #false rest2)]
                     [(css:delim=:=? 2nd #\|) (css-throw-syntax-error exn:css:missing-identifier 2nd)]
                     [else (css-throw-syntax-error exn:css:unrecognized 1st)])]
              [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:delim=:=? 2nd #\|) (css:ident? 3rd))
               (define ns (css-declared-namespace namespaces 1st))
               (cond [(false? ns) (css-throw-syntax-error exn:css:namespace 1st)]
                     [else (values (css:ident-datum 3rd) ns rest3)])]
              [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:delim=:=? 2nd #\|))
               (css-throw-syntax-error exn:css:missing-identifier 3rd)]
              [(or (css:ident? 1st) (css:delim=:=? 1st #\*))
               (css-throw-syntax-error exn:css:unrecognized 2nd)]
              [else (css-throw-syntax-error exn:css:unrecognized 1st)]))
      (define-values (op value-part) (css-car op-part #false))
      (define-values (value ci-part) (css-car value-part #false))
      (define-values (i terminal) (css-car ci-part))
      (unless (eof-object? op)
        (cond [(eof-object? value) (css-throw-syntax-error exn:css:missing-value op)]
              [(nor (eof-object? i) (css:ident=:=? i 'i)) (css-throw-syntax-error exn:css:overconsumption i)]
              [(css-tokens? terminal) (css-throw-syntax-error exn:css:overconsumption terminal)]))
      (define val : (U String Symbol)
        (cond [(css:string? value) (css:string-datum value)]
              [(css:ident? value) (css:ident-datum value)]
              [(or (css:whitespace? value) (eof-object? value)) ""]
              [else (css-throw-syntax-error exn:css:type value)]))
      (cond [(or (css:whitespace? op) (eof-object? op)) (make-css-attribute-selector attr namespace)]
            [(css:delim=:=? op #\=) (make-css-attribute~selector attr namespace #\= val (css:ident? i))]
            [(css:match? op) (make-css-attribute~selector attr namespace (css:match-datum op) val (css:ident? i))]
            [else (css-throw-syntax-error exn:css:unrecognized op)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-open-input-port : (-> CSS-StdIn Input-Port)
    ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
    (lambda [/dev/stdin]
      (if (list? /dev/stdin)
          (let ([total : Index (length /dev/stdin)]
                [cursor : Integer 0])
            (make-input-port (if (pair? /dev/stdin) (css-token-source (car /dev/stdin)) '/dev/cssin/null)
                             (λ [[buf : Bytes]]
                               (λ _ (cond [(>= cursor total) eof]
                                          [(set! cursor (add1 cursor))
                                           => (λ _ (list-ref /dev/stdin (sub1 cursor)))])))
                             (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                               (λ _ (cond [(>= (+ skip cursor) total) eof]
                                          [else (list-ref /dev/stdin (+ skip cursor))])))
                             void))
          (let* ([/dev/rawin (cond [(port? /dev/stdin) /dev/stdin]
                                   [(path? /dev/stdin) (open-input-file /dev/stdin)]
                                   [(regexp-match? #px"\\.css$" /dev/stdin) (open-input-file (~a /dev/stdin))]
                                   [(string? /dev/stdin) (open-input-string /dev/stdin '/dev/cssin/string)]
                                   [(bytes? /dev/stdin) (open-input-bytes /dev/stdin '/dev/cssin/bytes)]
                                   [else (open-input-string (~s /dev/stdin) '/dev/cssin/error)])]
                 [/dev/cssin : Input-Port (css-fallback-encode-input-port /dev/rawin)]
                 [peek-pool : (Listof Any) null])
            (define portname : (U String Symbol)
              (let ([src (object-name /dev/cssin)])
                (cond [(path? src) (path->string (simple-form-path src))]
                      [else (string->symbol (~a src))])))
            (make-input-port portname
                             (λ [[buf : Bytes]]
                               (λ _ (cond [(null? peek-pool) (css-consume-token /dev/cssin portname)]
                                          [else (let-values ([(rest peeked) (split-at-right peek-pool 1)])
                                                  (set! peek-pool rest)
                                                  (car peeked))])))
                             (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                               (λ _ (and (for ([idx (in-range (length peek-pool) (add1 skip))])
                                           (set! peek-pool (cons (css-consume-token /dev/cssin portname) peek-pool)))
                                         (list-ref peek-pool (- (length peek-pool) skip 1)))))
                             (thunk (unless (eq? /dev/rawin /dev/cssin) (close-input-port /dev/cssin))
                                    (unless (eq? /dev/rawin /dev/stdin) (close-input-port /dev/rawin)))
                             #false #false
                             (thunk (port-next-location /dev/cssin))
                             (thunk (port-count-lines! /dev/cssin)))))))
  
  (define css-read-syntax : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (define stx (read-char-or-special css))
      (cond [(or (eof-object? stx) (css-token? stx)) stx]
            [else (css-make-bad-token (css-srcloc css '/dev/cssin/error #false #false #false)
                                      css:bad:stdin struct:css-token (~s stx))])))

  (define css-peek-syntax : (->* (Input-Port) (Natural) CSS-Syntax-Any)
    (lambda [css [skip 0]]
      (define stx (peek-char-or-special css skip))
      (cond [(or (eof-object? stx) (css-token? stx)) stx]
            [else (css-make-bad-token (css-srcloc css '/dev/cssin/error #false #false #false)
                                      css:bad:stdin struct:css-token (~s stx))])))
  
  (define css-read-syntax/skip-whitespace : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (define token (css-read-syntax css))
      (cond [(not (css:whitespace? token)) token]
            [else (css-read-syntax/skip-whitespace css)])))

  (define css-peek-syntax/skip-whitespace : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (let peek/skip-whitespace : CSS-Syntax-Any ([skip : Natural 0])
        (define token (css-peek-syntax css skip))
        (cond [(not (css:whitespace? token)) token]
              [else (peek/skip-whitespace (add1 skip))]))))

  (define css-selector-combinator? : (-> CSS-Syntax-Any Boolean : #:+ (U CSS:WhiteSpace CSS:Delim CSS:||))
    (lambda [token]
      (or (css:whitespace? token)
          (and (css:delim? token) (memq (css:delim-datum token) '(#\~ #\+ #\>)) #true)
          (css:||? token))))

  (define css-function->url : (-> CSS:Function CSS:URL)
    (lambda [url]
      (define-values (href modifiers) (css-car (css:function-arguments url)))
      (if (not (css:string? href))
          (css-remake-token url css:url 'about:invalid null)
          (let ([datum (if (css:string=:=? href "") 'about:invalid (css:string-datum href))])
            (let filter-modifiers : CSS:URL ([sreifidom : (Listof CSS-URL-Modifier) null]
                                             [tail : (Listof CSS-Token) modifiers])
              (define-values (head rest) (css-car tail))
              (cond [(eof-object? head) (css-remake-token url css:url datum (reverse sreifidom))]
                    [(or (css:ident? head) (css:function? head) (css:url? head)) (filter-modifiers (cons head sreifidom) rest)]
                    [else (css-make-syntax-error exn:css:unrecognized head) (filter-modifiers sreifidom rest)]))))))

  (define css-media-queries-support? : (-> (Listof CSS-Media-Query) CSS-Media-Preferences Boolean)
    (lambda [queries preferences]
      (or (null? queries)
          (for/or ([query (in-list queries)])
                  (css-query-support? query preferences))))))

(module grammar typed/racket ;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet
  (provide (all-defined-out))
  
  (require (submod ".." digitama))
  (require (submod ".." tokenizer))
  (require (submod ".." parser))

  (require (for-syntax syntax/parse))

  ;; https://drafts.csswg.org/css-syntax/#css-stylesheets
  (define-type CSS-Media-Rule (Pairof (Listof CSS-Grammar-Rule) CSS-Media-Preferences))
  (define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule CSS-Media-Rule))
  (define-type CSS-StyleSheet-Pool (HashTable Natural CSS-StyleSheet))

  (struct: css-style-rule : CSS-Style-Rule ([selectors : (Listof+ CSS-Complex-Selector)] [descriptors : CSS-Descriptors]))
  
  (struct: css-stylesheet : CSS-StyleSheet
    ([pool : CSS-StyleSheet-Pool]
     [location : (U String Symbol)]
     [timestamp : Integer]
     [preferences : CSS-Media-Preferences]
     [imports : (Listof Positive-Integer)]
     [namespaces : CSS-NameSpace]
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
      (define init-viewport : CSS-Media-Preferences (current-css-media-preferences))
      (or (and (string? location) (hash-has-key? pool identity)
               (let ([stylesheet (hash-ref pool identity)])
                 (and (not (css-stylesheet-outdated? stylesheet))
                      (css-update-imported-stylesheets stylesheet)
                      stylesheet)))
          (let ([rules (css-consume-stylesheet /dev/cssin)])
            (when (positive? identity) (hash-set! pool identity css-stylesheet-placeholder))
            (define namespaces : CSS-NameSpace (make-hasheq))
            (define-values (viewport imports grammars)
              (css-syntax-rules->grammar-rules location rules namespaces #true #true init-viewport pool))
            (define timestamp : Integer (if (string? location) (file-or-directory-modify-seconds location) (current-seconds)))
            (define stylesheet : CSS-StyleSheet (make-css-stylesheet pool location timestamp viewport imports namespaces grammars))
            (when (positive? identity) (hash-set! pool identity stylesheet))
            stylesheet))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-syntax-rules->grammar-rules : (->* ((U String Symbol) (Listof CSS-Syntax-Rule) CSS-NameSpace Boolean Boolean)
                                                 (CSS-Media-Preferences CSS-StyleSheet-Pool)
                                                 (Values CSS-Media-Preferences (Listof Positive-Integer) (Listof CSS-Grammar-Rule)))
    (lambda [src syntaxes0 namespaces can-import0? allow-namespace0?
                 [init-viewport (current-css-media-preferences)]
                 [pool ((inst make-hasheq Natural CSS-StyleSheet))]]
      (define-values (viewport-srotpircsed !viewport-sexatnys)
        (for/fold ([viewport-srotpircsed : (Listof CSS-Descriptors) null]
                   [normal-sexatnys : (Listof CSS-Syntax-Rule) null])
                  ([stx : CSS-Syntax-Rule (in-list syntaxes0)])
          (define maybe-descriptor : (U CSS-Descriptors CSS-Syntax-Error Void)
            (when (and (css-@rule? stx) (css:@keyword=:=? (css-@rule-name stx) '#:@viewport))
              (define prelude : (Listof CSS-Token) (css-@rule-prelude stx))
              (define maybe-block : (Option CSS:Block) (css-@rule-block stx))
              (cond [(css-tokens? prelude) (css-make-syntax-error exn:css:overconsumption prelude)]
                    [maybe-block (css-components->descriptors (css:block-components maybe-block))]
                    [else (css-make-syntax-error exn:css:missing-block (css-@rule-name stx))])))
          (cond [(void? maybe-descriptor) (values viewport-srotpircsed (cons stx normal-sexatnys))]
                [(exn? maybe-descriptor) (values viewport-srotpircsed normal-sexatnys)]
                [else (values (cons maybe-descriptor viewport-srotpircsed) normal-sexatnys)])))
      (define viewport : CSS-Media-Preferences
        (cond [(null? viewport-srotpircsed) init-viewport]
              [else (css-cascade-viewport init-viewport
                                          (reverse viewport-srotpircsed)
                                          (current-css-viewport-descriptor-filter)
                                          (current-css-viewport-filter))]))
      (let syntax->grammar : (Values CSS-Media-Preferences (Listof Positive-Integer) (Listof CSS-Grammar-Rule))
        ([seititnedi : (Listof Positive-Integer) null]
         [srammarg : (Listof CSS-Grammar-Rule) null]
         [!viewport-syntaxes : (Listof CSS-Syntax-Rule) (reverse !viewport-sexatnys)]
         [can-import? : Boolean can-import0?]
         [allow-namespace? : Boolean allow-namespace0?])
        (if (null? !viewport-syntaxes)
            (values viewport (reverse seititnedi) (reverse srammarg))
            (let-values ([(stx rest) (values (car !viewport-syntaxes) (cdr !viewport-syntaxes))])
              (cond [(css-qualified-rule? stx)
                     (define maybe-rule : (U CSS-Style-Rule CSS-Syntax-Error) (css-qualified-rule->style-rule stx namespaces))
                     (syntax->grammar seititnedi (cond [(or (exn? maybe-rule) (null? (css-style-rule-descriptors maybe-rule))) srammarg]
                                                       [else (cons maybe-rule srammarg)])
                                      rest #false #false)]
                    [(css:@keyword=:=? (css-@rule-name stx) '#:@charset)
                     (css-make-syntax-error exn:css:misplaced (css-@rule-name stx))
                     (syntax->grammar seititnedi srammarg rest can-import? allow-namespace?)]
                    [(css:@keyword=:=? (css-@rule-name stx) '#:@import)
                     (cond [(false? can-import?)
                            (css-make-syntax-error exn:css:misplaced (css-@rule-name stx))
                            (syntax->grammar seititnedi srammarg rest #false #true)]
                           [else (let ([maybe-id (css-@import->stylesheet-identity stx src viewport pool)])
                                   (cond [(not (integer? maybe-id)) (syntax->grammar seititnedi srammarg rest #true #true)]
                                         [else (syntax->grammar (cons maybe-id seititnedi) srammarg rest #true #true)]))])]
                    [(css:@keyword=:=? (css-@rule-name stx) '#:@namespace)
                     (cond [(false? allow-namespace?)
                            (css-make-syntax-error exn:css:misplaced (css-@rule-name stx))
                            (syntax->grammar seititnedi srammarg rest #false #false)]
                           [else (let ([ns (css-@namespace->namespace stx)])
                                   (when (pair? ns) (hash-set! namespaces (car ns) (cdr ns)))
                                   (syntax->grammar seititnedi srammarg rest #false #true))])]
                    [(css:@keyword=:=? (css-@rule-name stx) '#:@media)
                     (define media-rules (css-@media->media-rule stx viewport namespaces pool))
                     (syntax->grammar seititnedi
                                      (cond [(list? media-rules) (append (reverse media-rules) srammarg)]
                                            [(pair? media-rules) (cons media-rules srammarg)]
                                            [else srammarg])
                                      rest #false #false)]
                    [(css:@keyword=:=? (css-@rule-name stx) '#:@support)
                     (define expand-stxes (css-@support->syntax-rules stx))
                     (syntax->grammar seititnedi srammarg (if (pair? expand-stxes) (append expand-stxes rest) rest) #false #false)]
                    [else (syntax->grammar seititnedi (cons stx srammarg) rest #false #false)]))))))
  
  (define css-@import->stylesheet-identity : (-> CSS-@Rule Any CSS-Media-Preferences CSS-StyleSheet-Pool
                                                 (U Positive-Integer False CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-cascade/#at-import
    (lambda [import parent-href preferences pool]
      (define-values (uri maybe-condition) (css-car (css-@rule-prelude import)))
      (define name : CSS:@Keyword (css-@rule-name import))
      (define maybe-block : (Option CSS:Block) (css-@rule-block import))
      (define maybe-target.css : (U Path CSS-Syntax-Error)
        (cond [(eof-object? uri) (css-make-syntax-error exn:css:empty (css-@rule-name import))]
              [(or (css:string=:=? uri "") (css:url=:=? uri symbol?)) (css-make-syntax-error exn:css:empty uri)]
              [(css:string? uri) (css:string=> uri (curry css-url-string->path parent-href))]
              [(css:url? uri) (css-url-string->path parent-href ((inst css:url=> String) uri ~a))]
              [else (css-make-syntax-error exn:css:unrecognized uri)]))
      (cond [(exn? maybe-target.css) maybe-target.css]
            [(css:block? maybe-block) (css-make-syntax-error exn:css:overconsumption maybe-block)]
            [(false? (regexp-match? #px"\\.css$" maybe-target.css)) (css-make-syntax-error exn:css:resource uri)]
            [(false? (file-exists? maybe-target.css)) (css-make-syntax-error exn:css:resource uri)]
            [else (let-values ([(maybe-support maybe-media-list) (css-car maybe-condition)])
                    (define-values (support? media-list)
                      (if (css:function=:=? maybe-support 'supports)
                          (let* ([components (css:function-arguments maybe-support)]
                                 [supports (list (css-remake-token maybe-support css:block #\( components))]
                                 [query (css-parse-feature-query supports name)])
                            (values (css-query-support? query (current-css-feature-support?)) maybe-media-list))
                          (values #true maybe-condition)))
                    (and support? (css-media-queries-support? (css-parse-media-queries media-list name) preferences)
                         (parameterize ([current-css-media-preferences preferences])
                           (read-css-stylesheet maybe-target.css pool))
                         (css-stylesheet-path->identity maybe-target.css)))])))
    
  (define css-@namespace->namespace : (-> CSS-@Rule (U (Pairof Symbol String) CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-namespaces/#syntax
    (lambda [ns]
      (define-values (1st rest) (css-car (css-@rule-prelude ns)))
      (define-values (2nd terminal) (css-car rest))
      (define maybe-block : (Option CSS:Block) (css-@rule-block ns))
      (define namespace : (U String CSS-Syntax-Error)
        (let ([uri (if (eof-object? 2nd) 1st 2nd)])
          (cond [(css:string? uri) (css:string-datum uri)]
                [(css:url? uri) ((inst css:url=> String) uri ~a)]
                [(eof-object? 1st) (css-make-syntax-error exn:css:empty (css-@rule-name ns))]
                [else (css-make-syntax-error exn:css:unrecognized uri)])))
      (cond [(exn? namespace) namespace]
            [(css:block? maybe-block) (css-make-syntax-error exn:css:overconsumption maybe-block)]
            [(css-tokens? terminal) (css-make-syntax-error exn:css:overconsumption terminal)]
            [(css:ident? 1st) (cons (css:ident-datum 1st) namespace)]
            [(eof-object? 2nd) (cons '|| namespace)]
            [else (css-make-syntax-error exn:css:unrecognized 1st)])))

  (define css-@media->media-rule : (-> CSS-@Rule CSS-Media-Preferences CSS-NameSpace CSS-StyleSheet-Pool
                                       (U (Listof CSS-Grammar-Rule) CSS-Media-Rule CSS-Syntax-Error Void))
    ;;; https://drafts.csswg.org/css-conditional/#contents-of
    ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
    (lambda [media preferences namespaces pool]
      (define name : CSS:@Keyword (css-@rule-name media))
      (define maybe-block : (Option CSS:Block) (css-@rule-block media))
      (cond [(false? maybe-block) (css-make-syntax-error exn:css:missing-block name)]
            [(css-null? (css:block-components maybe-block)) (void)]
            [else (when (css-media-queries-support? (css-parse-media-queries (css-@rule-prelude media) name) preferences)
                    (define stxes : (Listof CSS-Syntax-Rule) (css-parse-rules (css:block-components maybe-block)))
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
      (define maybe-block : (Option CSS:Block) (css-@rule-block support))
      (cond [(false? maybe-block) (css-make-syntax-error exn:css:missing-block name)]
            [(css-null? (css:block-components maybe-block)) null]
            [(not (css-query-support? (css-parse-feature-query (css-@rule-prelude support) name) (current-css-feature-support?))) null]
            [else (css-parse-rules (css:block-components maybe-block))])))
  
  (define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule (Option CSS-NameSpace) (U CSS-Style-Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#style-rules
    ;;; https://drafts.csswg.org/selectors/#invalid
    (lambda [qr namespaces]
      (define prelude : (Listof+ CSS-Token) (css-qualified-rule-prelude qr))
      (define components : (Listof CSS-Token) (css:block-components (css-qualified-rule-block qr)))
      (define maybe-selectors : (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error) (css-components->selectors prelude namespaces))
      (cond [(exn? maybe-selectors) maybe-selectors]
            [else (make-css-style-rule maybe-selectors (css-components->descriptors components))])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-cascade-viewport : (->* (CSS-Media-Preferences (Listof CSS-Descriptors))
                                      (CSS-Declared-Value-Filter (CSS-Value-Filter CSS-Media-Datum))
                                      CSS-Media-Preferences)
    ;;; https://drafts.csswg.org/css-device-adapt/#atviewport-rule
    (lambda [viewport-preferences viewport-descriptors
                                  [viewport-descriptor-filter (current-css-viewport-descriptor-filter)]
                                  [viewport-filter (current-css-viewport-filter)]]
      (call-with-css-preferences #:media viewport-preferences
        (viewport-filter (css-cascade-descriptors viewport-descriptor-filter viewport-descriptors)
                         viewport-preferences #false #false))))

  (define css-cascade : (All (CSS-Datum) (-> (Listof CSS-StyleSheet) CSS-Subject CSS-Declared-Value-Filter (CSS-Value-Filter CSS-Datum)
                                             (HashTable Symbol CSS-Datum) (Option (HashTable Symbol CSS-Datum))
                                             (HashTable Symbol CSS-Datum)))
    ;;; https://drafts.csswg.org/css-cascade/#filtering
    ;;; https://drafts.csswg.org/css-cascade/#cascading
    (lambda [stylesheets subject desc-filter value-filter initial-values inherit-values]
      (define declared-values : CSS-Declared-Values (make-hasheq))
      (let cascade-stylesheets ([batch : (Listof CSS-StyleSheet) stylesheets])
        (for ([stylesheet (in-list batch)])
          (define child-identities : (Listof Positive-Integer) (css-stylesheet-imports stylesheet))
          (cascade-stylesheets (for/list : (Listof CSS-StyleSheet) ([import (in-list child-identities)])
                                 (hash-ref (css-stylesheet-pool stylesheet) import)))
          (css-cascade-rules (css-stylesheet-rules stylesheet) subject desc-filter
                             (css-stylesheet-preferences stylesheet) declared-values)))
      (value-filter declared-values initial-values inherit-values
                    (css-descriptor-ref declared-values 'all
                                        (λ [[desc-name : Symbol] [desc-values : (Option (Listof+ CSS-Token))]]
                                          (and desc-values
                                               (let ([v (car desc-values)])
                                                 (cond [(not (css:ident? v)) #false]
                                                       [else (css:ident-datum v)]))))))))

  (define css-cascade-rules : (->* ((Listof CSS-Grammar-Rule) CSS-Subject CSS-Declared-Value-Filter)
                                   (CSS-Media-Preferences CSS-Declared-Values) CSS-Declared-Values)
    ;;; https://drafts.csswg.org/css-cascade/#filtering
    ;;; https://drafts.csswg.org/css-cascade/#cascading
    ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
    ;;; https://drafts.csswg.org/selectors/#data-model
    (lambda [rules subject desc-filter
                   [top-preferences (current-css-media-preferences)]
                   [descbase ((inst make-hasheq Symbol CSS-Declared-Value))]]
      (define-type Style-Metadata (Vector CSS-Complex-Selector CSS-Descriptors CSS-Media-Preferences))
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
                          (define selector : (Option CSS-Complex-Selector)
                            (for/fold ([selected : (Option CSS-Complex-Selector) #false])
                                      ([selector (in-list selectors)])
                              (define matched : (Option CSS-Complex-Selector) (css-selector-match selector subject))
                              (cond [(false? selected) matched]
                                    [(false? matched) selected]
                                    [else (let ([s (css-complex-selector-specificity selected)]
                                                [m (css-complex-selector-specificity matched)])
                                            (if (< s m) matched selected))])))
                          (values (cond [(false? selector) styles]
                                        [else (cons (vector selector (css-style-rule-descriptors style) preferences) styles)])
                                  (and single-preference?)))]))))
      (unless (null? selected-rules-style)
        (define ordered-sources
          (sort selected-rules-style
                (λ [[sm1 : Style-Metadata] [sm2 : Style-Metadata]]
                  ; NOTE: the input style list has not been reversed
                  (>= (css-complex-selector-specificity (vector-ref sm1 0))
                      (css-complex-selector-specificity (vector-ref sm2 0))))))
        (call-with-css-preferences #:media top-preferences
          (if (and single-preference?)
              (let ([source-ref (λ [[src : Style-Metadata]] : CSS-Descriptors (vector-ref src 1))])
                (css-cascade-descriptors desc-filter (map source-ref ordered-sources) descbase))
              (for ([src (in-list ordered-sources)])
                (define alter-preferences : CSS-Media-Preferences (vector-ref src 2))
                (if (eq? alter-preferences top-preferences)
                    (css-cascade-descriptors desc-filter (vector-ref src 1) descbase)
                    (call-with-css-preferences #:media alter-preferences
                      (css-cascade-descriptors desc-filter (vector-ref src 1) descbase)))))))
       descbase))

  (define css-cascade-descriptors : (->* (CSS-Declared-Value-Filter (U CSS-Descriptors (Listof CSS-Descriptors)))
                                         (CSS-Declared-Values) CSS-Declared-Values)
    ;;; https://drafts.csswg.org/css-cascade/#shorthand
    ;;; https://drafts.csswg.org/css-cascade/#importance
    (lambda [desc-filter descriptors [descbase ((inst make-hasheq Symbol CSS-Declared-Value))]]
      (for ([property (in-list descriptors)])
        (cond [(list? property) (css-cascade-descriptors desc-filter property descbase)]
              [else (let* ([desc-name (css:ident-datum (css-declaration-name property))]
                           [desc-name (if (css-declaration-custom? property) desc-name (symbol-downcase desc-name))])
                      (define declared-values : (Listof+ CSS-Token) (css-declaration-values property))
                      (define important? : Boolean (css-declaration-important? property))
                      (define fstub : (-> CSS-Declared-Value) (λ [] (cons declared-values #false)))
                      (when (or important? (not (cdr (hash-ref descbase desc-name fstub))))
                        (define-values (desc-values deprecated?)
                          (cond [(css-declaration-custom? property) (values declared-values #false)]
                                [(not (eq? desc-name 'all)) (desc-filter desc-name declared-values)]
                                [else (css-all-descriptor-filter desc-name declared-values)]))
                        (when deprecated? (css-make-syntax-error exn:css:deprecated (css-declaration-name property)))
                        (cond [(list? desc-values) (hash-set! descbase desc-name (cons desc-values important?))]
                              [(procedure? desc-values) (css-make-syntax-error desc-values (css-declaration-name property))]
                              [(pair? desc-values) (css-make-syntax-error (cdr desc-values) (car desc-values))]
                              [(hash? desc-values) (for ([(name argl) (in-hash desc-values)])
                                                     (when (or important? (not (cdr (hash-ref descbase name fstub))))
                                                       (hash-set! descbase name (cons argl important?))))])))]))
      descbase))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->descriptors : (-> (Listof CSS-Token) CSS-Descriptors)
    (lambda [components]
      (let make-style-rule : CSS-Descriptors ([seitreporp : CSS-Descriptors null] [tokens : (Listof CSS-Token) components])
        (define-values (id any-values) (css-car tokens))
        (define-values (:values rest)
          (let collect : (Values (Listof CSS-Token) (Listof CSS-Token)) ([seulav : (Listof CSS-Token) null]
                                                                         [rest : (Listof CSS-Token) any-values])
            (define-values (head tail) (css-car rest))
            (cond [(or (eof-object? head) (css:delim=:=? head #\;)) (values (reverse seulav) tail)]
                  [(and (css:block=:=? head #\{) (css:@keyword? id)) (values (reverse (cons head seulav)) tail)]
                  [else (collect (cons head seulav) tail)])))
        (cond [(eof-object? id) (reverse seitreporp)]
              [else (let ([property (cond [(css:ident? id) (css-components->declaration id :values)]
                                          [else (css-make-syntax-error exn:css:unrecognized (cons id :values))])])
                      (make-style-rule (css-cons property seitreporp) rest))]))))

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
      (simple-form-path uri.css))))

(require (submod "." digitama))
(require (submod "." parser))
(require (submod "." grammar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module test/digitama typed/racket
  (provide (all-defined-out))
  
  (require racket/flonum)
  
  (define-type Unit (U 'KB 'MB 'GB 'TB))
  (define-type Unit* (Listof Unit))
  (define units : Unit* '(KB MB GB TB))
  (define ~size : (case-> [Integer 'Bytes [#:precision (U Integer (List '= Integer))] -> String]
                          [Flonum Unit [#:precision (U Integer (List '= Integer))] -> String])
    (lambda [size unit #:precision [prcs '(= 3)]]
      (if (symbol=? unit 'Bytes)
          (cond [(< (abs size) 1024) (format "~aBytes" size)]
                [else (~size (fl/ (real->double-flonum size) 1024.0) 'KB #:precision prcs)])
          (let try-next-unit : String ([s : Flonum size] [us : (Option Unit*) (member unit units)])
            (cond [(false? us) "Typed Racket is buggy if you see this message"]
                  [(or (fl< (abs s) 1024.0) (null? (cdr us))) (string-append (~r s #:precision prcs) (symbol->string (car us)))]
                  [else (try-next-unit (fl/ s 1024.0) (cdr us))])))))
  
  (define-syntax (time-run stx)
    (syntax-case stx []
      [(_ sexp ...)
       #'(let ([momery0 : Natural (current-memory-use)])
           (define-values (result cpu real gc) (time-apply (thunk sexp ...) null))
           (printf "memory: ~a cpu time: ~a real time: ~a gc time: ~a~n"
                   (~size (- (current-memory-use) momery0) 'Bytes)
                   cpu real gc)
           (car result))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test typed/racket
  (require (submod ".." digitama))
  (require (submod ".." parser))
  (require (submod ".." grammar))
  (require (submod ".." test/digitama))

  (require (only-in typed/racket/gui get-display-size))

  (define-values (width height) (get-display-size))
  (define-values (in out) (make-pipe))
  (define css-logger (make-logger 'css #false))
  (define css (thread (thunk (let forever ([/dev/log (make-log-receiver css-logger 'debug)])
                               (match (sync/enable-break /dev/log)
                                 [(vector _ message urgent _)
                                  (cond [(eof-object? urgent) (close-output-port out)]
                                        [else (displayln message out) (forever /dev/log)])])))))

  (collect-garbage)
  (current-logger css-logger)
  (css-deprecate-media-type #true)
  (current-css-media-type 'screen)
  (current-css-media-preferences
   ((inst make-hash Symbol CSS-Media-Datum)
    (list (cons 'orientation 'landscape)
          (cons 'width (or width 0))
          (cons 'height (or height 0)))))
  
  (define tamer-sheet : CSS-StyleSheet (time-run (read-css-stylesheet (simplify-path (build-path 'up "tamer" "tamer.css")))))
  (define tamer-subject : CSS-Subject (make-css-subject #:type 'body #:id '#:123))
  (define tamer-header : CSS-Subject (make-css-subject #:type 'html #:id '#:header))

  (define css-descriptor-filter : CSS-Declared-Value-Filter
    (lambda [suitcased-name desc-values]
      (values desc-values #false)))

  (define css-filter : (CSS-Value-Filter Datum)
    (lambda [declared-values all default-values inherit-values]
      (for/hash : (HashTable Symbol Datum) ([desc-name (in-hash-keys declared-values)])
        (values desc-name (css-descriptor-ref declared-values desc-name)))))

  tamer-subject
  (time-run (css-cascade (list tamer-sheet) tamer-subject css-descriptor-filter css-filter ((inst make-hasheq Symbol Datum)) #false))
  (collect-garbage)
  tamer-header
  (time-run (css-cascade (list tamer-sheet) tamer-header css-descriptor-filter css-filter ((inst make-hasheq Symbol Datum)) #false))

  (map (λ [[in : String]] : (Pairof String Integer)
         (let ([maybe-complex-selectors (css-parse-selectors in)])
           (cond [(exn:css? maybe-complex-selectors) (cons in -1)]
                 [else (let* ([s (car maybe-complex-selectors)]
                              [a (css-complex-selector-A s)]
                              [b (css-complex-selector-B s)]
                              [c (css-complex-selector-C s)])
                         (cons in (+ (* a 100) (* b 10) c)))])))
       (list "* + *"
             "li"
             "li::first-line"
             "ul li"
             "ul ol+li"
             "h1 + *[rel=up]"
             "ul ol li.red"
             "li.red.level"
             "#x34y"
             ":not(FOO)#s12"
             ".foo :matches(.bar, #baz)"
             "body #darkside [sith] p"))

  (log-message css-logger 'debug "exit" eof)
  (copy-port in (current-output-port)))
