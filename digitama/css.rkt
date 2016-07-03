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

; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet
(provide read-css-stylesheet
         css-namespace-rule->namespace
         css-media-rule->rules
         css-qualified-rule->style-rule)

(provide css-query-support?
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
       (with-syntax ([token-datum (format-id #'token "~a-datum" (syntax-e #'token))]
                     [token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                     [token->syntax (format-id #'token "~a->syntax" (syntax-e #'token))]
                     [token->string (format-id #'token "~a->string" (syntax-e #'token))]
                     [token-source (format-id #'token "~a-source" (syntax-e #'token))]
                     [token-line (format-id #'token "~a-line" (syntax-e #'token))]
                     [token-column (format-id #'token "~a-column" (syntax-e #'token))]
                     [token-position (format-id #'token "~a-position" (syntax-e #'token))]
                     [token-span (format-id #'token "~a-span" (syntax-e #'token))]
                     [token-position<? (format-id #'token "~a-position<?" (syntax-e #'token))]
                     #|[([id? id-datum] ...) (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                                               (list (format-id <id> "~a?" (syntax-e <id>))
                                                     (format-id <id> "~a-datum" (syntax-e <id>))))]|#)
         #'(begin (struct: token : Token ([source : Any] [line : Natural] [column : Natural] [position : Natural] [span : Natural]
                                                         [datum : Datum]))
                  (struct: subid : SubID subrest ...) ...
                  (define-token id #:+ ID #:-> parent #:as Type rest ...) ...

                  (define token->datum : (-> Token Datum)
                    (lambda [instance]
                      ;;; Meanwhile Typed Racket is buggy for this way
                      ;(cond [(id? instance) (id-datum instance)]
                      ;      ...
                      ;      [else (assert (object-name instance) symbol?)])
                      (token-datum instance)))

                  (define token->syntax : (-> Token Syntax)
                    (lambda [instance]
                      (datum->syntax #false (token->datum instance)
                                     (list (token-source instance)
                                           (token-line instance) (token-column instance)
                                           (token-position instance) (token-span instance)))))

                  (define token->string : (-> Token String)
                    (lambda [instance]
                      (format "~a:~a:~a: ~a: ~s" (token-source instance)
                              (token-line instance) (token-column instance)
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
    [css:bad        #:+ CSS:Bad #:-> css-token #:as Datum [token : Symbol]]
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

  (struct: css:bad:eof : CSS:Bad:EOF css:bad ())
  (struct: css:bad:eol : CSS:Bad:EOL css:bad ())
  (struct: css:bad:char : CSS:Bad:Char css:bad ())
  (struct: css:bad:blank : CSS:Bad:Blank css:bad ())
  (struct: css:bad:range : CSS:Bad:Range css:bad ())
  (struct: css:bad:range:index : CSS:Bad:Range:Index css:bad ())
  (struct: css:bad:stdin : CSS:Bad:StdIn css:bad ())

  ;;; https://drafts.csswg.org/css-syntax/#parsing
  (define-type CSS-StdIn (U Input-Port Path-String Bytes (Listof CSS-Token)))
  (define-type CSS-URL-Modifier (U CSS:Ident CSS:Function CSS:URL))
  (define-type CSS-Syntax-Any (U CSS-Token EOF))
  (define-type CSS-Syntax-Terminal (U CSS:Delim CSS:Close EOF))
  (define-type CSS-Syntax-Rule (U CSS-Qualified-Rule CSS-@Rule))
  (define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule))

  (define-type CSS-Syntax-Error exn:css)
  (define-type Make-CSS-Syntax-Error (-> String Continuation-Mark-Set (Listof Syntax) CSS-Syntax-Error))

  (struct exn:css exn:fail:syntax ())
  (struct exn:css:deprecated exn:css ())
  (struct exn:css:unrecognized exn:css ())
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
                   (let-values ([(token others) (if (pair? tokens) (values (car tokens) (cdr tokens)) (values eof null))])
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

  (struct: css-@rule : CSS-@Rule ([name : CSS:@Keyword] [prelude : (Listof CSS-Token)] [block : (Option CSS:Block)]))
  (struct: css-qualified-rule : CSS-Qualified-Rule ([prelude : (Pairof CSS-Token (Listof CSS-Token))] [block : CSS:Block]))
  (struct: css-declaration : CSS-Declaration ([name : CSS:Ident] [values : (Listof CSS-Token)]
                                                                 [important? : Boolean] [case-sentitive? : Boolean]))

  ;; https://drafts.csswg.org/selectors/#grammar
  ;; https://drafts.csswg.org/selectors/#structure
  (define-syntax (define-selectors stx)
    (syntax-case stx []
      [(_ Selector [s-id #:+ S-ID rest ...] ...)
       #'(begin (define-type Selector (U S-ID ...))
                (struct: s-id : S-ID rest ...) ...)]))

  (define-selectors CSS-Simple-Selector
    [css-type-selector #:+ CSS-Type-Selector ([name : (U CSS:Ident CSS:Delim)] [namespace : CSS-Selector-NameSpace])]
    [css-universal-selector #:+ CSS-Universal-Selector ([namespace : CSS-Selector-NameSpace])]
    [css-class-selector #:+ CSS-Class-Selector ([value : CSS:Ident])] ; <=> [class~=value]
    [css-id-selector #:+ CSS-ID-Selector ([value : CSS:Hash])]        ; <=> [`id`~=value] Note: you name `id` in your application
    [css-pseudo-selector #:+ CSS-Pseudo-Selector ([name : (U CSS:Ident CSS:Function)] [element? : Boolean])]
    [css-attribute-selector #:+ CSS-Attribute-Selector ([name : CSS:Ident] [namespace : CSS-Selector-NameSpace])]
    [css-attribute=selector #:+ CSS-Attribute=Selector css-attribute-selector
                            ([operator : CSS:Delim] [value : CSS:String] [force-ci? : Boolean])])
  
  (define-type CSS-Selector-NameSpace (Option CSS:Ident))
  (define-type CSS-Compound-Selector (Listof CSS-Simple-Selector))
  (define-type CSS-Complex-Selector (Listof (U CSS-Compound-Selector Symbol)))
  
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
  
  ;; https://drafts.csswg.org/css-syntax/#css-stylesheets
  ;; https://drafts.csswg.org/cssom/#css-object-model
  (define-type CSS-Media-Preferences (HashTable (U Symbol Keyword) CSS-Media-Datum))
  (define-type CSS-Feature-Support? (-> Symbol (Listof Datum) Boolean))
  (define-type CSS-Imports (HashTable Bytes CSS-StyleSheet))
  (define-type CSS-NameSpace (HashTable Symbol String))

  (define-values (current-css-media-preferences current-css-media-feature-filter current-css-feature-support?)
    (values (make-parameter ((inst make-hasheq (U Symbol Keyword) CSS-Media-Datum)))
            (make-parameter css-media-feature-filter)
            (make-parameter (const #false))))
  
  (struct: css-style-rule : CSS-Style-Rule ([selectors : (Listof CSS-Complex-Selector)] [properties : (Listof CSS-Declaration)]))

  (struct: css-stylesheet : CSS-StyleSheet
    ([location : Any]
     [imports : CSS-Imports]
     [namespaces : CSS-NameSpace]
     [rules : (Listof CSS-Grammar-Rule)]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-type Quantity->Scalar (case-> [Nonnegative-Exact-Rational Symbol -> (U Nonnegative-Exact-Rational Flonum-Nan)]
                                        [Exact-Rational Symbol -> (U Exact-Rational Flonum-Nan)]
                                        [Nonnegative-Real Symbol -> Nonnegative-Real]
                                        [Real Symbol -> Real]))
  
  (define css-dimension->scalar : (->* ((U CSS:Dimension (Pairof Real Symbol))) ((Option Symbol)) Real)
    (lambda [dim [type #false]]
      (cond [(css:dimension? dim) (css-dimension->scalar (css:dimension-datum dim) type)]
            [else (let-values ([(n unit) (values (car dim) (cdr dim))])
                    (case (if (symbol? type) type unit)
                      [(length px cm mm q in pc pt) (css-length->scalar n unit)]
                      [(angle deg grad rad turn) (css-angle->scalar n unit)]
                      [(time s ms min h) (css-time->scalar n unit)]
                      [(frequency hz khz) (css-frequency->scalar n unit)]
                      [(resolution dpi dpcm dppx) (css-resolution->scalar n unit)]
                      [else +nan.0]))])))
  
  (define css-length->scalar : Quantity->Scalar
    ;;; https://drafts.csswg.org/css-values/#absolute-lengths
    (lambda [n unit]
      (case unit
        [(px) n]
        [(cm) (* n 9600/254)]
        [(mm) (* n 9600/254 1/10)]
        [(q) (* n 9600/254 1/40)]
        [(in) (* n 96)]
        [(pc) (* n 96 1/6)]
        [(pt) (* n 96 1/72)]
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
  (define css-car : (All (CSS-DT) (-> (Listof CSS-DT) (Values (U CSS-DT EOF) (Listof CSS-DT))))
    (lambda [dirty]
      (let skip-whitespace : (Values (U CSS-DT EOF) (Listof CSS-DT)) ([rest dirty])
        (cond [(null? rest) (values eof null)]
              [else (let-values ([(head tail) (values (car rest) (cdr rest))])
                      (cond [(css:whitespace? head) (skip-whitespace tail)]
                            [else (values head tail)]))]))))

  (define css-null? : (-> (Listof Any) Boolean)
    (lambda [dirty]
      (let skip-whitespace : Boolean ([rest dirty])
        (or (null? rest)
            (and (css:whitespace? (car rest))
                 (skip-whitespace (cdr rest)))))))

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

  (struct css-srcloc ([in : Input-Port] [line : (Option Positive-Integer)] [col : (Option Natural)] [pos : (Option Positive-Integer)])
    #:type-name CSS-Srcloc)

  (define-syntax (css-make-token stx)
    (syntax-case stx []
      [(_ src make-css:token #:datum datum extra ...)
       #'(let-values ([(start-position) (css-srcloc-pos src)]
                      [(line column position) (port-next-location (css-srcloc-in src))])
           (make-css:token (or (object-name (css-srcloc-in src)) '/dev/cssin)
                           (or (css-srcloc-line src) line 0)
                           (or (css-srcloc-col src) column 0)
                           (or start-position 0)
                           (cond [(not (and (integer? position) (integer? start-position))) 0]
                                 [else (max (- position start-position) 0)])
                           datum extra ...))]
      [(_ src make-css:token datum extra ...)
       #'(css-make-token src make-css:token #:datum datum datum extra ...)]))

  (define-syntax (css-remake-token stx)
    (syntax-case stx []
      [(_ [start-token end-token] make-css:token datum extra ...)
       #'(make-css:token (css-token-source start-token)
                         (css-token-line start-token) (css-token-column start-token)
                         (css-token-position start-token)
                         (max (- (+ (css-token-position end-token) (css-token-span end-token))
                                 (css-token-position start-token)) 0)
                         datum datum extra ...)]
      [(_ here-token make-css:token datum ...)
       #'(css-remake-token [here-token here-token] make-css:token datum ...)]))

  (define-syntax (css-make-bad-token stx)
    (syntax-case stx []
      [(_ src css:bad:sub token datum)
       #'(let ([bad (css-make-token src css:bad:sub datum (assert (object-name token) symbol?))])
           (log-message (current-logger) 'warning 'exn:css:read (css-token->string bad) bad)
           bad)]))
  
  (define css-consume-token : (-> Input-Port (U EOF CSS-Token))
    ;;; (if still): https://drafts.csswg.org/css-syntax/#input-preprocessing
    ;;; (if still): https://drafts.csswg.org/css-syntax/#rule-defs (distinguish delim-token and from () [] {} ,:; and so on.
    ;;; https://drafts.csswg.org/css-syntax/#error-handling
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-token
    (lambda [/dev/cssin]
      (define-values (line column position) (port-next-location /dev/cssin))
      (define ch (read-char /dev/cssin))
      (define srcloc (css-srcloc /dev/cssin line column position))
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
                             (css-make-token srcloc css:dimension #:datum (cons n unit) representation (cons n unit))]
                            [(and (char? ch1) (char=? ch1 #\%)) (read-char css)
                             (css-make-token srcloc css:percentage #:datum (cons n '%) representation (real->fraction n))]
                            [(exact-integer? n) (css-make-token srcloc css:integer #:datum n representation n)]
                            [else (css-make-token srcloc css:flonum #:datum n representation (real->double-flonum n))])))])))

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
          (let ([name : String (css-consume-name (css-srcloc-in srcloc) #\#)])
            (css-make-token srcloc css:hash (string->keyword name)
                        (if (css-identifier-prefix? ch1 ch2 ch3) 'id 'unrestricted)))
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
      [(_ id :-> ->T (lambda [cssin [args : T defval] ...] body ...))
       #'(define (id [/dev/stdin : CSS-StdIn (current-input-port)] [args : T defval] ...) : ->T
           (define /dev/cssin : Input-Port (css-open-input-port /dev/stdin))
           (dynamic-wind (thunk (port-count-lines! /dev/cssin))
                         (thunk ((λ [[cssin : Input-Port] [args : T defval] ...] : ->T body ...) /dev/cssin args ...))
                         (thunk (close-input-port /dev/cssin))))]))

  ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
  (define-css-parser-entry read-css-stylesheet :-> CSS-StyleSheet
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet
    ;;; https://drafts.csswg.org/css-syntax/#charset-rule
    ;;; https://drafts.csswg.org/css-namespaces
    ;;; https://drafts.csswg.org/css-cascade/#at-import
    ;;; https://drafts.csswg.org/css-conditional/#processing
    ;;; https://drafts.csswg.org/css-conditional/#contents-of
    (lambda [/dev/cssin [imports : CSS-Imports (make-hash)]]
      (define namespaces : CSS-NameSpace (make-hasheq))
      (define media-preferences : CSS-Media-Preferences (current-css-media-preferences))
      (define rules : (Listof CSS-Grammar-Rule)
        (let syntax->grammar : (Listof CSS-Grammar-Rule)
          ([selur : (Listof CSS-Grammar-Rule) null]
           [rules : (Listof CSS-Syntax-Rule) (css-consume-stylesheet /dev/cssin)]
           [can-import? : Boolean #true]
           [allow-namespace? : Boolean #true])
          (cond [(null? rules) (reverse selur)]
                [else (let-values ([(rule rest) (values (car rules) (cdr rules))])
                        (cond [(css-qualified-rule? rule)
                               (syntax->grammar (css-cons (css-qualified-rule->style-rule rule) selur) rest #false #false)]
                              [(css:@keyword=:=? (css-@rule-name rule) '#:@charset)
                               (css-make-syntax-error exn:css:misplaced (css-@rule-name rule))
                               (syntax->grammar selur rest can-import? allow-namespace?)]
                              [(css:@keyword=:=? (css-@rule-name rule) '#:@import)
                               (cond [(false? can-import?)
                                      (css-make-syntax-error exn:css:misplaced (css-@rule-name rule))
                                      (syntax->grammar selur rest #false allow-namespace?)]
                                     [else (let ([ns (css-namespace-rule->namespace rule)])
                                             (when (pair? ns) (hash-set! namespaces (car ns) (cdr ns)))
                                             (syntax->grammar selur rest #true allow-namespace?))])]
                              [(css:@keyword=:=? (css-@rule-name rule) '#:@namespace)
                               (cond [(false? allow-namespace?)
                                      (css-make-syntax-error exn:css:misplaced (css-@rule-name rule))
                                      (syntax->grammar selur rest #false #false)]
                                     [else (let ([ns (css-namespace-rule->namespace rule)])
                                             (when (pair? ns) (hash-set! namespaces (car ns) (cdr ns)))
                                             (syntax->grammar selur rest #false #true))])]
                              [(css:@keyword=:=? (css-@rule-name rule) '#:@media)
                               (define subrules (css-media-rule->rules rule))
                               (syntax->grammar selur (if (pair? subrules) (append subrules rest) rest) #false #false)]
                              [(css:@keyword=:=? (css-@rule-name rule) '#:@support)
                               (define subrules (css-support-rule->rules rule))
                               (syntax->grammar selur (if (pair? subrules) (append subrules rest) rest) #false #false)]
                              [else (syntax->grammar (cons rule selur) rest #false #false)]))])))
      (make-css-stylesheet (or (object-name /dev/cssin) '/dev/cssin) imports namespaces rules)))

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
      (let consume-declaration+@rule : (Listof (U CSS-Declaration CSS-@Rule))
        ([mixed-list : (Listof (U CSS-Declaration CSS-@Rule)) null])
        (define token (css-read-syntax /dev/cssin))
        (cond [(eof-object? token) (reverse mixed-list)]
              [(or (css:whitespace? token) (css:delim=:=? token #\;)) (consume-declaration+@rule mixed-list)]
              [(css:@keyword? token) (consume-declaration+@rule (cons (css-consume-@rule /dev/cssin token) mixed-list))]
              [(css:ident? token) (let-values ([(components _) (css-consume-components /dev/cssin #\;)])
                                    (consume-declaration+@rule (css-cons (css-components->declaration token components) mixed-list)))]
              [else (let-values ([(_c _t) (css-consume-components /dev/cssin #\;)])
                      (css-make-syntax-error exn:css:missing-identifier token)
                      (consume-declaration+@rule mixed-list))]))))
  
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
  
  (define-css-parser-entry css-parse-selectors :-> (U (Listof CSS-Complex-Selector) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-selector
    ;;; https://drafts.csswg.org/selectors/#selector-list
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [/dev/cssin]
      (with-handlers ([exn:css? (λ [[errcss : exn:css]] errcss)])
        (let consume-selector : (Listof CSS-Complex-Selector)
          ([selectors : (Listof CSS-Complex-Selector) null]
           [last-terminal : CSS-Syntax-Terminal eof])
          (define token (css-read-syntax/skip-whitespace /dev/cssin))
          (cond [(and (eof-object? token) (eof-object? last-terminal)) (reverse selectors)]
                [(eof-object? token) (css-throw-syntax-error exn:css:empty last-terminal)]
                [else (let-values ([(components terminal) (css-consume-complex-selector /dev/cssin token)])
                        (consume-selector (cons components selectors) terminal))])))))

  (define-css-parser-entry css-parse-relative-selectors :-> (U (Listof CSS-Complex-Selector) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-relative-selector
    ;;; https://drafts.csswg.org/selectors/#the-scope-pseudo
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [/dev/cssin]
      (css-make-syntax-error exn:css:unrecognized eof)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-namespace-rule->namespace : (-> CSS-@Rule (U (Pairof Symbol String) CSS-Syntax-Error))
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
            [(not (css-null? terminal)) (css-make-syntax-error exn:css:overconsumption terminal)]
            [(css:ident? 1st) (cons (css:ident-datum 1st) namespace)]
            [(eof-object? 2nd) (cons '_ namespace)]
            [else (css-make-syntax-error exn:css:unrecognized 1st)])))

  (define css-media-rule->rules : (-> CSS-@Rule (U (Listof CSS-Syntax-Rule) CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-conditional/#contents-of
    ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
    (lambda [media]
      (define name : CSS:@Keyword (css-@rule-name media))
      (define queries : (Listof CSS-Media-Query) (css-parse-media-queries (css-@rule-prelude media) name))
      (define maybe-block : (Option CSS:Block) (css-@rule-block media))
      (if (false? maybe-block)
          (css-make-syntax-error exn:css:missing-block name)
          (if (or (null? queries) (ormap (λ [[q : CSS-Media-Query]] (css-query-support? q (current-css-media-preferences))) queries))
              (css-parse-rules (css:block-components maybe-block))
              null))))

  (define css-support-rule->rules : (-> CSS-@Rule (U (Listof CSS-Syntax-Rule) CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-conditional/#contents-of
    ;;; https://drafts.csswg.org/css-conditional/#at-supports
    (lambda [support]
      (define name : CSS:@Keyword (css-@rule-name support))
      (define maybe-query : (U CSS-Feature-Query CSS-Syntax-Error) (css-parse-feature-query (css-@rule-prelude support) name))
      (define maybe-block : (Option CSS:Block) (css-@rule-block support))
      (cond [(exn? maybe-query) maybe-query]
            [(false? maybe-block) (css-make-syntax-error exn:css:missing-block name)]
            [(css-query-support? maybe-query (current-css-feature-support?)) (css-parse-rules (css:block-components maybe-block))]
            [else null])))
  
  (define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule (U CSS-Style-Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#style-rules
    ;;; https://drafts.csswg.org/selectors/#invalid
    (lambda [qr]
      (define prelude : (Listof CSS-Token) (css-qualified-rule-prelude qr))
      (define selectors : (U (Listof CSS-Complex-Selector) CSS-Syntax-Error) (css-parse-selectors prelude))
      (cond [(exn? selectors) selectors]
            [(null? selectors) (css-make-syntax-error exn:css:empty (css-qualified-rule-block qr))]
            [else (let ([components (css:block-components (css-qualified-rule-block qr))])
                    (define srotpircsed : (Listof CSS-Declaration)
                      (for/fold ([descriptors : (Listof CSS-Declaration) null])
                                ([mixed-list (in-list (css-parse-declarations components))])
                        (cond [(css-declaration? mixed-list) (cons mixed-list descriptors)]
                              [else (css-make-syntax-error exn:css:unrecognized (css-@rule-name mixed-list))
                                    descriptors])))
                    (make-css-style-rule selectors (reverse srotpircsed)))])))

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
      (let consume-rules : (Listof CSS-Syntax-Rule) ([rules : (Listof CSS-Syntax-Rule) null])
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
      (define-values (prelude maybe-block) (css-consume-rule-item css #:@rule? #false))
      (cond [(css:block? maybe-block) (make-css-qualified-rule (cons reconsumed prelude) maybe-block)]
            [else (css-make-syntax-error exn:css:missing-block (cons reconsumed prelude))])))

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
      (let consume-item : (Values (Listof CSS-Token) (Option CSS:Block))
        ([prelude : (Listof CSS-Token) null]
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
      (define-values (components close) (css-consume-block-body css close-char))
      (define end-token : CSS-Token
        (cond [(css-token? close) close]
              [(null? components) open]
              [else (last components)]))
      (css-remake-token [open end-token] css:block (css:delim-datum open) components)))

  (define css-consume-function : (-> Input-Port CSS:Function CSS:Function)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    ;;; https://drafts.csswg.org/css-values/#functional-notations
    (lambda [css func]
      (define fname : Symbol (css:function-datum func))
      (cond [(false? (symbol-unreadable? fname)) func]
            [else (let-values ([(components close) (css-consume-block-body css #\))])
                    (define end-token : CSS-Token
                      (cond [(css-token? close) close]
                            [(null? components) func]
                            [else (last components)]))
                    (css-remake-token [func end-token] css:function
                                      (string->symbol (symbol->string fname)) ; <==> (symbol-unreadable->symbol fname)
                                      (filter-not css:whitespace? components)))])))
  
  (define css-consume-components : (->* (Input-Port) ((Option Char) Boolean) (Values (Listof CSS-Token) CSS-Syntax-Terminal))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    ;;; https://drafts.csswg.org/css-values/#comb-comma
    (lambda [css [terminal-char #false] [omit-terminal? #false]]
      (let consume-component : (Values (Listof CSS-Token) CSS-Syntax-Terminal) ([stnenopmoc : (Listof CSS-Token) null])
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
      (let consume-components : (Listof (Listof CSS-Token)) ([componentses : (Listof (Listof CSS-Token)) null])
        (define-values (components terminal) (css-consume-components css #\, omit-comma?))
        (cond [(not (eof-object? terminal)) (consume-components (cons components componentses))]
              [(not omit-comma?) (reverse (cons components componentses))]
              [else (filter-not css-null? (reverse (cons components componentses)))]))))

  (define css-components->declaration : (-> CSS:Ident (Listof CSS-Token) (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
    ;;; https://drafts.csswg.org/css-syntax/#typedef-declaration-value
    ;;; https://drafts.csswg.org/css-cascade/#importance
    ;;; https://drafts.csswg.org/css-variables/#defining-variables
    ;;; https://drafts.csswg.org/css-values/#component-whitespace
    (lambda [id-token components]
      (define-values (maybe-: value-list) (css-car components))
      (cond [(not (css:delim=:=? maybe-: #\:)) (css-make-syntax-error exn:css:missing-colon id-token)]
            [else (let verify : (U CSS-Declaration CSS-Syntax-Error)
                    ([lla : (Listof CSS-Token) null]
                     [info : (U False CSS-Syntax-Error CSS:Delim) #false]
                     [rest : (Listof CSS-Token) value-list])
                    (cond [(pair? rest)
                           (define-values (1st tail) (values (car rest) (cdr rest)))
                           (cond [(css:whitespace? 1st) (verify lla info tail)]
                                 [(and (css:delim? info) (css:ident=:=? 1st 'important)) (verify lla info tail)]
                                 [(css:delim? info) (verify lla (css-make-syntax-error exn:css:unrecognized info) null)]
                                 [(css:delim=:=? 1st #\!) (verify lla 1st tail)]
                                 [(or (css:bad? 1st) (css:close? 1st)) (verify lla (css-make-syntax-error exn:css:malformed 1st) null)]
                                 [else (verify (cons 1st lla) #false tail)])]
                          [(exn:css? info) info]
                          [(null? lla) (css-make-syntax-error exn:css:missing-value id-token)]
                          [else (make-css-declaration id-token (reverse lla) (css:delim? info) (css:ident=:=? id-token --symbol?))]))])))

  (define css-consume-block-body : (-> Input-Port Char (Values (Listof CSS-Token) CSS-Syntax-Terminal))
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css close-char]
      (let consume-body : (Values (Listof CSS-Token) CSS-Syntax-Terminal) ([components : (Listof CSS-Token) null])
        (define token (css-read-syntax css))
        (cond [(eof-object? token) (css-make-syntax-error exn:css:missing-delimiter token) (values (reverse components) eof)]
              [(css:close=:=? token close-char) (values (reverse components) token)]
              [else (consume-body (cons (css-consume-component-value css token) components))]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->media-type+query : (-> CSS:Ident Boolean (Listof CSS-Token) CSS-Media-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-query
    (lambda [media only? conditions]
      (define downcased-type : Symbol (css:ident=> media symbol-downcase))
      (define-values (maybe-and maybe-conditions) (css-car conditions))
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
                   [(css:ident? name) (css-throw-syntax-error exn:css:missing-value condition)]
                   [(css:function? condition) (css-throw-syntax-error exn:css:enclosed condition)]
                   [else (css-throw-syntax-error exn:css:unrecognized condition)])]
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
      (let components->junction : (U CSS-And CSS-Or)
        ([junctions : (Listof CSS-Token) (if (false? maybe-head) null (list maybe-head))]
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
                    (define media : CSS-Media-Datum (hash-ref support? '#:@media (λ _ 'all)))
                    (define result (and (symbol? media) (memq (css-media-type-name query) (list media 'all))))
                    (if (css-media-type-only? query) (and result #true) (not result))]
                   [else (and (pair? query)
                              (css-query-support? (car query) support?)
                              (css-query-support? (cdr query) support?))])]
            [else (and (procedure? support?)
                       (css-declaration? query)
                       (support? (css:ident-datum (css-declaration-name query))
                                 (map css-token->datum (css-declaration-values query))))])))

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
            [(not (css-null? terminal)) (css-throw-syntax-error exn:css:overconsumption terminal)]
            [(not (css:delim=? d0 d1)) (css-throw-syntax-error exn:css:malformed (list d0 value1 d1))]
            [else (make-css-and (list (css-make-media-feature value1 value0 po0 d0)
                                      (css-make-media-feature value1 value2 op1 d1)))])))
  
  (define css-declaration->media-query : (-> CSS-Declaration CSS:Block CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    (lambda [property broken-condition]
      (define-values (media-value rest) (css-car-media-value (css-declaration-values property)))
      (cond [(eof-object? media-value) (css-throw-syntax-error exn:css:enclosed broken-condition)]
            [(not (css-null? rest)) (css-throw-syntax-error exn:css:enclosed broken-condition)]
            [else (css-make-media-feature (css-declaration-name property) media-value #\: #false)])))

  (define css-car-comparison-operator : (-> (Listof CSS-Token) (Values (U CSS:Delim EOF) Char Char (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
    (lambda [components]
      (define-values (d rest) (css-car components))
      (define-values (maybe-= terminal) (if (pair? rest) (values (car rest) (cdr rest)) (values eof null)))
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
    (lambda [descriptor maybe-value ophint maybe-op]
      (define errobj (filter css-token? (list descriptor maybe-op maybe-value)))
      (define name (css:ident=> descriptor (compose1 string-downcase symbol->string)))
      (define-values (downcased-name op min/max?)
        (cond [(string-prefix? name "min-") (values (string->symbol (substring name 4)) #\≥ #true)]
              [(string-prefix? name "max-") (values (string->symbol (substring name 4)) #\≤ #true)]
              [else (values (string->symbol name) ophint #false)]))
      (when (and min/max?)
        (cond [(or (not maybe-value) (css:delim? maybe-op)) (css-throw-syntax-error exn:css:misplaced errobj)]
              [(not (css:numeric? maybe-value)) (css-throw-syntax-error exn:css:unrecognized errobj)]))
      (define-values (v deprecated?) ((current-css-media-feature-filter) downcased-name maybe-value min/max?))
      (unless (not deprecated?) (css-make-syntax-error exn:css:deprecated descriptor))
      (cond [(void? v) downcased-name]
            [(procedure? v) (css-throw-syntax-error v errobj)]
            [else (make-css-media-feature downcased-name v op)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-complex-selector : (-> Input-Port CSS-Token (Values CSS-Complex-Selector CSS-Syntax-Terminal))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#combinators
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [css reconsumed]
      (let consume-compound-selector : (Values CSS-Complex-Selector CSS-Syntax-Terminal)
        ([srotceles : (Listof (U CSS-Compound-Selector Symbol)) null]
         [token : CSS-Syntax-Any reconsumed])
        (cond [(or (eof-object? token) (css:delim=:=? token #\,))
               (define selectors (if (null? srotceles) null (if (eq? (car srotceles) '>>) (cdr srotceles) srotceles)))
               (cond [(null? selectors) (css-throw-syntax-error exn:css:empty token)]
                     [else (values (reverse selectors) token)])]
              [(css-selector-combinator? token)
               (let-values ([(combinator last-token) (css-consume-combinator css token)])
                 (if (css-selector-combinator? last-token)
                     (css-throw-syntax-error exn:css:unrecognized last-token)
                     (consume-compound-selector (cons combinator srotceles) last-token)))]
              [else (let-values ([(compound-selector last-token) (css-consume-compound-selector css token)])
                      (consume-compound-selector (cons compound-selector srotceles) last-token))]))))

  (define css-consume-combinator : (-> Input-Port (U CSS:WhiteSpace CSS:Delim CSS:||) (Values Symbol CSS-Syntax-Any))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [css reconsumed]
      (cond [(css:whitespace? reconsumed)
             (define next (css-read-syntax/skip-whitespace css))
             (cond [(css-selector-combinator? next) (css-consume-combinator css next)]
                   [else (values '>> next)])]
            [(css:delim=:=? reconsumed #\>)
             (define next (css-read-syntax css))
             (define next2 (css-read-syntax/skip-whitespace css))
             (cond [(not (css:delim=:=? next #\>)) (values '> next2)]
                   [else (values '>> (css-read-syntax/skip-whitespace css))])]
            [(css:delim=:=? reconsumed #\+) (values '+ (css-read-syntax/skip-whitespace css))]
            [(css:delim=:=? reconsumed #\~) (values '~ (css-read-syntax/skip-whitespace css))]
            [(css:||? reconsumed) (values '|| (css-read-syntax/skip-whitespace css))]
            [else (css-throw-syntax-error exn:css:unrecognized reconsumed)])))
  
  (define css-consume-compound-selector : (-> Input-Port CSS-Token (Values CSS-Compound-Selector CSS-Syntax-Any))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    (lambda [css reconsumed]
      (define selectors0 : (Listof CSS-Simple-Selector)
        (cond [(or (css:ident? reconsumed) (css:delim=:=? reconsumed #\|) (css:delim=:=? reconsumed #\*))
               (list (css-consume-elemental-selector css reconsumed))]
              [(or (css:delim? reconsumed) (css:hash? reconsumed))
               (list (css-consume-simple-selector css reconsumed)
                     (make-css-universal-selector (css-remake-token reconsumed css:ident '_)))]
              [else (css-throw-syntax-error exn:css:unrecognized reconsumed)]))
      (let consume-simple-selector : (Values CSS-Compound-Selector CSS-Syntax-Any) ([srotceles selectors0])
        (define token (css-read-syntax css))
        (if (or (eof-object? token) (css:delim=:=? token #\,) (css-selector-combinator? token))
            (values (reverse srotceles) token)
            (consume-simple-selector (cons (css-consume-simple-selector css token) srotceles))))))

  (define css-consume-elemental-selector : (-> Input-Port (U CSS:Ident CSS:Delim) (U CSS-Type-Selector CSS-Universal-Selector))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#elemental-selectors
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    (lambda [css reconsumed]
      (define next (css-peek-syntax css 0))
      (define next2 (css-peek-syntax css 1))
      (cond [(css:delim=:=? reconsumed #\|)
             (css-read-syntax css)
             (cond [(css:ident? next) (make-css-type-selector next #false)]
                   [(css:delim=:=? next #\*) (make-css-universal-selector #false)]
                   [else (css-throw-syntax-error exn:css:missing-identifier next)])]
            [(css:delim=:=? next #\|)
             (css-read-syntax css) (css-read-syntax css)
             (define ns : CSS:Ident
               (cond [(css:ident? reconsumed) reconsumed]
                     [else (css-remake-token reconsumed css:ident '*)]))
             (cond [(css:ident? next2) (make-css-type-selector next2 ns)]
                   [(css:delim=:=? next2 #\*) (make-css-universal-selector ns)]
                   [else (css-throw-syntax-error exn:css:missing-identifier next)])]
            [else (make-css-type-selector reconsumed (css-remake-token reconsumed css:ident '_))])))

  (define css-consume-simple-selector : (-> Input-Port CSS-Token CSS-Simple-Selector)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [css reconsumed]
      (cond [(css:hash? reconsumed) (make-css-id-selector reconsumed)]
            [(css:delim=:=? reconsumed #\.)
             (define next (css-read-syntax css))
             (cond [(not (css:ident? next)) (css-throw-syntax-error exn:css:missing-identifier next)]
                   [else (make-css-class-selector next)])]
            [(css:delim=:=? reconsumed #\:)
             (define next (css-read-syntax css))
             (define element? : Boolean (css:delim=:=? next #\:))
             (define next2 (if element? (css-read-syntax css) next))
             (cond [(css:ident? next2) (make-css-pseudo-selector next2 element?)]
                   [(css:function? next2) (make-css-pseudo-selector (css-consume-function css next2) element?)]
                   [else (css-throw-syntax-error exn:css:missing-identifier next)])]
            [(css:block=:=? reconsumed #\[) (css-simple-block->attribute-selector reconsumed)]
            [(css:delim=:=? reconsumed #\[) (css-simple-block->attribute-selector (css-consume-simple-block css reconsumed #\]))]
            [else (css-throw-syntax-error exn:css:unrecognized reconsumed)])))
  
  (define css-simple-block->attribute-selector : (-> CSS:Block CSS-Attribute-Selector)
    ;;; https://drafts.csswg.org/selectors/#attribute-selectors
    ;;; https://drafts.csswg.org/selectors/#attrnmsp
    ;;; https://drafts.csswg.org/selectors/#attribute-case
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    (lambda [block]
      (define-values (1st rest1) (css-car (css:block-components block)))
      (define-values (2nd rest2) (if (null? rest1) (values eof null) (values (car rest1) (cdr rest1))))
      (define-values (3rd rest3) (if (null? rest2) (values eof null) (values (car rest2) (cdr rest2))))
      (define-values (attr namespace op-part)
        (cond [(eof-object? 1st) (css-throw-syntax-error exn:css:empty block)]
              [(or (css:match? 1st) (css:delim=:=? 1st #\=))
               (css-throw-syntax-error exn:css:missing-identifier block)]
              [(or (eof-object? 2nd) (css:match? 2nd) (css:delim=:=? 2nd #\=))
               (cond [(css:ident? 1st) (values 1st (css-remake-token 1st css:ident '_) rest1)]
                     [else (css-throw-syntax-error exn:css:missing-identifier 1st)])]
              [(or (eof-object? 3rd) (css:match? 3rd) (css:delim=:=? 3rd #\=))
               (cond [(and (css:delim=:=? 1st #\|) (css:ident? 2nd)) (values 2nd #false rest2)]
                     [(css:delim=:=? 2nd #\|) (css-throw-syntax-error exn:css:missing-identifier 2nd)]
                     [else (css-throw-syntax-error exn:css:unrecognized 1st)])]
              [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:delim=:=? 2nd #\|) (css:ident? 3rd))
               (values 3rd (if (css:ident? 1st) 1st (css-remake-token 1st css:ident '*)) rest3)]
              [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:delim=:=? 2nd #\|))
               (css-throw-syntax-error exn:css:missing-identifier 3rd)]
              [(or (css:ident? 1st) (css:delim=:=? 1st #\*))
               (css-throw-syntax-error exn:css:unrecognized 2nd)]
              [else (css-throw-syntax-error exn:css:unrecognized 1st)]))
      (define-values (op value-part) (css-car op-part))
      (define-values (value ci-part) (css-car value-part))
      (define-values (i terminal) (css-car ci-part))
      (unless (eof-object? op)
        (cond [(eof-object? value) (css-throw-syntax-error exn:css:missing-value op)]
              [(not (or (eof-object? i) (css:ident=:=? i 'i))) (css-throw-syntax-error exn:css:overconsumption i)]
              [(not (css-null? terminal)) (css-throw-syntax-error exn:css:overconsumption terminal)]))
      (define val : CSS:String
        (cond [(css:string? value) value]
              [(css:ident? value) (css-remake-token value css:string (css:ident=> value symbol->string))]
              [(css-token? value) (css-throw-syntax-error exn:css:unrecognized value)]
              [else (css-remake-token attr css:string "placeholder")]))
      (define ci? : Boolean (css:ident? i))
      (cond [(eof-object? op) (make-css-attribute-selector attr namespace)]
            [(css:delim=:=? op #\=) (make-css-attribute=selector attr namespace op val ci?)]
            [(css:match? op) (make-css-attribute=selector attr namespace (css-remake-token op css:delim (css:match-datum op)) val ci?)]
            [else (css-throw-syntax-error exn:css:unrecognized op)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-open-input-port : (-> CSS-StdIn Input-Port)
    ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
    (lambda [/dev/stdin]
      (if (list? /dev/stdin)
          (let ([total : Index (length /dev/stdin)]
                [cursor : Integer 0])
            (make-input-port '/dev/cssin
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
                                   [(string? /dev/stdin) (open-input-string /dev/stdin '/dev/cssin)]
                                   [(byte? /dev/stdin) (open-input-bytes /dev/stdin '/dev/cssin)]
                                   [else (current-input-port)])]
                 [/dev/cssin : Input-Port (css-fallback-encode-input-port /dev/rawin)]
                 [peek-pool : (Listof Any) null])
            (make-input-port (or (object-name /dev/rawin) '/dev/cssin)
                             (λ [[buf : Bytes]]
                               (λ _ (cond [(null? peek-pool) (css-consume-token /dev/cssin)]
                                          [else (let-values ([(rest peeked) (split-at-right peek-pool 1)])
                                                  (set! peek-pool rest)
                                                  (car peeked))])))
                             (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                               (λ _ (and (for ([idx (in-range (length peek-pool) (add1 skip))])
                                           (set! peek-pool (cons (css-consume-token /dev/cssin) peek-pool)))
                                         (list-ref peek-pool (- (length peek-pool) skip 1)))))
                             (thunk (unless (eq? /dev/rawin /dev/cssin)
                                      (close-input-port /dev/cssin))
                                    (unless (eq? /dev/rawin /dev/stdin)
                                      (close-input-port /dev/rawin)))
                             #false #false
                             (thunk (port-next-location /dev/cssin))
                             (thunk (port-count-lines! /dev/cssin)))))))
  
  (define css-read-syntax : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (define stx (read-char-or-special css))
      (cond [(or (eof-object? stx) (css-token? stx)) stx]
            [else (css-make-bad-token (css-srcloc css #false #false #false)
                                      css:bad:stdin struct:css-token (~s stx))])))

  (define css-peek-syntax : (->* (Input-Port) (Natural) CSS-Syntax-Any)
    (lambda [css [skip 0]]
      (define stx (peek-char-or-special css skip))
      (cond [(or (eof-object? stx) (css-token? stx)) stx]
            [else (css-make-bad-token (css-srcloc css #false #false #false)
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

  (define css-cons : (All (CSS-DT) (-> (U CSS-Syntax-Error CSS-DT) (Listof CSS-DT) (Listof CSS-DT)))
    (lambda [item items]
      (cond [(exn? item) items]
            [else (cons item items)])))

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
                    [else (css-make-syntax-error exn:css:unrecognized head) (filter-modifiers sreifidom rest)])))))))

(require (submod "." digitama))
(require (submod "." parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test0 typed/racket
  (require (submod ".."))

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
           (define-values (result cpu real gc) ((inst time-apply Any) (thunk sexp ...) null))
           (printf "memory: ~a cpu time: ~a real time: ~a gc time: ~a~n"
                    (~size (- (current-memory-use) momery0) 'Bytes)
                    cpu real gc)
           (car result))]))
  
  (define-values (in out) (make-pipe))
  (define css-logger (make-logger 'css #false))
  (define css (thread (thunk (let forever ([/dev/log (make-log-receiver css-logger 'debug)])
                               (match (sync/enable-break /dev/log)
                                 [(vector _ message urgent _)
                                  (cond [(eof-object? urgent) (close-output-port out)]
                                        [else (displayln message out) (forever /dev/log)])])))))

  (collect-garbage)
  (define CSSes : (Listof Path-String)
    (for/list : (Listof Path-String)
      ([filename (in-directory (simplify-path (build-path (collection-file-path "manual-fonts.css" "scribble") 'up)))]
       #:when (and (regexp-match? "\\.css" filename)
                   (not (regexp-match? #px"manual-fonts\\.css$" filename))))
      filename))
  (parameterize ([current-logger css-logger])
    (time-run (map read-css-stylesheet CSSes)))

  (collect-garbage)
  (for ([t (in-range 32)])
    (time-run (for-each read-css-stylesheet CSSes)))

  (log-message css-logger 'debug "exit" eof)
  (copy-port in (current-output-port)))
  