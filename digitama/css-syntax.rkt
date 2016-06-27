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
(provide (all-from-out (submod "." digitama)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama typed/racket
  (provide (except-out (all-defined-out) symbol-ci=? keyword-ci=?))

  (require (for-syntax syntax/parse))
  
  (define-syntax (struct: stx)
    (syntax-case stx [:]
      [(_ id : ID rest ...)
       (with-syntax ([make-id (datum->syntax #'id (string->symbol (format "make-~a" (syntax-e #'id))))])
         #'(begin (define-type ID id)
                  (struct id rest ... #:prefab #:extra-constructor-name make-id)))]))
  
  (define-syntax (define-token stx)
    (syntax-parse stx
      [(_ id #:+ ID #:-> parent #:as Racket-Type (~optional (~seq #:=? maybe=?)) extra-fields ...)
       (with-syntax ([id-datum (datum->syntax #'id (string->symbol (format "~a-datum" (syntax-e #'id))))]
                     [id? (datum->syntax #'id (string->symbol (format "~a?" (syntax-e #'id))))]
                     [id=? (datum->syntax #'id (string->symbol (format "~a=?" (syntax-e #'id))))]
                     [id=:=? (datum->syntax #'id (string->symbol (format "~a=:=?" (syntax-e #'id))))]
                     [type=? (or (attribute maybe=?) #'(λ [v1 v2] (or (eq? v1 v2) (equal? v1 v2))))])
         #'(begin (struct: id : ID parent ([datum : Racket-Type] extra-fields ...))

                  (define id=? : (-> ID ID Boolean)
                    (lambda [left right]
                      (define left-value : Racket-Type (id-datum left))
                      (define right-value : Racket-Type (id-datum right))
                      (type=? left-value right-value)))
                  
                  (define id=:=? : (-> Any Racket-Type Boolean : #:+ ID)
                    (lambda [token racket-value]
                      (and (id? token)
                           (let ([css-value : Racket-Type (id-datum token)])
                             (type=? css-value racket-value)))))))]))

  (define-syntax (define-tokens stx)
    (syntax-case stx []
      [(_ token #:+ Token fields #:with [[subid #:+ SubID subfields] ...]
          [id #:+ ID #:-> parent #:as Type rest ...] ...)
       (with-syntax ([token? (datum->syntax #'token (string->symbol (format "~a?" (syntax-e #'token))))]
                     [token->datum (datum->syntax #'token (string->symbol (format "~a->datum" (syntax-e #'token))))]
                     [token-position (datum->syntax #'token (string->symbol (format "~a-position" (syntax-e #'token))))]
                     [token-position<? (datum->syntax #'token (string->symbol (format "~a-position<?" (syntax-e #'token))))])
         #'(begin (struct: token : Token fields)
                  (struct: subid : SubID token subfields) ...
                  (define-token id #:+ ID #:-> parent #:as Type rest ...) ...
                  
                  (define token->datum : (-> Token Any)
                    (lambda [token]
                      (define v (struct->vector token))
                      (vector-ref v (sub1 (vector-length v)))))
                  
                  (define token-position<? : (-> Token Token Boolean)
                    (lambda [token1 token2]
                      (< (token-position token1)
                         (token-position token1))))))]))

  ;;; https://drafts.csswg.org/css-syntax/#tokenization
  ;; https://drafts.csswg.org/css-syntax/#component-value
  ;; https://drafts.csswg.org/css-syntax/#current-input-token
  (define-tokens css-token #:+ CSS-Token ([source : Any] [line : Natural] [column : Natural] [position : Natural] [span : Natural])
    #:with [[css:numeric #:+ CSS:Numeric ([representation : String])]]
    [css:bad        #:+ CSS:Bad #:-> css-token #:as CSS-Bad]
    [css:close      #:+ CSS:Close #:-> css-token #:as Char]
    [css:cd         #:+ CSS:CD #:-> css-token #:as Symbol #:=? eq?]
    [css:||         #:+ CSS:|| #:-> css-token #:as Symbol #:=? (const #true)]
    [css:match      #:+ CSS:Match #:-> css-token #:as Char]
    [css:ident      #:+ CSS:Ident #:-> css-token #:as Symbol #:=? symbol-ci=?]
    [css:url        #:+ CSS:URL #:-> css-token #:as (U Bytes 'about:invalid)     [modifiers : (Listof CSS-URL-Modifier)]]
    [css:function   #:+ CSS:Function #:-> css-token #:as Symbol #:=? symbol-ci=? [arguments : (Listof CSS-Token)]]
    [css:block      #:+ CSS:Block #:-> css-token #:as Char                       [components : (Listof CSS-Token)]]
    [css:hash       #:+ CSS:Hash #:-> css-token #:as Keyword #:=? keyword-ci=?   [flag : (U 'id 'unrestricted)]]
    [css:@keyword   #:+ CSS:@Keyword #:-> css-token #:as Keyword #:=? keyword-ci=?]
    [css:string     #:+ CSS:String #:-> css-token #:as String #:=? string=?]
    [css:delim      #:+ CSS:Delim #:-> css-token #:as Char]
    [css:urange     #:+ CSS:URange #:-> css-token #:as (Pairof Index Index)]
    [css:integer    #:+ CSS:Integer #:-> css:numeric #:as Integer]
    [css:ratio      #:+ CSS:Ratio #:-> css:numeric #:as Positive-Exact-Rational]
    [css:number     #:+ CSS:Number #:-> css:numeric #:as Real]
    [css:percentage #:+ CSS:Percentage #:-> css:numeric #:as Real #:=? (λ [f1 f2] (= (* f1 0.01) f2))]
    [css:dimension  #:+ CSS:Dimension #:-> css:numeric #:as (Pairof Real Symbol)
                    #:=? (λ [d1 d2] (and (= (car d1) (car d2)) (symbol-ci=? (cdr d1) (cdr d2))))]
    [css:whitespace #:+ CSS:WhiteSpace #:-> css-token #:as (U Bytes Char)
                    #:=? (λ [ws1 ws2] (cond [(and (char? ws1) (char? ws2)) (char=? ws1 ws2 #\space)]  ; whitespace
                                            [(and (bytes? ws1) (bytes? ws2)) (bytes=? ws1 ws2)]       ; comment
                                            [else #false]))])

  (struct: css-bad : CSS-Bad ([token : Symbol] [datum : Any]))
  (struct: css:bad:eof : CSS:Bad:EOF css-bad ())
  (struct: css:bad:eol : CSS:Bad:EOL css-bad ())
  (struct: css:bad:char : CSS:Bad:Char css-bad ())
  (struct: css:bad:blank : CSS:Bad:Blank css-bad ())
  (struct: css:bad:range : CSS:Bad:Range css-bad ())
  (struct: css:bad:range:index : CSS:Bad:Range:Index css-bad ())
  (struct: css:bad:stdin : CSS:Bad:StdIn css-bad ())
  
  ;;; https://drafts.csswg.org/css-syntax/#parsing
  (define-type CSS-StdIn (U Input-Port Path-String Bytes (Listof CSS-Token)))
  (define-type CSS-URL-Modifier (U CSS:Ident CSS:Function CSS:URL))
  (define-type CSS-Syntax-Any (U CSS-Token EOF))
  (define-type CSS-Syntax-Terminal (U CSS:Delim CSS:Close EOF))
  (define-type CSS-Syntax-Rule (U CSS-Qualified-Rule CSS-@Rule))
  (define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule))

  (define-type CSS-Syntax-Error exn:fail:syntax)
  (define-type Make-CSS-Syntax-Error (-> String Continuation-Mark-Set (Listof Syntax) CSS-Syntax-Error))

  (struct exn:css exn:fail:syntax ())
  (struct exn:css:unrecognized exn:css ())
  (struct exn:css:misplaced exn:css:unrecognized ())
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
      (define err : CSS-Syntax-Error
        (with-handlers ([exn:fail:syntax? (λ [[e : exn:fail:syntax]] e)])
          (raise-syntax-error 'exn:css:syntax (format "~a" (object-name exn:css)) #false #false
                              (for/list : (Listof (Syntaxof Any)) ([token (in-list tokens)])
                                (datum->syntax #false (css-token->datum token)
                                               (list (css-token-source token) (css-token-line token) (css-token-column token)
                                                     (css-token-position token) (css-token-span token)))))))
      (define errobj (exn:css (exn-message err) (exn-continuation-marks err) (exn:fail:syntax-exprs err)))
      (log-message (current-logger) 'warning 'exn:css:syntax
                   (cond [(null? tokens) (format "~a: ~a" (object-name exn:css) eof)]
                         [else (let ([token (car tokens)])
                                 (format "~a:~a:~a: ~a: ~a" (css-token-source token)
                                         (css-token-line token) (css-token-column token)
                                         (object-name exn:css) (map css-token->datum tokens)))])
                   errobj)
      errobj))

  (define-syntax (css-throw-syntax-error stx)
    (syntax-case stx []
      [(_ sexp ...)
       #'(raise (css-make-syntax-error sexp ...))]))

  (struct: css-@rule : CSS-@Rule ([name : CSS:@Keyword] [prelude : (Listof CSS-Token)] [block : (Option CSS:Block)]))
  (struct: css-qualified-rule : CSS-Qualified-Rule ([prelude : (Listof CSS-Token)] [block : CSS:Block]))
  (struct: css-declaration : CSS-Declaration ([name : CSS:Ident] [values : (Listof CSS-Token)]
                                                                 [important? : Boolean] [case-sentitive? : Boolean]))

  ;;  https://drafts.csswg.org/selectors/#grammar
  ;;  https://drafts.csswg.org/selectors/#structure
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
  
  ;; https://drafts.csswg.org/css-syntax/#css-stylesheets
  ;; https://drafts.csswg.org/cssom/#css-object-model
  (define-type CSS-Features (HashTable Symbol (U Symbol Integer Real)))
  (define-type CSS-Imports (HashTable Bytes CSS-StyleSheet))
  (define-type CSS-NameSpace (HashTable Symbol String))
  
  (struct: css-style-rule : CSS-Style-Rule ([selectors : (Listof CSS-Complex-Selector)] [properties : (Listof CSS-Declaration)]))

  (struct: css-stylesheet : CSS-StyleSheet
    ([location : Any]
     [imports : CSS-Imports]
     [namespaces : CSS-NameSpace]
     [rules : (Listof CSS-Grammar-Rule)]))

  ;; https://drafts.csswg.org/css-conditional/#at-supports
  ;; https://drafts.csswg.org/mediaqueries/#mq-syntax
  ;; https://drafts.csswg.org/mediaqueries/#media-types
  ;; https://drafts.csswg.org/mediaqueries/#mq-features
  (define-type CSS-Media-Query (U CSS-Media-Type (Pairof CSS-Media-Type CSS-Feature-Query) CSS-Syntax-Error))
  (define-type CSS-Feature-Query (U CSS-Not CSS-And CSS-Or CSS-Media-Feature CSS-Declaration CSS-Syntax-Error))
  (define-type CSS-Media-Value (U CSS:Integer CSS:Number CSS:Dimension CSS:Ident CSS:Ratio))

  (struct: css-media-type : CSS-Media-Type ([name : CSS:Ident] [only? : Boolean]))
  (struct: css-media-feature : CSS-Media-Feature ([name : CSS:Ident] [value : CSS-Media-Value] [operator : Char]))
  (struct: css-not : CSS-Not ([condition : CSS-Feature-Query]))
  (struct: css-and : CSS-And ([conditions : (Listof CSS-Feature-Query)]))
  (struct: css-or : CSS-Or ([conditions : (Listof CSS-Feature-Query)]))

  (define css-media-value? : (-> Any Boolean : #:+ CSS-Media-Value)
    (lambda [v]
      (or (css:ident? v)
          (css:integer? v) (css:number? v)
          (css:dimension? v) (css:ratio? v))))
  
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

  (define-syntax (make-token stx)
    (syntax-case stx []
      [(_ src make-css:token datum ...)
       #'(let-values ([(start-position) (css-srcloc-pos src)]
                      [(line column position) (port-next-location (css-srcloc-in src))])
           (make-css:token (or (object-name (css-srcloc-in src)) '/dev/cssin)
                           (or (css-srcloc-line src) line 0)
                           (or (css-srcloc-col src) column 0)
                           (or start-position 0)
                           (cond [(not (and (integer? position) (integer? start-position))) 0]
                                 [else (max (- position start-position) 0)])
                           datum ...))]))

  (define-syntax (remake-token stx)
    (syntax-case stx []
      [(_ [start-token end-token] make-css:token datum ...)
       #'(make-css:token (css-token-source start-token)
                         (css-token-line start-token) (css-token-column start-token)
                         (css-token-position start-token)
                         (max (- (+ (css-token-position end-token) (css-token-span end-token))
                                 (css-token-position start-token)) 0)
                         datum ...)]
      [(_ here-token make-css:token datum ...)
       #'(remake-token [here-token here-token] make-css:token datum ...)]))

  (define make-bad-token : (-> CSS-Srcloc (-> Symbol Any CSS-Bad) Struct-TypeTop Any CSS:Bad)
    (lambda [src css:bad:sub token datum]
      (define bad (make-token src css:bad (css:bad:sub (assert (object-name token) symbol?) datum)))
      (log-message (current-logger) 'warning 'exn:css:read
                   (format "~a:~a:~a: ~a: ~a: ~s" (css-token-source bad)
                           (css-token-line bad) (css-token-column bad)
                           (object-name css:bad:sub) (object-name token) datum)
                   bad)
      bad))
  
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
                    [(#\' #\") (css-consume-string-token srcloc ch)]
                    [(#\+ #\.) (css-consume-numeric-token srcloc ch)]
                    [(#\^ #\$ #\| #\~ #\*) (css-consume-match-token srcloc ch)]
                    [(#\#) (css-consume-hash-token srcloc)]
                    [(#\@) (css-consume-@keyword-token srcloc)]
                    [(#\/) (css-consume-comment-token srcloc)]
                    [(#\< #\-) (css-consume-cd-token srcloc ch)]
                    [(#\null) (make-token srcloc css:delim #\uFFFD)]
                    [(#\) #\] #\}) (make-token srcloc css:close ch)]
                    [else (make-token srcloc css:delim ch)])])))

  (define css-consume-cd-token : (-> CSS-Srcloc Char CSS-Token)
    ;;; https://drafts.csswg.org/css-syntax/#CDO-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#CDC-token-diagram
    (lambda [srcloc open/close]
      (define css : Input-Port (css-srcloc-in srcloc))
      (if (char=? open/close #\<)
          (let ([cdo : (U EOF String) (peek-string 3 0 css)])
            (cond [(and (string? cdo) (string=? cdo "!--")) (read-string 3 css) (make-token srcloc css:cd '<!--)]
                  [else (make-token srcloc css:delim #\<)]))
          (let ([cdc : (U EOF String) (peek-string 2 0 css)])
            (cond [(eof-object? cdc) (make-token srcloc css:delim #\-)]
                  [(string=? cdc "->") (read-string 2 css) (make-token srcloc css:cd '-->)]
                  [(css-identifier-prefix? #\- (string-ref cdc 0) (string-ref cdc 1)) (css-consume-ident-token srcloc #\-)]
                  [else (css-consume-numeric-token srcloc #\-)])))))

  (define css-consume-comment-token : (-> CSS-Srcloc (U CSS:WhiteSpace CSS:Delim CSS:Bad))
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (cond [(or (eof-object? ch1) (not (char=? ch1 #\*))) (make-token srcloc css:delim #\/)]
            [(regexp-match #px".*?\\*/" css) => (λ [**/] (make-token srcloc css:whitespace (bytes-append #"/" (car **/))))]
            [else (make-bad-token srcloc css:bad:eof struct:css:whitespace #"/*")])))

  (define css-consume-whitespace-token : (-> CSS-Srcloc CSS:WhiteSpace)
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (make-token srcloc css:whitespace #\space)))
  
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
                           (make-token srcloc css:ident (string->symbol name))]
                          [(or (not (string-ci=? name "url")) (regexp-match-peek #px"^.\\s*[\"']" css))
                           (read-char css) (make-token srcloc css:function (string->unreadable-symbol name) null)]
                          [else (read-char css) (css-consume-url-token srcloc)]))])))
      
  (define css-consume-string-token : (-> CSS-Srcloc Char (U CSS:String CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-string-token
    (lambda [srcloc quotation]
      (define css : Input-Port (css-srcloc-in srcloc))
      (let consume-string-token : (U CSS:String CSS:Bad) ([chars : (Listof Char) null])
        (define ch : (U EOF Char) (read-char css))
        (cond [(or (eof-object? ch) (char=? ch quotation))
               (when (eof-object? ch) (make-bad-token srcloc css:bad:eof struct:css:string (list->string (reverse chars))))
               (make-token srcloc css:string (list->string (reverse chars)))]
              [(char=? ch #\newline)
               (make-bad-token srcloc css:bad:eol struct:css:string (list->string (reverse chars)))]
              [(not (char=? ch #\\))
               (consume-string-token (cons ch chars))]
              [else (let ([next (peek-char css)])
                      (cond [(eof-object? next) (consume-string-token chars)]
                            [(char=? next #\newline) (read-char css) (consume-string-token (cons ch chars))]
                            [else (consume-string-token (cons (css-consume-escaped-char css) chars))]))]))))

  (define css-consume-numeric-token : (-> CSS-Srcloc Char (U CSS:Numeric CSS:Delim CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [srcloc sign/digit]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (cond [(not (css-number-prefix? sign/digit ch1 ch2)) (make-token srcloc css:delim sign/digit)]
            [else (let-values ([(n representation) (css-consume-number css sign/digit)])
                    (let ([ch1 : (U EOF Char) (peek-char css 0)]
                          [ch2 : (U EOF Char) (peek-char css 1)]
                          [ch3 : (U EOF Char) (peek-char css 2)])
                      (cond [(css-identifier-prefix? ch1 ch2 ch3)
                             (define unit : Symbol (string->symbol (string-downcase (css-consume-name css #false))))
                             (make-token srcloc css:dimension representation (cons n unit))]
                            [(and (char? ch1) (char=? ch1 #\%)) (read-char css)
                             (make-token srcloc css:percentage representation n)]
                            [(exact-integer? n) (make-token srcloc css:integer representation n)]
                            [else (make-token srcloc css:number representation n)])))])))

  (define css-consume-url-token : (-> CSS-Srcloc (U CSS:URL CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-url-token
    ;;; https://drafts.csswg.org/css-values/#urls
    ;;; https://drafts.csswg.org/css-values/#url-empty
    ;;; https://drafts.csswg.org/css-values/#about-invalid
    (lambda [srcloc]
      (define chars->url : (-> (Listof Char) Bytes) (λ [chars] (string->bytes/utf-8 (list->string chars))))
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (define start : (U EOF Char) (read-char css))
      (cond [(or (eof-object? start) (char=? start #\)))
             (when (eof-object? start) (make-bad-token srcloc css:bad:eof struct:css:url #""))            
             (make-token srcloc css:url 'about:invalid null)]
            [else (let consume-url-token : (U CSS:URL CSS:Bad) ([chars (list start)])
                    (define ch : (U EOF Char) (read-char css))
                    (cond [(or (eof-object? ch) (char=? ch #\)))
                           (when (eof-object? ch) (make-bad-token srcloc css:bad:eof struct:css:url (chars->url (reverse chars))))
                           (make-token srcloc css:url (chars->url (reverse chars)) null)]
                          [(char-whitespace? ch)
                           (css-consume-whitespace css)
                           (define end : (U EOF Char) (read-char css))
                           (define uri : Bytes (chars->url (reverse chars)))
                           (cond [(or (eof-object? end) (char=? end #\))) (make-token srcloc css:url uri null)]
                                 [else (css-consume-bad-url-remnants css (make-bad-token srcloc css:bad:blank struct:css:url uri))])]
                          [(css-valid-escape? ch (peek-char css))
                           (consume-url-token (cons (css-consume-escaped-char css) chars))]
                          [(or (memq ch '(#\\ #\" #\' #\()) (css-char-non-printable? ch))
                           (css-consume-bad-url-remnants css (make-bad-token srcloc css:bad:char struct:css:url ch))]
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
      (cond [(and (index? start) (index? end) (<= start end #x10FFFF)) (make-token srcloc css:urange (cons start end))]
            [(> end #x10FFFF) (make-bad-token srcloc css:bad:range:index struct:css:urange end)]
            [else (make-bad-token srcloc css:bad:range struct:css:urange (cons start end))])))

  (define css-consume-hash-token : (-> CSS-Srcloc (U CSS:Hash CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#hash-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (or (css-char-name? ch1) (css-valid-escape? ch1 ch2))
          (let ([name : String (css-consume-name (css-srcloc-in srcloc) #\#)])
            (make-token srcloc css:hash (string->keyword name)
                        (if (css-identifier-prefix? ch1 ch2 ch3) 'id 'unrestricted)))
          (make-token srcloc css:delim #\#))))

  (define css-consume-@keyword-token : (-> CSS-Srcloc (U CSS:@Keyword CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#at-keyword-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (css-identifier-prefix? ch1 ch2 ch3)
          (let ([name : String (css-consume-name (css-srcloc-in srcloc) #\@)])
            (make-token srcloc css:@keyword (string->keyword name)))
          (make-token srcloc css:delim #\@))))

  (define css-consume-match-token : (-> CSS-Srcloc Char (U CSS:Match CSS:|| CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#include-match-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#column-token-diagram
    (lambda [srcloc prefix]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch : (U EOF Char) (peek-char css))
      (cond [(and (char=? prefix #\|) (eq? ch #\|)) (read-char css) (make-token srcloc css:|| '||)]
            [(eq? ch #\=) (read-char css) (make-token srcloc css:match prefix)]
            [else (make-token srcloc css:delim prefix)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-whitespace : (-> Input-Port Void)
    (lambda [css]
      (regexp-match #px"\\s*" css)
      (void)))
  
  (define css-consume-name : (-> Input-Port (Option Char) String)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-name
    (lambda [css leader]
      (let consume-name : String ([srahc : (Listof Char) (if leader (list leader) null)])
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
    (lambda [/dev/cssin [features : CSS-Features (make-hasheq)] [imports : CSS-Imports (make-hash)]]
      (define namespaces : CSS-NameSpace (make-hasheq))
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
                               (syntax->grammar selur (if (pair? subrules) (append subrules rules) rules) #false #false)]
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
    (lambda [/dev/cssin]
      (define all : CSS-Media-Type (make-css-media-type (make-css:ident (object-name /dev/cssin) 0 0 0 0 'all) #true))
      (define maybe-eof (css-peek-syntax/skip-whitespace /dev/cssin))
      (if (eof-object? maybe-eof) (list all)
          (for/list : (Listof CSS-Media-Query) ([entry (in-list (css-consume-componentses /dev/cssin #:omit-comma? #true))])
            (with-handlers ([exn:fail:syntax? (λ [[e : exn:fail:syntax]] e)])
              (define-values (token tokens) (css-car entry))
              (define-values (next rest) (css-car tokens))
              (cond [(css:ident=:=? token 'not)
                     (cond [(css:ident? next) (css-components->media-type+query next #false rest)]
                           [else (cons all (css-components->negation token tokens #true))])]
                    [(css:ident? token)
                     (define-values (maybe-type maybe-and) (if (css:ident=:=? token 'only) (values next rest) (values token tokens)))
                     (cond [(eof-object? maybe-type) (css-make-syntax-error exn:css:missing-identifier maybe-type)]
                           [(css:ident? maybe-type) (css-components->media-type+query maybe-type #true maybe-and)]
                           [else (css-make-syntax-error exn:css:unrecognized maybe-type)])]
                    [else (cons all (css-components->feature-query entry #:@media? #true))]))))))

  (define-css-parser-entry css-parse-feature-query :-> CSS-Feature-Query
    ;;; https://drafts.csswg.org/mediaqueries/#media-types
    ;;; https://drafts.csswg.org/css-conditional/#at-supports
    (lambda [/dev/cssin]
      (define-values (conditions _) (css-consume-components /dev/cssin))
      (with-handlers ([exn:fail:syntax? (λ [[e : exn:fail:syntax]] e)])
        (css-components->feature-query conditions #:@media? #false))))
  
  (define-css-parser-entry css-parse-selectors :-> (U (Listof CSS-Complex-Selector) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-selector
    ;;; https://drafts.csswg.org/selectors/#selector-list
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [/dev/cssin]
      (with-handlers ([exn:fail:syntax? (λ [[e : exn:fail:syntax]] e)])
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
                [(css:url? uri) (~a (css:url-datum uri))]
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
      (define prelude : (Listof CSS-Token) (css-@rule-prelude media))
      (define maybe-block : (Option CSS:Block) (css-@rule-block media))
      (cond [(false? maybe-block) (css-make-syntax-error exn:css:missing-block name)]
            [else (css-parse-rules (css:block-components maybe-block))])))
  
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
            [else (css-make-syntax-error exn:css:missing-block reconsumed)])))

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
      (remake-token [open end-token] css:block (css:delim-datum open) components)))

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
                    (remake-token [func end-token] css:function
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
        (cond [(eof-object? terminal) (reverse (cons components componentses))]
              [else (consume-components (cons components componentses))]))))

  (define css-components->declaration : (-> CSS:Ident (Listof CSS-Token) (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
    ;;; https://drafts.csswg.org/css-syntax/#typedef-declaration-value
    ;;; https://drafts.csswg.org/css-cascade/#importance
    ;;; https://drafts.csswg.org/css-variables/#defining-variables
    (lambda [id-token components]
      (define-values (maybe-: value-list) (css-car components))
      (cond [(not (css:delim=:=? maybe-: #\:)) (css-make-syntax-error exn:css:missing-colon id-token)]
            [else (let verify : (U CSS-Declaration CSS-Syntax-Error)
                    ([lla : (Listof CSS-Token) null]
                     [info : (U False CSS-Syntax-Error CSS:Delim) #false]
                     [rest : (Listof CSS-Token) (cdr value-list)])
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
                          [else (make-css-declaration id-token (reverse lla) (css:delim? info)
                                                      (string-prefix? (symbol->string (css:ident-datum id-token)) "--"))]))])))

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
    (lambda [media not? conditions]
      (define type (css:ident-datum media))
      (define-values (maybe-and maybe-conditions) (css-car conditions))
      (cond [(memq type '(only not and or)) (css-make-syntax-error exn:css:misplaced media)]
            [(eof-object? maybe-and) (make-css-media-type media not?)]
            [(not (css:ident=:=? maybe-and 'and)) (css-make-syntax-error exn:css:unrecognized maybe-and)]
            [(css-null? maybe-conditions) (css-make-syntax-error exn:css:missing-feature maybe-and)]
            [else (cons (make-css-media-type media not?) (css-components->junction maybe-conditions 'and #false #true))])))
  
  (define css-components->feature-query : (-> (Listof CSS-Token) #:@media? Boolean CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-only
    ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
    (lambda [conditions #:@media? media?]
      (define-values (token rest) (css-car conditions))
      (define-values (operator chain) (css-car rest))
      (cond [(eof-object? token) (css-throw-syntax-error exn:css:missing-feature conditions)]
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
             (define subany (css:block-components condition))
             (define-values (name any-values) (css-car subany))
             (define-values (op value-list) (css-car any-values))
             (cond [(css:block=:=? name #\() (css-components->feature-query subany #:@media? media?)]
                   [(css:ident=:=? name 'not) (css-components->negation name any-values media?)]
                   [(and (css:ident? name) (eof-object? op)) (css-media-feature name name #\?)]
                   [(and (css:ident? name) (css:delim=:=? op #\:))
                    (define descriptor (css-components->declaration name any-values))
                    (cond [(exn? descriptor) (if media? (css-throw-syntax-error exn:css:enclosed condition) (raise descriptor))]
                          [(and media?) (css-declaration->media-query descriptor condition)]
                          [else descriptor])]
                   [(and media?) (css-components->media-range-query subany condition)]
                   [(and (css-null? subany) 'bug:=> (eof-object? name)) (css-throw-syntax-error exn:css:empty condition)]
                   [else (css-throw-syntax-error exn:css:unrecognized condition)])]
            [(css:function? condition) (css-throw-syntax-error exn:css:enclosed condition)]
            [else (css-throw-syntax-error exn:css:unrecognized condition)])))

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
    (lambda [conditions op leader media?]
      (define make-junction (if (eq? op 'and) make-css-and make-css-or))
      (let components->junction : (U CSS-And CSS-Or)
        ([junctions : (Listof CSS-Token) (if (false? leader) null (list leader))]
         [rest-conditions : (Listof CSS-Token) conditions])
        (define-values (condition rest) (css-car rest-conditions))
        (define-values (token others) (css-car rest))
        (cond [(eof-object? condition) (make-junction (map (curry css-component->feature-query media?) (reverse junctions)))]
              [(css:ident=:=? condition 'not) (css-throw-syntax-error exn:css:misplaced condition)]
              [(or (eof-object? token) (css:ident=:=? token op)) (components->junction (cons condition junctions) others)]
              [(or (css:ident=:=? token 'and) (css:ident=:=? token 'or)) (css-throw-syntax-error exn:css:misplaced token)]
              [else (css-throw-syntax-error exn:css:overconsumption token)]))))

  (define css-components->media-range-query : (-> (Listof CSS-Token) CSS:Block CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
    (lambda [components broken-condition]
      (with-handlers ([exn? (λ _ (css-throw-syntax-error exn:css:enclosed broken-condition))])
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
              [(css:ident? value0) (make-css-media-feature value0 value1 op0)]
              [(and (eof-object? d1) (css:ident? value1)) (make-css-media-feature value1 value0 po0)]
              [(not (css:ident? value1)) (css-throw-syntax-error exn:css:missing-identifier value1)]
              [(or (eof-object? value2) (css:ident? value2)) (css-throw-syntax-error exn:css:missing-value rest2)]
              [(not (css-null? terminal)) (css-throw-syntax-error exn:css:overconsumption terminal)]
              [(not (css:delim=? d0 d1)) (css-throw-syntax-error exn:css:malformed (list d0 d1))]
              [else (make-css-and (list (make-css-media-feature value1 value0 po0)
                                        (make-css-media-feature value1 value2 op1)))]))))
  
  (define css-declaration->media-query : (-> CSS-Declaration CSS:Block CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-ratio
    (lambda [descriptor broken-condition]
      (define property : CSS:Ident (css-declaration-name descriptor))
      (define-values (feature-name op)
        (let ([name (symbol->string (css:ident-datum property))])
          (cond [(string-prefix? name "min-") (values (remake-token property css:ident (string->symbol (substring name 4))) #\≥)]
                [(string-prefix? name "max-") (values (remake-token property css:ident (string->symbol (substring name 4))) #\≤)]
                [else (values property #\=)])))
      (with-handlers ([exn? (λ _ (css-throw-syntax-error exn:css:enclosed broken-condition))])
        (define-values (media-value rest) (css-car-media-value (css-declaration-values descriptor)))
        (cond [(eof-object? media-value) (css-throw-syntax-error exn:css:missing-value property)]
              [(css-null? rest) (make-css-media-feature feature-name media-value op)]
              [else (css-throw-syntax-error exn:css:overconsumption rest)]))))

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
    (lambda [components]
      (define-values (value rest) (css-car components))
      (define-values (maybe-/ maybe-rest) (css-car rest))
      (define-values (maybe-int terminal) (css-car maybe-rest))
      (cond [(css:delim=:=? maybe-/ #\/)
             (values (if (and (css:integer? value) (positive? (css:integer-datum value))
                              (css:integer? maybe-int) (positive? (css:integer-datum maybe-int)))
                         (let ([width : Positive-Integer (max (css:integer-datum value) 1)]
                               [height : Positive-Integer (max (css:integer-datum maybe-int) 1)])
                           (remake-token [value maybe-int] css:ratio (format "~a/~a" width height) (/ width height)))
                         (css-throw-syntax-error exn:css:malformed (filter css-token? (list value maybe-/ maybe-int))))
                     terminal)]
            [(css-media-value? value) (values value rest)]
            [(eof-object? value) (values eof rest)]
            [else (values (css-throw-syntax-error exn:css:unrecognized value) rest)])))

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
                     (make-css-universal-selector (remake-token reconsumed css:ident '_)))]
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
                     [else (remake-token reconsumed css:ident '*)]))
             (cond [(css:ident? next2) (make-css-type-selector next2 ns)]
                   [(css:delim=:=? next2 #\*) (make-css-universal-selector ns)]
                   [else (css-throw-syntax-error exn:css:missing-identifier next)])]
            [else (make-css-type-selector reconsumed (remake-token reconsumed css:ident '_))])))

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
               (cond [(css:ident? 1st) (values 1st (remake-token 1st css:ident '_) rest1)]
                     [else (css-throw-syntax-error exn:css:missing-identifier 1st)])]
              [(or (eof-object? 3rd) (css:match? 3rd) (css:delim=:=? 3rd #\=))
               (cond [(and (css:delim=:=? 1st #\|) (css:ident? 2nd)) (values 2nd #false rest2)]
                     [(css:delim=:=? 2nd #\|) (css-throw-syntax-error exn:css:missing-identifier 2nd)]
                     [else (css-throw-syntax-error exn:css:unrecognized 1st)])]
              [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:delim=:=? 2nd #\|) (css:ident? 3rd))
               (values 3rd (if (css:ident? 1st) 1st (remake-token 1st css:ident '*)) rest3)]
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
              [(css:ident? value) (remake-token value css:string (symbol->string (css:ident-datum value)))]
              [(css-token? value) (css-throw-syntax-error exn:css:unrecognized value)]
              [else (remake-token attr css:string "placeholder")]))
      (define ci? : Boolean (css:ident? i))
      (cond [(eof-object? op) (make-css-attribute-selector attr namespace)]
            [(css:delim=:=? op #\=) (make-css-attribute=selector attr namespace op val ci?)]
            [(css:match? op) (make-css-attribute=selector attr namespace (remake-token op css:delim (css:match-datum op)) val ci?)]
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
            [else (make-bad-token (css-srcloc css #false #false #false)
                                  css:bad:stdin struct:css-token stx)])))

  (define css-peek-syntax : (->* (Input-Port) (Natural) CSS-Syntax-Any)
    (lambda [css [skip 0]]
      (define stx (peek-char-or-special css skip))
      (cond [(or (eof-object? stx) (css-token? stx)) stx]
            [else (make-bad-token (css-srcloc css #false #false #false)
                                  css:bad:stdin struct:css-token stx)])))
  
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
          (remake-token url css:url 'about:invalid null)
          (let ([datum (if (css:string=:=? href "") 'about:invalid (string->bytes/utf-8 (css:string-datum href)))])
            (let filter-modifiers : CSS:URL ([sreifidom : (Listof CSS-URL-Modifier) null]
                                             [tail : (Listof CSS-Token) modifiers])
              (define-values (head rest) (css-car tail))
              (cond [(eof-object? head) (remake-token url css:url datum (reverse sreifidom))]
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
  