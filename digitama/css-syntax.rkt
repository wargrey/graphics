#lang typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.w3.org/Style/CSS/specs.en.html                                                   ;;;
;;;                                                                                             ;;;
;;; https://drafts.csswg.org/css-syntax                                                         ;;;
;;; https://drafts.csswg.org/css-values                                                         ;;;
;;; https://drafts.csswg.org/css-cascade                                                        ;;;
;;; https://drafts.csswg.org/selectors                                                          ;;;
;;; https://drafts.csswg.org/css-namespaces                                                     ;;;
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
         css-parse-selectors)

(define read-css-stylesheet : (->* () (CSS-StdIn) CSS-StyleSheet)
  ;;; https://drafts.csswg.org/css-syntax/#css-stylesheets
  ;;; https://drafts.csswg.org/css-syntax/#charset-rule
  (lambda [[/dev/stdin (current-input-port)]]
    (define rules : (Listof CSS-Grammar-Rule)
      (let syntax->grammar : (Listof CSS-Grammar-Rule)
        ([selur : (Listof CSS-Grammar-Rule) null]
         [rules : (Listof CSS-Syntax-Rule) (css-parse-stylesheet /dev/stdin)])
        (if (null? rules) (reverse selur)
            (let-values ([(rule rest) (values (car rules) (cdr rules))])
              (if (css-qualified-rule? rule)
                  (syntax->grammar (css-cons (css-qualified-rule->style-rule rule) selur) rest)
                  (if (css:@keyword=:=? (css-@rule-name rule) '#:@charset)
                      (let ([invalid (css-make-syntax-error exn:css:unrecognized (css-@rule-name rule))])
                        (syntax->grammar selur rest))
                      (syntax->grammar (cons rule selur) rest)))))))
    (make-css-stylesheet (or (object-name /dev/stdin) '/dev/cssin) rules)))

(define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule (U CSS-Style-Rule CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-syntax/#style-rules
  ;;; https://drafts.csswg.org/selectors/#invalid
  (lambda [qr]
    (define prelude : CSS-Component-Values (css-qualified-rule-prelude qr))
    (define selectors : (U (Listof CSS-Complex-Selector) CSS-Syntax-Error) (css-parse-selectors prelude))
    (cond [(exn? selectors) selectors]
          [(null? selectors) (css-make-syntax-error exn:css:empty (css-simple-block-open (css-qualified-rule-block qr)))]
          [else (let ([components (css-simple-block-components (css-qualified-rule-block qr))])
                  (define srotpircsed : (Listof CSS-Declaration)
                    (for/fold ([descriptors : (Listof CSS-Declaration) null])
                              ([mixed-list (in-list (css-parse-declarations components))])
                      (cond [(css-declaration? mixed-list) (cons mixed-list descriptors)]
                            [else (css-make-syntax-error exn:css:unrecognized (css-@rule-name mixed-list))
                                  descriptors])))
                  (make-css-style-rule selectors (reverse srotpircsed)))])))

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
      [(_ id #:+ ID #:-> parent #:as Racket-Type (~optional (~seq #:on-datum maybe-datum)) (~optional (~seq #:=? maybe=?)))
       (with-syntax ([id-datum (datum->syntax #'id (string->symbol (format "~a-datum" (syntax-e #'id))))]
                     [id? (datum->syntax #'id (string->symbol (format "~a?" (syntax-e #'id))))]
                     [id=:=? (datum->syntax #'id (string->symbol (format "~a=:=?" (syntax-e #'id))))]
                     [type=? (or (attribute maybe=?) #'(λ [v1 v2] (or (eq? v1 v2) (equal? v1 v2))))])
         #'(begin (struct: id : ID parent ([datum : Racket-Type]))
                  
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
  (define-tokens css-token #:+ CSS-Token ([source : Any] [line : Natural] [column : Natural] [position : Natural] [span : Natural])
    #:with [[css:numeric #:+ CSS:Numeric ([representation : String])]]
    [css:bad #:+ CSS:Bad #:-> css-token #:as CSS-Bad]
    [css:cd #:+ CSS:CD #:-> css-token #:as Symbol #:=? eq?]
    [css:|| #:+ CSS:|| #:-> css-token #:as Symbol #:=? (const #true)]
    [css:match #:+ CSS:Match #:-> css-token #:as Char]
    [css:ident #:+ CSS:Ident #:-> css-token #:as Symbol #:=? symbol-ci=?]
    [css:function #:+ CSS:Function #:-> css-token #:as Symbol #:=? symbol-ci=?]
    [css:hash #:+ CSS:Hash #:-> css-token #:as (Pairof Keyword (U 'id 'unrestricted))]
    [css:@keyword #:+ CSS:@Keyword #:-> css-token #:as Keyword #:=? keyword-ci=?]
    [css:string #:+ CSS:String #:-> css-token #:as String #:=? string=?]
    [css:url #:+ CSS:URL #:-> css-token #:as (U Bytes 'about:invalid)]
    [css:delim #:+ CSS:Delim #:-> css-token #:as Char]
    [css:urange #:+ CSS:URange #:-> css-token #:as (Pairof Index Index)]
    [css:integer #:+ CSS:Integer #:-> css:numeric #:as Integer]
    [css:number #:+ CSS:Number #:-> css:numeric #:as Real]
    [css:percentage #:+ CSS:Percentage #:-> css:numeric #:as Real #:=? (λ [f1 f2] (= (* f1 0.01) f2))]
    [css:dimension #:+ CSS:Dimension #:-> css:numeric #:as (Pairof Real Symbol)
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
  (define-type CSS-StdIn (U Input-Port Path-String Bytes CSS-Component-Values))
  (define-type CSS-Syntax-Any (U CSS-Component-Value EOF))
  (define-type CSS-Syntax-Rule (U CSS-Qualified-Rule CSS-@Rule))
  (define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule))
  (define-type CSS-Syntax-Error exn:fail:syntax)

  (struct exn:css exn:fail:syntax ())
  (struct exn:css:eof exn:css ())
  (struct exn:css:non-identifier exn:css ())
  (struct exn:css:empty exn:css ())
  (struct exn:css:unrecognized exn:css ())
  (struct exn:css:overconsumption exn:css ())
  (struct exn:css:missing-identifier exn:css ())
  (struct exn:css:missing-colon exn:css ())
  (struct exn:css:missing-block exn:css ())
  (struct exn:css:missing-value exn:css ())

  (define css-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) CSS-Syntax-Error) CSS-Syntax-Any CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#style-rules
    ;;; https://drafts.csswg.org/selectors/#invalid
    (lambda [exn:css v]
      (define tokens : (Listof CSS-Token)
        (cond [(eof-object? v) null]
              [(css-function? v) (cons (css-function-name v) (filter css:ident? (css-function-arguments v)))]
              [(css-simple-block? v) (filter css:ident? (css-simple-block-components v))]
              [else (list v)]))
      (define errobj : CSS-Syntax-Error
        (with-handlers ([exn:fail:syntax? (λ [[e : exn:fail:syntax]] e)])
          (raise-syntax-error 'exn:css:syntax (format "~a" (object-name exn:css)) #false #false
                              (for/list : (Listof (Syntaxof Any)) ([token (in-list tokens)])
                                (datum->syntax #false (css-token->datum token)
                                               (list (css-token-source token) (css-token-line token) (css-token-column token)
                                                     (css-token-position token) (css-token-span token)))))))
      (log-message (current-logger) 'warning 'exn:css:syntax
                   (cond [(null? tokens) (format "~a" (object-name exn:css))]
                         [else (let ([token (car tokens)])
                                 (format "~a:~a:~a: ~a: ~a" (css-token-source token)
                                         (css-token-line token) (css-token-column token)
                                         (object-name exn:css) (css-token->datum token)))])
                   errobj)
           errobj))

  ;;  https://drafts.csswg.org/css-syntax/#component-value
  (define-type CSS-Component-Value (U CSS-Token CSS-Simple-Block CSS-Function))
  (define-type CSS-Component-Values (Listof CSS-Component-Value))

  (define css-component-value? : (-> Any Boolean : #:+ CSS-Component-Value)
    (lambda [v]
      (or (css-function? v)
          (css-simple-block? v)
          (css-token? v))))
  
  (struct: css-function : CSS-Function ([name : CSS:Function] [arguments : CSS-Component-Values]))
  (struct: css-simple-block : CSS-Simple-Block ([open : CSS:Delim] [components : CSS-Component-Values]))
  (struct: css-@rule : CSS-@Rule ([name : CSS:@Keyword] [prelude : CSS-Component-Values] [block : (Option CSS-Simple-Block)]))
  (struct: css-qualified-rule : CSS-Qualified-Rule ([prelude : CSS-Component-Values] [block : CSS-Simple-Block]))
  (struct: css-declaration : CSS-Declaration ([name : CSS:Ident] [important? : Boolean] [case-sentitive? : Boolean]
                                                                 [arguments : CSS-Component-Values]))

  ;;  https://drafts.csswg.org/selectors/#grammar
  ;;  https://drafts.csswg.org/selectors/#structure
  (define-syntax (define-selectors stx)
    (syntax-case stx []
      [(_ Selector [s-id #:+ S-ID rest ...] ...)
       #'(begin (define-type Selector (U S-ID ...))
                (struct: s-id : S-ID rest ...) ...)]))

  (define-selectors CSS-Simple-Selector
    [css-type-selector #:+ CSS-Type-Selector ([name : (U CSS:Ident CSS:Delim)] [namespace : CSS-Selector-NameSpace])]
    [css-class-selector #:+ CSS-Class-Selector ([value : CSS:Ident])] ; <=> [class~=value]
    [css-id-selector #:+ CSS-ID-Selector ([value : CSS:Hash])]        ; <=> [`id`~=value] Note: you name `id` in your application
    [css-attribute-selector #:+ CSS-Attribute-Selector ([name : CSS:Ident] [namespace : CSS-Selector-NameSpace])]
    [css-attribute=selector #:+ CSS-Attribute=Selector css-attribute-selector ([operator : CSS:Delim] [value : (U CSS:Ident CSS:String)])]
    [css-pseudo-selector #:+ CSS-Pseudo-Selector ([name : (U CSS:Ident CSS:Function)] [arguments : CSS-Component-Values]
                                                                                      [element? : Boolean])])

  (define-type CSS-Selector-NameSpace (U CSS:Ident CSS:Delim Boolean))
  (define-type CSS-Compound-Selector (Listof CSS-Simple-Selector))
  (define-type CSS-Complex-Selector (Listof (U CSS-Compound-Selector Symbol)))
  
  ;; https://drafts.csswg.org/css-syntax/#css-stylesheets
  ;; https://drafts.csswg.org/cssom/#css-object-model
  (struct: css-stylesheet : CSS-StyleSheet ([location : Any] [rules : (Listof CSS-Grammar-Rule)]))
  (struct: css-style-rule : CSS-Style-Rule ([selectors : (Listof CSS-Complex-Selector)] [properties : (Listof CSS-Declaration)]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                           (if (and (integer? position) (integer? start-position))
                               (max (- position start-position) 0)
                               0)
                           datum ...))]))

  (define-syntax (remake-token stx)
    (syntax-case stx []
      [(_ here-token make-css:token datum ...)
       #'(make-css:token (css-token-source here-token)
                         (css-token-line here-token) (css-token-column here-token)
                         (css-token-position here-token) (css-token-span here-token)
                         datum ...)]))

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
      (define ch (read-char /dev/cssin))
      (define-values (line column position) (port-next-location /dev/cssin))
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
             (read-char) (css-consume-unicode-range-token srcloc)]
            [else (let ([name : String (css-consume-name css id0)])
                    (define ch : (U EOF Char) (peek-char css))
                    (cond [(or (eof-object? ch) (not (char=? ch #\()))
                           (make-token srcloc css:ident (string->symbol name))]
                          [(or (not (string-ci=? name "url")) (regexp-match-peek #px"^.\\s*[\"']" css))
                           (read-char css) (make-token srcloc css:function (string->unreadable-symbol name))]
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
             (make-token srcloc css:url 'about:invalid)]
            [else (let consume-url-token : (U CSS:URL CSS:Bad) ([chars (list start)])
                    (define ch : (U EOF Char) (read-char css))
                    (cond [(or (eof-object? ch) (char=? ch #\)))
                           (when (eof-object? ch) (make-bad-token srcloc css:bad:eof struct:css:url (chars->url (reverse chars))))
                           (make-token srcloc css:url (chars->url (reverse chars)))]
                          [(char-whitespace? ch)
                           (css-consume-whitespace css)
                           (define end : (U EOF Char) (read-char css))
                           (define uri : Bytes (chars->url (reverse chars)))
                           (cond [(or (eof-object? end) (char=? end #\))) (make-token srcloc css:url uri)]
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
          (make-token srcloc css:hash
                      (cons (string->keyword (css-consume-name (css-srcloc-in srcloc) #\#))
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
          (make-token srcloc css:@keyword (string->keyword (css-consume-name (css-srcloc-in srcloc) #\@)))
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
  (define-css-parser-entry css-parse-stylesheet :-> (Listof CSS-Syntax-Rule)
    ;;; https://drafts.csswg.org/css-syntax/#parse-stylesheet
    ;;; https://drafts.csswg.org/css-syntax/#declaration-rule-list
    (lambda [/dev/cssin]
      (define rules : (Listof CSS-Syntax-Rule) (css-consume-rules /dev/cssin #true))
      (define rule : (Option CSS-Syntax-Rule) (and (pair? rules) (car rules)))
      (if (and (css-@rule? rule) (css:@keyword=:=? (css-@rule-name rule) '#:@charset))
          (cdr rules)
          rules)))

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
        (cond [(eof-object? stx) (css-make-syntax-error exn:css:eof stx)]
              [(css:@keyword? stx) (css-consume-@rule /dev/cssin stx)]
              [else (css-consume-qualified-rule /dev/cssin stx)]))
      (define end (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(or (eof-object? end) (exn? retval)) retval]
            [else (css-make-syntax-error exn:css:overconsumption end)])))

  (define-css-parser-entry css-parse-declaration :-> (U CSS-Declaration CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#declaration
    ;;; https://drafts.csswg.org/css-syntax/#parse-declaration
    (lambda [/dev/cssin [stop-char : (Option Char) #false]]
      (define token (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(not (css:ident? token)) (css-make-syntax-error exn:css:non-identifier token)]
            [else (css-components->declaration token (css-consume-components /dev/cssin stop-char))])))

  (define-css-parser-entry css-parse-declarations :-> (Listof (U CSS-Declaration CSS-@Rule))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-declarations
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-list-of-declarations
    (lambda [/dev/cssin]
      (let consume-declaration+@rule : (Listof (U CSS-Declaration CSS-@Rule))
        ([mixed-list : (Listof (U CSS-Declaration CSS-@Rule)) null])
        (define token (css-read-syntax /dev/cssin))
        (cond [(eof-object? token)
               (reverse mixed-list)]
              [(or (css:whitespace? token) (css:delim=:=? token #\;))
               (consume-declaration+@rule mixed-list)]
              [(css:ident? token)
               (consume-declaration+@rule
                (css-cons (css-components->declaration token (css-consume-components /dev/cssin #\;)) mixed-list))]
              [(css:@keyword? token)
               (consume-declaration+@rule (cons (css-consume-@rule /dev/cssin token) mixed-list))]
              [else (css-make-syntax-error exn:css:non-identifier token)
               (css-consume-components /dev/cssin #\;)
               (consume-declaration+@rule mixed-list)]))))
  
  (define-css-parser-entry css-parse-component-value :-> (U CSS-Component-Value CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#parse-component-value
    (lambda [/dev/cssin]
      (define token (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(eof-object? token) (css-make-syntax-error exn:css:eof token)]
            [else (let ([retval (css-consume-component-value /dev/cssin token)])
                    (define end (css-read-syntax/skip-whitespace /dev/cssin))
                    (cond [(eof-object? end) retval]
                          [else (css-make-syntax-error exn:css:overconsumption end)]))])))

  (define-css-parser-entry css-parse-component-values :-> CSS-Component-Values
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [/dev/cssin [stop-char : (Option Char) #false]]
      (css-consume-components /dev/cssin stop-char)))

  (define-css-parser-entry css-parse-component-valueses :-> (Listof CSS-Component-Values)
    ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
    ;;; https://drafts.csswg.org/css-values/#comb-comma
    (lambda [/dev/cssin [stop-char : (Option Char) #false]]
      (let consume-components : (Listof CSS-Component-Values) ([componentses : (Listof CSS-Component-Values) null])
        (define token (css-read-syntax /dev/cssin))
        (cond [(eof-object? token) (reverse componentses)]
              [else (let ([components (css-consume-components /dev/cssin #\, token)])
                      (cond [(null? (filter-not css:whitespace? components)) componentses]
                            [else (consume-components (cons components componentses))]))]))))

  (define-css-parser-entry css-parse-selectors :-> (U (Listof CSS-Complex-Selector) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-selector
    ;;; https://drafts.csswg.org/selectors/#selector-list
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [/dev/cssin]
      (with-handlers ([exn:fail:syntax? (λ [[e : exn:fail:syntax]] e)])
        (let consume-selector : (Listof CSS-Complex-Selector) ([selectors : (Listof CSS-Complex-Selector) null])
          (define token (css-read-syntax/skip-whitespace /dev/cssin))
          (cond [(eof-object? token) (reverse selectors)]
                [else (consume-selector (cons (css-consume-complex-selector /dev/cssin token) selectors))])))))

  (define-css-parser-entry css-parse-relative-selectors :-> (U (Listof CSS-Complex-Selector) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-relative-selector
    ;;; https://drafts.csswg.org/selectors/#the-scope-pseudo
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [/dev/cssin]
      (css-make-syntax-error exn:css:unrecognized eof)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  (define css-consume-qualified-rule : (-> Input-Port CSS-Component-Value (U CSS-Qualified-Rule CSS-@Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    (lambda [css reconsumed-token]
      (define-values (prelude maybe-block) (css-consume-rule-item css #:@rule? #false))
      (cond [(css-simple-block? maybe-block) (make-css-qualified-rule (cons reconsumed-token prelude) maybe-block)]
            [else (css-make-syntax-error exn:css:missing-block reconsumed-token)])))

  (define css-consume-component-value : (-> Input-Port CSS-Component-Value CSS-Component-Value)
    ;;; https://drafts.csswg.org/css-syntax/#component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    (lambda [css reconsumed-token]
      (cond [(css:delim=:=? reconsumed-token #\{) (make-css-simple-block reconsumed-token (css-consume-block-body css #\}))]
            [(css:delim=:=? reconsumed-token #\[) (make-css-simple-block reconsumed-token (css-consume-block-body css #\]))]
            [(css:delim=:=? reconsumed-token #\() (make-css-simple-block reconsumed-token (css-consume-block-body css #\)))]
            [(css:function? reconsumed-token) (make-css-function reconsumed-token (css-consume-block-body css #\)))]
            [else reconsumed-token])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-rule-item : (-> Input-Port #:@rule? Boolean (Values CSS-Component-Values (Option CSS-Simple-Block)))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-qualified-rule
    (lambda [css #:@rule? at-rule?]
      (let consume-item : (Values CSS-Component-Values (Option CSS-Simple-Block))
        ([prelude : CSS-Component-Values null]
         [simple-block : (Option CSS-Simple-Block) #false])
        (define token (css-read-syntax css))
        (cond [(or (eof-object? token) (and at-rule? (css:delim=:=? token #\;)))
               (when (eof-object? token) (css-make-syntax-error exn:css:eof (if (null? prelude) eof (car prelude))))
               (values (reverse prelude) simple-block)]
              [(css:delim=:=? token #\{) (values (reverse prelude) (make-css-simple-block token (css-consume-block-body css #\})))]
              [(and (css-simple-block? token) (css:delim=:=? (css-simple-block-open token) #\{)) (values (reverse prelude) token)]
              [else (consume-item (cons (css-consume-component-value css token) prelude) simple-block)]))))

  (define css-consume-block-body : (-> Input-Port Char CSS-Component-Values)
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css close-char]
      (let consume-body : CSS-Component-Values ([components : CSS-Component-Values null])
        (define token (css-read-syntax css))
        (cond [(eof-object? token) (css-make-syntax-error exn:css:eof token) (reverse components)]
              [(css:delim=:=? token close-char) (reverse components)]
              [else (consume-body (cons (css-consume-component-value css token) components))]))))
  
  (define css-consume-components : (->* (Input-Port (Option Char)) ((Option CSS-Component-Value)) CSS-Component-Values)
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [css [stop-char #false] [reconsumed-token #false]]
      (let consume-component : CSS-Component-Values
        ([components : CSS-Component-Values
                     (cond [(false? reconsumed-token) null]
                           [else (list (css-consume-component-value css reconsumed-token))])])
        (define token (css-read-syntax css))
        (cond [(eof-object? token) (reverse components)]
              [(and (char? stop-char) (css:delim=:=? token stop-char)) (reverse components)]
              [else (consume-component (cons (css-consume-component-value css token) components))]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-complex-selector : (-> Input-Port CSS-Component-Value CSS-Complex-Selector)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#combinators
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [css reconsumed-token]
      (let consume-compound-selector : CSS-Complex-Selector
        ([srotceles : (Listof (U CSS-Compound-Selector Symbol)) null]
         [token : CSS-Syntax-Any reconsumed-token])
        (cond [(or (eof-object? token) (css:delim=:=? token #\,))
               (define selectors (if (null? srotceles) null (if (eq? (car srotceles) '>>) (cdr srotceles) srotceles)))
               (cond [(null? selectors) (raise (css-make-syntax-error exn:css:empty token))]
                     [else (reverse selectors)])]
              [(css-selector-combinator? token)
               (let-values ([(combinator last-token) (css-consume-combinator css token)])
                 (if (css-selector-combinator? last-token)
                     (raise (css-make-syntax-error exn:css:unrecognized last-token))
                     (consume-compound-selector (cons combinator srotceles) last-token)))]
              [else (let-values ([(compound-selector last-token) (css-consume-compound-selector css token)])
                      (consume-compound-selector (cons compound-selector srotceles) last-token))]))))

  (define css-consume-combinator : (-> Input-Port (U CSS:WhiteSpace CSS:Delim CSS:||) (Values Symbol CSS-Syntax-Any))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [css reconsumed-token]
      (cond [(css:whitespace? reconsumed-token)
             (define next (css-read-syntax/skip-whitespace css))
             (cond [(css-selector-combinator? next) (css-consume-combinator css next)]
                   [else (values '>> next)])]
            [(css:delim=:=? reconsumed-token #\>)
             (define next (css-read-syntax css))
             (define next2 (css-read-syntax/skip-whitespace css))
             (cond [(not (css:delim=:=? next #\>)) (values '> next2)]
                   [else (values '>> (css-read-syntax/skip-whitespace css))])]
            [(css:delim=:=? reconsumed-token #\+) (values '+ (css-read-syntax/skip-whitespace css))]
            [(css:delim=:=? reconsumed-token #\~) (values '~ (css-read-syntax/skip-whitespace css))]
            [(css:||? reconsumed-token) (values '|| (css-read-syntax/skip-whitespace css))]
            [else (raise (css-make-syntax-error exn:css:unrecognized reconsumed-token))])))
  
  (define css-consume-compound-selector : (-> Input-Port CSS-Component-Value (Values CSS-Compound-Selector CSS-Syntax-Any))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [css reconsumed-token]
      (define selectors0 : (Listof CSS-Simple-Selector)
        (cond [(or (css:ident? reconsumed-token) (css:delim=:=? reconsumed-token #\|) (css:delim=:=? reconsumed-token #\*))
               (list (css-consume-elemental-selector css reconsumed-token))]
              [(or (css:delim? reconsumed-token) (css:hash? reconsumed-token))
               (list (css-consume-simple-selector css reconsumed-token)
                     (make-css-type-selector (remake-token reconsumed-token css:delim #\*) #true))]
              [else (raise (css-make-syntax-error exn:css:unrecognized reconsumed-token))]))
      (let consume-simple-selector : (Values CSS-Compound-Selector CSS-Syntax-Any) ([srotceles selectors0])
        (define token (css-read-syntax css))
        (if (or (eof-object? token) (css:delim=:=? token #\,) (css-selector-combinator? token))
            (values (reverse srotceles) token)
            (consume-simple-selector (cons (css-consume-simple-selector css token) srotceles))))))

  (define css-consume-elemental-selector : (-> Input-Port (U CSS:Ident CSS:Delim) CSS-Type-Selector)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#elemental-selectors
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    (lambda [css reconsumed-token]
      (define next (css-peek-syntax css 0))
      (define next2 (css-peek-syntax css 1))
      (cond [(css:delim=:=? reconsumed-token #\|)
             (cond [(or (css:ident? next) (css:delim=:=? next #\*)) (css-read-syntax css) (make-css-type-selector next #false)]
                   [else (raise (css-make-syntax-error exn:css:missing-identifier next))])]
            [(css:delim=:=? next #\|)
             (cond [(or (css:ident? next2) (css:delim=:=? next2 #\*))
                    (css-read-syntax css) (css-read-syntax css) (make-css-type-selector next2 reconsumed-token)]
                   [else (raise (css-make-syntax-error exn:css:missing-identifier next))])]
            [else (make-css-type-selector reconsumed-token #true)])))

  (define css-consume-simple-selector : (-> Input-Port CSS-Component-Value CSS-Simple-Selector)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [css reconsumed-token]
      (cond [(css:hash? reconsumed-token) (make-css-id-selector reconsumed-token)]
            [(css:delim=:=? reconsumed-token #\.)
             (define next (css-read-syntax css))
             (cond [(not (css:ident? next)) (raise (css-make-syntax-error exn:css:missing-identifier next))]
                   [else (make-css-class-selector next)])]
            [(css:delim=:=? reconsumed-token #\:)
             (define next (css-read-syntax css))
             (define element? : Boolean (css:delim=:=? next #\:))
             (define next2 (if element? (css-read-syntax css) next))
             (cond [(css:ident? next2) (make-css-pseudo-selector next2 null element?)]
                   [(css-function? next2) (make-css-pseudo-selector (css-function-name next2) (css-function-arguments next2) element?)]
                   [(css:function? next2) (make-css-pseudo-selector next2 (css-consume-block-body css #\)) element?)]
                   [else (raise (css-make-syntax-error exn:css:non-identifier next))])]
            [(and (css-simple-block? reconsumed-token) (css:delim=:=? (css-simple-block-open reconsumed-token) #\[))
             (css-components->attribute-selector (css-simple-block-open reconsumed-token) (css-simple-block-components reconsumed-token))]
            [(css:delim=:=? reconsumed-token #\[)
             (css-components->attribute-selector reconsumed-token (css-consume-block-body css #\]))]
            [else (raise (css-make-syntax-error exn:css:unrecognized reconsumed-token))])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->declaration : (-> CSS:Ident CSS-Component-Values (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
    ;;; https://drafts.csswg.org/css-cascade/#importance
    (lambda [id-token components]
      (define :components : (Option CSS-Component-Values) (memf (negate css:whitespace?) components))
      (cond [(or (false? :components) (null? :components) (not (css:delim=:=? (car :components) #\:)))
             (css-make-syntax-error exn:css:missing-colon id-token)]
            [else (let clean : (U CSS-Declaration CSS-Syntax-Error)
                    ([seulav : CSS-Component-Values null]
                     [skip-whitespace? : Boolean #true]
                     [dirty-list : CSS-Component-Values (cdr :components)])
                    (if (pair? dirty-list)
                        (let-values ([(head tail) (values (car dirty-list) (cdr dirty-list))])
                          (cond [(and (css:whitespace? head) skip-whitespace?) (clean seulav #true tail)]
                                [(css:delim=:=? head #\!) (clean (cons head seulav) #true tail)]
                                [else (clean (cons head seulav) (css:whitespace? head) tail)]))
                        (let ([stnenopmoc (memf (negate css:whitespace?) seulav)])
                          (cond [(false? stnenopmoc) (css-make-syntax-error exn:css:missing-value id-token)]
                                [else (let ([size (length stnenopmoc)])
                                        (define important? : Boolean
                                          (and (> size 1)
                                               (css:delim=:=? (list-ref stnenopmoc 1) #\!)
                                               (css:ident=:=? (list-ref stnenopmoc 0) 'important)))
                                        (define value-list : CSS-Component-Values
                                          (dropf-right (reverse stnenopmoc)
                                                       (λ [com] (or (css:whitespace? com)
                                                                    (css:delim=:=? com #\!)
                                                                    (css:ident=:=? com 'important)))))
                                        (define cs? : Boolean (string-prefix? (symbol->string (css:ident-datum id-token)) "--"))
                                        (cond [(null? value-list) (css-make-syntax-error exn:css:missing-value id-token)]
                                              [else (make-css-declaration id-token important? cs? value-list)]))]))))])))

  (define css-components->attribute-selector : (-> CSS:Delim CSS-Component-Values CSS-Attribute-Selector)
    ;;; https://drafts.csswg.org/selectors/#attribute-selectors
    ;;; https://drafts.csswg.org/selectors/#attrnmsp
    (lambda [open-token components]
      (define value-part : (Option CSS-Component-Values) (memf (λ [v] (or (css:match? v) (css:delim=:=? v #\=))) components))
      (define value-size : Index (if (false? value-part) 0 (length value-part)))
      (define attr-part : CSS-Component-Values (drop-right components value-size))
      (define attr-size : Index (length attr-part))
      (define-values (attr namespace)
        (if (null? attr-part)
            (raise (css-make-syntax-error exn:css:missing-identifier open-token))
            (let-values ([(1st rest1) (values (car attr-part) (cdr attr-part))])
              (if (null? rest1)
                  (cond [(css:ident? 1st) (values 1st #true)]
                        [else (raise (css-make-syntax-error exn:css:non-identifier 1st))])
                  (let-values ([(2nd rest2) (values (car rest1) (cdr rest1))])
                    (if (null? rest2)
                        (cond [(and (css:delim=:=? 1st #\|) (css:ident? 2nd)) (values 2nd #false)]
                              [(css:delim=:=? 1st #\|) (raise (css-make-syntax-error exn:css:non-identifier 2nd))]
                              [else (raise (css-make-syntax-error exn:css:unrecognized 1st))])
                        (let-values ([(3rd rest3) (values (car rest2) (cdr rest2))])
                          (cond [(pair? rest3) (raise (css-make-syntax-error exn:css:overconsumption (car rest3)))]
                                [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:delim=:=? 2nd #\|) (css:ident? 3rd))
                                 (values 3rd 1st)]
                                [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:delim=:=? 2nd #\|))
                                 (raise (css-make-syntax-error exn:css:non-identifier 3rd))]
                                [(or (css:ident? 1st) (css:delim=:=? 1st #\*))
                                 (raise (css-make-syntax-error exn:css:unrecognized 2nd))]
                                [else (raise (css-make-syntax-error exn:css:unrecognized 1st))]))))))))
      (define-values (#{operator : (U CSS:Delim CSS:Match False)} #{value : (U CSS:Ident CSS:String False)})
        (cond [(false? value-part) (values #false #false)]
              [else (let-values ([(op rest1) (values (car value-part) (cdr value-part))])
                      (cond [(null? rest1) (raise (css-make-syntax-error exn:css:missing-value op))]
                            [(not (or (css:match? op) (css:delim=:=? op #\=))) (raise (css-make-syntax-error exn:css:unrecognized op))]
                            [else (let-values ([(v maybe-null) (values (car rest1) (cdr rest1))])
                                    (cond [(pair? maybe-null) (raise (css-make-syntax-error exn:css:overconsumption (car maybe-null)))]
                                          [(or (css:ident? v) (css:string? v)) (values op v)]
                                          [else (raise (css-make-syntax-error exn:css:unrecognized v))]))]))]))
        (cond [(and (css:delim=:=? operator #\=) (or (css:ident? value) (css:string? value)))
               (make-css-attribute=selector attr namespace operator value)]
              [(and (css:match? operator) (or (css:ident? value) (css:string? value)))
               (make-css-attribute=selector attr namespace (remake-token operator css:delim (css:match-datum operator)) value)]
              [else (make-css-attribute-selector attr namespace)])))

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
      (cond [(or (eof-object? stx) (css-component-value? stx)) stx]
            [else (make-bad-token (css-srcloc css #false #false #false)
                                  css:bad:stdin struct:css-token stx)])))

  (define css-peek-syntax : (->* (Input-Port) (Natural) CSS-Syntax-Any)
    (lambda [css [skip 0]]
      (define stx (peek-char-or-special css skip))
      (cond [(or (eof-object? stx) (css-component-value? stx)) stx]
            [else (make-bad-token (css-srcloc css #false #false #false)
                                  css:bad:stdin struct:css-token stx)])))
  
  (define css-read-syntax/skip-whitespace : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (define token (css-read-syntax css))
      (cond [(not (css:whitespace? token)) token]
            [else (css-read-syntax/skip-whitespace css)])))
  
  (define css-cons : (All (CSS-DT) (-> (U CSS-Syntax-Error CSS-DT) (Listof CSS-DT) (Listof CSS-DT)))
    (lambda [item items]
      (cond [(exn? item) items]
            [else (cons item items)])))

  (define css-selector-combinator? : (-> CSS-Syntax-Any Boolean : #:+ (U CSS:WhiteSpace CSS:Delim CSS:||))
    (lambda [token]
      (or (css:whitespace? token)
          (and (css:delim? token) (memq (css:delim-datum token) '(#\~ #\+ #\>)) #true)
          (css:||? token)))))

(require (submod "." digitama))
(require (submod "." parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test typed/racket
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
  (for ([t (in-range 16)])
    (time-run (for-each read-css-stylesheet CSSes)))

  (log-message css-logger 'debug "exit" eof)
  (copy-port in (current-output-port)))
  