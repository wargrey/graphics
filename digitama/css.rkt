#lang typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.w3.org/Style/CSS/specs.en.html                                                   ;;;
;;;                                                                                             ;;;
;;; https://drafts.csswg.org/css-syntax                                                         ;;;
;;; https://drafts.csswg.org/css-values                                                         ;;;
;;; https://drafts.csswg.org/css-cascade                                                        ;;;
;;; https://drafts.csswg.org/selectors                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(provide (all-from-out (submod "." digitama)))

; https://drafts.csswg.org/css-syntax/#parser-entry-points
(provide (rename-out [css-parse-stylesheet read-css-stylesheet]
                     [css-parse-rule read-css-rule]
                     [css-parse-rules read-css-rules]
                     [css-parse-declaration read-css-declaration]
                     [css-parse-declarations read-css-declarations]
                     [css-parse-component-value read-css-component-value]
                     [css-parse-component-values read-css-component-values]
                     [css-parse-component-valueses read-css-component-valueses]))

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
    [css:number #:+ CSS:Number #:-> css:numeric #:as Float]
    [css:percentage #:+ CSS:Percentage #:-> css:numeric #:as Float #:=? (λ [f1 f2] (= (* f1 0.01) f2))]
    [css:dimension #:+ CSS:Dimension #:-> css:numeric #:as (Pairof Float Symbol)
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
  (define-type CSS-StdIn (U Input-Port String Bytes CSS-Component-Values))
  (define-type CSS-Syntax-Any (U CSS-Component-Value EOF))
  (define-type CSS-Syntax-Rule (U CSS-Qualified-Rule CSS-@Rule))
  (define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule))
  (define-type CSS-Syntax-Error exn:fail:syntax)

  (struct exn:css exn:fail:syntax ())
  (struct exn:css:eof exn:css ())
  (struct exn:css:non-eof exn:css ())
  (struct exn:css:non-rule exn:css ())
  (struct exn:css:non-identifier exn:css ())
  (struct exn:css:unrecognized exn:css ())
  (struct exn:css:missing-colon exn:css ())
  (struct exn:css:missing-block exn:css ())

  (define css-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) CSS-Syntax-Error) CSS-Syntax-Any CSS-Syntax-Error)
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

  ;; https://drafts.csswg.org/css-syntax/#css-stylesheets
  (struct: css-declaration : CSS-Declaration ([name : CSS:Ident] [important? : Boolean] [arguments : CSS-Component-Values]))
  (struct: css-style-rule : CSS-Style-Rule ([selectors : CSS-Component-Values] [properties : (Listof CSS-Declaration)]))
  (struct: css-stylesheet : CSS-StyleSheet ([location : Any] [rules : (Listof CSS-Grammar-Rule)]))

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
(module digitama/tokenizer typed/racket ;;; https://drafts.csswg.org/css-syntax/#tokenization
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

  (define make-bad-token : (-> CSS-Srcloc (-> Symbol Any CSS-Bad) Struct-TypeTop Any CSS:Bad)
    (lambda [src css:bad:sub token datum]
      (define bad (make-token src css:bad (css:bad:sub (assert (object-name token) symbol?) datum)))
      (log-message (current-logger) 'warning 'exn:css:read
                   (format "~a:~a:~a: ~a: ~a: ~s" (css-token-source bad)
                           (css-token-line bad) (css-token-column bad)
                           (object-name css:bad:sub) (object-name token) datum)
                   bad)
      bad))
  
  (define css-consume-token : (->* (Input-Port) (Boolean) (U EOF CSS-Token))
    ;;; (if still): https://drafts.csswg.org/css-syntax/#input-preprocessing
    ;;; (if still): https://drafts.csswg.org/css-syntax/#rule-defs (distinguish delim-token and from () [] {} ,:; and so on.
    ;;; https://drafts.csswg.org/css-syntax/#error-handling
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-token
    (lambda [/dev/cssin [keep-whitespace? #true]]
      (define-values (line column position) (port-next-location /dev/cssin))
      (define srcloc (css-srcloc /dev/cssin line column position))
      (define ch (read-char /dev/cssin))
      (cond [(eof-object? ch) eof]
            [(char-whitespace? ch) (css-consume-whitespace-token srcloc keep-whitespace?)]
            [(char-numeric? ch) (css-consume-numeric-token srcloc ch)]
            [(css-char-name-prefix? ch) (css-consume-ident-token srcloc ch)]
            [else (case ch
                    [(#\' #\") (css-consume-string-token srcloc ch)]
                    [(#\+ #\.) (css-consume-numeric-token srcloc ch)]
                    [(#\^ #\$ #\| #\~ #\*) (css-consume-match-token srcloc ch)]
                    [(#\#) (css-consume-hash-token srcloc)]
                    [(#\@) (css-consume-@keyword-token srcloc)]
                    [(#\/) (css-consume-comment-token srcloc keep-whitespace?)]
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

  (define css-consume-comment-token : (-> CSS-Srcloc Boolean (U EOF CSS-Token))
    (lambda [srcloc keep-whitespace?]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (cond [(or (eof-object? ch1) (not (char=? ch1 #\*))) (make-token srcloc css:delim #\/)]
            [(regexp-match #px".*?\\*/" css)
             => (λ [**/] (cond [(not keep-whitespace?) (css-consume-token css keep-whitespace?)]
                               [else (make-token srcloc css:whitespace (bytes-append #"/" (car **/)))]))]
            [else (make-bad-token srcloc css:bad:eof struct:css:whitespace #"/*")])))

  (define css-consume-whitespace-token : (-> CSS-Srcloc Boolean (U EOF CSS-Token))
    (lambda [srcloc keep-whitespace?]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (if (not keep-whitespace?)
          (css-consume-token css keep-whitespace?)
          (make-token srcloc css:whitespace #\space))))
  
  (define css-consume-ident-token : (-> CSS-Srcloc Char (U CSS:Ident CSS:Function CSS:URL CSS:URange CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-ident-like-token
    ;;; https://drafts.csswg.org/css-syntax/#urange-syntax
    ;;; https://drafts.csswg.org/css-values/#urls
    (lambda [srcloc id0]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (read-char css))
      (define ch2 : (U EOF Char) (peek-char css))
      (cond [(and (char-ci=? id0 #\u) (char? ch1) (char=? ch1 #\+)
                  (or (char-hexdigit? ch2) (and (char? ch2) (char=? ch2 #\?))))
             (css-consume-unicode-range-token srcloc)]
            [else (let ([name : String (css-consume-name css (if (eof-object? ch1) (list id0) (list ch1 id0)))])
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
    ;;;(TODO): https://drafts.csswg.org/css-syntax/#anb
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
                             (define unit : Symbol (string->symbol (string-downcase (css-consume-name css null))))
                             (make-token srcloc css:dimension representation (cons (real->double-flonum n) unit))]
                            [(and (char? ch1) (char=? ch1 #\%)) (read-char css)
                             (make-token srcloc css:percentage representation (real->double-flonum n))]
                            [(exact-integer? n) (make-token srcloc css:integer representation n)]
                            [else (make-token srcloc css:number representation (real->double-flonum n))])))])))

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
                      (cons (string->keyword (css-consume-name (css-srcloc-in srcloc) (list #\#)))
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
          (make-token srcloc css:@keyword (string->keyword (css-consume-name (css-srcloc-in srcloc) (list #\@))))
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
  
  (define css-consume-name : (-> Input-Port (Listof Char) String)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-name
    (lambda [css srahc]
      (define ch : (U EOF Char) (peek-char css))
      (cond [(css-char-name? ch)
             (read-char css)
             (css-consume-name css (cons ch srahc))]
            [(css-valid-escape? ch (peek-char css 1))
             (read-char css)
             (css-consume-name css (cons (css-consume-escaped-char css) srahc))]
            [else (list->string (reverse srahc))])))

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

(module digitama/parser typed/racket ;;; https://drafts.csswg.org/css-syntax/#parsing
  (provide (all-defined-out))

  (require (submod ".." digitama))
  (require (submod ".." digitama/tokenizer))

  (define-syntax (define-css-parser-entry stx)
    ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
    (syntax-case stx [: :-> lambda]
      [(_ id :-> ->T (lambda [cssin [args : T defval] ...] body ...))
       #'(define (id [/dev/stdin : CSS-StdIn (current-input-port)] [args : T defval] ...
                     #:strip-whitespace? [strip-whitespace? : Boolean #false]) : ->T
           (define /dev/cssin : Input-Port (css-open-input-port /dev/stdin (not strip-whitespace?)))
           (dynamic-wind (thunk (port-count-lines! /dev/cssin))
                         (thunk ((λ [[cssin : Input-Port] [args : T defval] ...] : ->T body ...) /dev/cssin args ...))
                         (thunk (close-input-port /dev/cssin))))]))
  
  ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
  (define-css-parser-entry css-parse-stylesheet :-> CSS-StyleSheet
    ;;; https://drafts.csswg.org/css-syntax/#css-stylesheets
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-stylesheet
    ;;; https://drafts.csswg.org/css-syntax/#charset-rule
    (lambda [/dev/cssin]
      (define rules : (Listof CSS-Grammar-Rule)
        (let syntax->grammar : (Listof CSS-Grammar-Rule)
          ([selur : (Listof CSS-Grammar-Rule) null]
           [rules : (Listof CSS-Syntax-Rule) (css-consume-rules /dev/cssin #true)])
          (if (null? rules) (reverse selur)
              (let-values ([(rule rest) (values (car rules) (cdr rules))])
                (if (css-qualified-rule? rule)
                    (syntax->grammar (cons (css-qualified-rule->style-rule rule) selur) rest)
                    (if (css:@keyword=:=? (css-@rule-name rule) '#:@charset)
                        (cond [(null? selur) (syntax->grammar selur rest)]
                              [else (css-make-syntax-error exn:css:unrecognized (css-@rule-name rule))
                                    (syntax->grammar selur rest)])
                        (syntax->grammar (cons rule selur) rest)))))))
      (make-css-stylesheet (object-name /dev/cssin) rules)))

  (define-css-parser-entry css-parse-rules :-> (Listof CSS-Syntax-Rule)
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-rules
    (lambda [/dev/cssin [toplevel? : Boolean #false]]
      (css-consume-rules /dev/cssin toplevel?)))

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
            [else (css-make-syntax-error exn:css:non-eof end)])))

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
              [else (css-make-syntax-error exn:css:non-rule token)
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
                          [else (css-make-syntax-error exn:css:non-eof end)]))])))

  (define-css-parser-entry css-parse-component-values :-> CSS-Component-Values
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [/dev/cssin [stop-char : (Option Char) #false]]
      (css-consume-components /dev/cssin stop-char)))

  (define-css-parser-entry css-parse-component-valueses :-> (Listof CSS-Component-Values)
    ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
    (lambda [/dev/cssin [stop-char : (Option Char) #false]]
      (let consume-components : (Listof CSS-Component-Values) ([componentses : (Listof CSS-Component-Values) null])
        (define token (css-read-syntax /dev/cssin))
        (cond [(eof-object? token) (reverse componentses)]
              [else (consume-components (cons (css-consume-components /dev/cssin #\, token) componentses))]))))

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

  (define css-consume-simple-block : (-> Input-Port CSS:Delim Char CSS-Simple-Block)
    ;;; https://drafts.csswg.org/css-syntax/#block
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    (lambda [css open-token close-char]
      (define-values (components close-token) (css-consume-block-body css close-char))
      (make-css-simple-block open-token components)))

  (define css-consume-function : (-> Input-Port CSS:Function CSS-Function)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css name-token]
      (define-values (arguments _) (css-consume-block-body css #\)))
      (make-css-function name-token arguments)))

  (define css-consume-component-value : (-> Input-Port CSS-Component-Value CSS-Component-Value)
    ;;; https://drafts.csswg.org/css-syntax/#component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-component-value
    (lambda [css reconsumed-token]
      (cond [(css:delim=:=? reconsumed-token #\{) (css-consume-simple-block css reconsumed-token #\})]
            [(css:delim=:=? reconsumed-token #\() (css-consume-simple-block css reconsumed-token #\))]
            [(css:delim=:=? reconsumed-token #\[) (css-consume-simple-block css reconsumed-token #\])]
            [(css:function? reconsumed-token) (css-consume-function css reconsumed-token)]
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
              [(css:delim=:=? token #\{)
               (values (reverse prelude) (css-consume-simple-block css token #\}))]
              [(and (css-simple-block? token) (css:delim=:=? (css-simple-block-open token) #\{))
               (values (reverse prelude) token)]
              [else (consume-item (cons (css-consume-component-value css token) prelude) simple-block)]))))

  (define css-consume-block-body : (-> Input-Port Char (Values CSS-Component-Values CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css close-char]
      (let consume-body : (Values CSS-Component-Values CSS:Delim) ([components : CSS-Component-Values null])
        (define token (css-read-syntax css))
        (cond [(eof-object? token)
               (css-make-syntax-error exn:css:eof token)
               (values (reverse components) (make-token (css-srcloc css #false #false #false) css:delim close-char))]
              [(css:delim=:=? token close-char)
               (values (reverse components) token)]
              [else (consume-body (cons (css-consume-component-value css token) components))]))))
  
  (define css-consume-components : (->* (Input-Port (Option Char)) ((Option CSS-Component-Value)) CSS-Component-Values)
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [/dev/cssin [stop-char #false] [reconsumed-token #false]]
      (let consume-component : CSS-Component-Values
        ([components : CSS-Component-Values
          (cond [(false? reconsumed-token) null]
                [else (list (css-consume-component-value /dev/cssin reconsumed-token))])])
        (define token (css-read-syntax /dev/cssin))
        (cond [(eof-object? token) (reverse components)]
              [(and (char? stop-char) (css:delim=:=? token stop-char)) (reverse components)]
              [else (consume-component (cons (css-consume-component-value /dev/cssin token) components))]))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->declaration : (-> CSS:Ident CSS-Component-Values (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
    ;;; https://drafts.csswg.org/css-cascade/#importance
    (lambda [id-token components]
      (define :components : (Option CSS-Component-Values) (memf (negate css:whitespace?) components))
      (cond [(or (false? :components) (null? :components) (not (css:delim=:=? (car :components) #\:)))
             (css-make-syntax-error exn:css:missing-colon id-token)]
            [else (let ([stnenopmoc (reverse (filter-not css:whitespace? (cdr :components)))])
                    (define important? : Boolean
                      (and (> (length stnenopmoc) 1)
                           (css:delim=:=? (list-ref stnenopmoc 1) #\!)
                           (css:ident=:=? (list-ref stnenopmoc 0) 'important)))
                    (make-css-declaration id-token important?
                                          (cond [(false? important?) (cdr :components)]
                                                [else (dropf-right (cdr :components)
                                                                   (λ [com] (or (css:whitespace? com)
                                                                                (css:delim=:=? com #\!)
                                                                                (css:ident=:=? com 'important))))])))])))

  (define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule CSS-Style-Rule)
    ;;; https://drafts.csswg.org/css-syntax/#style-rules
    (lambda [qr]
      (define components : CSS-Component-Values (css-simple-block-components (css-qualified-rule-block qr)))
      (define srotpircsed : (Listof CSS-Declaration)
        (for/fold ([descriptors : (Listof CSS-Declaration) null])
                  ([mixed-list (in-list (css-parse-declarations components))])
          (cond [(css-declaration? mixed-list) (cons mixed-list descriptors)]
                [else (css-make-syntax-error exn:css:unrecognized (css-@rule-name mixed-list))
                      descriptors])))
      (make-css-style-rule (css-qualified-rule-prelude qr) (reverse srotpircsed))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-open-input-port : (-> CSS-StdIn Boolean Input-Port)
    ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
    (lambda [/dev/stdin keep-whitespace?]
      (if (list? /dev/stdin)
          (let* ([/dev/cssin (if keep-whitespace? /dev/stdin (filter-not css:whitespace? /dev/stdin))]
                 [total : Index (length /dev/cssin)]
                 [cursor : Integer 0])
            (make-input-port '/dev/cssin
                             (λ [[buf : Bytes]]
                               (λ _ (cond [(>= cursor total) eof]
                                          [(set! cursor (add1 cursor))
                                           => (λ _ (list-ref /dev/cssin (sub1 cursor)))])))
                             (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                               (λ _ (cond [(>= (+ skip cursor) total) eof]
                                          [else (list-ref /dev/cssin (+ skip cursor))])))
                             void))
          (let ([/dev/rawin (cond [(string? /dev/stdin) (open-input-string /dev/stdin '/dev/cssin)]
                                  [(byte? /dev/stdin) (open-input-bytes /dev/stdin '/dev/cssin)]
                                  [(port? /dev/stdin) /dev/stdin]
                                  [else (current-input-port)])])
            ; (make-input-port/read-to-peek) is (less than 1.5 times) slower than (make-input-port),
            ; but this cost is too subtle to becoming the performance killer even for large datasource.
            (define /dev/cssin : Input-Port (css-fallback-encode-input-port /dev/rawin))
            (make-input-port/read-to-peek (or (object-name /dev/rawin) '/dev/cssin)
                                          (λ _ (λ _ (css-consume-token /dev/cssin keep-whitespace?)))
                                          #false ; so we have the (peek) been implemented.
                                          (thunk (unless (eq? /dev/rawin /dev/cssin)
                                                   (close-input-port /dev/cssin))
                                                 (unless (eq? /dev/rawin /dev/stdin)
                                                   (close-input-port /dev/rawin)))
                                          (thunk (port-next-location /dev/cssin))
                                          (thunk (port-count-lines! /dev/cssin)))))))
  
  (define css-read-syntax : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (define stx (read-char-or-special css))
      (cond [(or (eof-object? stx) (css-token? stx) (css-function? stx) (css-simple-block? stx)) stx]
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
            [else (cons item items)]))))

(require (submod "." digitama))
(require (submod "." digitama/parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test typed/racket
  (require (submod ".."))
  
  (define tamer.css (simplify-path (build-path (current-directory) 'up "tamer" "test.css")))
  (time (with-input-from-file tamer.css (thunk (read-css-stylesheet))))
  (collect-garbage)
  (for ([t (in-range 32)])
    (time (with-input-from-file tamer.css
            (thunk (void (read-css-stylesheet)))))))
  