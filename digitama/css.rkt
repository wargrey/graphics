#lang typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.w3.org/Style/CSS/specs.en.html                                                   ;;;
;;;                                                                                             ;;;
;;; https://drafts.csswg.org/css-syntax                                                         ;;;
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
  (provide (all-defined-out))

  (define-syntax (struct: stx)
    (syntax-case stx [:]
      [(_ id : ID rest ...)
       #'(begin (define-type ID id)
                (struct id rest ... #:prefab))]))

  ;;; https://drafts.csswg.org/css-syntax/#tokenization
  (struct: css-srcloc : CSS-Srcloc
    ([in : Input-Port] [line : (Option Positive-Integer)] [col : (Option Natural)] [pos : (Option Positive-Integer)]))
  
  (struct: css-token : CSS-Token ([src : Any] [line : Natural] [column : Natural] [position : Natural] [span : Natural]))

  (struct: css:bad : CSS:Bad css-token ([token : Symbol] [datum : Any]))
  (struct: css:bad:eof : CSS:Bad:EOF css:bad ())
  (struct: css:bad:eol : CSS:Bad:EOL css:bad ())
  (struct: css:bad:char : CSS:Bad:Char css:bad ())
  (struct: css:bad:blank : CSS:Bad:Blank css:bad ())
  (struct: css:bad:range : CSS:Bad:Range css:bad ())
  (struct: css:bad:range:index : CSS:Bad:Range:Index css:bad ())
  (struct: css:bad:stdin : CSS:Bad:StdIn css:bad ())

  (struct: css:<!-- : CSS:<!-- css-token ())
  (struct: css:--> : CSS:--> css-token ())
  (struct: css:column : CSS:Column css-token ())
  (struct: css:whitespace : CSS:WhiteSpace css-token ())
  (struct: css:comment : CSS:Comment css:whitespace ([content : Bytes]))
  
  (struct: css:ident : CSS:Ident css-token ([datum : Symbol]))
  (struct: css:function : CSS:Function css-token ([datum : Symbol]))
  (struct: css:@keyword : CSS:@Keyword css-token ([datum : Keyword]))
  (struct: css:hash : CSS:Hash css-token ([datum : Keyword] [type : (U 'id 'unrestricted)]))
  (struct: css:delim : CSS:Delim css-token ([datum : Char]))
  (struct: css:numeric : CSS:Numeric css-token ([representation : String] [datum : Real]))
  (struct: css:no% : CSS:No% css:numeric ())
  (struct: css:dimension : CSS:Dimension css:numeric ([unit : Symbol]))
  (struct: css:string : CSS:String css-token ([datum : String]))
  (struct: css:url : CSS:URL css-token ([datum : String]))
  (struct: css:match : CSS:Match css-token ([datum : Char]))
  (struct: css:urange : CSS:URange css-token ([start : Integer] [end : Integer]))

  ;;; https://drafts.csswg.org/css-syntax/#parsing
  ;;  https://drafts.csswg.org/css-syntax/#component-value
  (define-type CSS-Component-Value (U CSS-Token CSS-Simple-Block CSS-Function))
  (define-type CSS-StdIn (U Input-Port String Bytes (Listof CSS-Component-Value)))
  (define-type CSS-Syntax-Any (U CSS-Component-Value EOF))
  (define-type CSS-Syntax-Rule (U CSS-Qualified-Rule CSS-@Rule))
  (define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule))

  (struct: css-simple-block : CSS-Simple-Block ([open : CSS:Delim] [components : (Listof CSS-Component-Value)] [close : CSS:Delim]))
  (struct: css-function : CSS-Function ([name : CSS:Function] [arguments : (Listof CSS-Component-Value)]))
  (struct: css-qualified-rule : CSS-Qualified-Rule ([prelude : (Listof CSS-Component-Value)] [block : CSS-Simple-Block]))
  (struct: css-@rule : CSS-@Rule ([name : CSS:@Keyword] [prelude : (Listof CSS-Component-Value)] [block : (Option CSS-Simple-Block)]))

  ;; https://drafts.csswg.org/css-syntax/#css-stylesheets
  (struct: css-declaration : CSS-Declaration ([name : CSS:Ident] [important? : Boolean] [values : (Listof CSS-Component-Value)]))
  (struct: css-style-rule : CSS-Style-Rule ([selectors : (Listof CSS-Component-Value)] [properties : (Listof CSS-Declaration)]))
  (struct: css-stylesheet : CSS-StyleSheet ([location : Any] [rules : (Listof CSS-Grammar-Rule)])
    #:extra-constructor-name make-css-stylesheet)

  (define css-component-value? : (-> Any Boolean : #:+ CSS-Component-Value)
    (lambda [v]
      (or (css-token? v)
          (css-function? v)
          (css-simple-block? v))))
  
  (define css:delim=:=? : (-> Any Char Boolean : #:+ CSS:Delim)
    (lambda [token ch]
      (and (css:delim? token)
           (char=? (css:delim-datum token) ch))))

  (define css:ident=:=? : (-> Any Symbol Boolean : #:+ CSS:Ident)
    (lambda [token id]
      (and (css:ident? token)
           (or (symbol=? (css:ident-datum token) id)
               (string-ci=? (symbol->string (css:ident-datum token))
                            (symbol->string id))))))

  (define css-function=:=? : (-> Any Symbol Boolean : #:+ CSS:Function)
    (lambda [token id]
      (and (css:function? token)
           (or (symbol=? (css:function-datum token) id)
               (string-ci=? (symbol->string (css:function-datum token))
                            (symbol->string id))))))

  (define css:@keyword=:=? : (-> Any Keyword Boolean : #:+ CSS:@Keyword)
    (lambda [token meta-id]
      (and (css:@keyword? token)
           (or (eq? (css:@keyword-datum token) meta-id)
               (string-ci=? (keyword->string (css:@keyword-datum token))
                            (keyword->string meta-id))))))

  (define css-token->datum : (-> CSS-Token Any)
    (lambda [token]
      (define v : (Vectorof Any) (struct->vector token))
      (match (if (css:numeric? token) (vector-drop v 7) (vector-drop v 6))
        [(vector datum) datum]
        [(vector datum suffix) (cons datum suffix)]
        [_ (cond [(css:whitespace? token) #\space]
                 [(css:column? token) '||]
                 [else (object-name token)])]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama/tokenizer typed/racket ;;; https://drafts.csswg.org/css-syntax/#tokenization
  (provide (all-defined-out))

  (require (submod ".." digitama))

  (define-syntax (make-token stx)
    (syntax-case stx []
      [(_ src css:token argl ...)
       #'(let-values ([(start-position) (css-srcloc-pos src)]
                      [(line column position) (port-next-location (css-srcloc-in src))])
           (css:token (or (object-name (css-srcloc-in src)) '/dev/cssin)
                      (or (css-srcloc-line src) line 0)
                      (or (css-srcloc-col src) column 0)
                      (or start-position 0)
                      (if (and (integer? position) (integer? start-position))
                          (max (- position start-position) 0)
                          0)
                      argl ...))]))

  (define-syntax (make-bad-token stx)
    (syntax-case stx []
      [(_ src css:bad:sub token datum)
       #'(let ([bad (make-token src css:bad:sub (assert (object-name token) symbol?) datum)])
           (log-message (current-logger) 'warning 'exn:css:read
                        (format "~a:~a:~a: ~a: ~a: `~a`" (css-token-src bad)
                                (or (css-token-line bad) -1) (or (css-token-column bad) -1)
                                (css:bad-token bad) (object-name bad) datum)
                        bad)
           bad)]))
  
  (define css-consume-token : (->* (Input-Port) (Boolean) (U EOF CSS-Token))
    ;;; (if still): https://drafts.csswg.org/css-syntax/#input-preprocessing
    ;;; (if still): https://drafts.csswg.org/css-syntax/#rule-defs (distinguish delim-token and from () [] {} ,:; and so on.
    ;;; https://drafts.csswg.org/css-syntax/#error-handling
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-token
    (lambda [/dev/cssin [keep-whitespace? #false]]
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
          (let ([cd : (U EOF String) (peek-string 3 0 css)])
            (cond [(and (string? cd) (string=? cd "!--")) (read-string 3 css) (make-token srcloc css:<!--)]
                  [else (make-token srcloc css:delim #\<)]))
          (let ([ch1 : (U EOF Char) (peek-char css 0)]
                [ch2 : (U EOF Char) (peek-char css 1)])
            (cond [(and (char? ch1) (char? ch2) (char=? ch1 #\-) (char=? ch1 #\>)) (read-string 2 css) (make-token srcloc css:-->)]
                  [(css-identifier-prefix? #\- ch1 ch2) (css-consume-ident-token srcloc #\-)]
                  [else (css-consume-numeric-token srcloc #\-)])))))

  (define css-consume-comment-token : (-> CSS-Srcloc Boolean (U EOF CSS-Token))
    (lambda [srcloc keep-whitespace?]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (cond [(or (eof-object? ch1) (not (char=? ch1 #\*)))
             (make-token srcloc css:delim #\/)]
            [(regexp-match #px".*?\\*/" css)
             => (λ [**/] (cond [(not keep-whitespace?) (css-consume-token css keep-whitespace?)]
                               [else (make-token srcloc css:comment (bytes-append #"/" (car **/)))]))]
            [else (make-bad-token srcloc css:bad:eof css:comment '/*)])))

  (define css-consume-whitespace-token : (-> CSS-Srcloc Boolean (U EOF CSS-Token))
    (lambda [srcloc keep-whitespace?]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (if (not keep-whitespace?)
          (css-consume-token css keep-whitespace?)
          (make-token srcloc css:whitespace))))
  
  (define css-consume-ident-token : (-> CSS-Srcloc Char (U CSS:Ident CSS:Function CSS:URL CSS:URange CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-ident-like-token
    ;;; https://drafts.csswg.org/css-syntax/#urange-syntax
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
                           (read-char css) (make-token srcloc css:function (string->symbol name))]
                          [else (read-char css) (css-consume-url-token srcloc)]))])))
      
  (define css-consume-string-token : (-> CSS-Srcloc Char (U CSS:String CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-string-token
    (lambda [srcloc quotation]
      (define css : Input-Port (css-srcloc-in srcloc))
      (let consume-string-token : (U CSS:String CSS:Bad) ([chars : (Listof Char) null])
        (define ch : (U EOF Char) (read-char css))
        (cond [(or (eof-object? ch) (char=? ch quotation))
               (when (eof-object? ch)
                 (make-bad-token srcloc css:bad:eof css:string (list->string (reverse chars))))
               (make-token srcloc css:string (list->string (reverse chars)))]
              [(char=? ch #\newline)
               (make-bad-token srcloc css:bad:eol css:string (list->string (reverse chars)))]
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
                             (make-token srcloc css:dimension representation n unit)]
                            [(and (char? ch1) (char=? ch1 #\%)) (read-char css)
                             (make-token srcloc css:no% representation n)]
                            [else (make-token srcloc css:numeric representation n)])))])))

  (define css-consume-url-token : (-> CSS-Srcloc (U CSS:URL CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-url-token
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (define start : (U EOF Char) (read-char css))
      (cond [(or (eof-object? start) (char=? start #\)))
             (when (eof-object? start)
               (make-bad-token srcloc css:bad:eof css:url ""))            
             (make-token srcloc css:url "")]
            [else (let consume-url-token : (U CSS:URL CSS:Bad) ([chars (list start)])
                    (define ch : (U EOF Char) (read-char css))
                    (cond [(or (eof-object? ch) (char=? ch #\)))
                           (when (eof-object? ch)
                             (make-bad-token srcloc css:bad:eof css:url (list->string (reverse chars))))
                           (make-token srcloc css:url (list->string (reverse chars)))]
                          [(char-whitespace? ch)
                           (css-consume-whitespace css)
                           (define end : (U EOF Char) (read-char css))
                           (define uri : String (list->string (reverse chars)))
                           (if (or (eof-object? end) (char=? end #\)))
                               (make-token srcloc css:url uri)
                               (css-consume-bad-url-remnants css (make-bad-token srcloc css:bad:blank css:url uri)))]
                          [(css-valid-escape? ch (peek-char css))
                           (consume-url-token (cons (css-consume-escaped-char css) chars))]
                          [(or (memq ch '(#\\ #\" #\' #\()) (css-char-non-printable? ch))
                           (css-consume-bad-url-remnants css (make-bad-token srcloc css:bad:char css:url ch))]
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
      (cond [(> end #x10FFFF) (make-bad-token srcloc css:bad:range:index css:urange end)]
            [(> start end) (make-bad-token srcloc css:bad:range css:urange (cons start end))]
            [else (make-token srcloc css:urange start end)])))

  (define css-consume-hash-token : (-> CSS-Srcloc (U CSS:Hash CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#hash-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (or (css-char-name? ch1) (css-valid-escape? ch1 ch2))
          (make-token srcloc css:hash
                      (string->keyword (css-consume-name (css-srcloc-in srcloc) (list #\#)))
                      (if (css-identifier-prefix? ch1 ch2 ch3) 'id 'unrestricted))
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

  (define css-consume-match-token : (-> CSS-Srcloc Char (U CSS:Match CSS:Delim CSS:Column))
    ;;; https://drafts.csswg.org/css-syntax/#include-match-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#column-token-diagram
    (lambda [srcloc prefix]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch : (U EOF Char) (peek-char css))
      (cond [(and (char? ch) (char=? ch #\=)) (read-char css) (make-token srcloc css:match prefix)]
            [(and (char=? prefix #\|) (char? ch) (char=? ch #\|)) (read-char css) (make-token srcloc css:column)]
            [else (make-token srcloc css:delim prefix)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-whitespace : (-> Input-Port Void)
    (lambda [css]
      (regexp-match #px"\\s*" css)
      (void)))
  
  (define css-consume-name : (-> Input-Port (Listof Char) String)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-name
    (lambda [css chars]
      (define ch : (U EOF Char) (peek-char css))
      (cond [(css-char-name? ch)
             (read-char css)
             (css-consume-name css (cons ch chars))]
            [(css-valid-escape? ch (peek-char css 1))
             (read-char css)
             (css-consume-name css (cons (css-consume-escaped-char css) chars))]
            [else (list->string (reverse chars))])))

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

  (define css-consume-hexadecimal : (->* (Input-Port Integer) (Integer #:\s?$? Boolean) (Values Integer Integer))
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
            [(let ([charset? (regexp-match #px"^@charset \"(.*?)\";" magic)]) (and charset? (cadr charset?)))
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
                     #:keep-whitespace? [keep-whitespace? : Boolean #false]) : ->T
           (define /dev/cssin : Input-Port (css-open-input-port /dev/stdin keep-whitespace?))
           (dynamic-wind (thunk (port-count-lines! /dev/cssin))
                         (thunk ((λ [[cssin : Input-Port] [args : T defval] ...] : ->T body ...) /dev/cssin args ...))
                         (thunk (close-input-port /dev/cssin))))]))
  
  
  (define-type CSS-Syntax-Error exn:fail:syntax)

  (define css-make-syntax-error : (-> (U CSS-Syntax-Any (Listof CSS-Syntax-Any)) String Any * CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#error-handling
    (lambda [tokens msgfmt . argl]
      (define message : String (if (null? argl) msgfmt (apply format msgfmt argl)))
      (define token-list : (Listof CSS-Token)
        (filter css-token? (cond [(eof-object? tokens) null]
                                 [(list? tokens) tokens]
                                 [else (list tokens)])))
      (define topic : Symbol 'exn:css:syntax)
      (define errobj : CSS-Syntax-Error
        (with-handlers ([exn:fail:syntax? (λ [[e : exn:fail:syntax]] e)])
          (raise-syntax-error topic message #false #false
                              (for/list : (Listof (Syntaxof Any)) ([token : CSS-Token (in-list (filter css-token? token-list))])
                                (datum->syntax #false
                                               (css-token->datum token)
                                               (list (css-token-src token)
                                                     (css-token-line token) (css-token-column token)
                                                     (css-token-position token) (css-token-span token)))))))
      (log-message (current-logger) 'error topic message errobj)
      errobj))
  
  ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
  (define-css-parser-entry css-parse-stylesheet :-> CSS-StyleSheet
    ;;; https://drafts.csswg.org/css-syntax/#css-stylesheets
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-stylesheet
    (lambda [/dev/cssin]
      (define datasource (object-name /dev/cssin))
      (define selur : (Listof (U CSS-Style-Rule CSS-@Rule))
        (for/fold ([rules : (Listof (U CSS-Style-Rule CSS-@Rule)) null])
                  ([rule : CSS-Syntax-Rule (in-list (css-consume-rules /dev/cssin #true))])
          (cond [(and (null? rules) (css-@rule? rule) (css:@keyword=:=? (css-@rule-name rule) '#:@charset)) null]
                [(css-qualified-rule? rule) (cons (css-qualified-rule->style-rule rule) rules)]
                [else (cons rule rules)])))
      (make-css-stylesheet datasource (reverse selur))))

  (define-css-parser-entry css-parse-rules :-> (Listof CSS-Syntax-Rule)
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-rules
    (lambda [/dev/cssin [toplevel? : Boolean #false]]
      (css-consume-rules /dev/cssin toplevel?)))

  (define-css-parser-entry css-parse-rule :-> (U CSS-Syntax-Rule CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#parse-rule
    (lambda [/dev/cssin]
      (define stx (css-read-syntax/skip-whitespace /dev/cssin))
      (define retval : (U CSS-Qualified-Rule CSS-@Rule CSS-Syntax-Error)
        (cond [(eof-object? stx) (css-make-syntax-error stx "unexpected end of stream")]
              [(css:@keyword? stx) (css-consume-@rule /dev/cssin stx)]
              [else (css-consume-qualified-rule /dev/cssin stx)]))
      (define end (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(or (eof-object? end) (exn? retval)) retval]
            [else (css-make-syntax-error end "too many rules found")])))

  (define-css-parser-entry css-parse-declaration :-> (U CSS-Declaration CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#declaration
    ;;; https://drafts.csswg.org/css-syntax/#parse-declaration
    (lambda [/dev/cssin [stop-char : (Option Char) #false]]
      (define token (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(not (css:ident? token)) (css-make-syntax-error token "only ident token should be here")]
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
              [else (css-make-syntax-error token "unexpected token in the declarations")
               (css-consume-components /dev/cssin #\;)
               (consume-declaration+@rule mixed-list)]))))
  
  (define-css-parser-entry css-parse-component-value :-> (U CSS-Component-Value CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#parse-component-value
    (lambda [/dev/cssin]
      (define token (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(eof-object? token) (css-make-syntax-error token "unexpected end of stream")]
            [else (let ([retval (css-consume-component-value /dev/cssin token)])
                    (define end (css-read-syntax/skip-whitespace /dev/cssin))
                    (cond [(eof-object? end) retval]
                          [else (css-make-syntax-error end "too many component values found")]))])))

  (define-css-parser-entry css-parse-component-values :-> (Listof CSS-Component-Value)
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [/dev/cssin [stop-char : (Option Char) #false]]
      (css-consume-components /dev/cssin stop-char)))

  (define-css-parser-entry css-parse-component-valueses :-> (Listof (Listof CSS-Component-Value))
    ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
    (lambda [/dev/cssin [stop-char : (Option Char) #false]]
      (let consume-components : (Listof (Listof CSS-Component-Value))
        ([componentses : (Listof (Listof CSS-Component-Value)) null])
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
              [(css-CDO/CDC? token) (consume-rules (if toplevel? rules (css-cons (css-consume-qualified-rule css token) rules)))]
              [else (consume-rules (css-cons (css-consume-qualified-rule css token) rules))]))))

  (define css-consume-@rule : (-> Input-Port CSS:@Keyword CSS-@Rule)
    ;;; https://drafts.csswg.org/css-syntax/#at-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-at-rule
    (lambda [css reconsumed-at-token]
      (define-values (prelude maybe-block) (css-consume-rule-item css #:@rule? #true))
      (css-@rule reconsumed-at-token prelude maybe-block)))

  (define css-consume-qualified-rule : (-> Input-Port CSS-Component-Value (U CSS-Qualified-Rule CSS-@Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    (lambda [css reconsumed-token]
      (define-values (prelude maybe-block) (css-consume-rule-item css #:@rule? #false))
      (cond [(css-simple-block? maybe-block) (css-qualified-rule (cons reconsumed-token prelude) maybe-block)]
            [else (css-make-syntax-error reconsumed-token "qualified rule should have a simple block")])))

  (define css-consume-simple-block : (-> Input-Port CSS:Delim Char CSS-Simple-Block)
    ;;; https://drafts.csswg.org/css-syntax/#block
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    (lambda [css open-token close-char]
      (define-values (components close-token) (css-consume-block-body css close-char))
      (css-simple-block open-token components close-token)))

  (define css-consume-function : (-> Input-Port CSS:Function CSS-Function)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css name-token]
      (define-values (arguments _) (css-consume-block-body css #\)))
      (css-function name-token arguments)))

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
  (define css-consume-rule-item : (-> Input-Port #:@rule? Boolean (Values (Listof CSS-Component-Value) (Option CSS-Simple-Block)))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-qualified-rule
    (lambda [css #:@rule? at-rule?]
      (let consume-item : (Values (Listof CSS-Component-Value) (Option CSS-Simple-Block))
        ([prelude : (Listof CSS-Component-Value) null]
         [simple-block : (Option CSS-Simple-Block) #false])
        (define token (css-read-syntax css))
        (cond [(or (eof-object? token) (and at-rule? (css:delim=:=? token #\;)))
               (when (eof-object? token)
                 (css-make-syntax-error (reverse prelude) "unexpected end of stream"))
               (values (reverse prelude) simple-block)]
              [(css:delim=:=? token #\{)
               (values (reverse prelude) (css-consume-simple-block css token #\}))]
              [(and (css-simple-block? token) (css:delim=:=? (css-simple-block-open token) #\{))
               (values (reverse prelude) token)]
              [else (consume-item (cons (css-consume-component-value css token) prelude) simple-block)]))))

  (define css-consume-block-body : (-> Input-Port Char (Values (Listof CSS-Component-Value) CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css close-char]
      (let consume-body : (Values (Listof CSS-Component-Value) CSS:Delim)
        ([components : (Listof CSS-Component-Value) null])
        (define token (css-read-syntax css))
        (cond [(eof-object? token)
               (css-make-syntax-error token "unexpected end of stream")
               (values (reverse components) (make-token (css-srcloc css #false #false #false) css:delim close-char))]
              [(css:delim=:=? token close-char)
               (values (reverse components) token)]
              [else (consume-body (cons (css-consume-component-value css token) components))]))))
  
  (define css-consume-components : (->* (Input-Port (Option Char)) ((Option CSS-Component-Value)) (Listof CSS-Component-Value))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [/dev/cssin [stop-char #false] [reconsumed-token #false]]
      (let consume-component : (Listof CSS-Component-Value)
        ([components : (Listof CSS-Component-Value)
          (cond [(false? reconsumed-token) null]
                [else (list (css-consume-component-value /dev/cssin reconsumed-token))])])
        (define token (css-read-syntax /dev/cssin))
        (cond [(eof-object? token) (reverse components)]
              [(and (char? stop-char) (css:delim=:=? token stop-char)) (reverse components)]
              [else (consume-component (cons (css-consume-component-value /dev/cssin token) components))]))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->declaration : (-> CSS:Ident (Listof CSS-Component-Value) (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
    (lambda [id-token components]
      (define :components : (Option (Listof CSS-Component-Value)) (memf (negate css:whitespace?) components))
      (cond [(or (false? :components) (not (css:delim=:=? (car :components) #\:)))
             (css-make-syntax-error id-token "missing colon in declaration: ~a" (css-token->datum id-token))]
            [else (let ([stnenopmoc (reverse (filter-not css:whitespace? (cdr :components)))])
                    (define important? : Boolean
                      (and (> (length stnenopmoc) 1)
                           (css:delim=:=? (list-ref stnenopmoc 1) #\!)
                           (css:ident=:=? (list-ref stnenopmoc 0) 'important)))
                    (css-declaration id-token important?
                                     (cond [(false? important?) (cdr :components)]
                                           [else (dropf-right (cdr :components)
                                                              (λ [com] (or (css:whitespace? com)
                                                                           (css:delim=:=? com #\!)
                                                                           (css:ident=:=? com 'important))))])))])))

  (define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule CSS-Style-Rule)
    ;;; https://drafts.csswg.org/css-syntax/#style-rules
    (lambda [qr]
      (define components : (Listof CSS-Component-Value) (css-simple-block-components (css-qualified-rule-block qr)))
      (define srotpircsed : (Listof CSS-Declaration)
        (for/fold ([descriptors : (Listof CSS-Declaration) null])
                  ([mixed (in-list (css-parse-declarations components))])
          (cond [(css-declaration? mixed) (cons mixed descriptors)]
                [else (css-make-syntax-error (cons (css-@rule-name mixed) (css-@rule-prelude mixed))
                                             "ignored the unexpected at-rule found inside the style rule.")
                      descriptors])))
      (css-style-rule (css-qualified-rule-prelude qr) (reverse srotpircsed))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-open-input-port : (-> CSS-StdIn Boolean Input-Port)
    ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
    (lambda [/dev/stdin keep-whitespace?]
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
          (let ([/dev/rawin (cond [(string? /dev/stdin) (open-input-string /dev/stdin '/dev/cssin)]
                                  [(byte? /dev/stdin) (open-input-bytes /dev/stdin '/dev/cssin)]
                                  [(port? /dev/stdin) /dev/stdin]
                                  [else (current-input-port)])])
            ; (make-input-port/read-to-peek) is (less than 1.5 times) slower than (make-input-port),
            ; but this cost is too subtle to becoming the performance killer even for large datasource.
            (define /dev/cssin : Input-Port (css-fallback-encode-input-port /dev/rawin))
            (make-input-port/read-to-peek (object-name /dev/rawin)
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
      (cond [(or (eof-object? stx) (css-component-value? stx)) stx]
            [else (make-bad-token (css-srcloc css #false #false #false)
                                  css:bad:stdin css-token stx)])))
  
  (define css-read-syntax/skip-whitespace : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (define token (css-read-syntax css))
      (cond [(not (css:whitespace? token)) token]
            [else (css-read-syntax/skip-whitespace css)])))
  
  (define css-CDO/CDC? : (-> CSS-Syntax-Any Boolean : #:+ (U CSS:<!-- CSS:-->))
    (lambda [token]
      (or (css:<!--? token)
          (css:-->? token))))

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
  (time (with-input-from-file tamer.css (thunk (void (read-css-stylesheet))))))
  