#lang at-exp typed/racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.w3.org/Style/CSS/specs.en.html                                                   ;;;
;;;                                                                                             ;;;
;;; https://drafts.csswg.org/css-syntax                                                         ;;;
;;; https://drafts.csswg.org/css-cascade                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define read-css : (->* () (Input-Port) CSS-Stylesheet)
  (lambda [[/dev/rawin (current-input-port)]]
    (css-parse-stylesheet /dev/rawin)))

(define string->css : (->* (String) (Any) CSS-Stylesheet)
  (lambda [css [src #false]]
    (read-css (open-input-string css (or src '/dev/stdin/string)))))

(define bytes->css : (->* (Bytes) (Any) CSS-Stylesheet)
  (lambda [css [src #false]]
    (read-css (open-input-bytes css (or '/dev/stdin/bytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama/tokenizer typed/racket ;;; https://drafts.csswg.org/css-syntax/#tokenization
  (provide (all-defined-out))

  (define-syntax (struct: stx)
    (syntax-case stx [:]
      [(_ id : ID rest ...)
       #'(begin (define-type ID id)
                (struct id rest ... #:prefab))]))
  
  (struct: css-srcloc : CSS-Srcloc
    ([in : Input-Port] [line : (Option Positive-Integer)] [col : (Option Natural)] [pos : (Option Positive-Integer)]))
  
  (struct: css-token : CSS-Token
    ([src : Any] [line : (Option Positive-Integer)] [column : (Option Natural)]
                 [position : (Option Positive-Integer)] [span : (Option Natural)]))

  (struct <!-- css-token () #:prefab)
  (struct --> css-token () #:prefab)
  
  (struct: css:bad : CSS:Bad css-token ([type : Symbol] [reason : String]))
  (struct: css:column : CSS:Column css-token ())
  (struct: css:whitespace : CSS:WhiteSpace css-token ())
  (struct: css:comment : CSS:Comment css:whitespace ([content : Bytes]))
  
  (struct: css:ident : CSS:Ident css-token ([datum : Symbol]))
  (struct: css:function : CSS:Function css-token ([datum : Symbol]))
  (struct: css:@keyword : CSS:@Keyword css-token ([datum : Keyword]))
  (struct: css:hash : CSS:Hash css-token ([datum : Keyword] [type : (U 'id 'unrestricted)]))
  (struct: css:delim : CSS:Delim css-token ([datum : Char]))
  (struct: css:numeric : CSS:Numeric css-token ([representation : String] [datum : Real]))
  (struct: css:dimension : CSS:Dimension css:numeric ([unit : Symbol]))
  (struct: css:string : CSS:String css-token ([datum : String]))
  (struct: css:url : CSS:URL css-token ([datum : String]))
  (struct: css:match : CSS:Match css-token ([datum : Char]))
  (struct: css:urange : CSS:URange css-token ([start : Integer] [end : Integer]))

  (define-syntax (make-token stx)
    (syntax-case stx []
      [(_ src css:token argl ...)
       #'(let-values ([(start-position) (css-srcloc-pos src)]
                      [(line column position) (port-next-location (css-srcloc-in src))])
           (define token (css:token (object-name (css-srcloc-in src))
                                    (css-srcloc-line src) (css-srcloc-col src) start-position
                                    (and (integer? position) (integer? start-position)
                                         (max (- position start-position) 0))
                                    argl ...))
           (when (css:bad? token)
             (log-message (current-logger) 'warning 'exn:css:read
                          (format "~a:~a:~a: ~a: ~a" (css-token-src token)
                                  (or (css-token-line token) -1) (or (css-token-column token) -1)
                                  (css:bad-type token) (css:bad-reason token))
                          token))
           token)]))

  (define css-token->datum : (-> CSS-Token Any)
    (lambda [token]
      (define v : (Vectorof Any) (struct->vector token))
      (match (if (css:numeric? token) (vector-drop v 7) (vector-drop v 6))
        [(vector datum) datum]
        [(vector datum suffix) (cons datum suffix)]
        [_ (cond [(css:whitespace? token) #\space]
                 [(css:column? token) '||]
                 [else (object-name token)])])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-token : (-> Input-Port (U EOF CSS-Token))
    ;;; (if still): https://drafts.csswg.org/css-syntax/#input-preprocessing
    ;;; (if still): https://drafts.csswg.org/css-syntax/#rule-defs (distinguish delim-token and from () [] {} ,:; and so on.
    ;;; https://drafts.csswg.org/css-syntax/#error-handling
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-token
    (lambda [/dev/cssin]
      (define codepoint : (U Char EOF) (read-char /dev/cssin))
      (define-values (line column position) (port-next-location /dev/cssin))
      (define srcloc (css-srcloc /dev/cssin line column position))
      (cond [(eof-object? codepoint) eof]
            [(char-whitespace? codepoint) (css-consume-whitespace /dev/cssin) (make-token srcloc css:whitespace)]
            [(char-numeric? codepoint) (css-consume-numeric-token srcloc codepoint)]
            [(css-char-name-prefix? codepoint) (css-consume-ident-token srcloc codepoint)]
            [else (case codepoint
                    [(#\' #\") (css-consume-string-token srcloc codepoint null)]
                    [(#\+ #\.) (css-consume-numeric-token srcloc codepoint)]
                    [(#\^ #\$ #\| #\~ #\*) (css-consume-match-token srcloc codepoint)]
                    [(#\#) (css-consume-hash-token srcloc)]
                    [(#\@) (css-consume-keyword-token srcloc)]
                    [(#\/) (css-consume-comment-token srcloc)]
                    [(#\< #\-) (css-consume-cd-token srcloc codepoint)]
                    [(#\null) (make-token srcloc css:delim #\uFFFD)]
                    [else (make-token srcloc css:delim codepoint)])])))
  
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
      
  (define css-consume-string-token : (-> CSS-Srcloc Char (Listof Char) (U CSS:String CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-string-token
    (lambda [srcloc quotation prefix]
      (define css : Input-Port (css-srcloc-in srcloc))
      (let consume-string-token : (U css:string css:bad) ([chars prefix])
        (define ch : (U EOF Char) (read-char css))
        (cond [(or (eof-object? ch) (char=? ch quotation))
               (when (eof-object? ch)
                 (make-token srcloc css:bad 'string "unexpected end of stream"))
               (make-token srcloc css:string (list->string (reverse chars)))]
              [(char=? ch #\newline)
               (make-token srcloc css:bad 'string "unexpected newline")]
              [(not (char=? ch #\\))
               (consume-string-token (cons ch chars))]
              [else (let ([next (peek-char css)])
                      (cond [(eof-object? next) (consume-string-token chars)]
                            [(char=? next #\newline) (read-char css) (consume-string-token (cons ch chars))]
                            [else (consume-string-token (cons (css-consume-escaped-char css) chars))]))]))))

  (define css-consume-numeric-token : (-> CSS-Srcloc Char (U CSS:Numeric CSS:Delim CSS:Bad))
    ;;;(TODO): https://drafts.csswg.org/css-syntax/#anb
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [srcloc sign/digit] ; NOTE: this is invoked after (css-consume-cd-token)
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (cond [(not (css-number-prefix? sign/digit ch1 ch2)) (make-token srcloc css:delim sign/digit)]
            [else (let-values ([(n representation) (css-consume-number css sign/digit)])
                    (cond [(string? n) (make-token srcloc css:bad 'number n)]
                          [else (let ([ch1 : (U EOF Char) (peek-char css 0)]
                                      [ch2 : (U EOF Char) (peek-char css 1)]
                                      [ch3 : (U EOF Char) (peek-char css 2)])
                                  (cond [(css-identifier-prefix? ch1 ch2 ch3)
                                         (define unit : Symbol (string->symbol (css-consume-name css null)))
                                         (make-token srcloc css:dimension representation n unit)]
                                        [(and (char? ch1) (char=? ch1 #\%)) (read-char css)
                                         (make-token srcloc css:dimension representation n '%)]
                                        [else (make-token srcloc css:numeric representation n)]))]))])))

  (define css-consume-url-token : (-> CSS-Srcloc (U CSS:URL CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-url-token
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (define ch : (U EOF Char) (read-char css))
      (if (or (eof-object? ch) (char=? ch #\)))
          (make-token srcloc css:url "")
          (let consume-url-token : (U css:url css:bad) ([chars (list ch)])
            (define next : (U EOF Char) (read-char css))
            (cond [(or (eof-object? next) (char=? next #\)))
                   (when (eof-object? next)
                     (make-token srcloc css:bad 'url "unexpected end of stream"))
                   (make-token srcloc css:url (list->string (reverse chars)))]
                  [(char-whitespace? next)
                   (css-consume-whitespace css)
                   (define end : (U EOF Char) (read-char css))
                   (if (or (eof-object? end) (char=? end #\)))
                       (make-token srcloc css:url (list->string (reverse chars)))
                       (css-consume-bad-url-remnants css (make-token srcloc css:bad 'url "unexpected whitespace")))]
                  [(css-valid-escape? next (peek-char css))
                   (consume-url-token (cons (css-consume-escaped-char css) chars))]
                  [(or (memq next '(#\\ #\" #\' #\()) (css-char-non-printable? next))
                   (css-consume-bad-url-remnants css (make-token srcloc css:bad 'url (format "invalid char: ~s" next)))]
                  [else (consume-url-token (cons next chars))])))))

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
      (cond [(> end #x10FFFF) (make-token srcloc css:bad 'urange (format "end value out of the unicode range: ~x" end))]
            [(> start end) (make-token srcloc css:bad 'urange (format "invalid unicode range: #x~x > #x~x" start end))]
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

  (define css-consume-keyword-token : (-> CSS-Srcloc (U CSS:@Keyword CSS:Delim))
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

  (define css-consume-cd-token : (-> CSS-Srcloc Char (U <!-- CSS:Delim CSS:Numeric CSS:Bad -->))
    ;;; https://drafts.csswg.org/css-syntax/#CDO-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#CDC-token-diagram
    (lambda [srcloc open/close]
      (define css : Input-Port (css-srcloc-in srcloc))
      (if (char=? open/close #\<)
          (let ([cd : (U EOF String) (peek-string 3 0 css)])
            (cond [(and (string? cd) (string=? cd "!--")) (read-string 3 css) (make-token srcloc <!--)]
                  [else (make-token srcloc css:delim #\<)]))
          (let ([cd : (U EOF String) (peek-string 2 0 css)])
            (cond [(and (string? cd) (string=? cd "->")) (read-string 2 css) (make-token srcloc -->)]
                  [else (css-consume-numeric-token srcloc #\-)])))))

  (define css-consume-comment-token : (-> CSS-Srcloc (U CSS:Comment CSS:Delim CSS:Bad))
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (cond [(or (eof-object? ch1) (not (char=? ch1 #\*))) (make-token srcloc css:delim #\/)]
            [(regexp-match #px".*?\\*/" css) => (λ [**/] (make-token srcloc css:comment (bytes-append #"/" (car **/))))]
            [else (make-token srcloc css:bad 'comment "unclosed comment")])))
  
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

  (define css-consume-number : (-> Input-Port Char (Values (U Real String) String))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [css sign/digit]
      (let consume-number : (Values (U Real String) String) ([chars (list sign/digit)])
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
                            [else (values (format "invalid char in Number: ~a" representation) representation)]))]))))

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
                   (and (char? ch2) (char=? ch2 #\-)))
               (css-valid-escape? ch2 ch3)))))

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
  
  (require (submod ".." digitama/tokenizer))

  (define-type CSS-Syntax-Error exn:fail:syntax)
  (define-type CSS-Rule (U CSS-Qualified-Rule CSS-@Rule))
  (define-type CSS-Component-Value (U CSS-Token CSS-Simple-Block CSS-Function))

  (struct: css-declaration : CSS-Declaration ([name : CSS:Ident] [important? : Boolean] [values : (Listof CSS-Component-Value)]))
  (struct: css-simple-block : CSS-Simple-Block ([open : CSS:Delim] [components : (Listof CSS-Component-Value)] [close : CSS:Delim]))
  (struct: css-function : CSS-Function ([name : CSS:Function] [arguments : (Listof CSS-Component-Value)]))
  
  (struct: css-qualified-rule : CSS-Qualified-Rule ([prelude : (Listof CSS-Component-Value)] [block : CSS-Simple-Block]))
  (struct: css-@rule : CSS-@Rule ([name : CSS:@Keyword] [prelude : (Listof CSS-Component-Value)] [block : (Option CSS-Simple-Block)]))

  (struct: css-stylesheet : CSS-Stylesheet ([source : Any] [charset : String] [rules : (Listof CSS-Rule)])
    #:extra-constructor-name make-css-stylesheet)

  (define css-make-syntax-error : (-> (U EOF CSS-Token (Listof CSS-Token)) String Any * CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#error-handling
    (lambda [tokens msgfmt . argl]
      (define message : String (if (null? argl) msgfmt (apply format msgfmt argl)))
      (define token-list : (Listof CSS-Token) (cond [(eof-object? tokens) null] [(list? tokens) tokens] [else (list tokens)]))
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

  (define css-cons : (All (CSS-DT) (-> (U CSS-Syntax-Error CSS-DT) (Listof CSS-DT) (Listof CSS-DT)))
    (lambda [item items]
      (cond [(exn? item) items]
            [else (cons item items)])))
  
  ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
  (define css-parse-stylesheet : (-> Input-Port CSS-Stylesheet)
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-stylesheet
    (lambda [/dev/rawin]
      (define datasource : Any (object-name /dev/rawin))
      (define rules : (Listof CSS-Rule) (css-parse-rules /dev/rawin #:top-level? #true))
      (cond [(null? rules) (make-css-stylesheet datasource "UTF-8" null)]
            [else (let ([rule : CSS-Rule (car rules)])
                    (if (and (css-@rule? rule) (css:@keyword-datum=? (css-@rule-name rule) '#:@charset))
                        (let* ([token (car (css-@rule-prelude rule))]
                               [name (if (css:string? token) (css:string-datum token) "UTF-8")])
                          (make-css-stylesheet datasource (css-fallback-charset name) (cdr rules)))
                        (make-css-stylesheet datasource "UTF-8" rules)))])))

  (define css-parse-rules : (-> Input-Port [#:top-level? Boolean] (Listof CSS-Rule))
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-list-of-rules
    (lambda [/dev/rawin #:top-level? [toplevel? #false]]
    (define /dev/cssin : Input-Port (css-fallback-encode-input-port /dev/rawin))
    (dynamic-wind (thunk (port-count-lines! /dev/cssin))
                  (thunk (css-consume-rules /dev/cssin toplevel? null))
                  (thunk (unless (eq? /dev/rawin /dev/cssin)
                           (close-input-port /dev/cssin))))))

  (define css-parse-rule : (-> Input-Port (U CSS-Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#parse-rule
    (lambda [/dev/cssin]
      (define token (css-consume-token/skip-whitespace /dev/cssin))
      (define retval : (U CSS-Rule CSS-Syntax-Error)
        (cond [(eof-object? token) (css-make-syntax-error token "unexpected end of stream")]
              [(css:@keyword? token) (css-consume-at-rule /dev/cssin token)]
              [else (css-consume-qualified-rule /dev/cssin token)]))
      (define next (css-consume-token /dev/cssin))
      (cond [(or (eof-object? next) (exn? retval)) retval]
            [else (css-make-syntax-error (list next) "too many rules found")])))

  (define css-parse-declaration : (-> Input-Port [#:stop-char (Option Char)] (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#declaration
    ;;; https://drafts.csswg.org/css-syntax/#parse-declaration
    (lambda [/dev/cssin #:stop-char [stop-char #false]]
      (define token (css-consume-token/skip-whitespace /dev/cssin))
      (cond [(not (css:ident? token)) (css-make-syntax-error token "only ident token should be here")]
            [else (css-components->declaration token (css-parse-component-values /dev/cssin #:stop-char stop-char))])))

  (define css-parse-declaration+at-rules : (-> Input-Port (Listof (U CSS-Declaration CSS-@Rule)))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-declarations
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-list-of-declarations
    (lambda [/dev/cssin]
      (let consume-declaration+at-rule : (Listof (U CSS-Declaration CSS-@Rule))
        ([mixed-list : (Listof (U CSS-Declaration CSS-@Rule)) null])
        (define token (css-consume-token /dev/cssin))
        (cond [(eof-object? token)
               (reverse mixed-list)]
              [(or (css:whitespace? token) (css:delim-datum=? token #\;))
               (consume-declaration+at-rule mixed-list)]
              [(css:ident? token)
               (consume-declaration+at-rule
                (css-cons (css-components->declaration token (css-parse-component-values /dev/cssin #:stop-char #\;))
                          mixed-list))]
              [else (css-parse-component-values /dev/cssin #:stop-char #\;)
               (consume-declaration+at-rule mixed-list)]))))
  
  (define css-parse-component-value : (-> Input-Port (U CSS-Component-Value CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#parse-component-value
    (lambda [/dev/cssin]
      (define token (css-consume-token /dev/cssin))
      (cond [(eof-object? token) (css-make-syntax-error token "unexpected end of stream")]
            [else (let ([retval (css-consume-component-value /dev/cssin token)])
                    (define next (css-consume-token /dev/cssin))
                    (cond [(eof-object? next) retval]
                          [else (css-make-syntax-error next "too many component values found")]))])))

  (define css-parse-component-values : (-> Input-Port [#:stop-char (Option Char)] (Listof CSS-Component-Value))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [/dev/cssin #:stop-char [stop-char #false]]
      (let consume-component : (Listof CSS-Component-Value) ([components : (Listof CSS-Component-Value) null])
        (define token (css-consume-token /dev/cssin))
        (cond [(eof-object? token) (reverse components)]
              [(and (char? stop-char) (css:delim-datum=? token stop-char)) (reverse components)]
              [else (consume-component (cons (css-consume-component-value /dev/cssin token)
                                             components))]))))

  (define css-parse-component-valueses : (-> Input-Port [#:stop-char (Option Char)] (Listof (Listof CSS-Component-Value)))
    ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
    (lambda [/dev/cssin #:stop-char [stop-char #false]]
      (let consume-components : (Listof (Listof CSS-Component-Value))
        ([componentses : (Listof (Listof CSS-Component-Value)) null])
        (define ch (peek-char /dev/cssin))
        (cond [(eof-object? ch) (reverse componentses)]
              [else (consume-components (cons (css-parse-component-values /dev/cssin #:stop-char #\,) componentses))]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-rules : (-> Input-Port Boolean (Listof CSS-Rule) (Listof CSS-Rule))
    ;;; https://drafts.csswg.org/css-syntax/#consume-list-of-rules
    (lambda [css toplevel? rules0]
      (let consume-rules : (Listof CSS-Rule) ([rules rules0])
        (define token (css-consume-token css))
        (cond [(eof-object? token) (reverse rules)]
              [(css:whitespace? token) (consume-rules rules)]
              [(css:@keyword? token) (consume-rules (css-cons (css-consume-at-rule css token) rules))]
              [(css-CDO/CDC? token) (consume-rules (if toplevel? rules (css-cons (css-consume-qualified-rule css token) rules)))]
              [else (consume-rules (css-cons (css-consume-qualified-rule css token) rules))]))))

  (define css-consume-at-rule : (-> Input-Port CSS:@Keyword CSS-@Rule)
    ;;; https://drafts.csswg.org/css-syntax/#at-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-at-rule
    (lambda [css reconsumed-at-token]
      (define-values (prelude maybe-block) (css-consume-rule-item css #:@rule? #true))
      (css-@rule reconsumed-at-token prelude maybe-block)))

  (define css-consume-qualified-rule : (-> Input-Port CSS-Token (U CSS-Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    (lambda [css reconsumed-token]
      (define-values (prelude maybe-block) (css-consume-rule-item css #:@rule? #false))
      (cond [(css-simple-block? maybe-block) (css-qualified-rule (cons reconsumed-token prelude) maybe-block)]
            [else (css-make-syntax-error (list reconsumed-token) "qualified rule should have a simple block")])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-token/skip-whitespace : (-> Input-Port (U EOF CSS-Token))
    (lambda [css]
      (define token (css-consume-token css))
      (cond [(not (css:whitespace? token)) token]
            [else (css-consume-token/skip-whitespace css)])))
  
  (define css-consume-rule-item : (-> Input-Port #:@rule? Boolean (Values (Listof CSS-Component-Value) (Option CSS-Simple-Block)))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-qualified-rule
    (lambda [css #:@rule? at-rule?]
      (let consume-rule : (Values (Listof CSS-Component-Value) (Option CSS-Simple-Block))
        ([prelude : (Listof CSS-Component-Value) null]
         [simple-block : (Option CSS-Simple-Block) #false])
        (define token (css-consume-token css))
        (cond [(or (eof-object? token) (and at-rule? (css:delim-datum=? token #\;)))
               (when (eof-object? token)
                 (css-make-syntax-error (filter css-token? (reverse prelude)) "unexpected end of stream"))
               (values (reverse prelude) simple-block)]
              [(css:delim-datum=? token #\{)
               (values (reverse prelude) (css-consume-simple-block css token #\}))]
              [else (consume-rule (cons (css-consume-component-value css token) prelude) simple-block)]))))

  (define css-consume-simple-block : (-> Input-Port CSS:Delim Char CSS-Simple-Block)
    ;;; https://drafts.csswg.org/css-syntax/#block
    (lambda [css open-token close-char]
      (define-values (components close-token) (css-consume-components css null close-char))
      (css-simple-block open-token components close-token)))

  (define css-consume-function : (-> Input-Port CSS:Function CSS-Function)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css name-token]
      (define-values (arguments _) (css-consume-components css null #\)))
      (css-function name-token arguments)))
  
  (define css-consume-component-value : (-> Input-Port CSS-Token CSS-Component-Value)
    ;;; https://drafts.csswg.org/css-syntax/#component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-component-value
    (lambda [css reconsumed-token]
      (cond [(css:delim-datum=? reconsumed-token #\{) (css-consume-simple-block css reconsumed-token #\})]
            [(css:delim-datum=? reconsumed-token #\() (css-consume-simple-block css reconsumed-token #\))]
            [(css:delim-datum=? reconsumed-token #\[) (css-consume-simple-block css reconsumed-token #\])]
            [(css:function? reconsumed-token) (css-consume-function css reconsumed-token)]
            [else reconsumed-token])))

  (define css-consume-components : (-> Input-Port (Listof CSS-Component-Value) Char (Values (Listof CSS-Component-Value) CSS:Delim))
    (lambda [css components close-char]
      (define token (css-consume-token css))
      (cond [(eof-object? token)
             (css-make-syntax-error token "unexpected end of stream")
             (values (reverse components) (make-token (css-srcloc css #false #false #false) css:delim close-char))]
            [(css:delim-datum=? token close-char)
             (values (reverse components) token)]
            [else (css-consume-components css (cons token components) close-char)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-CDO/CDC? : (-> CSS-Token Boolean : #:+ (U <!-- -->))
    (lambda [token]
      (or (<!--? token)
          (-->? token))))

  (define css:delim-datum=? : (-> Any Char Boolean : #:+ CSS:Delim)
    (lambda [token ch]
      (and (css:delim? token)
           (char=? (css:delim-datum token) ch))))

  (define css:ident-datum=? : (-> Any Symbol Boolean : #:+ CSS:Ident)
    (lambda [token id]
      (and (css:ident? token)
           (or (symbol=? (css:ident-datum token) id)
               (string-ci=? (symbol->string (css:ident-datum token))
                            (symbol->string id))))))

  (define css-function-token=? : (-> Any Symbol Boolean : #:+ CSS:Function)
    (lambda [token id]
      (and (css:function? token)
           (or (symbol=? (css:function-datum token) id)
               (string-ci=? (symbol->string (css:function-datum token))
                            (symbol->string id))))))

  (define css:@keyword-datum=? : (-> Any Keyword Boolean : #:+ CSS:@Keyword)
    (lambda [token meta-id]
      (and (css:@keyword? token)
           (or (eq? (css:@keyword-datum token) meta-id)
               (string-ci=? (keyword->string (css:@keyword-datum token))
                            (keyword->string meta-id))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->declaration : (-> CSS:Ident (Listof CSS-Component-Value) (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
    (lambda [id-token components]
      (define :components : (Option (Listof CSS-Component-Value)) (memf (negate css:whitespace?) components))
      (cond [(or (false? :components) (not (css:delim-datum=? (car :components) #\:)))
             (css-make-syntax-error (list id-token) "missing colon in declaration: ~a" (css-token->datum id-token))]
            [else (let ([stnenopmoc (reverse (filter-not css:whitespace? (cdr :components)))])
                    (define important? : Boolean
                      (and (> (length stnenopmoc) 1)
                           (css:delim-datum=? (list-ref stnenopmoc 1) #\!)
                           (css:ident-datum=? (list-ref stnenopmoc 0) 'important)))
                    (css-declaration id-token important?
                                     (cond [(false? important?) (cdr :components)]
                                           [else (dropf-right (cdr :components)
                                                              (λ [com] (or (css:whitespace? com)
                                                                           (css:delim-datum=? com #\!)
                                                                           (css:ident-datum=? com 'important))))])))]))))

(require (submod "." digitama/tokenizer))
(require (submod "." digitama/parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test typed/racket
  (require (submod ".."))
  
  (define tamer.css (simplify-path (build-path (current-directory) 'up "tamer" "test.css")))
  (call-with-input-file tamer.css (λ [[in : Input-Port]] (time (read-css in)))))
  