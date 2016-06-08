#lang at-exp typed/racket

(provide (all-defined-out) current-css-source)

;;; http://www.w3.org/Style/CSS/specs.en.html

(define read-css : (->* () (Input-Port Any) CSSExpr)
  ;;; http://www.w3.org/TR/css-syntax-3/#tokenization
  (lambda [[/dev/cssin (current-input-port)] [src #false]]
    (parameterize ([current-css-source src])
      (port-count-lines! /dev/cssin)
      (css-parse-stylesheet /dev/cssin))))

(define string->css : (->* (String) (Any) Any)
  (lambda [css [src #false]]
    (read-css (open-input-string css) src)))

(define bytes->css : (->* (Bytes) (Any) Any)
  (lambda [css [src #false]]
    (read-css (open-input-bytes css) src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama/tokenizer typed/racket ;;; http://www.w3.org/TR/css-syntax-3/#tokenization
  (provide (all-defined-out))

  (define-type CSS-Token css-token)
  (struct css-token ([src : Any] [line : (Option Positive-Integer)] [column : (Option Natural)]
                                 [position : (Option Positive-Integer)] [span : (Option Natural)])
    #:prefab)

  (struct css:bad css-token ([message : String]) #:prefab)
  (struct css:whitespace css-token () #:prefab)
  (struct css:comment css-token ([content : Bytes]) #:prefab)
  
  (struct css:ident css-token ([datum : Symbol]) #:prefab)
  (struct css:function css-token ([datum : Symbol]) #:prefab)
  (struct css:keyword css-token ([datum : Keyword]) #:prefab)
  (struct css:hash css-token ([datum : Symbol] [type : (U 'id 'unrestricted)]) #:prefab)
  (struct css:delim css-token ([datum : Char]) #:prefab)
  (struct css:numeric css-token ([datum : Real]) #:prefab)
  (struct css:dimension css:numeric ([unit : Symbol]) #:prefab)
  (struct css:string css-token ([datum : String]) #:prefab)
  (struct css:url css-token ([datum : String]) #:prefab)
  (struct css:match css-token ([datum : Char]) #:prefab)
  (struct css:u+range css-token ([start : Integer] [end : Integer]) #:prefab)

  (struct <!-- css-token () #:prefab)
  (struct --> css-token () #:prefab)
  (struct || css-token () #:prefab)

  (define-syntax (make-token stx)
    (syntax-case stx []
      [(_ /dev/cssin start-position css:token argl ...)
       #'(let-values ([(line column position) (port-next-location /dev/cssin)])
           (define token (css:token (or (current-css-source) (object-name /dev/cssin))
                                    line column start-position
                                    (and (integer? position)
                                         (integer? start-position)
                                         (max (- position start-position) 0))
                                    argl ...))
           (when (css:bad? token)
             (log-message (current-logger) 'error 'css (css:bad-message token) token))
           token)]))

  (define css-token->datum : (-> CSS-Token Any)
    (lambda [token]
      (match (vector-drop (struct->vector token) 6)
        [(vector datum) datum]
        [(vector datum suffix) (cons datum suffix)]
        [_ (cond [(css:whitespace? token) #\space]
                 [(||? token) 'token]
                 [else (object-name token)])])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define current-css-source : (Parameterof Any) (make-parameter #false))
  (define css-consume-token : (-> Input-Port (U EOF CSS-Token))
    ;;; (if still): http://www.w3.org/TR/css-syntax-3/#input-preprocessing
    ;;; http://www.w3.org/TR/css-syntax-3/#error-handling
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-token
    (lambda [/dev/cssin]
      (define-values (line column position) (port-next-location /dev/cssin))
      (define codepoint : (U Char EOF) (read-char /dev/cssin))
      (cond [(eof-object? codepoint) eof]
            [(char-whitespace? codepoint) (css-consume-whitespace /dev/cssin) (make-token /dev/cssin position css:whitespace)]
            [(char-numeric? codepoint) (css-consume-numeric-token /dev/cssin position codepoint)]
            [(css-char-name-prefix? codepoint) (css-consume-ident-token /dev/cssin position codepoint)]
            [else (case codepoint
                    [(#\' #\") (css-consume-string-token /dev/cssin position codepoint null)]
                    [(#\+ #\.) (css-consume-numeric-token /dev/cssin position codepoint)]
                    [(#\^ #\$ #\| #\~ #\*) (css-consume-match-token /dev/cssin position codepoint)]
                    [(#\#) (css-consume-hash-token /dev/cssin position)]
                    [(#\@) (css-consume-keyword-token /dev/cssin position)]
                    [(#\/) (css-consume-comment-token /dev/cssin position)]
                    [(#\< #\-) (css-consume-cd-token /dev/cssin position codepoint)]
                    [(#\null) (make-token /dev/cssin position css:delim #\uFFFD)]
                    [else (make-token /dev/cssin position css:delim codepoint)])])))
  
  (define css-consume-ident-token : (-> Input-Port (Option Positive-Integer) Char (U css:ident css:function css:url css:u+range css:bad))
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-unicode-range-token0
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-an-ident-like-token0
    (lambda [css position id0]
      (define ch1 : (U EOF Char) (read-char css))
      (define ch2 : (U EOF Char) (peek-char css))
      (cond [(and (char-ci=? id0 #\u) (char? ch1) (char=? ch1 #\+)
                  (or (char-hexdigit? ch2) (and (char? ch2) (char=? ch2 #\?))))
             (css-consume-unicode-range-token css position)]
            [else (let ([name : String (css-consume-name css (if (eof-object? ch1) (list id0) (list ch1 id0)))])
                    (define ch : (U EOF Char) (peek-char css))
                    (cond [(and (char? ch) (char=? ch #\())
                           (read-char css)
                           (cond [(string-ci=? name "url") (css-consume-url-token css position)]
                                 [else (make-token css position css:function (string->symbol name))])]
                          [else (make-token css position css:ident (string->symbol name))]))])))
      
  (define css-consume-string-token : (-> Input-Port (Option Positive-Integer) Char (Listof Char) (U css:string css:bad))
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-string-token0
    (lambda [css position quotation prefix]
      (let consume-string-token : (U css:string css:bad) ([chars prefix])
        (define ch : (U EOF Char) (read-char css))
        (cond [(or (eof-object? ch) (char=? ch quotation))
               (make-token css position css:string (list->string (reverse chars)))]
              [(char=? ch #\newline)
               (make-token css position css:bad (format "unexpected newline in String: ~s" (list->string (reverse chars))))]
              [(not (char=? ch #\\))
               (consume-string-token (cons ch chars))]
              [else (let ([next (peek-char css)])
                      (cond [(eof-object? next) (consume-string-token chars)]
                            [(and (char? next) (char=? next #\newline)) (read-char css) (consume-string-token (cons ch chars))]
                            [else (consume-string-token (cons (css-consume-escaped-char css) chars))]))]))))

  (define css-consume-numeric-token : (-> Input-Port (Option Positive-Integer) Char (U css:numeric css:delim css:bad))
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-number
    (lambda [css position sign/digit] ; NOTE: this is invoked after (css-consume-cd-token)
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (cond [(not (css-number-prefix? sign/digit ch1 ch2)) (make-token css position css:delim sign/digit)]
            [else (let ([n : (U Real String) (css-consume-number css sign/digit)])
                    (cond [(string? n) (make-token css position css:bad n)]
                          [else (let ([ch1 : (U EOF Char) (peek-char css 0)]
                                      [ch2 : (U EOF Char) (peek-char css 1)]
                                      [ch3 : (U EOF Char) (peek-char css 2)])
                                  (cond [(css-identifier-prefix? ch1 ch2 ch3)
                                         (define unit : Symbol (string->symbol (css-consume-name css null)))
                                         (make-token css position css:dimension n unit)]
                                        [(and (char? ch1) (char=? ch1 #\%))
                                         (read-char css)
                                         (make-token css position css:dimension n '%)]
                                        [else (make-token css position css:numeric n)]))]))])))

  (define css-consume-url-token : (-> Input-Port (Option Positive-Integer) (U css:url css:bad))
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-url-token0
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-the-remnants-of-a-bad-url
    (lambda [css position]
      (define cleanup? : (Parameterof Boolean) (make-parameter #false))
      ((inst dynamic-wind (U css:url css:bad))
       (thunk (css-consume-whitespace css))
       (thunk (let ([ch : (U EOF Char) (read-char css)])
                (cond [(or (eof-object? ch) (char=? ch #\)))
                       (make-token css position css:url "")]
                      [(or (char=? ch #\") (char=? ch #\'))
                       (define quoted-url : (U css:string css:bad) (css-consume-string-token css position ch null))
                       (cond [(css:bad? quoted-url) (cleanup? #true) quoted-url]
                             [else (css-consume-whitespace css)
                              (let ([next : (U EOF Char) (read-char css)])
                                (cond [(or (eof-object? next) (char=? next #\)))
                                       (make-token css position css:url (css:string-datum quoted-url))]
                                      [else (cleanup? #true)
                                       (make-token css position css:bad (format "invalid char in URL token: ~s" next))]))])]
                      [else (let consume-unquoted-url-token : (U css:url css:bad) ([chars (list ch)])
                           (css-consume-whitespace css)
                           (define next : (U EOF Char) (read-char css))
                           (cond [(or (eof-object? next) (char=? next #\)))
                                  (make-token css position css:url (list->string (reverse chars)))]
                                 [(css-valid-escape? next (peek-char css))
                                  (consume-unquoted-url-token (cons (css-consume-escaped-char css) chars))]
                                 [(or (memq next '(#\\ #\" #\' #\()) (css-char-non-printable? next))
                                  (cleanup? #true)
                                  (make-token css position css:bad (format "invalid char in URL token: ~s" next))]
                                 [else (consume-unquoted-url-token (cons next chars))]))])))
       (thunk (when (cleanup?)
                (let cleanup : Void ()
                  (css-consume-whitespace css)
                  (regexp-match #px".*?[\\)]" css) ; consuming all but #\\ or #\)
                  (define ch : (U EOF Char) (read-char css))
                  (cond [(or (eof-object? ch) (char=? ch #\))) (void)]
                        [(css-valid-escape? ch (peek-char css)) (read-char css) (cleanup)]
                        [else (cleanup)])))))))

  (define css-consume-unicode-range-token : (-> Input-Port (Option Positive-Integer) css:u+range)
    (lambda [css position]
      (define-values (n rest) (css-consume-hexadecimal css 6))
      (define-values (start end)
        (let consume-? : (Values Integer Integer) ([s : Integer n]
                                                   [e : Integer n]
                                                   [? : Integer rest])
          (cond [(zero? ?) (values s e)]
                [else (let ([ch : (U EOF Char) (peek-char css)])
                        (cond [(or (eof-object? ch) (not (char=? ch #\?))) (values s e)]
                              [else (read-char css) (consume-? (* s 16)
                                                               (+ (* e 16) (char->hexadecimal #\f))
                                                               (sub1 ?))]))])))
      (cond [(not (= start end)) (make-token css position css:u+range start end)]
            [else (let ([ch1 (peek-char css 0)]
                        [ch2 (peek-char css 1)])
                    (cond [(and (char? ch1) (char=? ch1 #\-) (char-hexdigit? ch2))
                           (read-char css)
                           (define-values (end _) (css-consume-hexadecimal css 6))
                           (make-token css position css:u+range start end)]
                          [else (make-token css position css:u+range start start)]))])))

  (define css-consume-hash-token : (-> Input-Port (Option Positive-Integer) (U css:hash css:delim))
    ;;; http://www.w3.org/TR/css-syntax-3/#hash-token-diagram
    (lambda [css position]
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (or (css-char-name? ch1) (css-valid-escape? ch1 ch2))
          (make-token css position css:hash
                      (string->symbol (css-consume-name css (list #\#)))
                      (if (css-identifier-prefix? ch1 ch2 ch3) 'id 'unrestricted))
          (make-token css position css:delim #\#))))

  (define css-consume-keyword-token : (-> Input-Port (Option Positive-Integer) (U css:keyword css:delim))
    ;;; http://www.w3.org/TR/css-syntax-3/#at-keyword-token-diagram
    (lambda [css position]
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (css-identifier-prefix? ch1 ch2 ch3)
          (make-token css position css:keyword (string->keyword (css-consume-name css (list #\@))))
          (make-token css position css:delim #\@))))

  (define css-consume-match-token : (-> Input-Port (Option Positive-Integer) Char (U css:match css:delim ||))
    ;;; http://www.w3.org/TR/css-syntax-3/#include-match-token-diagram
    ;;; http://www.w3.org/TR/css-syntax-3/#column-token-diagram
    (lambda [css position prefix]
      (define ch : (U EOF Char) (peek-char css))
      (cond [(and (char? ch) (char=? ch #\=))
             (read-char css)
             (make-token css position css:match prefix)]
            [(and (char=? prefix #\|) (char? ch) (char=? ch #\|))
             (read-char css)
             (make-token css position ||)]
            [else (make-token css position css:delim prefix)])))

  (define css-consume-cd-token : (-> Input-Port (Option Positive-Integer) Char (U <!-- css:delim css:numeric css:bad -->))
    ;;; http://www.w3.org/TR/css-syntax-3/#CDO-token-diagram
    ;;; http://www.w3.org/TR/css-syntax-3/#CDC-token-diagram
    (lambda [css position open/close]
      (if (char=? open/close #\<)
          (let ([cd : (U EOF String) (peek-string 3 0 css)])
            (cond [(and (string? cd) (string=? cd "!--")) (read-string 3 css) (make-token css position <!--)]
                  [else (make-token css position css:delim #\<)]))
          (let ([cd : (U EOF String) (peek-string 2 0 css)])
            (cond [(and (string? cd) (string=? cd "->")) (read-string 2 css) (make-token css position -->)]
                  [else (css-consume-numeric-token css position #\-)])))))

  (define css-consume-comment-token : (-> Input-Port (Option Positive-Integer) (U css:comment css:delim css:bad))
    (lambda [css position]
      (define ch1 : (U EOF Char) (peek-char css 0))
      (cond [(or (eof-object? ch1) (not (char=? ch1 #\*))) (make-token css position css:delim #\/)]
            [(regexp-match #px".*?\\*/" css) => (λ [comment] (make-token css position css:comment (bytes-append #"/" (car comment))))]
            [else (make-token css position css:bad "unclosed comment")])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-whitespace : (-> Input-Port Void)
    (lambda [css]
      (regexp-match #px"\\s*" css)
      (void)))
  
  (define css-consume-name : (-> Input-Port (Listof Char) String)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-name
    (lambda [css chars]
      (define ch : (U EOF Char) (peek-char css))
      (cond [(css-char-name? ch)
             (read-char css)
             (css-consume-name css (cons ch chars))]
            [(css-valid-escape? ch (peek-char css 1))
             (read-char css)
             (css-consume-name css (cons (css-consume-escaped-char css) chars))]
            [else (list->string (reverse chars))])))

  (define css-consume-number : (-> Input-Port Char (U Real String))
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-number
    (lambda [css sign/digit]
      (let consume-number : (U Real String) ([chars (list sign/digit)])
        (define ch : (U EOF Char) (peek-char css))
        (cond [(and (char? ch)
                    (or (char-numeric? ch)
                        (char=? ch #\+)
                        (char=? ch #\-)
                        (css-decimal-point? ch (peek-char css 1))
                        (css-scientific-notation? ch (peek-char css 1) (peek-char css 2))))
               (read-char css)
               (consume-number (cons ch chars))]
              [else (let* ([representation : String (list->string (reverse chars))]
                           [maybe-number : (Option Complex) (string->number representation)])
                      (cond [(real? maybe-number) maybe-number]
                            [else (format "invalid char in Number: ~a" representation)]))]))))

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
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-an-escaped-code-point
    (lambda [css]
      (define esc : (U EOF Char) (read-char css))
      (cond [(eof-object? esc) #\uFFFD]
            [(not (char-hexdigit? esc)) esc]
            [else (let-values ([(hex _) (css-consume-hexadecimal css (sub1 6) (char->hexadecimal esc) #:\s?$? #true)])
                    (cond [(or (<= hex 0) (> hex #x10FFFF)) #\uFFFD] ; #\nul and max unicode
                          [(<= #xD800 hex #xDFFF) #\uFFFD] ; surrogate
                          [else (integer->char hex)]))])))    
  
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
    ;;; http://www.w3.org/TR/css-syntax-3/#escaping
    ;;; http://www.w3.org/TR/css-syntax-3/#starts-with-a-valid-escape
    (lambda [ch1 ch2]
      (and (char? ch1)
           (char=? ch1 #\\)
           (or (eof-object? ch2)
               (not (char=? ch2 #\newline))))))

  (define css-identifier-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; http://www.w3.org/TR/css-syntax-3/#would-start-an-identifier
    (lambda [ch1 ch2 ch3]
      (or (css-char-name-prefix? ch1)
          (css-valid-escape? ch1 ch2)
          (and (char? ch1)
               (char=? ch1 #\-)
               (css-char-name-prefix? ch2)
               (css-valid-escape? ch2 ch3)))))

  (define css-number-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; http://www.w3.org/TR/css-syntax-3/#starts-with-a-number
    (lambda [ch1 ch2 ch3]
      (or (and (char? ch1)
               (char<=? #\0 ch1 #\9))
          (and (char? ch1) (char? ch2)
               (char=? ch1 #\.)
               (char<=? #\0 ch2 #\9))
          (and (char? ch1) (char? ch2)
               (or (char=? ch1 #\+) (char=? ch1 #\-))
               (or (char<=? #\0 ch2 #\9)
                   (and (char? ch3)
                        (char=? ch2 #\.)
                        (char<=? #\0 ch3 #\9)))))))

  (define css-scientific-notation? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-number
    (lambda [ch1 ch2 ch3]
      (and (char? ch1) (char? ch2)
           (char-ci=? ch1 #\e)
           (or (char<=? #\0 ch2 #\9)
               (and (or (char=? ch2 #\+) (char=? ch2 #\-))
                    (char? ch3)
                    (char<=? #\0 ch3 #\9))))))


  (define css-decimal-point? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-number
    (lambda [ch1 ch2]
      (and (char? ch1) (char? ch2)
           (char=? ch1 #\.)
           (char<=? #\0 ch2 #\9)))))

(module digitama/parser typed/racket ;;; http://www.w3.org/TR/css-syntax-3/#parsing
  (provide (all-defined-out))
  
  (require (submod ".." digitama/tokenizer))
  
  (define-type CSSExpr css)
  (define-type CSS-Rule (U css-style-rule css-at-rule))
  (define-type CSS-Simple-Block css-simple-block)
  (define-type CSS-Function css-function)
  (define-type CSS-Declaration css-declaration)
  (define-type CSS-Component-Value (U CSS-Token CSS-Simple-Block CSS-Function))

  (struct css-declaration ([name : Symbol] [values : (Listof CSS-Component-Value)] [important? : Boolean]) #:prefab)
  (struct css-simple-block ([open : Char] [components : (Listof CSS-Component-Value)] [close : Char]) #:prefab)
  (struct css-function ([name : Symbol] [arguments : (Listof CSS-Component-Value)]) #:prefab)
  
  (struct css-style-rule ([prelude : (Listof Any)] [block : CSS-Simple-Block]) #:prefab)
  (struct css-at-rule ([name : Keyword] [prelude : (Listof Any)] [block : (Option CSS-Simple-Block)]) #:prefab)

  (struct css ([rules : (Listof CSS-Rule)])
    #:prefab
    #:extra-constructor-name make-css)

  (define css-make-syntax-error : (-> (U EOF CSS-Token (Listof CSS-Token)) String Any * exn:fail:syntax)
    (lambda [tokens msgfmt . argl]
      (define message : String (if (null? argl) msgfmt (apply format msgfmt argl)))
      (define errobj : exn:fail:syntax
        (make-exn:fail:syntax message (current-continuation-marks)
                              (let ([token-list (cond [(eof-object? tokens) null] [(list? tokens) tokens] [else (list tokens)])])
                                (for/list : (Listof Syntax) ([token : CSS-Token (in-list token-list)])
                                  (datum->syntax #false 1 (list 'source 1 2 3 4))))))
      (log-message (current-logger) 'error 'css message errobj)
      errobj))

  (define css-read-token : (-> Input-Port (U EOF CSS-Token))
    (lambda [/dev/cssin]
      (define token (css-consume-token /dev/cssin))
      (cond [(not (or (css:whitespace? token) (css:comment? token))) token]
            [else (css-read-token /dev/cssin)])))
  
  ;;; http://www.w3.org/TR/css-syntax-3/#parser-entry-points
  (define css-parse-stylesheet : (-> Input-Port CSSExpr)
    ;;; http://www.w3.org/TR/css-syntax-3/#parse-a-stylesheet
    (lambda [/dev/cssin]
      (make-css (css-consume-rules /dev/cssin #true null))))

  (define css-parse-rules : (-> Input-Port (Listof CSS-Rule))
    ;;; http://www.w3.org/TR/css-syntax-3/#parse-a-list-of-rules
    (lambda [/dev/cssin]
      (css-consume-rules /dev/cssin #false null)))

  (define css-parse-declaration : (-> Input-Port Any)
    ;;; http://www.w3.org/TR/css-syntax-3/#declaration
    ;;; http://www.w3.org/TR/css-syntax-3/#parse-a-declaration
    (lambda [/dev/cssin]
      (define token (css-read-token /dev/cssin))
      (cond [(not (css:ident? token)) (css-make-syntax-error token "ident token is required")]
            [else (css-consume-declaration /dev/cssin token)])))

  (define css-parse-declarations : (-> Input-Port (Listof Any))
    ;;; http://www.w3.org/TR/css-syntax-3/#parse-a-list-of-declarations
    (lambda [/dev/cssin]
      (let consume-component : (Listof Any) ([components null])
        (define token (css-read-token /dev/cssin))
        (cond [(eof-object? token) (reverse components)]
              [else (consume-component (cons (css-consume-component-value /dev/cssin token)
                                             components))]))))
  
  (define css-parse-component-value : (-> Input-Port (U CSS-Component-Value exn))
    ;;; http://www.w3.org/TR/css-syntax-3/#parse-a-component-value
    (lambda [/dev/cssin]
      (define token (css-read-token /dev/cssin))
      (cond [(eof-object? token) (css-make-syntax-error token "unexpected end of stream")]
            [else (let ([retval (css-consume-component-value /dev/cssin token)])
                    (define next (css-read-token /dev/cssin))
                    (cond [(eof-object? next) retval]
                          [else (css-make-syntax-error next "too many component values found")]))])))

  (define css-parse-component-values : (-> Input-Port (Listof Any))
    ;;; http://www.w3.org/TR/css-syntax-3/#parse-a-list-of-component-values
    (lambda [/dev/cssin]
      (let consume-component : (Listof Any) ([components null])
        (define token (css-read-token /dev/cssin))
        (cond [(eof-object? token) (reverse components)]
              [else (consume-component (cons (css-consume-component-value /dev/cssin token)
                                             components))]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-rules : (-> Input-Port Boolean (Listof CSS-Rule) (Listof CSS-Rule))
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-list-of-rules0
    (lambda [css toplevel? rules0]
      (let consume-rules : (Listof CSS-Rule) ([rules rules0])
        (define token (css-read-token css))
        (cond [(eof-object? token) (reverse rules)]
              [(css:keyword? token) (consume-rules (css-rule-cons (css-consume-at-rule css token) rules))]
              [(css-CDO/CDC? token) (consume-rules (if toplevel? rules (css-rule-cons (css-consume-qualified-rule css token) rules)))]
              [else (consume-rules (css-rule-cons (css-consume-qualified-rule css token) rules))]))))

  (define css-consume-at-rule : (-> Input-Port css:keyword CSS-Rule)
    ;;; http://www.w3.org/TR/css-syntax-3/#at-rule
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-an-at-rule0
    (lambda [css at-token]
      (define-values (prelude maybe-simple-block) (css-consume-rule-item css null))
      (css-at-rule (css:keyword-datum at-token) prelude maybe-simple-block)))

  (define css-consume-qualified-rule : (-> Input-Port CSS-Token (U CSS-Rule exn))
    ;;; http://www.w3.org/TR/css-syntax-3/#qualified-rule
    (lambda [css token]
      (define-values (prelude maybe-simple-block) (css-consume-rule-item css (list token)))
      (cond [(css-simple-block? maybe-simple-block) (css-style-rule prelude maybe-simple-block)]
            [else (css-make-syntax-error (list token) "style rule should have a simple block")])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-rule-item : (-> Input-Port (Listof CSS-Token) (Values (Listof CSS-Component-Value) (Option CSS-Simple-Block)))
    ;;; http://www.w3.org/TR/css-syntax-3/#qualified-rule
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-qualified-rule
    (lambda [css components]
      (let consume-rule : (Values (Listof CSS-Component-Value) (Option CSS-Simple-Block))
        ([prelude : (Listof CSS-Component-Value) components]
         [simple-block : (Option CSS-Simple-Block) #false])
        (define token (css-read-token css))
        (cond [(or (eof-object? token) (css-delim-token=? token #\;)) (values (reverse prelude) simple-block)]
              [(css-delim-token=? token #\{) (values (reverse prelude) (css-consume-simple-block css #\{ #\}))]
              [else (consume-rule (cons (css-consume-component-value css token) prelude) simple-block)]))))

  (define css-consume-declaration : (-> Input-Port css:ident (U CSS-Declaration exn))
    ;;; http://www.w3.org/TR/css-syntax-3/#declaration
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-declaration
    (lambda [css pushback-token]
      (css-sequence->declaration pushback-token (in-port css-consume-token css))))
  
  (define css-consume-component-value : (-> Input-Port CSS-Token CSS-Component-Value)
    ;;; http://www.w3.org/TR/css-syntax-3/#component-value
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-component-value
    (lambda [css pushback-token]
      (cond [(css-delim-token=? pushback-token #\{) (css-consume-simple-block css #\{ #\})]
            [(css-delim-token=? pushback-token #\() (css-consume-simple-block css #\( #\))]
            [(css-delim-token=? pushback-token #\[) (css-consume-simple-block css #\[ #\])]
            [(css:function? pushback-token) (css-consume-function css (css:function-datum pushback-token))]
            [else pushback-token])))

  (define css-consume-components : (-> Input-Port (Listof CSS-Component-Value) Char (Listof CSS-Component-Value))
    (lambda [css components close-char]
      (define token (css-read-token css))
      (if (or (eof-object? token) (css-delim-token=? token close-char))
          (reverse components)
          (css-consume-components css (cons token components) close-char))))

  (define css-consume-simple-block : (-> Input-Port Char Char CSS-Simple-Block)
    ;;; http://www.w3.org/TR/css-syntax-3/#simple-block
    (lambda [css open-char close-char]
      (css-simple-block open-char (css-consume-components css null close-char) close-char)))

  (define css-consume-function : (-> Input-Port Symbol CSS-Function)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-function
    (lambda [css fname]
      (css-function fname (css-consume-components css null #\)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-rule-cons : (-> (U exn CSS-Rule) (Listof CSS-Rule) (Listof CSS-Rule))
    (lambda [rule rules]
      (cond [(exn? rule) rules]
            [else (cons rule rules)])))

  (define css-sequence->declaration : (-> css:ident (Sequenceof CSS-Token) (U CSS-Declaration exn))
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-declaration
    (lambda [id-token tokens]
      (if (not (css-delim-token=? (sequence-ref tokens 0) #\:))
          (css-make-syntax-error (list id-token (sequence-ref tokens 0))
                                 "missing colon in declaration: ~a"
                                 (css-token->datum (sequence-ref tokens 0)))
          (let-values ([(seulav important?) (for/fold ([rvalues : (Listof CSS-Component-Value) null]
                                                       [important? : Boolean #false])
                                                      ([token : CSS-Token (sequence-tail tokens 1)])
                                              (values (cons token rvalues)
                                                      (or (css-delim-token=? token #\!)
                                                          (and important? (css-ident-token=? token 'important)))))])
            (css-declaration (css:ident-datum id-token)
                             (reverse (if important? (cddr seulav) seulav)) important?)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-CDO/CDC? : (-> CSS-Token Boolean : #:+ (U <!-- -->))
    (lambda [token]
      (or (<!--? token)
          (-->? token))))

  (define css-delim-token=? : (-> Any Char Boolean : #:+ css:delim)
    (lambda [token ch]
      (and (css:delim? token)
           (char=? (css:delim-datum token) ch))))

  (define css-ident-token=? : (-> Any Symbol Boolean : #:+ css:ident)
    (lambda [token id]
      (and (css:ident? token)
           (or (symbol=? (css:ident-datum token) id)
               (string-ci=? (symbol->string (css:ident-datum token))
                            (symbol->string id)))))))

(require (submod "." digitama/tokenizer))
(require (submod "." digitama/parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test typed/racket
  (require (submod ".."))
  
  (define tamer.css (build-path (current-directory) 'up "tamer" "test.css"))
  (call-with-input-file tamer.css (λ [[in : Input-Port]] (time (read-css in tamer.css)))))
  