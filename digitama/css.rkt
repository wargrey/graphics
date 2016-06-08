#lang at-exp typed/racket

(provide (all-defined-out))

;;; http://www.w3.org/Style/CSS/specs.en.html

(define read-css : (->* () (Input-Port) CSSExpr)
  (lambda [[/dev/rawin (current-input-port)]]
    (define /dev/cssin : Input-Port (css-fallback-encode-input-port /dev/rawin))
    (dynamic-wind (thunk (port-count-lines! /dev/cssin))
                  (thunk (css-parse-stylesheet /dev/cssin))
                  (thunk (unless (eq? /dev/rawin /dev/cssin)
                           (close-input-port /dev/cssin))))))

(define string->css : (->* (String) (Any) CSSExpr)
  (lambda [css [src #false]]
    (read-css (open-input-string css (or src '/dev/stdin/string)))))

(define bytes->css : (->* (Bytes) (Any) CSSExpr)
  (lambda [css [src #false]]
    (read-css (open-input-bytes css (or '/dev/stdin/bytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama/tokenizer typed/racket ;;; https://drafts.csswg.org/css-syntax/#tokenization
  (provide (all-defined-out))

  (define-type CSS-Token css-token)
  (define-type CSS-Source css-srcloc)

  (struct css-srcloc ([in : Input-Port] [line : (Option Positive-Integer)] [col : (Option Natural)] [pos : (Option Positive-Integer)]))
  (struct css-token ([src : Any] [line : (Option Positive-Integer)] [column : (Option Natural)]
                                 [position : (Option Positive-Integer)] [span : (Option Natural)])
    #:prefab)

  (struct css:bad css-token ([type : Symbol] [reason : String]) #:prefab)
  (struct css:whitespace css-token () #:prefab)
  (struct css:comment css:whitespace ([content : Bytes]) #:prefab)
  
  (struct css:ident css-token ([datum : Symbol]) #:prefab)
  (struct css:function css-token ([datum : Symbol]) #:prefab)
  (struct css:keyword css-token ([datum : Keyword]) #:prefab)
  (struct css:hash css-token ([datum : Symbol] [type : (U 'id 'unrestricted)]) #:prefab)
  (struct css:delim css-token ([datum : Char]) #:prefab)
  (struct css:numeric css-token ([representation : String] [datum : Real]) #:prefab)
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
      [(_ src css:token argl ...)
       #'(let-values ([(start-position) (css-srcloc-pos src)]
                      [(line column position) (port-next-location (css-srcloc-in src))])
           (define token (css:token (object-name (css-srcloc-in src))
                                    (css-srcloc-line src) (css-srcloc-col src) start-position
                                    (and (integer? position) (integer? start-position)
                                         (max (- position start-position) 0))
                                    argl ...))
           (when (css:bad? token)
             (log-message (current-logger) 'warning 'css
                          (format "~a:~a:~a: ~a: ~a" (css-token-src token)
                                  (css-token-line token) (css-token-column token)
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
                 [(||? token) 'token]
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
  
  (define css-consume-ident-token : (-> CSS-Source Char (U css:ident css:function css:url css:u+range css:bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-unicode-range-token0
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-ident-like-token0
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
      
  (define css-consume-string-token : (-> CSS-Source Char (Listof Char) (U css:string css:bad))
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

  (define css-consume-numeric-token : (-> CSS-Source Char (U css:numeric css:delim css:bad))
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

  (define css-consume-url-token : (-> CSS-Source (U css:url css:bad))
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

  (define css-consume-unicode-range-token : (-> CSS-Source css:u+range)
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define-values (n rest) (css-consume-hexadecimal (css-srcloc-in srcloc) 6))
      (define-values (start end)
        (let consume-? : (Values Integer Integer) ([s : Integer n]
                                                   [e : Integer n]
                                                   [? : Integer rest])
          (cond [(zero? ?) (values s e)]
                [else (let ([ch : (U EOF Char) (peek-char css)])
                        (cond [(or (eof-object? ch) (not (char=? ch #\?))) (values s e)]
                              [else (read-char css) (consume-? (* s 16) (+ (* e 16) (char->hexadecimal #\f)) (sub1 ?))]))])))
      (cond [(not (= start end)) (make-token srcloc css:u+range start end)]
            [else (let ([ch1 (peek-char css 0)]
                        [ch2 (peek-char css 1)])
                    (cond [(and (char? ch1) (char=? ch1 #\-) (char-hexdigit? ch2))
                           (read-char css)
                           (define-values (end _) (css-consume-hexadecimal (css-srcloc-in srcloc) 6))
                           (make-token srcloc css:u+range start end)]
                          [else (make-token srcloc css:u+range start start)]))])))

  (define css-consume-hash-token : (-> CSS-Source (U css:hash css:delim))
    ;;; https://drafts.csswg.org/css-syntax/#hash-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (or (css-char-name? ch1) (css-valid-escape? ch1 ch2))
          (make-token srcloc css:hash
                      (string->symbol (css-consume-name (css-srcloc-in srcloc) (list #\#)))
                      (if (css-identifier-prefix? ch1 ch2 ch3) 'id 'unrestricted))
          (make-token srcloc css:delim #\#))))

  (define css-consume-keyword-token : (-> CSS-Source (U css:keyword css:delim))
    ;;; https://drafts.csswg.org/css-syntax/#at-keyword-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (css-identifier-prefix? ch1 ch2 ch3)
          (make-token srcloc css:keyword (string->keyword (css-consume-name (css-srcloc-in srcloc) (list #\@))))
          (make-token srcloc css:delim #\@))))

  (define css-consume-match-token : (-> CSS-Source Char (U css:match css:delim ||))
    ;;; https://drafts.csswg.org/css-syntax/#include-match-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#column-token-diagram
    (lambda [srcloc prefix]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch : (U EOF Char) (peek-char css))
      (cond [(and (char? ch) (char=? ch #\=)) (read-char css) (make-token srcloc css:match prefix)]
            [(and (char=? prefix #\|) (char? ch) (char=? ch #\|)) (read-char css) (make-token srcloc ||)]
            [else (make-token srcloc css:delim prefix)])))

  (define css-consume-cd-token : (-> CSS-Source Char (U <!-- css:delim css:numeric css:bad -->))
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

  (define css-consume-comment-token : (-> CSS-Source (U css:comment css:delim css:bad))
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

  (define css-consume-bad-url-remnants : (-> Input-Port css:bad css:bad)
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

  (define css-fallback-encode-input-port : (-> Input-Port Input-Port)
    ;;; https://drafts.csswg.org/css-syntax/#input-byte-stream
    ;;; https://drafts.csswg.org/css-syntax/#charset-rule
    (lambda [/dev/rawin]
      (define magic : (U EOF String) (peek-string 1024 0 /dev/rawin))
      (cond [(eof-object? magic) /dev/rawin]
            [(let ([charset? (regexp-match #px"^@charset \"(.*?)\";" magic)]) (and charset? (cadr charset?)))
             => (λ [v] (let ([charset (string-downcase v)])
                         (cond [(member charset '("utf-8" "utf-16be" "utf-16le")) /dev/rawin]
                               [else (with-handlers ([exn? (const /dev/rawin)])
                                       (reencode-input-port /dev/rawin charset
                                                            (string->bytes/utf-8 (format "@charset \u22~a\u22;" v))
                                                            #false (object-name /dev/rawin) #true))])))]
            [else /dev/rawin]))))

(module digitama/parser typed/racket ;;; https://drafts.csswg.org/css-syntax/#parsing
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

  (define css-make-syntax-error : (-> (U EOF CSS-Token (Listof CSS-Token)) String Any * exn)
    (lambda [tokens msgfmt . argl]
      (define message : String (if (null? argl) msgfmt (apply format msgfmt argl)))
      (define token-list : (Listof CSS-Token) (cond [(eof-object? tokens) null] [(list? tokens) tokens] [else (list tokens)]))
      (define errobj : exn
        (with-handlers ([exn? (λ [[e : exn]] e)])
          (raise-syntax-error 'exn:css:syntax message #false #false
                              (for/list : (Listof (Syntaxof Any)) ([token : CSS-Token (in-list (filter css-token? token-list))])
                                (datum->syntax #false
                                               (css-token->datum token)
                                               (list (css-token-src token)
                                                     (css-token-line token) (css-token-column token)
                                                     (css-token-position token) (css-token-span token)))))))
      (log-message (current-logger) 'error 'css message errobj)
      errobj))

  (define css-consume-token/skip-whitespace : (-> Input-Port (U EOF CSS-Token))
    (lambda [/dev/cssin]
      (define token (css-consume-token /dev/cssin))
      (cond [(not (or (css:whitespace? token) (css:comment? token))) token]
            [else (css-consume-token/skip-whitespace /dev/cssin)])))
  
  ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
  (define css-parse-stylesheet : (-> Input-Port CSSExpr)
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-stylesheet
    (lambda [/dev/cssin]
      (make-css (css-consume-rules /dev/cssin #true null))))

  (define css-parse-rules : (-> Input-Port (Listof CSS-Rule))
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-list-of-rules
    (lambda [/dev/cssin]
      (css-consume-rules /dev/cssin #false null)))

  (define css-parse-declaration : (-> Input-Port Any)
    ;;; https://drafts.csswg.org/css-syntax/#declaration
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-declaration
    (lambda [/dev/cssin]
      (define token (css-consume-token/skip-whitespace /dev/cssin))
      (cond [(not (css:ident? token)) (css-make-syntax-error token "ident token is required")]
            [else (css-consume-declaration /dev/cssin token)])))

  (define css-parse-declarations : (-> Input-Port (Listof Any))
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-list-of-declarations
    (lambda [/dev/cssin]
      (let consume-component : (Listof Any) ([components null])
        (define token (css-consume-token/skip-whitespace /dev/cssin))
        (cond [(eof-object? token) (reverse components)]
              [else (consume-component (cons (css-consume-component-value /dev/cssin token)
                                             components))]))))
  
  (define css-parse-component-value : (-> Input-Port (U CSS-Component-Value exn))
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-component-value
    (lambda [/dev/cssin]
      (define token (css-consume-token/skip-whitespace /dev/cssin))
      (cond [(eof-object? token) (css-make-syntax-error token "unexpected end of stream")]
            [else (let ([retval (css-consume-component-value /dev/cssin token)])
                    (define next (css-consume-token/skip-whitespace /dev/cssin))
                    (cond [(eof-object? next) retval]
                          [else (css-make-syntax-error next "too many component values found")]))])))

  (define css-parse-component-values : (-> Input-Port (Listof Any))
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-list-of-component-values
    (lambda [/dev/cssin]
      (let consume-component : (Listof Any) ([components null])
        (define token (css-consume-token/skip-whitespace /dev/cssin))
        (cond [(eof-object? token) (reverse components)]
              [else (consume-component (cons (css-consume-component-value /dev/cssin token)
                                             components))]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-rules : (-> Input-Port Boolean (Listof CSS-Rule) (Listof CSS-Rule))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-list-of-rules0
    (lambda [css toplevel? rules0]
      (let consume-rules : (Listof CSS-Rule) ([rules rules0])
        (define token (css-consume-token/skip-whitespace css))
        (cond [(eof-object? token) (reverse rules)]
              [(css:keyword? token) (consume-rules (css-rule-cons (css-consume-at-rule css token) rules))]
              [(css-CDO/CDC? token) (consume-rules (if toplevel? rules (css-rule-cons (css-consume-qualified-rule css token) rules)))]
              [else (consume-rules (css-rule-cons (css-consume-qualified-rule css token) rules))]))))

  (define css-consume-at-rule : (-> Input-Port css:keyword CSS-Rule)
    ;;; https://drafts.csswg.org/css-syntax/#at-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-at-rule0
    (lambda [css at-token]
      (define-values (prelude maybe-simple-block) (css-consume-rule-item css null))
      (css-at-rule (css:keyword-datum at-token) prelude maybe-simple-block)))

  (define css-consume-qualified-rule : (-> Input-Port CSS-Token (U CSS-Rule exn))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    (lambda [css token]
      (define-values (prelude maybe-simple-block) (css-consume-rule-item css (list token)))
      (cond [(css-simple-block? maybe-simple-block) (css-style-rule prelude maybe-simple-block)]
            [else (css-make-syntax-error (list token) "style rule should have a simple block")])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-rule-item : (-> Input-Port (Listof CSS-Token) (Values (Listof CSS-Component-Value) (Option CSS-Simple-Block)))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-qualified-rule
    (lambda [css components]
      (let consume-rule : (Values (Listof CSS-Component-Value) (Option CSS-Simple-Block))
        ([prelude : (Listof CSS-Component-Value) components]
         [simple-block : (Option CSS-Simple-Block) #false])
        (define token (css-consume-token/skip-whitespace css))
        (cond [(or (eof-object? token) (css-delim-token=? token #\;)) (values (reverse prelude) simple-block)]
              [(css-delim-token=? token #\{) (values (reverse prelude) (css-consume-simple-block css #\{ #\}))]
              [else (consume-rule (cons (css-consume-component-value css token) prelude) simple-block)]))))

  (define css-consume-declaration : (-> Input-Port css:ident (U CSS-Declaration exn))
    ;;; https://drafts.csswg.org/css-syntax/#declaration
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-declaration
    (lambda [css pushback-token]
      (css-sequence->declaration pushback-token (in-port css-consume-token css))))
  
  (define css-consume-component-value : (-> Input-Port CSS-Token CSS-Component-Value)
    ;;; https://drafts.csswg.org/css-syntax/#component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-component-value
    (lambda [css pushback-token]
      (cond [(css-delim-token=? pushback-token #\{) (css-consume-simple-block css #\{ #\})]
            [(css-delim-token=? pushback-token #\() (css-consume-simple-block css #\( #\))]
            [(css-delim-token=? pushback-token #\[) (css-consume-simple-block css #\[ #\])]
            [(css:function? pushback-token) (css-consume-function css (css:function-datum pushback-token))]
            [else pushback-token])))

  (define css-consume-components : (-> Input-Port (Listof CSS-Component-Value) Char (Listof CSS-Component-Value))
    (lambda [css components close-char]
      (define token (css-consume-token/skip-whitespace css))
      (if (or (eof-object? token) (css-delim-token=? token close-char))
          (reverse components)
          (css-consume-components css (cons token components) close-char))))

  (define css-consume-simple-block : (-> Input-Port Char Char CSS-Simple-Block)
    ;;; https://drafts.csswg.org/css-syntax/#simple-block
    (lambda [css open-char close-char]
      (css-simple-block open-char (css-consume-components css null close-char) close-char)))

  (define css-consume-function : (-> Input-Port Symbol CSS-Function)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css fname]
      (css-function fname (css-consume-components css null #\)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-rule-cons : (-> (U exn CSS-Rule) (Listof CSS-Rule) (Listof CSS-Rule))
    (lambda [rule rules]
      (cond [(exn? rule) rules]
            [else (cons rule rules)])))

  (define css-sequence->declaration : (-> css:ident (Sequenceof CSS-Token) (U CSS-Declaration exn))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-declaration
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
  (call-with-input-file tamer.css (λ [[in : Input-Port]] (time (read-css in)))))
  