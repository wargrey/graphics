#lang at-exp typed/racket

(provide (all-defined-out))

;;; http://www.w3.org/Style/CSS/specs.en.html

(define read-css : (->* () (Input-Port Any) Any)
  ;;; http://www.w3.org/TR/css-syntax-3/#tokenization
  (lambda [[/dev/cssin (current-input-port)] [src #false]]
    (parameterize ([current-css-source src])
      (port-count-lines! /dev/cssin)
      (let parse : Void ()
        (match (css-consume-token /dev/cssin)
          [(? string? token) (printf "String: '~a'~n" token)]
          [(? symbol? token) (printf "Identity: ~a~n" token)]
          [(? list? token) (printf "Function: ~a~n" token)]
          [(? path-for-some-system? token) (printf "URL: ~a~n" token)]
          [(? hash? token) (printf "Hash: ~a~n" token)]
          [(? keyword? token) (printf "@Keyword: ~a~n" token)]
          [(? regexp? token) (printf "Match: ~a~n" token)]
          [(? char? token) (printf "Delimiter: ~a~n" token)]
          [(? eof-object?) (printf "EOF~n")]
          [_ (void)])
        (unless (eof-object? (peek-char /dev/cssin)) (parse))))))

(define string->css : (->* (String) (Any) Any)
  (lambda [css [src #false]]
    (read-css (open-input-string css) src)))

(define bytes->css : (->* (Bytes) (Any) Any)
  (lambda [css [src #false]]
    (read-css (open-input-bytes css) src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama typed/racket
  (provide (all-defined-out))

  (define-type CSS-Char (U EOF Char))

  (define current-css-source : (Parameterof Any) (make-parameter #false))

  (struct css ()
    #:type-name CSSExpr
    #:prefab
    #:extra-constructor-name make-css)

  (define-syntax (css-throw-bad-token stx)
    (syntax-case stx []
      [(_ /dev/cssin message)
       #'(let-values ([(line column position) (port-next-location /dev/cssin)])
           (raise (make-exn:fail:read message (current-continuation-marks)
                                      (list (make-srcloc (or (current-css-source)
                                                             (object-name /dev/cssin))
                                                         line column position #false)))))]
      [(_ /dev/cssin msgfmt argl ...)
       #'(css-throw-bad-token /dev/cssin (format msgfmt argl ...))]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-token : (-> Input-Port Any)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-token
    (lambda [/dev/cssin]
      (css-consume-whitespace-and-comment /dev/cssin)
      (define codepoint : (U Char EOF) (read-char /dev/cssin))
      (cond [(eof-object? codepoint) eof]
            [(css-char-name-prefix? codepoint) (css-consume-ident-token /dev/cssin (list codepoint))]
            [else (case codepoint
                    [(#\' #\") (css-consume-string-token /dev/cssin codepoint null)]
                    ;[(#\+ #\. #\-) (css-consume-number-token /dev/cssin codepoint null)]
                    [(#\^ #\$ #\| #\~ #\*) (css-consume-match-token /dev/cssin codepoint)]
                    [(#\#) (css-consume-hash-token /dev/cssin)]
                    [(#\@) (css-consume-keyword-token /dev/cssin)]
                    [else codepoint])])))

  (define css-consume-ident-token : (-> Input-Port (Listof Char) (U Symbol (List Symbol) Path-For-Some-System))
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-an-ident-like-token0
    (lambda [css chars]
      (define name : String (css-consume-name css chars))
      (define ch : CSS-Char (peek-char css))
      (cond [(and (char? ch) (char=? ch #\())
             (read-char css)
             (if (string-ci=? name "url")
                 (css-consume-url-token css #false)
                 (list (string->symbol name)))]
            [else (string->symbol name)])))
  
  (define css-consume-string-token : (-> Input-Port Char (Listof Char) String)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-string-token0
    (lambda [css quotation chars]
      (define ch : CSS-Char (read-char css))
      (cond [(or (eof-object? ch) (char=? ch quotation)) (list->string (reverse chars))]
            [(char=? ch #\newline) (css-throw-bad-token css "unexpected newline in string")]
            [(not (char=? ch #\\)) (css-consume-string-token css quotation (cons ch chars))]
            [else (match (peek-char css)
                    [(? eof-object?) (css-consume-string-token css quotation chars)]
                    [(or #\newline) (read-char css) (css-consume-string-token css quotation (cons ch chars))]
                    [_ (css-consume-string-token css quotation (cons (css-consume-escaped-char css) chars))])])))

  (define css-consume-url-token : (-> Input-Port (Option exn) Path-For-Some-System)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-url-token0
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-the-remnants-of-a-bad-url
    (lambda [css maybe-exn]
      (cond [(false? maybe-exn)
             (with-handlers ([exn? (λ [[e : exn]] (css-consume-url-token css e))])
               (css-consume-whitespace-and-comment css)
               (define ch : CSS-Char (read-char css))
               (cond [(or (eof-object? ch) (char=? ch #\))) (string->css-path "")]
                     [(or (char=? ch #\") (char=? ch #\'))
                      (define quoted-url : String (css-consume-string-token css ch null))
                      (css-consume-whitespace-and-comment css)
                      (define next : CSS-Char (read-char css))
                      (cond [(or (eof-object? next) (char=? next #\))) (string->css-path quoted-url)]
                            [else (css-throw-bad-token css "invalid char in URL token")])]
                     [else (let consume-unquoted-url-token : Path-For-Some-System ([chars (list ch)])
                             (css-consume-whitespace-and-comment css)      
                             (define next : CSS-Char (read-char css))
                             (cond [(or (eof-object? next) (char=? next #\)))
                                    (string->css-path (list->string (reverse chars)))]
                                   [(css-valid-escape? next (peek-char css))
                                    (consume-unquoted-url-token (cons (css-consume-escaped-char css) chars))]
                                   [(or (memq next '(#\\ #\" #\' #\()) (char-non-printable? next))
                                    (css-throw-bad-token css "invalid char in URL token")]
                                   [else (consume-unquoted-url-token (cons next chars))]))]))]
            [(exn:fail:contract? maybe-exn)
             ; WARNING: this also means the cleanup has been done
             (css-throw-bad-token css "empty url token")]
            [else (let cleanup : Nothing ()
                    (css-consume-whitespace-and-comment css)
                    (regexp-match #px".*?[\\)]" css) ; consuming all but #\\ or #\)
                    (define ch : CSS-Char (read-char css))
                    (cond [(or (eof-object? ch) (char=? ch #\))) (css-throw-bad-token css (exn-message maybe-exn))]
                          [(css-valid-escape? ch (peek-char css)) (read-char css) (cleanup)]
                          [else (cleanup)]))])))

  (define css-consume-hash-token : (-> Input-Port (U (HashTable Symbol (U 'id 'unrestricted)) Char))
    ;;; http://www.w3.org/TR/css-syntax-3/#hash-token-diagram
    (lambda [css]
      (define ch1 : CSS-Char (peek-char css 0))
      (define ch2 : CSS-Char (peek-char css 1))
      (define ch3 : CSS-Char (peek-char css 2))
      (if (or (css-char-name? ch1) (css-valid-escape? ch2 ch3))
          (hasheq (string->symbol (css-consume-name css (list #\#)))
                  (if (css-identifier-prefix? ch1 ch2 ch3) 'id 'unrestricted))
          #\#)))

  (define css-consume-keyword-token : (-> Input-Port (U Keyword Char))
    ;;; http://www.w3.org/TR/css-syntax-3/#at-keyword-token-diagram
    (lambda [css]
      (define ch1 : CSS-Char (peek-char css 0))
      (define ch2 : CSS-Char (peek-char css 1))
      (define ch3 : CSS-Char (peek-char css 2))
      (if (css-identifier-prefix? ch1 ch2 ch3)
          (string->keyword (css-consume-name css (list #\@)))
          #\#)))

  (define css-consume-match-token : (-> Input-Port Char (U Regexp Char))
    ;;; http://www.w3.org/TR/css-syntax-3/#include-match-token-diagram
    ;;; http://www.w3.org/TR/css-syntax-3/#column-token-diagram
    (lambda [css prefix]
      (define ch : CSS-Char (peek-char css))
      (cond [(and (char? ch) (char=? ch #\=))
             (read-char css)
             (regexp (string prefix ch))]
            [(and (char=? prefix #\|) (char? ch) (char=? ch #\|))
             (read-char css)
             (regexp (make-string 2 prefix))]
            [else prefix])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-whitespace-and-comment : (-> Input-Port Void)
    (lambda [css]
      (regexp-match #px"\\s*(/\\*.*?\\*/\\s*)*" css)
      (void)))
  
  (define css-consume-escaped-char : (->* (Input-Port) (Index Integer) Char)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-an-escaped-code-point
    (lambda [css [hexdigit-count 0] [chint 0]]
      (if (zero? hexdigit-count)
          (let ([esc : CSS-Char (read-char css)])
            (cond [(eof-object? esc) #\uFFFD]
                  [(char-hexdigit? esc) (css-consume-escaped-char css 1 (char->hexadecimal esc))]
                  [else esc]))
          (let ([hex : CSS-Char (peek-char css)])
            (cond [(or (eof-object? hex) (not (char-hexdigit? hex)) (> hexdigit-count 5)) ; #\U{1,6}
                   (when (and (char? hex) (char-whitespace? hex)) (read-char css))
                   (cond [(or (<= chint 0) (> chint #x10FFFF)) #\uFFFD] ; #\nul and max unicode
                         [(<= #xD800 chint #xDFFF) #\uFFFD] ; surrogate
                         [else (integer->char chint)])]
                  [else (read-char css)
                   (css-consume-escaped-char css
                                             (add1 hexdigit-count)
                                             (+ (* chint 16) (char->hexadecimal hex)))])))))

  (define css-consume-name : (-> Input-Port (Listof Char) String)
    ;;; http://www.w3.org/TR/css-syntax-3/#consume-a-name
    (lambda [css chars]
      (define ch : CSS-Char (peek-char css))
      (cond [(css-char-name? ch)
             (read-char css)
             (css-consume-name css (cons ch chars))]
            [(css-valid-escape? ch (peek-char css 1))
             (read-char css)
             (css-consume-name css (cons (css-consume-escaped-char css) chars))]
            [else (list->string (reverse chars))])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define char-non-printable? : (-> CSS-Char Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char<=? #\null ch #\backspace)
               (char=? ch #\vtab)
               (char<=? #\u000E ch #\u001F)
               (char=? ch #\rubout)))))

  (define char-hexdigit? : (-> CSS-Char Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char<=? #\0 ch #\9)
               (char-ci<=? #\a ch #\f)))))

  (define char->hexadecimal : (-> Char Integer)
    (lambda [hexch]
      (cond [(char<=? #\A hexch #\F) (- (char->integer hexch) 55)]
            [(char<=? #\a hexch #\f) (- (char->integer hexch) 87)]
            [else (- (char->integer hexch) 48)])))

  (define string->css-path : (-> String Path-For-Some-System)
    (lambda [uri]
      (string->some-system-path uri 'unix)))

  (define css-char-name-prefix? : (-> CSS-Char Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char<=? #\a ch #\z)
               (char<=? #\A ch #\Z)
               (char>=? ch #\u0080)
               (char=? #\_  ch)))))

  (define css-char-name? : (-> CSS-Char Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (css-char-name-prefix? ch)
               (char<=? #\0 ch #\9)
               (char=? #\- ch)))))
  
  (define css-valid-escape? : (-> CSS-Char CSS-Char Boolean : #:+ Char)
    ;;; http://www.w3.org/TR/css-syntax-3/#starts-with-a-valid-escape
    (lambda [\? ch]
      (and (char? \?)
           (char=? \? #\\)
           (or (eof-object? ch)
               (not (char=? ch #\newline))))))

  (define css-identifier-prefix? : (-> CSS-Char CSS-Char CSS-Char Boolean : #:+ Char)
    ;;; http://www.w3.org/TR/css-syntax-3/#would-start-an-identifier
    (lambda [ch1 ch2 ch3]
      (or (css-char-name-prefix? ch1)
          (css-valid-escape? ch1 ch2)
          (and (char? ch1)
               (char=? ch1 #\-)
               (css-char-name-prefix? ch2)
               (css-valid-escape? ch2 ch3)))))

  (define css-number-prefix? : (-> CSS-Char CSS-Char CSS-Char Boolean : #:+ Char)
    ;;; http://www.w3.org/TR/css-syntax-3/#starts-with-a-number
    (lambda [ch1 ch2 ch3]
      (or (and (char? ch1)
               (char<=? #\0 ch1 #\9))
          (and (char? ch1)
               (char? ch2)
               (char=? ch1 #\.)
               (char<=? #\0 ch2 #\9))
          (and (char? ch1)
               (char? ch2)
               (or (char=? ch1 #\+) (char=? ch1 #\-))
               (or (char<=? #\0 ch2 #\9)
                   (and (char? ch3)
                        (char=? ch2 #\.)
                        (char<=? #\0 ch3 #\9))))))))

(require (submod "." digitama))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test racket
  (require (submod ".."))

  @string->css{"CSS string test: \"\09ABCD\11FEDC0\""}
  @string->css{^= $= ~= |= ||}
  @string->css{url(www.w3.org/TR/css-syntax-3/#consume-a-url-token0)}

  (define tamer.css (build-path (current-directory) 'up "tamer" "test.css"))
  (call-with-input-file tamer.css (λ [in] (time (read-css in tamer.css)))))
  