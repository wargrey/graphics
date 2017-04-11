#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require racket/pretty)
(require racket/list)

(require syntax/strip-context)

(require "digitama/digicore.rkt")
(require "digitama/grammar.rkt")

(define css-read
  (lambda [[/dev/cssin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks between `#lang` and contents
    (read-css-stylesheet /dev/cssin)))
  
(define css-read-syntax
  (lambda [[src #false] [/dev/cssin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks before real css content
    (define-values (line column position) (port-next-location /dev/cssin))
    (define bytes-bag (port->bytes /dev/cssin))
    (define all-rules (read-css-stylesheet bytes-bag))
    (define all-namespaces (css-stylesheet-namespaces all-rules))
    (define lang.css
      (cond [(and (pair? all-namespaces) (not (eq? (caar all-namespaces) '||)))
             (string->symbol (string-append (symbol->string (caar all-namespaces)) ".css"))]
            [(path? src)
             (define src.css (path-replace-extension (file-name-from-path src) ""))
             (define path.css (if (regexp-match? #px"\\.css$" src.css) src.css (path-replace-extension src.css ".css")))
             (string->symbol (path->string path.css))]
            [else '|this should not happen| 'lang.css]))
    (strip-context
     #`(module #,lang.css typed/racket/base
         (provide #,lang.css)
         (provide (all-from-out css/syntax))
         
         (require racket/pretty)
         (require racket/format)
         (require css/syntax)
         
         (define (load-lang.css) : CSS-StyleSheet
           (define /dev/rawin : Input-Port (open-input-bytes #,bytes-bag '#,src))
           (port-count-lines! /dev/rawin)
           (set-port-next-location! /dev/rawin #,line #,column #,position)
           (read-css-stylesheet /dev/rawin))
         
         (define-values (#,lang.css metrics)
           (let ([mem0 (current-memory-use)])
             (define-values (&lang.css cpu real gc) (time-apply load-lang.css null))
             (values (car &lang.css)
                     (format "[~a]memory: ~aMB cpu time: ~a real time: ~a gc time: ~a" '#,lang.css
                             (~r (/ (- (current-memory-use) mem0) 1024.0 1024.0) #:precision '(= 3))
                             cpu real gc))))

         (module+ main
           (pretty-print-columns 160)

           (define drracket? : Boolean (regexp-match? #px"DrRacket$" (find-system-path 'run-file)))
           (if drracket? #,lang.css (printf "~a~n~a~n" (pretty-format #,lang.css) metrics))
           (when drracket? (displayln metrics)))
         
         #,@(for/list ([mod (in-list (css-stylesheet-modules all-rules))]
                  #:when (pair? (css-@racket-rule-sexps mod)))
              (read (open-input-string (format "(module+ ~a ~a)" (css-@racket-rule-name mod)
                                         (subbytes bytes-bag
                                                   (sub1 (css-token-start (first (css-@racket-rule-sexps mod))))
                                                   (css-token-end (last (css-@racket-rule-sexps mod))))))))))))

(define (css-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["CSS Sources" "*.css"])]
      [(drracket:default-extension) "css"]
      [(color-lexer) (dynamic-require '(submod css/language-info highlight) 'css-lexer)]
      [else default])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module highlight racket/base
  (provide css-lexer)

  (require "digitama/digicore.rkt")
  (require "digitama/tokenizer.rkt")

  (define css-char->drtype ;: (-> Char Symbol)
    (lambda [delim]
      (case delim
        [(#\: #\, #\;) 'sexp-comment]
        [(#\+ #\- #\* #\/) 'symbol]
        [else 'constant])))
  
  (define css-id->drtype ;: (-> Symbol Boolean Symbol)
    (lambda [id func?]
      (case id
        [(inherit important true false) 'constant]
        [(initial unset revert) 'sexp-comment]
        [(only not and or) 'no-color]
        [else (cond [(and func?) 'parenthesis]
                    [(symbol-unreadable? id) 'no-color]
                    [else 'symbol])])))
  
  (define css-other->drtype ;: (-> CSS-Token Symbol)
    (lambda [token]
      (cond [(css:string? token) 'string]
            [(css:hash? token) 'hash-colon-keyword]
            [(css:@keyword? token) 'hash-colon-keyword]
            [(css:urange? token) 'constant]
            [(css:bad? token) 'error]
            [else 'other])))

  (define css-hlvalues ;: (-> CSS-Token Symbol (Option Symbol) (Values String Symbol (Option Symbol) (Option Integer) (Option Integer)))
    (lambda [t type subtype mode]
      (values "" type subtype (css-token-start t) (css-token-end t) 0 (not mode))))
  
  (define css-lexer ;: (-> Input-Port (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer)))
    (lambda [/dev/drin offset mode]
      (define t #|: CSS-Syntax-Any|# (css-consume-token /dev/drin '/dev/drin))
      (cond [(eof-object? t) (values eof 'eof #false #false #false)]
            [(css:whitespace? t) (css-hlvalues t (if (string? (css:whitespace-datum t)) 'comment 'white-space) #false mode)]
            [(css:ident? t) (css-hlvalues t (css-id->drtype (css:ident-norm t) #false) #false mode)]
            [(css:function? t) (css-hlvalues t (css-id->drtype (css:function-norm t) #true) '|(| mode)]
            [(css:open? t) (css-hlvalues t 'parenthesis (string->symbol (string (css:delim-datum t))) mode)]
            [(css:close? t) (css-hlvalues t 'parenthesis (string->symbol (string (css:close-datum t))) mode)]
            [(css:delim? t) (css-hlvalues t (css-char->drtype (css:delim-datum t)) #false mode)]
            [(css-numeric? t) (css-hlvalues t (if (css-nan? t) 'error 'constant) #false mode)]
            [(css:url? t) (css-hlvalues t 'parenthesis '|(| mode)]
            [else (css-hlvalues t (css-other->drtype t) #false mode)]))))
