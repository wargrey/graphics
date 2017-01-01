#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require syntax/strip-context)

(require "digitama/grammar.rkt")

(define css-read
  (lambda [[/dev/cssin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks between `#lang` and contents
    (read-css-stylesheet /dev/cssin)))
  
(define css-read-syntax
  (lambda [[src #false] [/dev/cssin (current-input-port)]]
    (define lang.css
      (cond [(not (path? src)) 'lang.css]
            [else (let ([src.css (path-replace-extension (file-name-from-path src) "")])
                    (string->symbol (path->string (cond [(regexp-match? #px"\\.css$" src.css) src.css]
                                                        [else (path-replace-extension src.css ".css")]))))]))
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks before real css content
    (define-values (line col pos) (port-next-location /dev/cssin))
    (syntax-property
     (strip-context
      #`(module #,lang.css typed/racket/base
          (provide (all-defined-out))
          (provide (all-from-out css/syntax))
          
          (require css/syntax)
          (require (submod css/language-info runtime))
          
          (define #,lang.css : CSS-StyleSheet
            (let ([/dev/rawin (open-input-bytes #,(port->bytes /dev/cssin) '#,src)])
              (port-count-lines! /dev/rawin)
              (set-port-next-location! /dev/rawin #,line #,col #,pos)
              (read-css-stylesheet /dev/rawin)))
          (when (DrRacket?) #,lang.css)))
     'module-language
     '#(css/language-info css-language-info #false)
     #true)))

(define (css-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["CSS Sources" "*.css"])]
      [(drracket:default-extension) "css"]
      [(color-lexer) (dynamic-require '(submod css/language-info highlight) 'css-lexer)]
      [else default])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-language-info
  (lambda [argument]
    (λ [key default]
      (case key
        [(configure-runtime) '(#((submod css/language-info runtime) DrRacket? #true))]
        [else default]))))

(module runtime typed/racket/base
  (provide (all-defined-out))
  
  (define DrRacket? : (Parameterof Boolean) (make-parameter #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module highlight typed/racket/base
  (provide css-lexer)

  (require "digitama/digicore.rkt")
  (require "digitama/tokenizer.rkt")

  (define css-char->DrType : (-> Char Symbol)
    (lambda [delim]
      (case delim
        [(#\: #\, #\;) 'sexp-comment]
        [(#\+ #\- #\* #\/) 'symbol]
        [else 'constant])))
  
  (define css-id->DrType : (-> Symbol Boolean Symbol)
    (lambda [id func?]
      (case id
        [(inherit important true false) 'constant]
        [(initial unset revert) 'sexp-comment]
        [(only not and or) 'no-color]
        [else (cond [(and func?) 'parenthesis]
                    [(symbol-unreadable? id) 'no-color]
                    [else 'symbol])])))
  
  (define css-other->DrType : (-> CSS-Token Symbol)
    (lambda [token]
      (cond [(css:string? token) 'string]
            [(css:hash? token) 'hash-colon-keyword]
            [(css:@keyword? token) 'hash-colon-keyword]
            [(css:urange? token) 'constant]
            [(css:bad? token) 'error]
            [else 'other])))

  (define css-hlvalues : (-> CSS-Token Symbol (Option Symbol) (Values String Symbol (Option Symbol) (Option Integer) (Option Integer)))
    (lambda [t type subtype]
      (values "" type subtype (css-token-start t) (css-token-end t))))
  
  (define css-lexer : (-> Input-Port (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer)))
    (lambda [/dev/cssin]
      (define t : CSS-Syntax-Any (css-consume-token /dev/cssin (format "~a" (object-name /dev/cssin))))
      (cond [(eof-object? t) (values eof 'eof #false #false #false)]
            [(css:whitespace? t) (css-hlvalues t (if (string? (css:whitespace-datum t)) 'comment 'white-space) #false)]
            [(css:ident? t) (css-hlvalues t (css-id->DrType (css:ident-norm t) #false) #false)]
            [(css:function? t) (css-hlvalues t (css-id->DrType (css:function-norm t) #true) '|(|)]
            [(css:open? t) (css-hlvalues t 'parenthesis (string->symbol (string (css:delim-datum t))))]
            [(css:close? t) (css-hlvalues t 'parenthesis (string->symbol (string (css:close-datum t))))]
            [(css:delim? t) (css-hlvalues t (css-char->DrType (css:delim-datum t)) #false)]
            [(css-numeric? t) (css-hlvalues t (if (css-nan? t) 'error 'constant) #false)]
            [(css:url? t) (css-hlvalues t 'parenthesis '|(|)]
            [else (css-hlvalues t (css-other->DrType t) #false)]))))
