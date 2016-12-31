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

  (require css/digitama/digicore)
  (require css/digitama/tokenizer)

  (define css-char->Dr.SubType : (-> (U CSS:Delim CSS:Close) (Option Symbol))
    (lambda [token]
      (cond [(css:close? token) (string->symbol (string (css:close-datum token)))]
            [(css:delim=<-? token '(#\( #\[ #\{)) (string->symbol (string (css:delim-datum token)))]
            [else #false])))
  
  (define css-id->Dr.Type : (-> Symbol Boolean Symbol)
    (lambda [id func?]
      (case id
        [(inherit important true false) 'constant]
        [(initial unset revert) 'sexp-comment]
        [(only not and or) 'no-color]
        [else (cond [(and func?) 'parenthesis]
                    [(symbol-unreadable? id) 'no-color]
                    [else 'symbol])])))
  
  (define css-other->Dr.Type : (-> CSS-Token Symbol)
    (lambda [token]
      (cond [(css:ident? token) (css-id->Dr.Type (css:ident-norm token) #false)]
            [(css:string? token) 'string]
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
            [(or (css:delim? t) (css:close? t)) (css-hlvalues t 'parenthesis (css-char->Dr.SubType t))]
            [(css-numeric? t) (css-hlvalues t 'constant #false)]
            [(css:function? t) (css-hlvalues t (css-id->Dr.Type (css:function-norm t) #true) '|(|)]
            [(css:url? t) (css-hlvalues t 'parenthesis '|(|)]
            [else (css-hlvalues t (css-other->Dr.Type t) #false)]))))
