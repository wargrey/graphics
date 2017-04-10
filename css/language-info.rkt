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
    (define magic (symbol->string (gensym "--")))
    (define-values (load metrics) (values (gensym "read-css-stylesheet") (gensym "metrics")))
    (define-values (requiring? DrRacket?) (values (gensym "requiring?") (gensym "DrRacket?")))
    (strip-context
     #`(module #,lang.css typed/racket/base
         (provide #,lang.css)
         (provide (all-from-out css/syntax))

         (require racket/pretty)
         (require racket/format)
         (require css/syntax)

         (define (#,load) : CSS-StyleSheet
           (define /dev/rawin : Input-Port (open-input-bytes #,(port->bytes /dev/cssin) '#,src))
           (port-count-lines! /dev/rawin)
           (set-port-next-location! /dev/rawin #,line #,col #,pos)
           (read-css-stylesheet /dev/rawin))
     
         (define-values (#,requiring? #,DrRacket?)
           (values (with-handlers ([exn? (λ _ #true)])
                     (not (equal? (vector-ref (current-command-line-arguments) 0) #,magic)))
                   (regexp-match? #px"DrRacket$" (find-system-path 'run-file))))

         (define-values (#,lang.css #,metrics)
           (let ([mem0 (current-memory-use)])
             (define-values (&lang.css cpu real gc) (time-apply #,load null))
             (values (car &lang.css)
                     (format "~a: memory: ~aMB cpu time: ~a real time ~a gc time ~a" '#,lang.css
                             (~r (/ (- (current-memory-use) mem0) 1024.0 1024.0) #:precision '(= 3))
                             cpu real gc))))

         (when (not #,requiring?) (if #,DrRacket? #,lang.css (printf "~a~n~a~n" (pretty-format #,lang.css) #,metrics)))
         (when #,DrRacket? (displayln #,metrics))

         (module configure-runtime typed/racket/base
           (current-command-line-arguments (vector #,magic)))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module highlight racket/base
  (provide css-lexer)

  (require "digitama/digicore.rkt")
  (require "digitama/tokenizer.rkt")

  (define css-char->DrType ;: (-> Char Symbol)
    (lambda [delim]
      (case delim
        [(#\: #\, #\;) 'sexp-comment]
        [(#\+ #\- #\* #\/) 'symbol]
        [else 'constant])))
  
  (define css-id->DrType ;: (-> Symbol Boolean Symbol)
    (lambda [id func?]
      (case id
        [(inherit important true false) 'constant]
        [(initial unset revert) 'sexp-comment]
        [(only not and or) 'no-color]
        [else (cond [(and func?) 'parenthesis]
                    [(symbol-unreadable? id) 'no-color]
                    [else 'symbol])])))
  
  (define css-other->DrType ;: (-> CSS-Token Symbol)
    (lambda [token]
      (cond [(css:string? token) 'string]
            [(css:hash? token) 'hash-colon-keyword]
            [(css:@keyword? token) 'hash-colon-keyword]
            [(css:urange? token) 'constant]
            [(css:bad? token) 'error]
            [else 'other])))

  (define css-hlvalues ;: (-> CSS-Token Symbol (Option Symbol) (Values String Symbol (Option Symbol) (Option Integer) (Option Integer)))
    (lambda [t type subtype]
      (values "" type subtype (css-token-start t) (css-token-end t))))
  
  (define css-lexer ;: (-> Input-Port (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer)))
    (lambda [/dev/cssin]
      (define t #|: CSS-Syntax-Any|# (css-consume-token /dev/cssin (format "~a" (object-name /dev/cssin))))
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
