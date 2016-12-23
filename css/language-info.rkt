#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require syntax/strip-context)

(require "syntax.rkt")

(define css-read
  (lambda [[/dev/cssin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks between `#lang` and contents
    (read-css-stylesheet /dev/cssin)))
  
(define css-read-syntax
  (lambda [[src #false] [/dev/cssin (current-input-port)]]
    (define modname
      (cond [(path? src) (string->symbol (path->string (path-replace-extension (file-name-from-path src) "")))]
            [else (if (symbol? src) src 'lang.css)]))
    (strip-context
     #`(module #,modname typed/racket/base
         (provide (all-defined-out))
         (provide (all-from-out css/syntax))
         
         (require css/syntax)
         
         (define css : CSS-StyleSheet (read-css-stylesheet (open-input-bytes #,(port->bytes /dev/cssin) '#,src)))))))

(define css-language-info
  (lambda [argument]
    (λ [key default]
      (case key
        ;[(configure-runtime) `(#(typed-racket/language-info configure ()))]
        [else default]))))

(define css-info
  (lambda [key default use-default]
    (case key
      [(drracket:default-filters) '(["CSS Sources" "*.css"])]
      [(drracket:default-extension) "css"]
      [else (use-default key default)])))
