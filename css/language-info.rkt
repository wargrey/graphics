#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require syntax/strip-context)

(require "syntax.rkt")

(define css-read
  (lambda [[/dev/cssin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks between `#lang` and contents
    (read-css-stylesheet /dev/cssin)))
  
(define css-read-syntax
  (lambda [[src #false] [/dev/cssin (current-input-port)]]
    (define modname (if src (string->symbol (path->string (path-replace-extension (file-name-from-path src) ""))) 'lang.css))
    (strip-context
     #`(module #,modname racket/base
         ; TODO: why the result looks right but actually is not css-stylesheet?
         #,(css-read /dev/cssin)))))

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
