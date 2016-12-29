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
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks between `#lang` and contents
    (syntax-property
     (strip-context
      #`(module #,lang.css typed/racket/base
          (provide (all-defined-out))
          (provide (all-from-out css/syntax))
          
          (require css/syntax)
          (require (submod css/language-info runtime))
          
          (define #,lang.css : CSS-StyleSheet (read-css-stylesheet (open-input-bytes #,(port->bytes /dev/cssin) '#,src)))
          (when (DrRacket?) #,lang.css)))
     'module-language
     '#(css/language-info css-language-info #false)
     #true)))

(define css-info
  (lambda [key default use-default]
    (case key
      [(drracket:default-filters) '(["CSS Sources" "*.css"])]
      [(drracket:default-extension) "css"]
      [else (use-default key default)])))

(define css-language-info
  (lambda [argument]
    (λ [key default]
      (case key
        [(configure-runtime) '(#((submod css/language-info runtime) DrRacket? #true))]
        [else default]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module runtime typed/racket/base
  (provide (all-defined-out))
  
  (define DrRacket? : (Parameterof Boolean) (make-parameter #false)))
