#lang at-exp typed/racket

(provide (all-defined-out))

;;; http://www.w3.org/Style/CSS/specs.en.html

(define read-css : (->* () (Input-Port) Any)
  (lambda [[/dev/cssin (current-input-port)]]
    (read /dev/cssin)))

(module digitama typed/racket
  (provide (all-defined-out))

  (struct css ()
    #:prefab
    #:extra-constructor-name make-css
    #:type-name CSSExpr)

  (define read-rest-comments : (-> Input-Port Void)
    (lambda [in]
      (void))))

(require (submod "." digitama))

(module* test racket
  (require (submod ".."))

  (require json)

  (define tamer.css (build-path (current-directory) 'up "stone" "tamer.css"))
  (with-input-from-file tamer.css
    (thunk (read-css)))

  @string->jsexpr{{"jsonrpc": "2.0", "result": 19, "id": 1}})
