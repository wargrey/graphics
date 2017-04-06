#lang typed/racket

(require "configure.rkt")
(require "../syntax.rkt")

(require racket/runtime-path)

(define-runtime-path tamer/tamer.css "tamer.css")

(css-configure-@media)
(collect-garbage)
(define tamer-sheet : CSS-StyleSheet (time-run (read-css-stylesheet tamer/tamer.css)))
(define tamer-root : CSS-Subject (make-css-subject #:type 'root #:id '#:header))
(define tamer-body : CSS-Subject (make-css-subject #:type 'module #:id '#:root #:classes '(main)))

(define css-all-filter : (CSS-Cascaded-Value+Filter (HashTable Symbol Any) Symbol)
  (lambda [declared-values inherited-values env]
    (displayln env)
    (for/hash : (HashTable Symbol Any) ([desc-name (in-hash-keys declared-values)])
      (values desc-name (css-ref declared-values inherited-values desc-name)))))

(define css-declaration-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name !]
    (λ [[initial : (Listof Any)] [declared-values : (Listof CSS-Token)]]
      (values (map css-token->datum declared-values) null))))

tamer-root
(match-define (list preference header-preference)
  (time-run (let-values ([(preference for-children)
                          (css-cascade (list tamer-sheet) (list tamer-root)
                                       css-declaration-parsers css-all-filter
                                       #false 'root)])
              (list preference for-children))))
header-preference

tamer-body
(time-run (let-values ([(preference for-children)
                        (css-cascade (list tamer-sheet) (list tamer-body tamer-root)
                                     css-declaration-parsers css-all-filter
                                     header-preference 'body)])
            for-children))
