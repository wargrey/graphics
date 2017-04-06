#lang typed/racket

(require "configure.rkt")
(require "../syntax.rkt")
(require "../image.rkt")

(require racket/runtime-path)

(define-runtime-path tamer/tamer.css "tamer.css")

(css-configure-@media)
(collect-garbage)
(define tamer-sheet : CSS-StyleSheet (time-run (read-css-stylesheet tamer/tamer.css)))
(define tamer-root : CSS-Subject (make-css-subject #:type 'root #:id '#:header))
(define tamer-syntax : CSS-Subject (make-css-subject #:type 'syntax #:id '#:syntax #:classes '(error maybe-error)))
(define tamer-reborn : CSS-Subject (make-css-subject #:type 'reborn #:id '#:reborn))

(define css-all-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name !]
    (λ [[initial : (Listof Any)] [declared-values : (Listof CSS-Token)]]
      (values (map css-token->datum declared-values) null))))

(define css-maybe-error-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-image-property-parsers suitcased-name)
        (and (eq? suitcased-name 'at-exp)
             (CSS<^> (list (<css:λracket>)
                           (<css:racket>)
                           (<css:block>)))))))

(define css-all-filter : (CSS-Cascaded-Value+Filter (HashTable Symbol Any) Symbol)
  (lambda [declared-values inherited-values type]
    (for/hash : (HashTable Symbol Any) ([desc-name (in-hash-keys declared-values)])
      (values desc-name (css-ref declared-values inherited-values desc-name)))))

(match-define (list $root *root)
  (time-run (let-values ([(root:values for-children)
                          (css-cascade (list tamer-sheet) (list tamer-root)
                                       css-all-parsers css-all-filter
                                       #false ':root)])
              (list root:values for-children))))
(cons tamer-root $root)

(match-define (list $syntax *syntax)
  (time-run (let-values ([($syntax *syntax)
                          (css-cascade (list tamer-sheet) (list tamer-syntax tamer-root)
                                       css-maybe-error-parsers css-all-filter
                                       *root 'syntax)])
              (list $syntax *syntax))))
(cons tamer-syntax $syntax)

(define $reborn
  (time-run (let-values ([($reborn *reborn)
                          (css-cascade (list tamer-sheet) (list tamer-reborn tamer-syntax tamer-root)
                                       css-all-parsers css-all-filter
                                       *syntax 'reborn)])
              $reborn)))
(cons tamer-reborn $reborn)
