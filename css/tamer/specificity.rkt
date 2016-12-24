#lang typed/racket

(require "time-run.rkt")
(require "../syntax.rkt")
(require "../digitama/selector.rkt")

(define-values (in out) (make-pipe))
(define css-logger (make-logger 'css #false))
(define css (thread (thunk (let forever ([/dev/log (make-log-receiver css-logger 'debug)])
                             (match (sync/enable-break /dev/log)
                               [(vector level message urgent _)
                                (cond [(eof-object? urgent) (close-output-port out)]
                                      [else (fprintf out "[~a] ~a~n" level message)
                                            (forever /dev/log)])])))))

(current-logger css-logger)

(time-run (map (λ [[in : String]] : (Pairof String Integer)
                 (let ([?complex-selectors (css-parse-selectors in)])
                   (cond [(exn:css? ?complex-selectors) (cons in -1)]
                         [else (cons in (css-selector-list-specificity (car ?complex-selectors)
                                                                       (λ [[a : Natural] [b : Natural] [c : Natural]]
                                                                         (+ (* a 100) (+ (* b 10) c)))))])))
               (list "* + *"
                     "li"
                     "li::first-line"
                     "ul li"
                     "ul ol+li"
                     "h1 + *[rel=up]"
                     "ul ol li.red"
                     "li.red.level"
                     "module.main"
                     "module"
                     ":root"
                     "#1234"
                     ":not(FOO)#s12"
                     ".foo :matches(.bar, #baz)"
                     "body #darkside [sith] p")))
