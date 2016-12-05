#lang typed/racket/gui

(require "../syntax.rkt")
(require "../digitama/selector.rkt")

(require digimon/format)
(require racket/runtime-path)

(define-syntax (time-run stx)
  (syntax-case stx []
    [(_ sexp ...)
     #'(let ([momery0 : Natural (current-memory-use)])
         (define-values (result cpu real gc) (time-apply (thunk sexp ...) null))
         (printf "memory: ~a cpu time: ~a real time: ~a gc time: ~a~n"
                 (~size (- (current-memory-use) momery0) 'Bytes)
                 cpu real gc)
         (car result))]))

(define-runtime-path tamer.css "../stone/tamer.css")
  
(define-values (width height) (get-display-size))
(define-values (in out) (make-pipe))
(define css-logger (make-logger 'css #false))
(define css (thread (thunk (let forever ([/dev/log (make-log-receiver css-logger 'debug)])
                             (match (sync/enable-break /dev/log)
                               [(vector level message urgent _)
                                (cond [(eof-object? urgent) (close-output-port out)]
                                      [else (fprintf out "[~a] ~a~n" level message)
                                            (forever /dev/log)])])))))

(current-logger css-logger)
(css-deprecate-media-type #true)
(default-css-media-type 'screen)
(default-css-media-preferences
  ((inst make-hash Symbol CSS-Media-Datum)
   (list (cons 'orientation 'landscape)
         (cons 'width (or width 0))
         (cons 'height (or height 0)))))
  
(collect-garbage)
(collect-garbage)
(collect-garbage)
(define tamer-sheet : CSS-StyleSheet (time-run (read-css-stylesheet tamer.css)))
(define tamer-root : CSS-Subject (make-css-subject #:type 'root #:id '#:header))
(define tamer-body : CSS-Subject (make-css-subject #:type 'module #:id '#:root #:classes '(main)))

(define css-declaration-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name !]
    (λ [[initial : (Listof CSS-Datum)] [declared-values : (Listof CSS-Token)]]
      (values (map css-token->datum declared-values) null))))

(define css-value-filter : (CSS-Cascaded-Value-Filter (Option (HashTable Symbol Any)))
  (lambda [declared-values default-values inherited-values]
    (for/hash : (HashTable Symbol Any) ([desc-name (in-hash-keys (css-values-descriptors declared-values))])
      (values desc-name (css-ref declared-values inherited-values desc-name)))))

tamer-sheet
tamer-root
(match-define (list preference header-preference)
  (time-run (let-values ([(preference for-children)
                          (css-cascade (list tamer-sheet) tamer-root css-declaration-parsers css-value-filter #false  #false)])
              (list preference for-children))))
header-preference

tamer-body
(time-run (let-values ([(preference for-children)
                        (css-cascade (list tamer-sheet) tamer-body css-declaration-parsers css-value-filter #false header-preference)])
            for-children))

(map (λ [[in : String]] : (Pairof String Integer)
       (let ([?complex-selectors (css-parse-selectors in)])
         (cond [(exn:css? ?complex-selectors) (cons in -1)]
               [else (let* ([s (car ?complex-selectors)]
                            [a (css-complex-selector-A s)]
                            [b (css-complex-selector-B s)]
                            [c (css-complex-selector-C s)])
                       (cons in (+ (* a 100) (* b 10) c)))])))
     (list "* + *"
           "li"
           "li::first-line"
           "ul li"
           "ul ol+li"
           "h1 + *[rel=up]"
           "ul ol li.red"
           "li.red.level"
           "#x34y"
           ":not(FOO)#s12"
           ".foo :matches(.bar, #baz)"
           "body #darkside [sith] p"))

(log-message css-logger 'debug "exit" eof)
(copy-port in (current-output-port))
