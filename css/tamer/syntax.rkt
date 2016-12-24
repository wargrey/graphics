#lang typed/racket/gui

(require "time-run.rkt")
(require "../syntax.rkt")
(require "../digitama/misc.rkt")

(require racket/runtime-path)

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
                          (css-cascade (list tamer-sheet) (list tamer-root)
                                       css-declaration-parsers css-value-filter
                                       #false  #false)])
              (list preference for-children))))
header-preference

tamer-body
(time-run (let-values ([(preference for-children)
                        (css-cascade (list tamer-sheet) (list tamer-body tamer-root)
                                     css-declaration-parsers css-value-filter
                                     #false header-preference)])
            for-children))

(log-message css-logger 'debug "exit" eof)
(copy-port in (current-output-port))
