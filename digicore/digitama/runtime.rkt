#lang racket/base

(require racket/list)
(require racket/path)
(require racket/port)
(require racket/bool)
(require racket/string)
(require racket/runtime-path)

(provide (except-out (all-defined-out) compiled-syntax-source-directory))

(define-runtime-path compiled-syntax-source-directory ".")

(define /dev/null (open-output-nowhere '/dev/null #true))

(define-values {digimon-world digimon-kernel digimon-gnome}
  (let* ([dir (path->string (simplify-path compiled-syntax-source-directory))]
         [px.split (regexp-match #px"(.+)/([^/]+?)/[^/]+?/?$" dir)])
    (values (cadr px.split) (caddr px.split) "DigiGnome")))

(void (unless (member digimon-world (current-library-collection-paths))
        (current-library-collection-paths (cons digimon-world (current-library-collection-paths)))
  
        (putenv "digimon-world" digimon-world)
        (putenv "digimon-gnome" digimon-gnome)
        (putenv "digimon-kernel" digimon-kernel)))

(define digimon-setenv
  {lambda [digimon]
    (putenv "digimon-zone" (path->string (build-path digimon-world digimon)))
    (for ([pathname (in-list (list "digivice" "digitam" "tamer" "stone"))])
      (putenv (format "digimon-~a" pathname) (path->string (digimon-path pathname #:digimon digimon))))})

(define digimon-path
  {lambda [pathname #:digimon [diginame digimon-gnome]]
    (build-path digimon-world diginame pathname)})

(define path->digimon-libpath
  {lambda [pathname #:submodule [subname #false]]
    (define fname (path->string (find-relative-path digimon-world (simplify-path pathname))))
    (if (symbol? subname) `(submod (lib ,fname) ,subname) `(lib ,fname))})

(define term-colorize
  {lambda [fg bg attrs content]
    (define color-code
      {lambda [color #:bgcolor? [bg? #false]]
        (define colors (hash "black" 0 "red" 1 "green" 2 "yellow" 3 "blue" 4 "magenta" 5 "cyan" 6 "white" 7
                             "lightblack" 8 "lightred" 9 "lightgreen" 10 "lightyellow" 11 "lightblue" 12
                             "lightmagenta" 13 "lightcyan" 14 "lightwhite" 15))
        (format "~a8;5;~a" (if bg? 4 3)
                (cond [(regexp-match? #px"\\d+" color) color]
                      [else (hash-ref colors color)]))})
    (regexp-replace #px"^(\\s*)(.+?)(\\s*)$" content
                    (format "\\1\033[~a;~a;~am\\2\033[0m\\3"
                            (string-replace (for/fold ([effects ""]) ([attr (in-list attrs)])
                                              (case (string-downcase (format "~a" attr))
                                                [{"bold" "bright"} (string-append effects ";1")]
                                                [{"dim"} (string-append effects ";2")]
                                                [{"underline"} (string-append effects ";4")]
                                                [{"blink"} (string-append effects ";5")]
                                                [{"reverse" "inverse"} (string-append effects ";7")]
                                                [{"hidden" "password"} (string-append effects ";8")]
                                                [else (error 'tarminal-colorize "Unsupported Terminal Attribute: ~a" attr)]))
                                            "^;" "" #:all? #false)
                            (if (false? fg) 39 (color-code (string-downcase (format "~a" fg))))
                            (if (false? bg) 49 (color-code (string-downcase (format "~a" bg)) #:bgcolor? #true))))})

(define echof
  {lambda [#:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] msgfmt . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))})

(define eechof
  {lambda [#:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] msgfmt . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))})

{module+ test
  (require racket/format)
  
  (for ([color (in-list '{black red green blue yellow magenta cyan white})])
    (echof "»»» 8/16 colors test:")
    (echof #:fgcolor color " ~a" color)
    (echof #:fgcolor (format "light~a" color) " light~a" color)
    (echof #:bgcolor color " ~a" color)
    (echof #:bgcolor (format "light~a" color) " light~a~n" color)
    (for ([effect (in-list '{bright dim underline blink reverse password})])
      (echof #:fgcolor color #:attributes (list effect) "~a " effect)
      (echof #:fgcolor (format "light~a" color) #:attributes (list effect) "light:~a " effect))
    (newline))
  
  (echof "»»» 256 colors test:~n")
  (for ([color (in-range 1 257)])
    (define caption (~a (sub1 color) #:width 4 #:align 'right))
    (echof #:fgcolor (sub1 color) caption)
    (when (zero? (remainder color 32))
      (newline)))
  
  (for ([color (in-range 1 257)])
    (define caption (~a (sub1 color) #:width 4 #:align 'right))
    (echof #:bgcolor (sub1 color) caption)
    (when (zero? (remainder color 32))
      (newline)))}
