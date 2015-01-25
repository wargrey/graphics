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
  {lambda [fg bg effects content]
    (let* ([colors (hash "black" "0" "red" "1" "green" "2" "yellow" "3" "blue" "4" "magenta" "5" "cyan" "6" "white" "7")]
           [fgcode (hash-ref colors (string-downcase (format "~a" fg)) #false)]
           [bgcode (hash-ref colors (string-downcase (format "~a" bg)) #false)])
      (regexp-replace #px"^(\\s*)(.+?)(\\s*)$" content
                      (format "\\1\033[~a~a\\2\033[m\\3"
                              (for/fold ([effects ""]) ([fmt (in-list effects)])
                                (case (string-downcase (format "~a" fmt))
                                  [{"off"} (string-append effects "0;")]
                                  [{"light"} (string-append effects "1;")]
                                  [{"underline"} (string-append effects "4;")]
                                  [{"blink"} (string-append effects "5;")]
                                  [{"reverse" "inverse"} (string-append effects "7;")]
                                  [{"invisible"} (string-append effects "8;")]
                                  [else effects]))
                              (cond [(and (false? fgcode) (false? bgcode)) "m"]
                                    [(false? fgcode) (string-append "4" bgcode "m")]
                                    [(false? bgcode) (string-append "3" fgcode "m")]
                                    [else (string-append "3" fgcode ";4" bgcode "m")]))))})

(define echof
  {lambda [#:fgcolor [fg #false] #:bgcolor [bg #false] #:effects [effects null] msgfmt . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg effects rawmsg) rawmsg))})

(define eechof
  {lambda [#:fgcolor [fg #false] #:bgcolor [bg #false] #:effects [effects null] msgfmt . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg effects rawmsg) rawmsg))})

{module+ test
  (echof "»»» ~a test~n" 'color)
  (for ([color (in-list '{black red green blue yellow magenta cyan white})])
    (for ([effect (in-list '{light underline blink reverse bold})])
      (echof #:fgcolor color #:effects (list effect) "fg:~a:~a " color effect)
      (eechof #:bgcolor color #:effects (list effect) "bg:~a:~a " color effect))
    (newline))}
