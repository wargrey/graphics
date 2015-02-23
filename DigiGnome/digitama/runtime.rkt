#lang at-exp racket

(require racket/runtime-path)

(require scribble/srcdoc)

(require (for-syntax racket/syntax))

(require (for-doc scribble/manual))

(provide digimon-runtime-source echof eechof)

(generate-delayed-documents)
(define-runtime-path digimon-runtime-source (simplify-path "runtime.rkt"))

(provide [thing-doc /dev/null output-port? @{An instance of @racket[open-output-nowhere].}])
(define /dev/null (open-output-nowhere '/dev/null #true))

(provide [parameter-doc digimon-world (parameter/c (or/c path? path-string?)) dir-path @{The root directory of the project.}]
         [parameter-doc digimon-gnome (parameter/c path-string?) dir-name @{The meta subproject name of the project.}]
         [parameter-doc current-digimon (parameter/c path-string?) dir-name @{The name of current subproject.}]
         [parameter-doc digimon-zone (parameter/c (or/c path? path-string?)) dir-path @{The root directory of current subproject.}])
(define-values {digimon-world digimon-gnome}
  (let* ([dir (path->string digimon-runtime-source)]
         [px.split (regexp-match #px"(.+)/([^/]+?)/[^/]+?/[^/]+?$" dir)])
    (values (make-parameter (cadr px.split) (immutable-guard 'digimon-world))
            (make-parameter (caddr px.split) (immutable-guard 'digimon-gnome)))))

(define current-digimon (make-parameter (digimon-gnome)))
(define digimon-zone (make-derived-parameter current-digimon (immutable-guard 'digimon-zone) {λ [name] (build-path (digimon-world) name)}))

(void (unless (member (digimon-world) (current-library-collection-paths))
        (current-library-collection-paths (cons (digimon-world) (current-library-collection-paths)))))

(define-syntax {define-digimon-dirpath stx}
  (syntax-case stx []
    [{_ id {desc-expr ...}}
     (with-syntax ([digimon-id (format-id #'id "digimon-~a" (syntax-e #'id))])
       #'{begin (provide [parameter-doc digimon-id (parameter/c (or/c path? path-string?)) dir-path {desc-expr ...}])
                (define digimon-id (make-derived-parameter digimon-zone (immutable-guard 'digimon-id)
                                                           {λ [zonedir] (build-path zonedir (symbol->string 'id))}))})]))

(define-digimon-dirpath stone @{The @bold{stone} directory of current subproject})
(define-digimon-dirpath digitama @{The @italic{digitama} directory of current subproject.})
(define-digimon-dirpath digivice @{The @italic{digivice} directory of current subproject.})
(define-digimon-dirpath tamer @{The @italic{tamer} directory of current subproject.})
(define-digimon-dirpath terminus @{The @italic{terminus} directory of current subproject.})

(provide [proc-doc/names path->digimon-libpath (->* {(or/c path? path-string?)} {(or/c symbol? #false)} module-path?) {{modpath} {{submodule #false}}}
                         @{Produces a @racket[lib]-form to the @italic{modpath} installed in this collection.}])
(define path->digimon-libpath
  {lambda [modpath [submodule #false]]
    (define fname (path->string (find-relative-path (digimon-world) (simplify-path modpath))))
    (if (symbol? submodule) `(submod (lib ,fname) ,submodule) `(lib ,fname))})

(provide (contract-out 
          [find-digimon-files (-> (-> (or/c path? path-string?) boolean?) (or/c path? path-string?) (listof (or/c path? path-string?)))]))
(define find-digimon-files
  {lambda [predicate start-path]
    (define px.exclude (pregexp (string-join #:before-first "/(\\.git|" #:after-last ")$"
                                             (map (compose1 path->string file-name-from-path) (use-compiled-file-paths)) "|")))
    (for/fold ([ps null]) ([p (in-directory start-path {λ [p] (not (regexp-match? px.exclude p))})])
      (if (predicate p) (append ps (list p)) ps))})

(provide echof)
(define echof
  {lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))})

(provide eechof)
(define eechof
  {lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))})

{module digitama racket
  (provide (all-defined-out))
  
  (define immutable-guard
    {lambda [pname]
      (curry error pname "Immutable Parameter: ~a")})
  
  (define term-colorize
    {lambda [fg bg attrs content]
      (define color-code
        {λ [color #:bgcolor? [bg? #false]]
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
                              (if (false? bg) 49 (color-code (string-downcase (format "~a" bg)) #:bgcolor? #true))))})}

(require (submod "." digitama))

{module+ test
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
