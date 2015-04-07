#lang at-exp racket

(require (for-syntax racket/syntax))

(require racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path digimon-runtime-source (simplify-path "runtime.rkt"))

(define-values {:house-garden: :cat: :paw: :book: :books: :page: :bookmark: :heart: :broken-heart: :collision: :pin: :backhand:}
  (values #\U1F3E1 #\U1F408 #\U1F43E #\U1F4D6 #\U1F4DA #\U1F4C4 #\U1F4D1 #\U1F49A #\U1F494 #\U1F4A5 #\U1F4CD #\U1F449))

(define /dev/stdin (current-input-port))
(define /dev/stdout (current-output-port))
(define /dev/stderr (current-error-port))
(define /dev/null (open-output-nowhere '/dev/null #true))

(define-values {digimon-world digimon-gnome}
  (let* ([dir (path->string digimon-runtime-source)]
         [px.split (regexp-match #px"(.+)/([^/]+?)/[^/]+?/[^/]+?$" dir)])
    (values (make-parameter (cadr px.split) (immutable-guard 'digimon-world))
            (make-parameter (caddr px.split) (immutable-guard 'digimon-gnome)))))

(define current-digimon (make-parameter (digimon-gnome)))
(define digimon-zone (make-derived-parameter current-digimon (immutable-guard 'digimon-zone) {λ [name] (build-path (digimon-world) name)}))

(void (unless (member (digimon-world) (current-library-collection-paths))
        (current-library-collection-paths (cons (digimon-world) (current-library-collection-paths)))
        
        ;;; Do not change the name of compiled file path, here we only escapes from DrRacket's convention.
        ;;; Since compiler will check the bytecodes in the core collection which have already been compiled into <path:compiled/>.
        (use-compiled-file-paths (list (build-path "compiled")))))

(define-syntax {define-digimon-dirpath stx}
  (syntax-case stx []
    [{_ id ...}
     (with-syntax ([{digimon-id ...} (map {λ [var] (with-syntax ([id var]) (format-id #'id "digimon-~a" (syntax-e #'id)))} (syntax->list #'{id ...}))])
       #'{begin (define digimon-id (make-derived-parameter digimon-zone (immutable-guard 'digimon-id)
                                                           {λ [zonedir] (build-path zonedir (symbol->string 'id))}))
                ...})]))

(define-digimon-dirpath stone digitama digivice tamer terminus)

(define path->digimon-libpath
  {lambda [modpath [submodule #false]]
    (define fname (path->string (find-relative-path (digimon-world) (simplify-path modpath))))
    (if (symbol? submodule) `(submod (lib ,fname) ,submodule) `(lib ,fname))})

(define find-digimon-files
  {lambda [predicate start-path #:search-compiled? [search-compiled? #false]]
    (define px.exclude (pregexp (string-join #:before-first "/(\\." #:after-last ")$"
                                             (cons "git" (cond [search-compiled? null]
                                                               [else (remove-duplicates (map (compose1 path->string file-name-from-path)
                                                                                             (use-compiled-file-paths)))])) "|")))
    (for/fold ([ps null]) ([p (in-directory start-path {λ [p] (not (regexp-match? px.exclude p))})])
      (if (predicate p) (cons p ps) ps))})

(define call-as-normal-termination
  {lambda [main/0]
    (define status (call/cc {λ [$?] (parameterize ([exit-handler $?]) (exit (main/0)))}))
    (exit (if (exact-nonnegative-integer? status) (min status 255) 0))})

(define ~n_w
  {lambda [count word]
    (format "~a ~a" count (plural count word))})

(define ~w=n
  {lambda [count word]
    (format "~a = ~a" (plural count word) count)})

(define echof
  {lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))})

(define eechof
  {lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))})

{module digitama racket
  (provide (all-defined-out))
  
  (define digimon-pathname/c (parameter/c path-string?))
  (define digimon-path/c (parameter/c (or/c path? path-string?)))
  
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
                              (if (false? bg) 49 (color-code (string-downcase (format "~a" bg)) #:bgcolor? #true))))})

  (define plural
    {lambda [n word]
      (define dict #hash{{"story" . "stories"} {"Story" . "Stories"}})
      (cond [(= n 1) word]
            [else (hash-ref dict word (string-append word "s"))])})}

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
