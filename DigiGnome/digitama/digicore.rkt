#lang at-exp typed/racket

(require (for-syntax racket/syntax))

(provide (except-out (all-defined-out) #%full-module plural term-colorize))

(define-type Term-Color (Option (U String Symbol Byte)))
(define-type Racket-Main (-> String * Void))
(define-type Info-Ref (->* {Symbol} {(-> Any)} Any))

(require/typed/provide setup/getinfo
                       [get-info/full {Path-String [#:namespace (Option Namespace)] [#:bootstrap? Any] -> (Option Info-Ref)}])

(define-syntax {#%full-module stx}
  #'(let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (resolved-module-path-name (cast rmp Resolved-Module-Path))))

(define-syntax {#%file stx}
  #'(let ([full (ann (#%full-module) (U Symbol Path))])
      (cond [(path? full) full]
            [else (with-handlers ([exn:fail:contract? {λ _ (current-directory)}])
                    ((inst car Path (Listof Symbol)) (cast full (Pairof Path (Listof Symbol)))))])))

(define-syntax {#%module stx}
  #'(let ([full (ann (#%full-module) (U Symbol Path))])
      (cond [(path? full) (string->symbol (path->string (path-replace-suffix (cast (file-name-from-path full) Path) "")))]
            [else (with-handlers ([exn:fail:contract? {λ _ '<anonymous>}])
                    (last (cdr (cast full (Pairof (U Path Symbol) (Listof Symbol))))))])))

(define digicore.rkt : Path (#%file))

(define-values {#{house-garden# : Char} #{cat# : Char} #{paw# : Char}} (values #\U1F3E1 #\U1F408 #\U1F43E))
(define-values {#{macroscope# : Char} #{telescope# : Char}} (values #\U1F52C #\U1F52D))
(define-values {#{book# : Char} #{books# : Char} #{open-book# : Char}} (values #\U1F4D4 #\U1F4DA #\U1F4D6))
(define-values {#{memo# : Char} #{page# : Char} #{bookmark# : Char}} (values #\U1F4DD #\U1F4C4 #\U1F4D1))
(define-values {#{heart# : Char} #{broken-heart# : Char} #{bomb# : Char} #{collision# : Char}} (values #\U1F49A #\U1F494 #\U1F4A3 #\U1F4A5))
(define-values {#{pin# : Char} #{crystal-ball# : Char} #{backhand# : Char}} (values #\U1F4CC #\U1F52E #\U1F449))

(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/eof : Input-Port ((cast open-input-bytes (-> Bytes Symbol Input-Port)) #"" '/dev/null))
(define /dev/null : Output-Port ((cast open-output-nowhere (-> Symbol Output-Port)) '/dev/null))

(define immutable-guard : (-> Symbol (Path-String -> Nothing))
  {lambda [pname]
    {λ [pval] (error pname "Immutable Parameter: ~a" pval)}})

(define-values {#{digimon-world : (Parameterof Nothing String)} #{digimon-gnome : (Parameterof Nothing String)}}
  (let* ([file (path->string digicore.rkt)]
         [px.split (regexp-match #px"(.+)/([^/]+?)/[^/]+?/[^/]+?$" file)])
    (values (make-parameter (cadr (cast px.split (Listof String))) (immutable-guard 'digimon-world))
            (make-parameter (caddr (cast px.split (Listof String))) (immutable-guard 'digimon-gnome)))))

(define digimon-system : (Parameterof Nothing Symbol)
  (make-parameter (match (path->string (system-library-subpath #false)) ;;; (system-type 'machine) leads to "forbidden exec /bin/uname" 
                    [{pregexp #px"solaris"} 'solaris]
                    [_ (system-type 'os)])))

(define current-digimon : (Parameterof String String) (make-parameter (digimon-gnome)))
(define current-tamer : (Parameterof Nothing String) (make-parameter (or (getenv "USER") (getenv "LOGNAME") "nobody")))

(define digimon-zone : (Parameterof Nothing Path)
  (make-derived-parameter current-digimon (immutable-guard 'digimon-zone) {λ [[name : String]] (build-path (digimon-world) name)}))

(void (unless (member (digimon-world) (current-library-collection-paths))
        (current-library-collection-paths (cons (build-path (digimon-world)) (current-library-collection-paths)))
        (print-boolean-long-form #true)
        
        ;;; Do not change the name of compiled file path, here we only escapes from DrRacket's convention.
        ;;; Since compiler will check the bytecodes in the core collection which have already been compiled into <path:compiled/>.
        (use-compiled-file-paths (list (build-path "compiled")))))

(define-syntax {define-digimon-dirpath stx}
  (syntax-case stx []
    [{_ id ...} (with-syntax ([{digimon-id ...} (map {λ [var] (with-syntax ([id var]) (format-id #'id "digimon-~a" (syntax-e #'id)))}
                                                     (syntax->list #'{id ...}))])
                  #'{begin (define digimon-id : (Parameterof Nothing Path)
                             (make-derived-parameter digimon-zone (immutable-guard 'digimon-id)
                                                     {λ [[zonedir : Path]] (build-path zonedir (symbol->string 'id))}))
                           ...})]))

(define-digimon-dirpath stone digitama digivice tamer village terminus)

(define path->digimon-modpath : (->* {Path-String} {Symbol} (U Module-Path (List 'submod Module-Path Symbol)))
  {lambda [modfile [submodule #false]]
    (define modpath : Module-Path (let ([modfile (simplify-path modfile)])
                                     (cond [(false? (regexp-match? #px"\\.rkt$" modfile)) modfile]
                                           [else `(lib ,(path->string (cast (find-relative-path (digimon-world) modfile) Path)))])))
    (if (symbol? submodule) `(submod ,modpath ,submodule) modpath)})

(define find-digimon-files : (-> (-> Path-String Boolean) Path-String [#:search-compiled? Boolean] (Listof Path-String))
  {lambda [predicate start-path #:search-compiled? [search-compiled? #false]]
    (define px.exclude : Regexp
      (let ([cmpls ((inst map String Path) {λ [p] (path->string (cast (file-name-from-path p) Path))} (use-compiled-file-paths))])
        (pregexp (string-join #:before-first "/(\\.git" (if search-compiled? null (remove-duplicates cmpls)) "|" #:after-last ")$"))))
    (filter predicate (sequence->list (in-directory start-path {λ [[p : Path-String]] (not (regexp-match? px.exclude p))})))})

(define file-readable? : (-> Path-String Boolean)
  {lambda [p]
    (and (file-exists? p)
         (memq 'read (file-or-directory-permissions p))
         #true)})

(define call-as-normal-termination : (-> (-> Any) Void)
  {lambda [main/0]
    (define-type SERVICE-EXIT (-> Any (Option Byte)))
    (define status : Any (let/ec $?
                           (parameterize ([exit-handler (cast $? (-> Any Any))])
                             (with-handlers ([exn? {λ [[e : exn]] (exit ({λ _ 'FATAL} (eprintf "~a~n" (exn-message e))))}]
                                             [void {λ [[e : Any]] (exit ({λ _ 'FATAL} (eprintf "(uncaught-exception-handler) => ~a~n" e)))}])
                               (exit (main/0))))))
    (define perror (curry eprintf "~a~n"))
    (define svc.startd : SERVICE-EXIT
      (match-lambda
        ['FATAL (and (perror 'SMF-EXIT-ERR-FATAL) 95)]
        ['ECONFIG (and (perror 'SMF-EXIT-ERR-CONFIG) 96)]
        ['ENOSERVICE (and (perror 'SMF-EXIT-ERR-NOSMF) 99)]
        ['EPERM (and (perror 'SMF-EXIT-ERR-PERM) 100)]
        [_ #false]))
      
    (cond [(exact-nonnegative-integer? status) (exit (min status 255))]
          [(and (symbol=? (digimon-system) 'solaris) (svc.startd status)) => exit]
          [else (exit 0)])})

(define car.eval : (->* (Any) (Namespace) Any)
  {lambda [sexp [ns (current-namespace)]]
    ((inst car Any Any) (cast ((inst call-with-values Any) {λ _ (eval sexp ns)} list) (Listof Any)))})

(define void.eval : (->* (Any) (Namespace) Void)
  {lambda [sexp [ns (current-namespace)]]
    (call-with-values {λ _ (eval sexp ns)} void)})

(define ~n_w : (-> Nonnegative-Integer String String)
  {lambda [count word]
    (format "~a ~a" count (plural count word))})

(define ~w=n : (-> Nonnegative-Integer String String)
  {lambda [count word]
    (format "~a = ~a" (plural count word) count)})

(define echof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  {lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))})

(define eechof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  {lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))})

(define term-colorize : (-> Term-Color Term-Color (Listof Symbol) String String)
  {lambda [fg bg attrs content]
    (define color-code : (-> String [#:bgcolor? Boolean] String)
      {λ [color #:bgcolor? [bg? #false]]
        (define colors : (HashTable String Byte)
          #hash{{"black" . 0} {"red" . 1} {"green" . 2} {"yellow" . 3} {"blue" . 4} {"magenta" . 5} {"cyan" . 6} {"white" . 7}
                              {"lightblack" . 8} {"lightred" . 9} {"lightgreen" . 10} {"lightyellow" . 11} {"lightblue" . 12}
                              {"lightmagenta" . 13} {"lightcyan" . 14} {"lightwhite" . 15}})
        (format "~a8;5;~a" (if bg? 4 3) (if (regexp-match? #px"\\d+" color) color (hash-ref colors color)))})
    (regexp-replace #px"^(\\s*)(.+?)(\\s*)$" content
                    (format "\\1\033[~a;~a;~am\\2\033[0m\\3"
                            (string-replace (for/fold : String ([effects ""]) ([attr : Symbol (in-list attrs)])
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

(define plural : (-> Nonnegative-Integer String String)
  {lambda [n word]
    (define dict : (HashTable String String) #hash{{"story" . "stories"} {"Story" . "Stories"}})
    (cond [(= n 1) word]
          [else (hash-ref dict word {λ _ (string-append word "s")})])})

{module* test racket
  (require (submod ".."))
  
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
