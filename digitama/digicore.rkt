#lang at-exp typed/racket

(require (for-syntax racket/syntax))

(provide (except-out (all-defined-out) #%full-module plural term-colorize))

(define-type Info-Ref (->* [Symbol] [(-> Any)] Any))
(define-type Term-Color (Option (U String Symbol Byte)))
(define-type Racket-Main (-> String * Void))
(define-type Place-Main (-> Place-Channel Void))
(define-type SymbolTable (HashTable Symbol Any))
(define-type Help-Table (Listof (U (List Symbol String) (List* Symbol (Listof (List (Listof String) Any (Listof String)))))))

(require/typed/provide setup/getinfo
                       [get-info/full (-> Path-String
                                          [#:namespace (Option Namespace)]
                                          [#:bootstrap? Any]
                                          (Option Info-Ref))])

(require/typed/provide racket/fasl
                       [s-exp->fasl (case-> [-> Any Bytes]
                                            [-> Any Output-Port Void])]
                       [fasl->s-exp (-> (U Input-Port Bytes) Any)])

(define-syntax (#%full-module stx)
  #'(let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (resolved-module-path-name (cast rmp Resolved-Module-Path))))

(define-syntax (#%file stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path))])
      (cond [(path? full) full]
            [else (with-handlers ([exn:fail:contract? (const (current-directory))])
                    ((inst car Path (Listof Symbol)) (cast full (Pairof Path (Listof Symbol)))))])))

(define-syntax (#%module stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path))])
      (cond [(path? full) ((compose1 string->symbol path->string)
                           (path-replace-suffix (cast (file-name-from-path full) Path) ""))]
            [else (with-handlers ([exn:fail:contract? (λ _ '<anonymous>)])
                    (last (cdr (cast full (Pairof (U Path Symbol) (Listof Symbol))))))])))

(define-syntax (define-strdict stx)
  (syntax-case stx [:]
    [(_ id : Type)
     #'(define-strdict id : Type null)]
    [(_ id : Type init-vals)
     (with-syntax ([%id  (format-id #'id "%~a"  (syntax-e #'id))]  ; make-hash
                   [$id  (format-id #'id "$~a"  (syntax-e #'id))]  ; hash-ref
                   [$id? (format-id #'id "?~a"  (syntax-e #'id))]  ; hash-has-key?
                   [$id# (format-id #'id "$~a#" (syntax-e #'id))]  ; hash-count
                   [$id@ (format-id #'id "$~a@" (syntax-e #'id))]  ; hash-keys
                   [$id* (format-id #'id "$~a*" (syntax-e #'id))]  ; hash-values
                   [$id+ (format-id #'id "$~a+" (syntax-e #'id))]  ; hash-ref!
                   [$id- (format-id #'id "$~a-" (syntax-e #'id))]) ; hash-remove!
       #'(begin (define %id : (HashTable String Type) ((inst make-hash String Type) init-vals))
                (define ($id@) : (Listof String) ((inst hash-keys String Type) %id))
                (define ($id*) : (Listof Type) ((inst hash-values String Type) %id))
                (define ($id#) : Index ((inst hash-count String Type) %id))
                (define ($id? [key : String]) : Boolean (hash-has-key? %id key))
                (define ($id- [key : String]) : Void ((inst hash-remove! String Type) %id key))
                (define $id+ : (-> String (U Type (-> Type)) Type)
                  (lambda [key setval]
                    ((inst hash-ref! String Type) %id key (if (procedure? setval) setval (thunk setval)))))
                (define $id : (case-> [String -> Type] [String (U Type (-> Type)) -> Type])
                  (case-lambda
                    [(key) ((inst hash-ref String Type Type) %id key)]
                    [(key defval) ((inst hash-ref String Type Type) %id key (if (procedure? defval) defval (thunk defval)))]))))]))

(define-syntax (define-symdict stx)
  (syntax-case stx [:]
    [(_ id : Type)
     #'(define-symdict id : Type null)]
    [(_ id : Type init-vals)
     (with-syntax ([%id  (format-id #'id "%~a"  (syntax-e #'id))]  ; make-hasheq
                   [$id  (format-id #'id "$~a"  (syntax-e #'id))]  ; hash-ref
                   [$id? (format-id #'id "?~a"  (syntax-e #'id))]  ; hash-has-key?
                   [$id# (format-id #'id "$~a#" (syntax-e #'id))]  ; hash-count
                   [$id@ (format-id #'id "$~a@" (syntax-e #'id))]  ; hash-keys
                   [$id* (format-id #'id "$~a*" (syntax-e #'id))]  ; hash-values
                   [$id+ (format-id #'id "$~a+" (syntax-e #'id))]  ; hash-ref!
                   [$id- (format-id #'id "$~a-" (syntax-e #'id))]) ; hash-remove!
       #'(begin (define %id : (HashTable Symbol Type) ((inst make-hasheq Symbol Type) init-vals))
                (define ($id@) : (Listof Symbol) ((inst hash-keys Symbol Type) %id))
                (define ($id*) : (Listof Type) ((inst hash-values Symbol Type) %id))
                (define ($id#) : Index ((inst hash-count Symbol Type) %id))
                (define ($id? [key : Symbol]) : Boolean (hash-has-key? %id key))
                (define (!id+ [key : Symbol] [val : Type]) : Void ((inst hash-set! Symbol Type) %id key val))
                (define ($id- [key : Symbol]) : Void ((inst hash-remove! Symbol Type) %id key))
                (define $id+ : (-> Symbol (U Type (-> Type)) Type)
                  (lambda [key setval]
                    ((inst hash-ref! Symbol Type) %id key (if (procedure? setval) setval (thunk setval)))))
                (define $id : (case-> [Symbol -> Type] [Symbol (U Type (-> Type)) -> Type])
                  (case-lambda
                    [(key) ((inst hash-ref Symbol Type Type) %id key)]
                    [(key defval) ((inst hash-ref Symbol Type Type) %id key (if (procedure? defval) defval (thunk defval)))]))))]))

(define-syntax (define/extract-symtable stx)
  (syntax-case stx []
    [(_ (symtable-sexp ...) defines ...)
     (with-syntax ([symtable (format-id #'symtable "~a" (gensym 'symboltable))])
       #'(begin (define symtable : SymbolTable (cast (symtable-sexp ...) SymbolTable))
                (define/extract-symtable symtable defines ...)))]
    [(_ symtable defines ...)
     (with-syntax ([(extract ...)
                    (for/list ([def-idl (in-list (syntax->list #'(defines ...)))])
                      (syntax-case def-idl [: =]
                        [([renamed-id key] : Type)
                         #'(define renamed-id : Type (cast (hash-ref symtable 'key) Type))]
                        [([renamed-id key] : Type = def-exp)
                         #'(define renamed-id : Type (cast (hash-ref symtable 'key (thunk def-exp)) Type))]
                        [(id : Type)
                         #'(define id : Type (cast (hash-ref symtable 'id) Type))]
                        [(id : Type = def-exp)
                         #'(define id : Type (cast (hash-ref symtable 'id (thunk def-exp)) Type))]))])
       #'(begin extract ...))]))

(define-syntax (define-parameter/extract-info stx)
  (syntax-case stx []
    [(_ infodir defines ...)
     (with-syntax* ([info-ref (format-id #'info-ref "~a" (gensym 'inforef))]
                    [(extract ...)
                     (for/list ([def-idl (in-list (syntax->list #'(defines ...)))])
                       (syntax-case def-idl [: =]
                         [([renamed-id key] : Type = def-exp)
                          #'(define renamed-id : (Parameterof Type) (make-parameter (cast (info-ref 'key (thunk def-exp)) Type)))]
                         [([renamed-id key] : Type)
                          #'(define renamed-id : (Parameterof Type) (make-parameter (cast (info-ref 'key) Type)))]
                         [(id : Type = def-exp)
                          #'(define id : (Parameterof Type) (make-parameter (cast (info-ref 'id (thunk def-exp)) Type)))]
                         [(id : Type)
                          #'(define id : (Parameterof Type) (make-parameter (cast (info-ref 'id) Type)))]))])
       #'(begin (define info-ref : Info-Ref (cast (get-info/full infodir) Info-Ref))
                extract ...))]))

(define digicore.rkt : Path (#%file))

(define house-garden# : Char #\U1F3E1)
(define cat# : Char #\U1F408)
(define paw# : Char #\U1F43E)

(define macroscope# : Char #\U1F52C)
(define telescope# : Char #\U1F52D)
(define crystal-ball# : Char #\U1F52E)
(define pin# : Char #\U1F4CC)
(define backhand# : Char #\U1F449)
(define collision# : Char #\U1F4A5)
(define bomb# : Char #\U1F4A3)

(define book# : Char #\U1F4D4)
(define books# : Char #\U1F4DA)
(define open-book# : Char #\U1F4D6)
(define memo# : Char #\U1F4DD)
(define page# : Char #\U1F4C4)
(define bookmark# : Char #\U1F4D1)

(define beating-heart# : Char #\U1F493)
(define broken-heart# : Char  #\U1F494)
(define two-heart# : Char #\U1F495)
(define sparkling-heart# : Char #\U1F496)
(define growing-heart# : Char #\U1F497)
(define arrow-heart# : Char #\U1F498)
(define blue-heart# : Char #\U1F499)
(define green-heart# : Char #\U1F49A)
(define yellow-heart# : Char #\U1F49B)
(define purple-heart# : Char #\U1F49C)

(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/eof : Input-Port ((cast open-input-bytes (-> Bytes Symbol Input-Port)) #"" '/dev/null))
(define /dev/null : Output-Port ((cast open-output-nowhere (-> Symbol Output-Port)) '/dev/null))

(define immutable-guard : (-> Symbol (Path-String -> Nothing))
  (lambda [pname]
    (λ [pval] (error pname "Immutable Parameter: ~a" pval))))

(define-values (#{digimon-world : (Parameterof Nothing String)} #{digimon-gnome : (Parameterof Nothing String)})
  (let* ([file (path->string digicore.rkt)]
         [px.split (regexp-match #px"(.+)/([^/]+?)/[^/]+?/[^/]+?$" file)])
    (values (make-parameter (cadr (cast px.split (Listof String))) (immutable-guard 'digimon-world))
            (make-parameter (caddr (cast px.split (Listof String))) (immutable-guard 'digimon-gnome)))))

(define digimon-kuzuhamon : (Parameterof Nothing String) (make-parameter "Kuzuhamon" (immutable-guard 'digimon-kuzuhamon)))

(define-values (#{current-digimon : (Parameterof String String)} #{current-tamer : (Parameterof Nothing String)})
  (values (make-parameter (digimon-gnome))
          (make-parameter (or (getenv "USER") (getenv "LOGNAME") #| daemon |# "root"))))

(define digimon-system : (Parameterof Nothing Symbol)
  (make-parameter (match (path->string (system-library-subpath #false))
                    ;;; (system-type 'machine) maight lead to "forbidden exec /bin/uname" 
                    [(pregexp #px"solaris") 'illumos]
                    [(pregexp #px"linux") 'linux]
                    [_ (system-type 'os)])))

(define digimon-zone : (Parameterof Nothing Path)
  (make-derived-parameter current-digimon
                          (immutable-guard 'digimon-zone)
                          (curry build-path (digimon-world))))

(void (unless (member (digimon-world) (current-library-collection-paths))
        (current-library-collection-paths (cons (build-path (digimon-world)) (current-library-collection-paths)))
        (print-boolean-long-form #true)
        
        ;;; Do not change the name of compiled file path, here we only escapes from DrRacket's convention.
        ;;; Since compiler will check the bytecodes in the core collection which have already been compiled into <path:compiled/>.
        (use-compiled-file-paths (list (build-path "compiled")))))

(define-syntax (define-digimon-dirpath stx)
  (syntax-case stx []
    [(_ id ...)
     (with-syntax ([(digimon-id ...)
                    (for/list ([var (in-list (syntax->list #'(id ...)))])
                      (with-syntax ([id var]) (format-id #'id "digimon-~a" (syntax-e #'id))))])
       #'(begin (define digimon-id : (Parameterof Nothing Path)
                  (make-derived-parameter digimon-zone
                                          (immutable-guard 'digimon-id)
                                          (λ [[zonedir : Path]]
                                            (build-path zonedir (symbol->string 'id)))))
                ...))]))

(define-digimon-dirpath stone digitama digivice tamer village terminus)

(define path->digimon-modpath : (->* [Path-String] [Symbol] (U Module-Path (List 'submod Module-Path Symbol)))
  (lambda [modfile [submodule #false]]
    (define modpath : Module-Path
      (let ([modfile (simplify-path modfile)])
        (cond [(false? (regexp-match? #px"\\.rkt$" modfile)) modfile]
              [else `(lib ,(path->string (cast (find-relative-path (digimon-world) modfile) Path)))])))
    (if (symbol? submodule) `(submod ,modpath ,submodule) modpath)))

(define find-digimon-files : (-> (-> Path-String Boolean) Path-String [#:search-compiled? Boolean] (Listof Path-String))
  (lambda [predicate start-path #:search-compiled? [search-compiled? #false]]
    (define px.exclude : Regexp
      (let ([cmpls (for/list : (Listof String) ([p : Path (in-list (use-compiled-file-paths))])
                     (path->string (cast (file-name-from-path p) Path)))])
        (pregexp (if search-compiled? "/\\.git$"
                     (string-join #:before-first "/(\\.git|" #:after-last ")$" (remove-duplicates cmpls) "|")))))
    (filter predicate (sequence->list (in-directory start-path (curry (negate regexp-match?) px.exclude))))))

(define file-readable? : (-> Path-String Boolean)
  (lambda [p]
    (and (file-exists? p)
         (memq 'read (file-or-directory-permissions p))
         #true)))

(define timer-thread : (-> Positive-Real (-> Natural Any) [#:adjustment Real] Thread)
  (lambda [interval on-timer #:adjustment [adjustment -0.001]]
    (thread (thunk (for ([times : Natural (in-naturals)])
                     (sync/timeout/enable-break (max (+ interval adjustment) 0) never-evt)
                     (on-timer times))))))

(define call-as-normal-termination : (-> (-> Any) [#:atinit (-> Any)] [#:atexit (-> Any)] Void)
  (lambda [#:atinit [atinit/0 void] main/0 #:atexit [atexit/0 void]]
    (define status : Any
      (let/ec $?
        (parameterize ([exit-handler (cast $? (-> Any Any))])
          (exit (with-handlers ([exn? (lambda [[e : exn]] (and (eprintf "~a~n" (exn-message e)) 'FATAL))]
                                [void (lambda [e] (and (eprintf "(uncaught-exception-handler) => ~a~n" e) 'FATAL))])
                  (dynamic-wind (thunk (with-handlers ([exn? (lambda [[e : exn]] (atexit/0) (raise e))])
                                         (atinit/0)))
                                (thunk (main/0))
                                (thunk (atexit/0))))))))
    
    (define service-exit : (-> Any (Option Byte))
      (match-lambda
        ['FATAL 95]
        ['ECONFIG 96]
        ['ENOSERVICE 99]
        ['EPERM 100]
        [_ #false]))
      
    (cond [(exact-nonnegative-integer? status) (exit (min status 255))]
          [(service-exit status) => exit]
          [else (exit 0)])))

(define car.eval : (->* (Any) (Namespace) Any)
  (lambda [sexp [ns (current-namespace)]]
    ((inst car Any Any) (cast ((inst call-with-values Any) (thunk (eval sexp ns)) list) (Listof Any)))))

(define void.eval : (->* (Any) (Namespace) Void)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (thunk (eval sexp ns)) void)))

(define ~n_w : (-> Nonnegative-Integer String String)
  (lambda [count word]
    (format "~a ~a" count (plural count word))))

(define ~w=n : (-> Nonnegative-Integer String String)
  (lambda [count word]
    (format "~a=~a" (plural count word) count)))

(define echof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))

(define eechof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))

(define vim-colors : (HashTable String Byte)
  #hash(("black" . 0) ("darkgray" . 8) ("darkgrey" . 8) ("lightgray" . 7) ("lightgrey" . 7) ("gray" . 7) ("grey" . 7) ("white" . 15)
                      ("darkred" . 1) ("darkgreen" . 2) ("darkyellow" . 3) ("darkblue" . 4) ("brown" . 5) ("darkmagenta" . 5) ("darkcyan" . 6)
                      ("red" . 9) ("lightred" . 9) ("green" . 10) ("lightgreen" . 10) ("yellow" . 11) ("lightyellow" . 11)
                      ("blue" . 12) ("lightblue" . 12) ("magenta" . 13) ("lightmagenta" . 13) ("cyan" . 14) ("lightcyan" . 14)))

(define term-colorize : (-> Term-Color Term-Color (Listof Symbol) String String)
  (lambda [fg bg attrs content]
    (define color-code : (-> String [#:bgcolor? Boolean] String)
      (lambda [color #:bgcolor? [bg? #false]]
        (format "~a8;5;~a" (if bg? 4 3) (if (regexp-match? #px"\\d+" color) color (hash-ref vim-colors color)))))
    (regexp-replace #px"^(\\s*)(.+?)(\\s*)$" content
                    (format "\\1\033[~a;~a;~am\\2\033[0m\\3"
                            (string-replace (for/fold : String ([effects ""]) ([attr : Symbol (in-list attrs)])
                                              (case (string-downcase (format "~a" attr))
                                                [{"bold" "bright"} (string-append effects ";1")]
                                                [{"dim"} (string-append effects ";2")]
                                                [{"underline" "undercurl"} (string-append effects ";4")]
                                                [{"blink"} (string-append effects ";5")]
                                                [{"reverse" "inverse"} (string-append effects ";7")]
                                                [{"hidden" "password"} (string-append effects ";8")]
                                                [else (error 'tarminal-colorize "Unsupported Terminal Attribute: ~a" attr)]))
                                            "^;" "" #:all? #false)
                            (if (false? fg) 39 (color-code (string-downcase (format "~a" fg))))
                            (if (false? bg) 49 (color-code (string-downcase (format "~a" bg)) #:bgcolor? #true))))))

(define plural : (-> Nonnegative-Integer String String)
  (lambda [n word]
    (define dict : (HashTable String String) #hash(("story" . "stories") ("Story" . "Stories")))
    (cond [(= n 1) word]
          [else (hash-ref dict word (λ _ (string-append word "s")))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test racket
  (require (submod ".."))
  
  (for ([color (in-list '(grey red green blue yellow magenta cyan))])
    (define-values [darkcolor lightcolor] (values (format "dark~a" color) (format "light~a" color)))
    (echof "»»» 8/16 colors test:")
    (echof #:fgcolor color " ~a" color)
    (echof #:fgcolor darkcolor " ~a" darkcolor)
    (echof #:fgcolor lightcolor " ~a" lightcolor)
    (echof #:bgcolor color " ~a" color)
    (echof #:bgcolor darkcolor " ~a" darkcolor)
    (echof #:bgcolor lightcolor " ~a~n" lightcolor)
    (for ([effect (in-list '(bright dim underline blink reverse password))])
      (echof #:fgcolor darkcolor #:attributes (list effect) "dark:~a " effect)
      (echof #:fgcolor lightcolor #:attributes (list effect) "light:~a " effect))
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
      (newline))))
