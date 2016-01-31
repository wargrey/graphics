#lang at-exp typed/racket

(provide (all-defined-out) Term-Color vim-colors)
(provide (all-from-out "sugar.rkt" "format.rkt"))

(require (for-syntax racket/syntax))

@require{sugar.rkt}
@require{format.rkt}

(define-type Racket-Main (-> String * Void))
(define-type Place-Main (-> Place-Channel Void))
(define-type SymbolTable (HashTable Symbol Any))
(define-type Racket-Place-Status (Vector Fixnum Fixnum Fixnum Natural Natural Natural Natural Natural Fixnum Fixnum Natural Natural))
(define-type Help-Table (Listof (U (List Symbol String) (List* Symbol (Listof (List (Listof String) Any (Listof String)))))))

(require/typed/provide racket/fasl
                       [s-exp->fasl (case-> [-> Any Bytes]
                                            [-> Any Output-Port Void])]
                       [fasl->s-exp (-> (U Input-Port Bytes) Any)])

(require/typed/provide racket
                       [#:opaque SIGUP exn:break:hang-up?]
                       [#:opaque SIGTERM exn:break:terminate?]
                       [vector-set-performance-stats! (-> Racket-Place-Status (Option Thread) Void)])

(require/typed/provide racket/date
                       [current-date (-> date)] ;;; should be date*
                       [date-display-format (Parameterof Symbol)]
                       [date->string (-> date Boolean String)])

(define digicore.rkt : Path (#%file))

(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/eof : Input-Port ((cast open-input-bytes (-> Bytes Symbol Input-Port)) #"" '/dev/null))
(define /dev/null : Output-Port ((cast open-output-nowhere (-> Symbol Output-Port)) '/dev/null))

(define immutable-guard : (-> Symbol (Path-String -> Nothing))
  (lambda [pname]
    (λ [pval] (error pname "Immutable Parameter: ~a" pval))))

(define digimon-world : (Parameterof Nothing String)
  (make-parameter (path->string (simple-form-path (build-path digicore.rkt 'up 'up 'up)))
                  (immutable-guard 'digimon-world)))

(define digimon-gnome : (Parameterof Nothing String)
  (make-parameter (path->string (last (drop-right (filter path? (explode-path digicore.rkt)) 2)))
                  (immutable-guard 'digimon-gnome)))

(define digimon-kuzuhamon : (Parameterof Nothing String) (make-parameter "kuzuhamon" (immutable-guard 'digimon-kuzuhamon)))
(define current-digimon : (Parameterof String String) (make-parameter (digimon-gnome)))
(define current-tamer : (Parameterof Nothing String) (make-parameter (or (getenv "USER") (getenv "LOGNAME") #| daemon |# "root")))
(define digimon-system : (Parameterof Nothing Symbol)
  (make-parameter (match (path->string (system-library-subpath #false))
                    ;;; (system-type 'machine) might lead to "forbidden exec /bin/uname" 
                    [(pregexp #px"solaris") 'illumos]
                    [(pregexp #px"linux") 'linux]
                    [_ (system-type 'os)])))

(define digimon-zone : (Parameterof Nothing Path)
  (make-derived-parameter current-digimon
                          (immutable-guard 'digimon-zone)
                          (curry build-path (digimon-world))))

(define-parameter/extract-info (digimon-world)
  [pkg-institution : (Option String) = #false]
  [pkg-domain : String = "gyoudmon.org"]
  [pkg-idun : String = (current-tamer)])

(define all-digimons : (Listof String)
  (let* ([dirnames : (Listof String) (map path->string (directory-list (digimon-world) #:build? #false))]
         [top-ref : (Option Info-Ref) (get-info/full (digimon-world))]
         [candidates : (Listof String) (cond [(false? top-ref) null]
                                             [else (cast (top-ref 'setup-collects (λ _ dirnames)) (Listof String))])])
    (remove-duplicates (filter string? (for/list : (Listof (Option String)) ([digimon (in-list (cons (digimon-gnome) candidates))])
                                         (define info-ref (get-info/full (build-path (digimon-world) digimon) #:bootstrap? #true)) 
                                         (and (procedure? info-ref)
                                              ($info+ digimon (thunk info-ref))
                                              digimon))))))

(define #%info : (->* (Symbol) ((Option (-> Any)) #:digimon String) Any)
  (lambda [key [defval #false] #:digimon [digimon (current-digimon)]]
    (define (try-setinfo)
      (define info-ref (get-info/full (build-path (digimon-world) digimon) #:bootstrap? #true))
      (if (procedure? info-ref) info-ref (error '#%info "on info.rkt found for ~a" digimon)))
    (define info-ref : Info-Ref ($info+ digimon try-setinfo))
    (info-ref key (or defval (thunk (error '#%info "no info for ~a" key))))))

(define #%digimon : (->* () (String) String)
  (lambda [[digimon (current-digimon)]]
    (define name (#%info 'collection #:digimon digimon))
    (if (string? name) name digimon)))

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

(define digimon-tongue : (Parameterof Nothing Path)
  (make-derived-parameter digimon-stone
                          (immutable-guard 'digimon-tongue)
                          (λ [[stonedir : Path]]
                            (build-path stonedir "tongue"))))


(define path->digimon-modpath : (->* [Path-String] [(Option Symbol)] (U Module-Path (List 'submod Module-Path Symbol)))
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

(define timer-thread : (-> Positive-Real (-> Natural Any) Thread)
  (lambda [interval on-timer/do-task]
    (thread (thunk (let* ([i-ms : Integer (exact-round (* interval 1000.0))]
                          [first-time : Real (+ (current-inexact-milliseconds) i-ms)])
                     (for ([times : Natural (in-naturals)])
                       (sync/enable-break (alarm-evt (+ (* times i-ms) first-time)))
                       (on-timer/do-task times)))))))

(define tcp-server : (-> Index (Input-Port Output-Port Positive-Index -> Any) [#:max-allow-wait Natural] [#:localhost (Option String)]
                         [#:timeout (Option Positive-Real)] [#:on-error (exn -> Any)] [#:custodian Custodian]
                         (Values (-> Void) Positive-Index))
  (lambda [port-hit on-connection #:max-allow-wait [maxwait (processor-count)] #:localhost [ip #false]
           #:timeout [timeout #false] #:on-error [on-error void] #:custodian [server-custodian (make-custodian)]]
    (parameterize ([current-custodian server-custodian])
      (define /dev/tcp : TCP-Listener (tcp-listen port-hit maxwait #true ip))
      (define-values (localhost portno remote rport)
        ((cast tcp-addresses (-> TCP-Listener Boolean (Values String Positive-Index String Index)))
         /dev/tcp #true))
      (define saved-params-incaseof-transferring-continuation : Parameterization (current-parameterization))
      (define (wait-accept-handle-loop) : Void
        (parameterize ([current-custodian (make-custodian server-custodian)])
          (define close-session : (-> Void) (thunk (custodian-shutdown-all (current-custodian))))
          (with-handlers ([exn:fail:network? (λ [[e : exn]] (on-error e) (close-session))])
            (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break /dev/tcp))
            (thread (thunk ((inst dynamic-wind Any)
                            (thunk (unless (false? timeout)
                                     (define server : Thread (current-thread))
                                     (timer-thread timeout (λ [times] (if (zero? times) (break-thread server) (close-session))))))
                            (thunk (call-with-parameterization
                                    saved-params-incaseof-transferring-continuation
                                    (thunk (parameterize ([current-custodian (make-custodian)])
                                             (with-handlers ([exn? on-error])
                                               (on-connection /dev/tcpin /dev/tcpout portno))))))
                            (thunk (close-session)))))))
        (wait-accept-handle-loop))
      (thread wait-accept-handle-loop)
      (values (thunk (custodian-shutdown-all server-custodian)) portno))))

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

(define echof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))

(define eechof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))

(module digitama typed/racket
  (provide (all-defined-out))

  (require "sugar.rkt")
  
  (define-type Term-Color (Option (U String Symbol Byte)))

  (define-strdict info : Info-Ref)
  
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
                              (if (false? bg) 49 (color-code (string-downcase (format "~a" bg)) #:bgcolor? #true)))))))

(require (submod "." digitama))


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
